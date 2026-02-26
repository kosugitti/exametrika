#' @title dataFormat
#' @description
#' This function serves the role of formatting the data prior to the analysis.
#' @param data is a data matrix of the type matrix or data.frame.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param id id indicates the column number containing the examinee ID.
#' If NULL (default), the first column is auto-detected as ID or response data.
#' If a column number is specified, that column is always used as the ID column.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param response.type Character string specifying the type of response data:
#'   "binary" for dichotomous data,
#'   "ordinal" for ordered polytomous data,
#'   "rated" for polytomous data with correct answers,
#'   "nominal" for unordered polytomous data.
#'   If NULL (default), the type is automatically detected.
#' @param CA A numeric vector specifying the correct answers for rated polytomous data.
#' Required when response.type is "rated".
#' @return
#' \describe{
#' \item{U}{For binary response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'  that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'  item j incorrectly.}
#' \item{Q}{For polytomous response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are non-negative integers. When input data is
#'  in factor format, the factor levels are converted to consecutive integers starting from 1.}
#' \item{ID}{The ID label given by the designated column or function.}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix. \eqn{z_{ij}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.
#' If the data contains NA values, -1 is assigned.}
#' \item{w}{Item weight vector}
#' \item{response.type}{Character string indicating the type of response data:
#'  "binary", "ordinal", "rated", or "nominal"}
#' \item{CategoryLabel}{List containing the original factor labels when polytomous responses
#'  are provided as factors. NULL if no factor data is present.}
#' \item{categories}{Numeric vector containing the number of response categories for each item.}
#' \item{CA}{For rated polytomous data, a numeric vector of correct answers. NULL for other types.}
#' }
#' @importFrom stats sd
#' @export
dataFormat <- function(data, na = NULL, id = NULL, Z = NULL, w = NULL,
                       response.type = NULL, CA = NULL) {
  # Check if the object is already formatted
  if (inherits(data, "exametrika")) {
    return(data)
  }
  if (!is.null(na) && is.na(na)) {
    na <- NULL
  }

  # Store original response type for later auto-detection
  original_response_type <- response.type

  # Validate manually specified response type (only if provided)
  if (!is.null(response.type) && !response.type %in% c("binary", "ordinal", "rated", "nominal")) {
    stop("response.type must be one of: binary, ordinal, rated, nominal")
  }

  data <- as.data.frame(unclass(data))
  data[data == na] <- NA

  # Check if U is either a matrix or a dataframe, otherwise stop the execution
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be matrix or data.frame")
  }

  # Check minimum number of rows (cases)
  if (nrow(data) < 10) {
    stop("Data must have at least 10 rows (cases) for reliable analysis")
  }

  # Helper function to check if a vector represents consecutive numbers
  is_consecutive_numbers <- function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    x_clean <- x[!is.na(x)]
    if (length(x_clean) <= 1) {
      return(TRUE)
    }

    x_num <- suppressWarnings(as.numeric(x_clean))
    if (any(is.na(x_num))) {
      return(FALSE)
    }

    x_sorted <- sort(unique(x_num))
    all(diff(x_sorted) == 1) && length(x_sorted) == length(x_num)
  }

  # Helper function to check if data looks like response data (has < 20 unique values)
  looks_like_response_data <- function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) {
      return(FALSE)
    }

    x_num <- suppressWarnings(as.numeric(x_clean))
    if (any(is.na(x_num))) {
      return(FALSE)
    }

    unique_vals <- unique(x_num)
    length(unique_vals) < 20 && all(unique_vals >= 0) && all(unique_vals == round(unique_vals))
  }

  # Phase 1: ID column identification and response matrix extraction
  if (!is.null(id)) {
    # Explicit ID column specified
    if (id > ncol(data)) {
      stop("ID column number exceeds the number of columns in data")
    }
    ID <- as.character(data[, id])
    response.matrix <- data[, -id, drop = FALSE]
  } else {
    # Auto-detect: analyze first column to determine if it's an ID column
    first_col <- data[, 1]

    if (is.factor(first_col)) {
      is_string_or_factor <- length(unique(first_col)) >= 20
    } else {
      is_string_or_factor <- is.character(first_col)
    }
    is_consecutive <- is_consecutive_numbers(first_col)

    if (is_string_or_factor || is_consecutive) {
      # First column is ID (strings, or unique consecutive integers)
      ID <- as.character(first_col)
      response.matrix <- data[, -1, drop = FALSE]
    } else {
      # First column treated as response data → auto-generate IDs
      ID <- paste0("Student", seq_len(nrow(data)))
      response.matrix <- data
      message(
        "No ID column detected. All columns treated as response data. ",
        "Sequential IDs (Student1, Student2, ...) were generated. ",
        "Use id= parameter to specify the ID column explicitly."
      )
      # Warn if first column has many unique values (might be an undetected ID column)
      n_unique_first <- length(unique(first_col[!is.na(first_col)]))
      if (n_unique_first > nrow(data) * 0.5 && n_unique_first < nrow(data)) {
        message(
          "Note: The first column ('", colnames(data)[1],
          "') has ", n_unique_first, " unique values out of ", nrow(data),
          " rows. If this is an ID column, specify id=1."
        )
      }
    }
  }

  # Check for duplicate IDs
  if (any(duplicated(ID))) {
    dup_ids <- unique(ID[duplicated(ID)])
    dup_details <- vapply(dup_ids, function(id) {
      rows <- which(ID == id)
      paste0("'", id, "' (rows ", paste(rows, collapse = ","), ")")
    }, character(1))
    stop(
      "Duplicated IDs found. Each ID must be unique.\n",
      paste(dup_details, collapse = "\n")
    )
  }

  # Store factor labels and convert factors to numeric (response columns only)
  CategoryLabel <- list()
  if (is.data.frame(response.matrix)) {
    for (col in names(response.matrix)) {
      if (is.factor(response.matrix[[col]])) {
        CategoryLabel[[col]] <- levels(response.matrix[[col]])
        response.matrix[[col]] <- as.numeric(response.matrix[[col]])
      }
    }
  }

  # Get Item-labels
  ItemLabel <- colnames(response.matrix)
  if (is.null(ItemLabel)) {
    ItemLabel <- paste0("Item", seq(1, NCOL(response.matrix)))
  }

  response.matrix <- as.matrix(response.matrix)
  response.matrix[is.na(response.matrix)] <- -1

  # Check if Z is indicator matrix
  if (!is.null(Z)) {
    if (!all(Z %in% c(0, 1))) {
      stop("The missing indicator matrix must contain only 0 or 1")
    }
    response.matrix <- ifelse(Z == 0, -1, response.matrix)
  }

  # Automatic response type detection (now moved to before validation)
  if (is.null(original_response_type)) {
    # Function to check if data is binary
    is_binary <- function(x, na_value = NULL) {
      # Remove both NA and specified na values
      x_clean <- x[!is.na(x) & x != -1] # Exclude missing indicator
      if (!is.null(na_value)) {
        x_clean <- x_clean[x_clean != na_value]
      }

      if (length(x_clean) == 0) {
        return(TRUE)
      } # Empty data considered as binary

      x_clean <- suppressWarnings(as.numeric(x_clean))
      all(x_clean %in% c(0, 1))
    }

    # Check if data appears to be ordinal (ordered categories)
    is_ordinal <- function(x, na_value = NULL) {
      x_clean <- x[!is.na(x) & x != -1] # Exclude missing indicator
      if (!is.null(na_value)) {
        x_clean <- x_clean[x_clean != na_value]
      }
      if (length(x_clean) == 0) {
        return(FALSE)
      }

      x_clean <- suppressWarnings(as.numeric(x_clean))
      if (any(is.na(x_clean))) {
        return(FALSE)
      }

      # Check if values are integers
      if (!all(x_clean == round(x_clean))) {
        return(FALSE)
      }

      unique_vals <- sort(unique(x_clean))

      # 2-category case: if not 0/1, stop with an error
      if (length(unique_vals) == 2) {
        if (!all(unique_vals %in% c(0, 1))) {
          stop("2-category data with non-binary values detected. Please specify response.type explicitly.")
        }
        return(FALSE) # 0/1 values are treated as binary
      }

      # Too many categories (>= 20)
      if (length(unique_vals) >= 20) {
        stop("Too many categories (>=20) for ordinal data. Please specify response.type explicitly.")
      }

      # 3 or more categories -> ordinal
      return(length(unique_vals) >= 3)
    }

    # Apply binary check to all response columns
    is_all_binary <- all(apply(response.matrix, 2, is_binary, na_value = na))

    if (is_all_binary) {
      response.type <- "binary"
    } else if (!is.null(CA)) {
      response.type <- "rated"
    } else {
      is_all_ordinal <- all(apply(response.matrix, 2, is_ordinal, na_value = na))

      if (is_all_ordinal) {
        response.type <- "ordinal"
      } else {
        response.type <- "nominal"
      }
    }
  } else {
    response.type <- original_response_type
  }

  if (response.type == "binary") {
    if (!all(response.matrix[response.matrix != -1] %in% c(0, 1))) {
      stop("For binary response type, data matrix can only contain the values 0, 1, NA, and the specified missing value")
    }
  } else if (response.type == "polytgomous") {
    # Check if all non-missing values are non-negative integers
    if (!all(response.matrix[response.matrix != -1] == floor(response.matrix[response.matrix != -1])) || any(response.matrix[response.matrix != -1] < 0)) {
      stop("For polytomous response type, data matrix must contain only non-negative integers, NA, and the specified missing value")
    }
  }

  ### This function finally makes each matrix as follow:
  # U is a matrix composed solely of 0s,1s and -1.
  # Z is the missing identifier matrix composed solely of 0s and 1s.
  if (!is.null(na)) {
    ## na value specified
    response.matrix <- ifelse(response.matrix == na, -1, response.matrix)
  }

  Z <- ifelse(response.matrix == -1, 0, 1)

  # If w is not specified, create a vector of 1s with length equal to the number of columns in U
  if (is.null(w)) {
    w <- rep(1, NCOL(response.matrix))
  }

  # check sd for each items
  sd.check <- apply(response.matrix, 2, function(x) sd(x, na.rm = TRUE))
  if (sum(is.na(sd.check)) != 0) {
    excluded_items <- ItemLabel[is.na(sd.check)]
    message(
      "The following items with no variance.Excluded from the data:\n",
      paste(excluded_items, collapse = ", "), "\n"
    )
    response.matrix <- response.matrix[, !is.na(sd.check), drop = FALSE]
    Z <- Z[, !is.na(sd.check), drop = FALSE]
    ItemLabel <- ItemLabel[!is.na(sd.check)]
    w <- w[!is.na(sd.check)]

    # Check if all items were excluded
    if (ncol(response.matrix) == 0) {
      stop("All items have no variance and were excluded. No valid response data remains.")
    }
  }

  # Warn about items with all missing values
  all_missing_cols <- which(apply(response.matrix, 2, function(x) all(x == -1)))
  if (length(all_missing_cols) > 0) {
    message(
      "Warning: The following items have all missing values: ",
      paste(ItemLabel[all_missing_cols], collapse = ", ")
    )
  }

  # Warn about items with zero variance (constant values, excluding all-missing)
  zero_var_cols <- which(!is.na(sd.check[seq_len(ncol(response.matrix))]) &
    sd.check[seq_len(ncol(response.matrix))] == 0)
  zero_var_cols <- setdiff(zero_var_cols, all_missing_cols)
  if (length(zero_var_cols) > 0) {
    message(
      "Warning: The following items have zero variance (constant values): ",
      paste(ItemLabel[zero_var_cols], collapse = ", ")
    )
  }

  # Warn about students with all missing responses
  all_missing_rows <- which(apply(response.matrix, 1, function(x) all(x == -1)))
  if (length(all_missing_rows) > 0) {
    message(
      "Warning: The following students have all missing responses: ",
      paste(ID[all_missing_rows], collapse = ", ")
    )
  }

  # Add category information
  categories <- apply(response.matrix, 2, function(x) length(unique(x[x != -1])))

  # Process category labels for each item
  for (i in seq_len(ncol(response.matrix))) {
    item_name <- ItemLabel[i]
    column_data <- response.matrix[, i]
    if (!item_name %in% names(CategoryLabel)) { # If not already processed as factor
      CategoryLabel[[item_name]] <- generate_category_labels(column_data, item_name)
    }
  }
  CA <- as.vector(unlist(CA))
  # check correct_answer
  if (response.type == "rated") {
    if (is.null(CA)) {
      stop("Correct Answer must be specified when polytype is rated")
    }
    if (length(CA) != ncol(response.matrix)) {
      stop("length of CA must match number of items")
    }

    for (i in 1:length(CA)) {
      if (!CA[i] %in% unique(response.matrix[, i][response.matrix[, i] != -1])) {
        stop(paste("CA for item", i, "is not a valid response category"))
      }
    }
    U <- matrix(NA, nrow = nrow(response.matrix), ncol = ncol(response.matrix))
    for (i in 1:nrow(response.matrix)) {
      U[i, ] <- ifelse(response.matrix[i, ] == -1, -1,
        ifelse(response.matrix[i, ] == CA, 1, 0)
      )
    }
  }


  # Create return list with appropriate matrix name based on response type
  ret.list <- list(
    ID = ID,
    ItemLabel = ItemLabel,
    Z = Z,
    w = w,
    response.type = response.type,
    categories = categories
  )
  # Add response matrix with appropriate name
  if (response.type == "binary") {
    ret.list$U <- response.matrix
  } else {
    ret.list$Q <- response.matrix
    ret.list$CategoryLabel <- CategoryLabel
    ret.list$CA <- CA
  }
  if (response.type == "rated") {
    ret.list$U <- U
  }

  # Return with appropriate class structure
  ret <- structure(ret.list, class = c("exametrika", "exametrikaData"))
  return(ret)
}

#' @title Long Format Data Conversion
#' @description
#' A function to reshape long data into a dataset suitable for exametrika.
#' @param data is a data matrix of the type matrix or data.frame. This must
#' contain at least three columns to identify the student, the item, and
#' the response. Additionally, it can include a column for the weight of
#' the items.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param Sid Specify the column number containing the student ID label vector.
#' @param Qid Specify the column number containing the Question label vector.
#' @param Resp Specify the column number containing the Response value vector.
#' @param w Specify the column number containing the weight vector.
#' @param response.type Character string specifying the type of response data:
#'   "binary" for dichotomous data,
#'   "ordinal" for ordered polytomous data,
#'   "rated" for polytomous data with correct answers,
#'   "nominal" for unordered polytomous data.
#'   If NULL (default), the type is automatically detected.
#' @param CA A numeric vector specifying the correct answers for rated polytomous data.
#' Required when response.type is "rated".
#' @return
#' \describe{
#' \item{U}{For binary response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'  that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'  item j incorrectly.}
#' \item{Q}{For polytomous response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are non-negative integers. When input data is
#'  in factor format, the factor levels are converted to consecutive integers starting from 1.}
#' \item{ID}{The ID label given by the designated column or function.}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix. \eqn{z_{ij}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.}
#' \item{w}{Item weight vector}
#' \item{response.type}{Character string indicating the type of response data:
#'  "binary", "ordinal", "rated", or "nominal"}
#' \item{CategoryLabel}{List containing the original factor labels when polytomous responses
#'  are provided as factors. NULL if no factor data is present.}
#' \item{categories}{Numeric vector containing the number of response categories for each item.}
#' \item{CA}{For rated polytomous data, a numeric vector of correct answers. NULL for other types.}
#' }
#' @export
#'
longdataFormat <- function(data, na = NULL,
                           Sid = NULL, Qid = NULL,
                           Resp = NULL, w = NULL,
                           response.type = NULL, CA = NULL) {
  # Check if already formatted
  if (inherits(data, "exametrika")) {
    return(data)
  }

  # Basic input checks
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be matrix or data.frame")
  }
  if (is.null(Sid)) {
    stop("Column number for identifier for Student must be specified.")
  }
  if (is.null(Qid)) {
    stop("Column number for identifier for Questions must be specified.")
  }
  if (is.null(Resp)) {
    stop("Column number for response pattern must be specified.")
  }

  # Extract vectors based on data type
  if (is.data.frame(data)) {
    Sid_vec <- data[[Sid]]
    Qid_vec <- data[[Qid]]
    Resp_vec <- data[[Resp]]
  } else {
    Sid_vec <- data[, Sid]
    Qid_vec <- data[, Qid]
    Resp_vec <- data[, Resp]
  }

  # Process Student IDs
  if (!is.numeric(Sid_vec)) {
    Sid_vec <- as.factor(Sid_vec)
    Sid_label <- unique(levels(Sid_vec))
    Sid_num <- as.numeric(Sid_vec)
  } else {
    Sid_num <- Sid_vec
    Sid_label <- unique(paste0("Student", Sid_num))
  }
  if (any(duplicated(Sid_vec))) {
    duplicated_ids <- Sid_vec[duplicated(Sid_vec)]
    stop(paste("Duplicated IDs found:", paste(unique(duplicated_ids), collapse = ",")))
  }

  # Process Question IDs
  if (!is.numeric(Qid_vec)) {
    Qid_vec <- as.factor(Qid_vec)
    Qid_label <- unique(levels(Qid_vec))
    Qid_num <- as.numeric(Qid_vec)
  } else {
    Qid_num <- Qid_vec
    Qid_label <- unique(paste0("Q", Qid_vec))
  }

  # Process Response vector and determine response type if not specified
  CategoryLabel <- NULL
  if (is.factor(Resp_vec)) {
    CategoryLabel <- levels(Resp_vec)
    Resp_vec <- as.numeric(Resp_vec)
  } else {
    Resp_vec <- as.numeric(Resp_vec)
  }

  if (is.null(response.type)) {
    if (all(Resp_vec[!is.na(Resp_vec)] %in% c(0, 1))) {
      response.type <- "binary"
    } else {
      response.type <- if (!is.null(CA)) "rated" else "nominal"
    }
  } else {
    response.type <- match.arg(
      response.type,
      c("binary", "ordinal", "rated", "nominal")
    )
  }

  # Validate responses based on response_type
  if (response.type == "binary" &&
    !all(Resp_vec[!is.na(Resp_vec)] %in% c(0, 1))) {
    stop("Binary response type specified but data contains non-binary values")
  }

  if (response.type == "rated") {
    if (is.null(CA)) {
      stop("CA must be specified when response.type is rated")
    }
    # Verify CA length matches number of unique items
    if (length(CA) != length(unique(Qid_vec))) {
      stop("Length of CA must match number of items")
    }
    # Verify each CA is a valid response category
    for (i in seq_along(CA)) {
      item_responses <- unique(Resp_vec[Qid_num == i])
      item_responses <- item_responses[!is.na(item_responses)]
      if (!CA[i] %in% item_responses) {
        stop(paste("CA for item", i, "is not a valid response category"))
      }
    }
  }

  # Create response matrix
  response_matrix <- matrix(NA, ncol = max(Qid_num), nrow = max(Sid_num))
  for (i in 1:length(Resp_vec)) {
    response_matrix[Sid_num[i], Qid_num[i]] <- Resp_vec[i]
  }

  # Handle NA values
  if (!is.null(na)) {
    response_matrix[response_matrix == na] <- -1
  }
  response_matrix[is.na(response_matrix)] <- -1

  # Create missing indicator matrix
  Z <- ifelse(response_matrix == -1, 0, 1)

  # Process weights
  if (is.null(w)) {
    w <- rep(1, NCOL(response_matrix))
  } else {
    if (is.data.frame(data)) {
      w_vec <- data[[w]]
    } else {
      w_vec <- data[, w]
    }
    w <- w_vec[unique(Qid_num)]
  }

  # Calculate categories for each item
  categories <- apply(response_matrix, 2, function(x) length(unique(x[x != -1])))

  # Create return list
  ret_list <- list(
    ID = Sid_label,
    ItemLabel = Qid_label,
    Z = Z,
    w = w,
    response.type = response.type,
    categories = categories
  )

  # Add response matrix with appropriate name
  if (response.type == "binary") {
    ret_list$U <- response_matrix
  } else {
    ret_list$Q <- response_matrix
    if (!is.null(CategoryLabel)) {
      ret_list$CategoryLabel <- list(response = CategoryLabel)
    }
    if (!is.null(CA)) {
      ret_list$CA <- CA
    }
  }

  # Return with appropriate class structure
  ret <- structure(ret_list,
    class = c("exametrika", "exametrikaData")
  )
  return(ret)
}
