#' @title dataFormat
#' @description
#' This function serves the role of formatting the data prior to the analysis.
#' @param data is a data matrix of the type matrix or data.frame.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param id id indicates the column number containing the examinee ID. The default is 1.
#' If no ID column is specified or if the specified column contains response data,
#' sequential IDs ("Student1", "Student2", etc.) will be generated and all columns
#' will be treated as response data.
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
dataFormat <- function(data, na = NULL, id = 1, Z = NULL, w = NULL,
                       response.type = NULL, CA = NULL) {
  # Check if the object is already formatted
  if (inherits(data, "exametrika")) {
    return(data)
  }
  if (!is.null(na) && is.na(na)) {
    na <- NULL
  }

  # Detect response type if not specified
  # Function to check if data is binary
  is_binary <- function(x, na_value = NULL) {
    # Remove both NA and specified na values
    x_clean <- x[!is.na(x)]
    if (!is.null(na_value)) {
      x_clean <- x_clean[x_clean != na_value]
    }

    if (length(x_clean) == 0) {
      return(TRUE)
    } # Empty data considered as binary

    if (is.factor(x)) {
      x_clean <- as.numeric(x_clean)
    } else {
      x_clean <- suppressWarnings(as.numeric(x_clean))
    }
    all(x_clean %in% c(0, 1))
  }

  # Check each column (correctly excluding ID column)
  check_cols <- setdiff(1:ncol(data), id)

  # Apply binary check to response columns only, passing na value
  is_all_binary <- all(sapply(data[, check_cols], is_binary, na_value = na))

  if (is.null(response.type)) {
    if (is_all_binary) {
      response.type <- "binary"
    } else {
      if (!is.null(CA)) {
        response.type <- "rated"
      } else {
        response.type <- "nominal"
      }
    }
  }

  # Validate manually specified response type
  if (!response.type %in% c("binary", "ordinal", "rated", "nominal")) {
    stop("response.type must be one of: binary, ordinal, rated, nominal")
  }

  data <- as.data.frame(unclass(data))
  data[data == na] <- NA

  # Store factor labels and convert factors to numeric
  CategoryLabel <- list()
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      # Store labels before conversion
      CategoryLabel[[col]] <- levels(data[[col]])
      # Convert to numeric (starting from 1)
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  # Check if U is either a matrix or a dataframe, otherwise stop the execution
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be matrix or data.frame")
  }

  # Function to check if a vector could be response data
  is_response_data <- function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) {
      return(FALSE)
    }

    # if contains character
    if (is.character(x)) {
      return(FALSE)
    }
    if (!all(suppressWarnings(!is.na(as.numeric(x_clean))))) {
      return(FALSE)
    }

    x_num <- suppressWarnings(as.numeric(x_clean))

    if (length(x_num) > 1) {
      sorted_x <- sort(x_num)
      if (all(diff(sorted_x) == 1) &&
        length(unique(x_num)) == length(x_num)) {
        return(FALSE)
      }
    }

    if (response.type == "binary") {
      return(all(x_num %in% c(0, 1)))
    } else {
      return(all(x_num == floor(x_num)) && all(x_num >= 0))
    }
  }

  # ID processing section
  if (id > ncol(data)) {
    ID <- paste0("Student", seq(1, NROW(data)))
    response.matrix <- data
  } else if (id != 1) {
    ID <- data[, id]
    ID <- as.factor(ID)
    if (any(duplicated(ID))) {
      duplicated_ids <- ID[duplicated(ID)]
      stop(paste("Duplicated IDs found:", paste(unique(duplicated_ids), collapse = ", ")))
    }
    response.matrix <- data[, -id]
  } else {
    potential_id <- data[, 1]
    potential_id_factor <- as.factor(potential_id)

    if (any(duplicated(potential_id_factor))) {
      if (!is.null(rownames(data))) {
        ID <- rownames(data)
        if (any(duplicated(ID))) {
          duplicated_ids <- ID[duplicated(ID)]
          stop(paste("Duplicated IDs found:", paste(unique(duplicated_ids), collapse = ", ")))
        }
      } else {
        ID <- paste0("Student", seq(1, NROW(data)))
      }
      response.matrix <- data
    } else {
      ID <- potential_id_factor
      response.matrix <- data[, -1]
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
    response.matrix <- response.matrix[, !is.na(sd.check)]
    Z <- Z[, !is.na(sd.check)]
    ItemLabel <- ItemLabel[!is.na(sd.check)]
    w <- w[!is.na(sd.check)]
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
      U[i, ] <- ifelse(response.matrix[i, ] == CA, 1, 0)
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
