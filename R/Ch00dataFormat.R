#' @title dataFormat
#' @description
#' This function serves the role of formatting the data prior to the analysis.
#' @param data is a data matrix of the type matrix or data.frame.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param id id indicates the column number containing the examinee ID. The default is 1.
#' If the answer pattern is contained in the first column, it is treated as if there is no ID vector.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return
#' \describe{
#' \item{U}{For binary response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'  that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'  item j incorrectly. If the data contains NA values, any value can be filled in the matrix U,
#'  represented by the following missing value index matrix Z. However, in this function,
#'  -1 is assigned.}
#' \item{Q}{For polytomous response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are non-negative integers. When input data is
#'  in factor format, the factor levels are converted to consecutive integers starting from 1, and
#'  the original factor labels are stored in factor_labels.}
#' \item{ID}{The ID label given by the designated column or function.}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix. \eqn{z_{ij}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.}
#' \item{w}{Item weight vector}
#' \item{response.type}{Character string indicating the type of response data:
#'  "binary" for binary responses or "polytomous" for polytomous responses.}
#' \item{factor_labels}{List containing the original factor labels when polytomous responses
#'  are provided as factors. NULL if no factor data is present.}
#' \item{categories}{Numeric vector containing the number of response categories for each item.
#'  For binary data, all elements are 2.}
#' }
#' @importFrom stats sd
#' @export
#'
dataFormat <- function(data, na = NULL, id = 1, Z = NULL, w = NULL) {
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
  response.type <- if (is_all_binary) "binary" else "polytomous"

  data <- as.data.frame(unclass(data))
  data[data == na] <- NA
  # Store factor labels if exists
  factor_labels <- list()
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      factor_labels[[col]] <- levels(data[[col]])
      # Convert to numeric (starting from 1)
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  # Check if U is either a matrix or a dataframe, otherwise stop the execution
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be matrix or data.frame")
  }

  # get ID vector
  if (is.null(rownames(data))) {
    ID <- rownames(data)
  } else {
    ID <- data[, id]
  }

  # Check if first column is data or ID
  # First, try to convert the ID column to numeric
  id_numeric <- suppressWarnings(as.numeric(ID))

  # Function to check if a vector could be response data
  is_response_data <- function(x) {
    if (all(is.na(x))) {
      return(FALSE)
    }
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) {
      return(FALSE)
    }

    # Check if all non-NA values are numeric
    if (!all(suppressWarnings(!is.na(as.numeric(x_clean))))) {
      return(FALSE)
    }

    x_num <- suppressWarnings(as.numeric(x_clean))

    if (response.type == "binary") {
      return(all(x_num %in% c(0, 1)))
    } else {
      return(all(x_num == floor(x_num)) && all(x_num >= 0))
    }
  }

  # Check if ID column appears to be response data
  if (is_response_data(ID)) {
    ID <- paste0("Student", seq(1, NROW(data)))
    response.matrix <- data
  } else {
    response.matrix <- data[, -id]
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
  # U is a matrix composed solely of 0s,1s and NA.
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
    # Add factor labels only for polytomous data
    if (length(factor_labels) > 0) {
      ret.list$factor_labels <- factor_labels
    }
  }

  # Return with appropriate class structure
  ret <- structure(ret.list, class = c("exametrika", "exametrikaData"))
  return(ret)
}


#' @title dataFormat for long-type data
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
#' @param response.type type of response data: "binary" or "polytomous" (can be abbreviated as "poly").
#' If NULL, the type is automatically determined from the data.
#' @return
#' \describe{
#' \item{U}{For binary response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'  that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'  item j incorrectly.}
#' \item{Q}{For polytomous response data. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are non-negative integers. When input data is
#'  in factor format, the factor levels are converted to consecutive integers starting from 1, and
#'  the original factor labels are stored in factor_labels.}
#' \item{ID}{The ID label given by the designated column or function.}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix. \eqn{z_{ij}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.}
#' \item{w}{Item weight vector}
#' \item{response.type}{Character string indicating the type of response data:
#'  "binary" for binary responses or "polytomous" for polytomous responses.}
#' \item{factor_labels}{List containing the original factor labels when polytomous responses
#'  are provided as factors. NULL if no factor data is present.}
#' \item{categories}{Numeric vector containing the number of response categories for each item.
#'  For binary data, all elements are 2.}
#' }
#' @export
#'
dataFormat.long <- function(data, na = NULL,
                            Sid = NULL, Qid = NULL,
                            Resp = NULL, w = NULL,
                            response.type = NULL) {
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
  factor_labels <- NULL
  if (is.factor(Resp_vec)) {
    factor_labels <- levels(Resp_vec)
    Resp_vec <- as.numeric(Resp_vec)
  } else {
    Resp_vec <- as.numeric(Resp_vec)
  }

  if (is.null(response.type)) {
    response.type <- if (all(Resp_vec[!is.na(Resp_vec)] %in% c(0, 1))) {
      "binary"
    } else {
      "polytomous"
    }
  } else {
    response.type <- match.arg(response.type, c("binary", "polytomous", "poly"))
    if (response.type == "poly") response.type <- "polytomous"
  }

  # Validate responses based on response_type
  if (response.type == "binary" && !all(Resp_vec[!is.na(Resp_vec)] %in% c(0, 1))) {
    stop("Binary response type specified but data contains non-binary values")
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
    if (!is.null(factor_labels)) {
      ret_list$factor_labels <- list(response = factor_labels)
    }
  }

  # Return with appropriate class structure
  ret <- structure(ret_list,
    class = c("exametrika", "exametrikaData")
  )
  return(ret)
}
