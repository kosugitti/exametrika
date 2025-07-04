#' @title Local Dependence Biclustering
#' @description
#' Latent dependence Biclustering, which incorporates biclustering and a Bayesian
#' network model.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ncls number of latent class(rank). The default is 2.
#' @param method specify the model to analyze the data.Local dependence latent
#' class model is set to "C", latent rank model is set "R". The default is "R".
#' @param conf For the confirmatory parameter, you can input either a vector with
#' items and corresponding fields in sequence, or a field membership profile
#' matrix. In the case of the former, the field membership profile matrix will be generated internally.
#' When providing a membership profile matrix, it needs to be either matrix or data.frame.
#' The number of fields(nfld) will be overwrite to the number of columns of this matrix.
#' @param g_list A list compiling graph-type objects for each rank/class.
#' @param adj_list A list compiling matrix-type adjacency matrices for each rank/class.
#' @param adj_file A file detailing the relationships of the graph for each rank/class,
#' listed in the order of starting point, ending point, and rank(class).
#' @param verbose verbose output Flag. default is TRUE
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{msg}{A character string indicating the model type. }
#'  \item{Nclass}{Optimal number of classes.}
#'  \item{Nfield}{Optimal number of fields.}
#'  \item{crr}{Correct Response Rate}
#'  \item{ItemLabel}{Label of Items}
#'  \item{FieldLabel}{Label of Fields}
#'  \item{adj_list}{List of Adjacency matrix used in the model}
#'  \item{g_list}{List of graph object used in the model}
#'  \item{IRP}{List of Estimated Parameters. This object is three-dimensional
#'  PIRP array, where each dimension represents the number of rank,number
#'  of field, and Dmax. Dmax denotes the maximum number of correct response
#'  patterns for each field.}
#'  \item{LFD}{Latent Field Distribution. see also [plot.exametrika]}
#'  \item{LRD}{Latent Rank Distribution. see also [plot.exametrika]}
#'  \item{FRP}{Marginal Field Reference Matrix}
#'  \item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{CCRR_table}{This table is a rearrangement of IRP into a data.frame
#'  format for output, consisting of combinations of rank ,field and PIRP.}
#'  \item{TRP}{Test Reference Profile}
#'  \item{RMD}{Rank Membership Distribution.}
#'  \item{FieldEstimated}{Given vector which correspondence between items
#'  and the fields.}
#'  \item{ClassEstimated}{An index indicating which class a student belongs
#'  to, estimated by confirmatory Ranklustering.}
#'  \item{Students}{Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. It also includes the rank with the maximum estimated membership probability,
#' as well as the rank-up odds and rank-down odds.}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#' }
#' @examples
#' \donttest{
#' # Example: Latent Dirichlet Bayesian Network model
#' # Create field configuration vector based on field assignments
#' conf <- c(
#'   1, 6, 6, 8, 9, 9, 4, 7, 7, 7, 5, 8, 9, 10, 10, 9, 9,
#'   10, 10, 10, 2, 2, 3, 3, 5, 5, 6, 9, 9, 10, 1, 1, 7, 9, 10
#' )
#'
#' # Create edge data for the network structure between fields
#' edges_data <- data.frame(
#'   "From Field (Parent) >>>" = c(
#'     6, 4, 5, 1, 1, 4, # Class/Rank 2
#'     3, 4, 6, 2, 4, 4, # Class/Rank 3
#'     3, 6, 4, 1, # Class/Rank 4
#'     7, 9, 6, 7 # Class/Rank 5
#'   ),
#'   ">>> To Field (Child)" = c(
#'     8, 7, 8, 7, 2, 5, # Class/Rank 2
#'     5, 8, 8, 4, 6, 7, # Class/Rank 3
#'     5, 8, 5, 8, # Class/Rank 4
#'     10, 10, 8, 9 # Class/Rank 5
#'   ),
#'   "At Class/Rank (Locus)" = c(
#'     2, 2, 2, 2, 2, 2, # Class/Rank 2
#'     3, 3, 3, 3, 3, 3, # Class/Rank 3
#'     4, 4, 4, 4, # Class/Rank 4
#'     5, 5, 5, 5 # Class/Rank 5
#'   )
#' )
#'
#' # Save edge data to temporary CSV file
#' tmp_file <- tempfile(fileext = ".csv")
#' write.csv(edges_data, file = tmp_file, row.names = FALSE)
#'
#' # Fit Latent Dirichlet Bayesian Network model
#' result.LDB <- LDB(
#'   U = J35S515,
#'   ncls = 5, # Number of latent classes
#'   conf = conf, # Field configuration vector
#'   adj_file = tmp_file # Path to the CSV file
#' )
#'
#' # Clean up temporary file
#' unlink(tmp_file)
#'
#' # Display model results
#' print(result.LDB)
#'
#' # Visualize different aspects of the model
#' plot(result.LDB, type = "Array") # Show bicluster structure
#' plot(result.LDB, type = "TRP") # Test Response Profile
#' plot(result.LDB, type = "LRD") # Latent Rank Distribution
#' plot(result.LDB,
#'   type = "RMP", # Rank Membership Profiles
#'   students = 1:9, nc = 3, nr = 3
#' )
#' plot(result.LDB,
#'   type = "FRP", # Field Reference Profiles
#'   nc = 3, nr = 2
#' )
#' # Field PIRP Profile showing correct answer counts for each rank and field
#' plot(result.LDB, type = "FieldPIRP")
#' }
#' @export

LDB <- function(U, Z = NULL, w = NULL, na = NULL,
                ncls = 2, method = "R",
                conf = NULL,
                g_list = NULL, adj_list = NULL, adj_file = NULL,
                verbose = FALSE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (U$response.type != "binary") {
    response_type_error(U$response.type, "LDB")
  }

  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  # Check the settings ------------------------------------------------------


  # Check the conf
  if (is.vector(conf)) {
    # check size
    if (length(conf) != NCOL(U)) {
      stop("conf vector size does NOT match with data.")
    }
    conf_mat <- matrix(0, nrow = NCOL(U), ncol = max(conf))
    for (i in 1:NROW(conf_mat)) {
      conf_mat[i, conf[i]] <- 1
    }
  } else if (is.matrix(conf) | is.data.frame(conf)) {
    if (NROW(conf) != NCOL(U)) {
      stop("conf matrix size does NOT match with data.")
    }
    if (any(!conf %in% c(0, 1))) {
      stop("The conf matrix should only contain 0s and 1s.")
    }
    if (any(rowSums(conf) > 1)) {
      stop("The row sums of the conf matrix must be equal to 1.")
    }
  }

  nfld <- NCOL(conf_mat)

  # graph check
  if (is.null(g_list) && is.null(adj_list) && is.null(adj_file)) {
    stop("Specify the graph in either matrix form, CSV file, or as a graph object.")
  }

  FieldLabel <- sprintf("Field%02d", 1:nfld)

  # g_list check
  if (!is.null(g_list)) {
    if (length(g_list) != ncls) {
      stop("The number of classes does not match the length of the list.
           Please specify a graph for all classes.")
    }
    adj_list <- list()
    for (j in 1:ncls) {
      if (!inherits(g_list[[1]], "igraph")) {
        stop("Some items in g_list are not recognized as graph objects.")
      }
      adj_list[[j]] <- fill_adj(g_list[[j]], FieldLabel)
    }
  }
  # adj_list check
  if (!is.null(adj_list)) {
    if (length(adj_list) != ncls) {
      stop("The number of classes does not match the length of the list.
           Please specify a graph for all classes.")
    }
    for (j in 1:ncls) {
      g <- igraph::graph_from_adjacency_matrix(adj_list[[j]])
      adj_list[[j]] <- fill_adj(g, FieldLabel)
    }
  }
  # adj_file check
  if (!is.null(adj_file)) {
    g_csv <- read.csv(adj_file)
    colnames(g_csv) <- c("From", "To", "Rank")
    g_csv$From <- sprintf("Field%02d", g_csv$From)
    g_csv$To <- sprintf("Field%02d", g_csv$To)
    adj_list <- list()
    for (i in 1:ncls) {
      adj_R <- g_csv[g_csv$Rank == i, 1:2]
      g_tmp <- igraph::graph_from_data_frame(adj_R)
      adj_list[[i]] <- fill_adj(g_tmp, FieldLabel)
    }
  }

  ret.Biclustering <- Biclustering(tmp,
    ncls = ncls,
    nfld = nfld,
    method = "R",
    conf = conf,
    verbose = FALSE
  )
  beta1 <- 1
  beta2 <- 1
  const <- exp(-testlength)


  maxnpa <- 4
  parent <- list()
  nparent <- array(NA, dim = c(nfld, ncls))
  for (i in 1:ncls) {
    adj <- adj_list[[i]]
    npa <- colSums(adj)
    parent[[i]] <- list()
    for (j in 1:nfld) {
      nparent[j, i] <- npa[j]
      if (npa[j] == 0) {
        parent[[i]][[j]] <- ""
      } else {
        parent[[i]][[j]] <- FieldLabel[adj[, j] == 1]
      }
    }
  }

  if (max(nparent) > maxnpa) {
    warning("[Caution!] The maximum number of parents per item is ", maxnpa, ", Please check.")
  }

  flddist <- colSums(ret.Biclustering$FieldMembership)
  fld <- ret.Biclustering$FieldEstimated
  csf <- tmp$U %*% ret.Biclustering$FieldMembership
  colnames(csf) <- FieldLabel
  nsf <- matrix(rep(flddist, nobs), nrow = nobs, byrow = TRUE)
  zeroVecS <- rep(0, nobs)

  #### SxRxF
  pirp_mat <- array(0, dim = c(nobs, ncls, nfld))
  for (i in 1:nobs) {
    for (j in 1:ncls) {
      for (k in 1:nfld) {
        if (parent[[j]][[k]][1] != "") {
          pirp_mat[i, j, k] <- sum(csf[i, parent[[j]][[k]]])
        }
      }
    }
  }
  pattern_max <- max(pirp_mat)
  Dmax <- pattern_max + 1
  #### SxRxFxDmax
  pirp_array <- array(0, dim = c(nobs, ncls, nfld, Dmax))
  for (i in 1:nobs) {
    for (j in 1:ncls) {
      for (k in 1:nfld) {
        vec <- rep(0, Dmax)
        vec[pirp_mat[i, j, k] + 1] <- 1
        pirp_array[i, j, k, ] <- vec
      }
    }
  }

  ### RxFxDmax
  n_pirp <- array(0, dim = c(ncls, nfld, Dmax))
  Smem_expand <- array(replicate(nfld * Dmax, ret.Biclustering$SmoothedMembership),
    dim = dim(pirp_array)
  )
  csf_expand <- array(replicate(ncls * Dmax, csf), dim = c(nobs, nfld, ncls, Dmax))
  n_pirp <- apply(csf_expand * (aperm(Smem_expand * pirp_array, perm = c(1, 3, 2, 4))), 2:4, sum)
  n_pirp <- aperm(n_pirp, perm = c(2, 1, 3))

  ### RxFxDmax
  denom_npirp <- array(0, dim = c(ncls, nfld, Dmax))
  nsf_expand <- array(replicate(ncls * Dmax, nsf), dim = c(nobs, nfld, ncls, Dmax))
  denom_npirp <- apply(nsf_expand * (aperm(Smem_expand * pirp_array, perm = c(1, 3, 2, 4))), 2:4, sum)
  denom_npirp <- aperm(denom_npirp, perm = c(2, 1, 3)) + beta1 + beta2 - 2
  denom0 <- sign(denom_npirp)

  denom_npirp <- denom_npirp + (1 - denom0) * 100
  param <- (n_pirp + beta2 - 1) / denom_npirp

  ### IRP
  pirp_trans <- aperm(pirp_array, perm = c(2, 3, 4, 1))
  param_expand <- array(replicate(nobs, param), dim = dim(pirp_trans))
  denom0_expand <- array(replicate(nobs, denom0), dim = dim(pirp_trans))

  tmp1 <- log(apply(pirp_trans * param_expand * denom0_expand, c(1, 2, 4), sum) + const)
  tmp1 <- aperm(tmp1, perm = c(3, 2, 1))
  csf_expand2 <- array(replicate(ncls, csf), dim = dim(tmp1))
  tmp1 <- csf_expand2 * tmp1

  tmp2 <- log(apply(pirp_trans * (1 - param_expand) * denom0_expand, c(1, 2, 4), sum) + const)
  tmp2 <- aperm(tmp2, perm = c(3, 2, 1))
  nsf_expand2 <- array(replicate(ncls, nsf - csf), dim = dim(tmp2))
  tmp2 <- nsf_expand2 * tmp2

  llsr <- apply(tmp1 + tmp2, c(1, 3), sum)
  minllsr <- apply(llsr, 1, min)
  expllsr <- exp(llsr - minllsr)
  clsmemb <- round(expllsr / rowSums(expllsr), 8)
  cls01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  cls <- apply(clsmemb, 1, which.max)

  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID
  RU <- ifelse(cls + 1 > ncls, NA, cls + 1)
  RD <- ifelse(cls - 1 < 1, NA, cls - 1)
  RUO <- clsmemb[cbind(1:nobs, RU)] / clsmemb[cbind(1:nobs, cls)]
  RDO <- clsmemb[cbind(1:nobs, RD)] / clsmemb[cbind(1:nobs, cls)]
  StudentRank <- cbind(StudentRank, cls, RUO, RDO)
  colnames(StudentRank) <- c(
    paste("Membership", 1:ncls), "Estimate",
    "Rank-Up Odds", "Rank-Down Odds"
  )
  clsdist <- colSums(cls01)
  clsmembdist <- colSums(clsmemb)


  # pifr --------------------------------------------------------------------

  clsmemb_expand <- array(replicate(nfld * Dmax, clsmemb), dim = dim(pirp_array))
  pifr <- param * denom0 * apply(clsmemb_expand * pirp_array, 2:4, sum) / apply(clsmemb, 2, sum)
  pifr <- apply(pifr, c(1, 2), sum)

  # Model Fit ----------------------------------------------------------------

  testell <- sum(clsmemb * llsr)
  nparam <- sum(denom0)
  FitIndices <- TestFit(tmp$U, tmp$Z, testell, nparam)

  # Output ------------------------------------------------------------------

  g_list <- list()
  for (i in 1:ncls) {
    g_list[[i]] <- igraph::graph_from_adjacency_matrix(adj_list[[i]])
  }

  CCRR_table <- as.data.frame(matrix(NA, nrow = ncls * nfld, ncol = 2 + Dmax))
  colnames(CCRR_table) <- c(
    "Rank", "Field", paste("PIRP", 0:(Dmax - 1))
  )
  CCRR_table[, 1] <- rep(paste("Rank", 1:ncls), each = nfld)
  CCRR_table[, 2] <- rep(paste("Field", 1:nfld), ncls)
  table_tmp <- param[1, , ]
  for (i in 2:ncls) {
    table_tmp <- rbind(table_tmp, param[i, , ])
  }
  table_tmp[table_tmp == 0] <- NA
  CCRR_table[, 3:NCOL(CCRR_table)] <- table_tmp

  colnames(pifr) <- FieldLabel
  rownames(pifr) <- paste("Rank", 1:NROW(pifr))
  pifr <- t(pifr)

  FRPIndex <- IRPindex(pifr)
  TRP <- t(t(pifr) %*% flddist)
  RMD <- clsmembdist
  TRPlag <- TRP[2:ncls]
  TRPmic <- sum(TRPlag[1:(ncls - 1)] - TRP[1:(ncls - 1)] < 0, na.rm = TRUE)
  FRPmic <- sum(abs(FRPIndex$C))
  SOACflg <- WOACflg <- FALSE
  if (TRPmic == 0) {
    WOACflg <- TRUE
    if (FRPmic == 0) {
      SOACflg <- TRUE
    }
  }

  ret <- structure(list(
    U = U,
    testlength = testlength,
    msg = "Rank",
    nobs = nobs,
    Nrank = ncls,
    Nfield = nfld,
    crr = crr(U),
    ItemLabel = tmp$ItemLabel,
    FieldLabel = FieldLabel,
    adj_list = adj_list,
    g_list = g_list,
    IRP = param,
    LFD = flddist,
    LRD = clsdist,
    FRP = pifr,
    CCRR_table = CCRR_table,
    FRPIndex = FRPIndex,
    TRP = TRP,
    RMD = colSums(clsmemb),
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = StudentRank,
    TestFitIndices = FitIndices,
    SOACflg = SOACflg,
    WOACflg = WOACflg
  ), class = c("exametrika", "LDB"))
  return(ret)
}
