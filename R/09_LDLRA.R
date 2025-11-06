#' @title LDparam set
#' @description
#' A function that extracts only the estimation of graph parameters
#' after the rank estimation is completed.
#' @param tmp tmp
#' @param adj_list adj_list
#' @param classRefMat values returned from emclus
#' @param ncls ncls
#' @param smoothpost smoothpost
#' @param beta1 Beta distribution parameter 1 for prior density. Default is 2.
#' @param beta2 Beta distribution parameter 2 for prior density. Default is 2.
#'
LD_param_est <- function(tmp, adj_list, classRefMat, ncls, smoothpost, beta1 = 2, beta2 = 2) {
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  maxnpa <- 4
  parent <- list()
  nparent <- array(NA, dim = c(testlength, ncls))
  for (i in 1:ncls) {
    adj <- adj_list[[i]]
    npa <- colSums(adj)
    parent[[i]] <- list()
    for (j in 1:testlength) {
      nparent[j, i] <- npa[j]
      if (npa[j] == 0) {
        parent[[i]][[j]] <- ""
      } else {
        parent[[i]][[j]] <- tmp$ItemLabel[adj[, j] == 1]
      }
    }
  }

  if (max(nparent) > maxnpa) {
    warning("[Caution!] The maximum number of parents per item is ", maxnpa, ", Please check.")
  }

  # PIRP  --------------------------------------------------------------
  npapat <- 2^max(nparent)
  #### JxRxPattern
  refmat <- array(NA, dim = c(testlength, ncls, npapat))
  for (i in 1:npapat) {
    refmat[, , i] <- classRefMat
  }
  refmat <- array(replicate(npapat, classRefMat), dim = c(testlength, ncls, npapat))

  ### SxJxRxPattern
  pat01 <- array(0, dim = c(nobs, testlength, ncls, npapat))
  for (s in 1:nobs) {
    for (j in 1:testlength) {
      for (cls in 1:ncls) {
        if (parent[[cls]][[j]][[1]] == "") {
          # No parents
          pos <- 1
        } else {
          pos.tmp <- tmp$U[s, parent[[cls]][[j]]]
          pos <- sum(2^(which(rev(pos.tmp) == 1) - 1)) + 1
        }
        pat01[s, j, cls, pos] <- 1
      }
    }
  }

  # Estimation --------------------------------------------------------------
  const <- exp(-testlength)

  n_correct <- array(0, dim = c(nobs, testlength, ncls, npapat))
  n_incorrect <- array(0, dim = c(nobs, testlength, ncls, npapat))

  # Expand dimensions for vectorized operations
  U_expanded <- array(replicate(ncls * npapat, tmp$U), dim = dim(pat01))
  Z_expanded <- array(replicate(ncls * npapat, tmp$Z), dim = dim(pat01))

  U_ezpanded <- array(rep(tmp$U, times = ncls * npapat), dim = c(nobs, testlength, ncls, npapat))
  Z_ezpanded <- array(rep(tmp$Z, times = ncls * npapat), dim = c(nobs, testlength, ncls, npapat))

  # Expand smoothpost
  # Using aperm and array to expand smoothpost
  smoothpost_expanded <- aperm(
    array(rep(smoothpost, times = testlength * npapat),
      dim = c(nobs, ncls, testlength, npapat)
    ),
    perm = c(1, 3, 2, 4)
  )
  # Vectorized calculations
  n_correct <- colSums(U_expanded * pat01 * smoothpost_expanded)
  n_incorrect <- colSums(Z_expanded * (1 - U_expanded) * pat01 * smoothpost_expanded)

  refmat <- (n_correct + beta1 - 1) / (n_correct + n_incorrect + beta1 + beta2 - 2)
  item_ell <- n_correct * log(refmat + const) + n_incorrect * log(1 - refmat + const)
  item_ell <- rowSums(apply(item_ell, 1:2, sum))
  test_ell <- sum(item_ell)

  refmat_expanded <- array(rep(refmat, nobs), dim = c(testlength, ncls, npapat, nobs))
  term1 <- aperm(U_expanded * pat01, perm = c(2, 3, 4, 1)) * log(refmat_expanded + const)
  term2 <- aperm(Z_expanded * (1 - U_expanded) * pat01, perm = c(2, 3, 4, 1)) * log(1 - refmat_expanded + const)
  llmat <- aperm(term1 + term2, perm = c(1, 3, 4, 2))
  llmat <- apply(llmat, 3:4, sum)
  exp_llmat <- exp(llmat)
  postdist <- exp_llmat / rowSums(exp_llmat)

  postdist_expand <- array(rep(postdist, testlength * npapat), dim = c(nobs, ncls, testlength, npapat))
  irp_tmp <- postdist_expand * aperm(pat01, perm = c(1, 3, 2, 4))
  irp_tmp <- apply(irp_tmp, 2:4, sum) / colSums(postdist)
  irp_tmp <- aperm(irp_tmp, perm = c(2, 1, 3))
  irp_tmp <- irp_tmp * refmat
  irp <- apply(irp_tmp, 1:2, sum)

  # Model Fit -------------------------------------------------------

  model_nparam <- sum(apply(sign(n_correct + n_incorrect), 1:2, sum))
  FitIndices <- TestFit(tmp$U, tmp$Z, test_ell, model_nparam)

  return(list(
    irp = irp,
    parent = parent,
    FitIndices = FitIndices,
    npapat = npapat,
    postdist = postdist,
    refmat = refmat
  ))
}

#' @title Local Dependence Latent Rank Analysis
#' @description
#' performs local dependence latent lank analysis(LD_LRA) by Shojima(2011)
#' @details
#' This function is intended to perform LD-LRA. LD-LRA is an analysis that
#' combines LRA and BNM, and it is used to analyze the network structure among
#' items in the latent rank. In this function, structural learning is not
#' performed, so you need to provide item graphs for each rank as separate files.
#' The file format for this is plain text CSV that includes edges (From, To) and
#' rank numbers.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ncls number of latent class(rank). The default is 2.
#' @param method specify the model to analyze the data.Local dependence latent
#' class model is set to "C", latent rank model is set "R". The default is "R".
#' @param g_list A list compiling graph-type objects for each rank/class.
#' @param adj_list A list compiling matrix-type adjacency matrices for each rank/class.
#' @param adj_file A file detailing the relationships of the graph for each rank/class,
#' listed in the order of starting point, ending point, and rank(class).
#' @param verbose verbose output Flag. default is TRUE
#' @param beta1 Beta distribution parameter 1 for prior density of rank reference matrix. Default is 2.
#' @param beta2 Beta distribution parameter 2 for prior density of rank reference matrix. Default is 2.
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom utils read.csv
#' @importFrom igraph V
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{msg}{A character string indicating the model type. }
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{crr}{correct response ratio}
#'  \item{adj_list}{adjacency matrix list}
#'  \item{g_list}{graph list}
#'  \item{referenceMatrix}{Learned Parameters.A three-dimensional array of patterns where
#'  item x rank x pattern.}
#'  \item{IRP}{Marginal Item Reference Matrix}
#'  \item{IRPIndex}{IRP Indices which include Alpha, Beta, Gamma.}
#'  \item{TRP}{Test Reference Profile matrix.}
#'  \item{LRD}{latent Rank/Class Distribution}
#'  \item{RMD}{Rank/Class Membership Distribution}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{Estimation_table}{Estimated parameters tables.}
#'  \item{CCRR_table}{Correct Response Rate tables}
#'  \item{Studens}{Student information. It includes estimated class
#'  membership, probability of class membership, RUO, and RDO.}
#' }
#' @examples
#' \donttest{
#' # Create sample DAG structure with different rank levels
#' # Format: From, To, Rank
#' DAG_dat <- matrix(c(
#'   "From", "To", "Rank",
#'   "Item01", "Item02", "1", # Simple structure for Rank 1
#'   "Item01", "Item02", "2", # More complex structure for Rank 2
#'   "Item02", "Item03", "2",
#'   "Item01", "Item02", "3", # Additional connections for Rank 3
#'   "Item02", "Item03", "3",
#'   "Item03", "Item04", "3"
#' ), ncol = 3, byrow = TRUE)
#'
#' # Method 1: Directly use graph and adjacency lists
#' g_list <- list()
#' adj_list <- list()
#'
#' for (i in 1:3) {
#'   adj_R <- DAG_dat[DAG_dat[, 3] == as.character(i), 1:2, drop = FALSE]
#'   g_tmp <- igraph::graph_from_data_frame(
#'     d = data.frame(
#'       From = adj_R[, 1],
#'       To = adj_R[, 2]
#'     ),
#'     directed = TRUE
#'   )
#'   adj_tmp <- igraph::as_adjacency_matrix(g_tmp)
#'   g_list[[i]] <- g_tmp
#'   adj_list[[i]] <- adj_tmp
#' }
#'
#' # Fit Local Dependence Latent Rank Analysis
#' result.LDLRA1 <- LDLRA(J12S5000,
#'   ncls = 3,
#'   g_list = g_list,
#'   adj_list = adj_list
#' )
#'
#' # Plot Item Reference Profiles (IRP) in a 4x3 grid
#' # Shows the probability patterns of correct responses for each item across ranks
#' plot(result.LDLRA1, type = "IRP", nc = 4, nr = 3)
#'
#' # Plot Test Reference Profile (TRP)
#' # Displays the overall pattern of correct response probabilities across ranks
#' plot(result.LDLRA1, type = "TRP")
#'
#' # Plot Latent Rank Distribution (LRD)
#' # Shows the distribution of students across different ranks
#' plot(result.LDLRA1, type = "LRD")
#' }
#'
#' @export
#'

LDLRA <- function(U, Z = NULL, w = NULL, na = NULL,
                  ncls = 2, method = "R",
                  g_list = NULL, adj_list = NULL, adj_file = NULL,
                  verbose = FALSE, beta1 = 2, beta2 = 2) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }


  if (U$response.type != "binary") {
    response_type_error(U$response.type, "LDLRA")
  }


  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  if (method == "C" | method == "Class") {
    if (verbose) {
      message("local dependence latent Class model is chosen.")
    }
    model <- 1
  } else if (method == "R" | method == "Rank") {
    if (verbose) {
      message("local dependence latent Rank model is chosen.")
    }
    model <- 2
  } else {
    stop("The method must be selected as either LD-LCA or LD-LRA.")
  }

  # graph check
  if (is.null(g_list) && is.null(adj_list) && is.null(adj_file)) {
    stop("Specify the graph in either matrix form, CSV file, or as a graph object.")
  }
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
      adj_list[[j]] <- fill_adj(g_list[[j]], tmp$ItemLabel)
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
      adj_list[[j]] <- fill_adj(g, tmp$ItemLabel)
    }
  }
  # adj_file check
  if (!is.null(adj_file)) {
    g_csv <- read.csv(adj_file)
    colnames(g_csv) <- c("From", "To", "Rank")
    adj_list <- list()
    for (i in 1:ncls) {
      adj_R <- g_csv[g_csv$Rank == i, 1:2]
      g_tmp <- igraph::graph_from_data_frame(adj_R)
      adj_list[[i]] <- fill_adj(g_tmp, tmp$ItemLabel)
    }
  }

  # Set filter --------------------------------------------------------------
  if (model == 1) {
    filmat <- diag(1, ncol = ncls, nrow = ncls)
  } else {
    fil0 <- (1 / 3 - 0.4) / 5 * (ncls - 5) + 0.4
    fil1 <- (1 / 3 - 0.2) / 5 * (ncls - 5) + 0.2
    fil2 <- (1 - fil0 - 2 * fil1) / 2
    filmat0 <- diag(fil0, ncls)
    filmat1 <- diag(fil1, (ncls + 1))[2:(ncls + 1), 1:ncls]
    filmat2 <- diag(fil2, (ncls + 2))[3:(ncls + 2), 1:ncls]
    filmat <- filmat0 + filmat1 + filmat2 + t(filmat1) + t(filmat2)
    filmat <- filmat / (rep(1, ncls) %*% t(rep(1, ncls)) %*% filmat)
  }

  ret.emclus <- emclus(tmp$U, tmp$Z, ncls, Fil = filmat, beta1 = beta1, beta2 = beta2)
  smoothpost <- ret.emclus$postDist %*% filmat
  ret.LDparam <- LD_param_est(tmp, adj_list, ret.emclus$classRefMat, ncls, smoothpost, beta1, beta2)
  irp <- ret.LDparam$irp
  npapat <- ret.LDparam$npapat
  FitIndices <- ret.LDparam$FitIndices
  postdist <- ret.LDparam$postdist
  refmat <- ret.LDparam$refmat
  parent <- ret.LDparam$parent

  # output ----------------------------------------------------------

  g_list <- list()
  for (i in 1:ncls) {
    g_list[[i]] <- igraph::graph_from_adjacency_matrix(adj_list[[i]])
  }

  ### refmat; Item x Rank x Pattern
  Estimation_table <- as.data.frame(matrix(NA, nrow = testlength * ncls, ncol = npapat + 2))
  model <- 2
  if (model == 1) {
    model_msg <- "Class"
  } else {
    model_msg <- "Rank"
  }

  colnames(Estimation_table) <- c("Item", model_msg, paste("PIRP", 1:npapat))
  Estimation_table[, 1] <- rep(tmp$ItemLabel, ncls)
  Estimation_table[, 2] <- rep(1:ncls, each = testlength)

  parent_vec <- unlist(lapply(adj_list, colSums))
  CCRR_table <- as.data.frame(matrix(NA, nrow = sum(2^parent_vec), ncol = 6))
  colnames(CCRR_table) <- c("Child Item", model_msg, "N of Parents", "Parent Items", "PIRP", "Conditional CRR")
  CCRR_table[, 1] <- rep(rep(tmp$ItemLabel, ncls), times = 2^parent_vec)
  CCRR_table[, 2] <- rep(rep(1:ncls, each = testlength), times = 2^parent_vec)
  CCRR_table[, 3] <- rep(parent_vec, times = 2^parent_vec)
  parent_items <- character(sum(2^parent_vec))
  CRR_vec <- numeric(sum(2^parent_vec))

  l1 <- 0
  l2 <- 0
  l3 <- 0
  for (i in 1:ncls) {
    for (j in 1:testlength) {
      l1 <- l1 + 1
      if (length(parent[[i]][[j]]) == 1 && parent[[i]][[j]] == "") {
        max_k <- 1
        l2 <- l2 + 1
        parent_items[l2] <- "No Parents"
      } else {
        max_k <- 2^length(parent[[i]][[j]])
        word <- paste(unlist(parent[[i]][[j]]), collapse = ", ")
        rep_times <- 2^length(parent[[i]][[j]])
        for (k in 1:rep_times) {
          l2 <- l2 + 1
          parent_items[l2] <- word
        }
      }
      for (k in 1:max_k) {
        l3 <- l3 + 1
        Estimation_table[l1, k + 2] <- refmat[j, i, k]
        CRR_vec[l3] <- refmat[j, i, k]
      }
    }
  }

  CCRR_table[, 4] <- parent_items
  CCRR_table[, 5] <- unlist(sapply(parent_vec, BitRespPtn))
  CCRR_table[, 6] <- CRR_vec

  # IRP index -------------------------------------------------------

  rownames(irp) <- tmp$ItemLabel
  colnames(irp) <- paste(model_msg, 1:ncls)
  IRPIndex <- IRPindex(irp)
  if (sum(IRPIndex$C) == 0) {
    SOACflg <- TRUE
  } else {
    SOACflg <- FALSE
  }

  # TRP -------------------------------------------------------------
  TRP <- colSums(irp)
  dif <- diff(TRP)
  WOACsum <- sum(dif > 0) + 1
  if (WOACsum == ncls) {
    WOACflg <- TRUE
  } else {
    WOACflg <- FALSE
  }

  # Student information ---------------------------------------------
  clsNum <- apply(postdist, 1, which.max)
  bMax <- matrix(rep(apply(postdist, 1, max), ncls), ncol = ncls)
  cls01 <- sign(postdist - bMax) + 1
  LRD <- colSums(cls01)
  RMD <- colSums(postdist)
  StudentClass <- postdist
  RU <- ifelse(clsNum + 1 > ncls, NA, clsNum + 1)
  RD <- ifelse(clsNum - 1 < 1, NA, clsNum - 1)
  RUO <- StudentClass[cbind(1:nobs, RU)] / StudentClass[cbind(1:nobs, clsNum)]
  RDO <- StudentClass[cbind(1:nobs, RD)] / StudentClass[cbind(1:nobs, clsNum)]
  StudentClass <- cbind(StudentClass, clsNum, RUO, RDO)
  colnames(StudentClass) <- c(
    paste("Membership", 1:ncls), "Estimate",
    "Rank-Up Odds", "Rank-Down Odds"
  )
  rownames(StudentClass) <- tmp$ID

  ret <- structure(list(
    U = U,
    testlength = testlength,
    msg = "Rank",
    nobs = nobs,
    Nclass = ncls,
    model = model,
    crr = crr(U),
    ItemLabel = tmp$ItemLabel,
    adj_list = adj_list,
    g_list = g_list,
    referenceMatrix = refmat,
    IRP = irp,
    IRPIndex = IRPIndex,
    TRP = as.vector(TRP),
    LRD = as.vector(LRD),
    RMD = as.vector(RMD),
    Students = StudentClass,
    TestFitIndices = FitIndices,
    Estimation_table = Estimation_table,
    CCRR_table = CCRR_table,
    SOACflg = SOACflg,
    WOACflg = WOACflg
  ), class = c("exametrika", "LDLRA"))
  return(ret)
}
