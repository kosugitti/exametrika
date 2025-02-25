#' @title Bicluster Network Model
#' @description
#' Bicluster Network Model: BINET is a model that combines the Bayesian
#' network model and Biclustering. BINET is very similar to LDB and LDR.
#' The most significant difference is that in LDB, the nodes represent
#' the fields, whereas in BINET, they represent the class. BINET
#' explores the local dependency structure among latent classes at each
#' latent field, where each field is a locus.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ncls number of classes
#' @param nfld number of fields
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
#'  \item{Nclass}{Optimal number of classes.}
#'  \item{Nfield}{Optimal number of fields.}
#'  \item{crr}{Correct Response Rate}
#'  \item{ItemLabel}{Label of Items}
#'  \item{FieldLabel}{Label of Fields}
#'  \item{all_adj}{Integrated Adjacency matrix used to plot graph.}
#'  \item{all_g}{Integrated graph object used to plot graph.see also
#'  [plot.exametrika]}
#'  \item{adj_list}{List of Adjacency matrix used in the model}
#'  \item{params}{A list of the estimated conditional probabilities.
#'  It indicates which path was obtained from which parent node(class) to
#'  which child node(class), held by `parent`, `child`, and `field`. The item
#'  Items contained in the field is in `fld`. Named `chap` includes the
#'  conditional correct response answer rate of the child node, while `pap`
#'  contains the pass rate of the parent node.}
#'  \item{PSRP}{Response pattern by the students belonging to the parent
#'  classes of Class c. A more comprehensible arrangement of `params.`}
#'  \item{LCD}{Latent Class Distribution. see also [plot.exametrika]}
#'  \item{LFD}{Latent Field Distribution. see also [plot.exametrika]}
#'  \item{CMD}{Class Membership Distribution.}
#'  \item{FRP}{Marginal bicluster reference matrix.}
#'  \item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{TRP}{Test Reference Profile}
#'  \item{LDPSR}{A rearranged set of parameters for output. It includes
#'  the field the items contained within that field, and the conditional
#'  correct response rate of parent nodes(class) and child node(class).}
#'  \item{FieldEstimated}{Given vector which correspondence between items
#'  and the fields.}
#'  \item{Students}{Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. }
#' \item{NextStage}{The next class that easiest for students to move to,
#' its membership probability, class-up odds, and the field required for
#' more.}
#'  \item{MG_FitIndices}{Multigroup as Null model.See also [TestFit]}
#'  \item{SM_FitIndices}{Saturated Model as Null model.See also [TestFit]}
#' }
#' @examples
#' \donttest{
#' # Example: Bicluster Network Model (BINET)
#' # BINET combines Bayesian network model and Biclustering to explore
#' # local dependency structure among latent classes at each field
#'
#' # Create field configuration vector based on field assignments
#' conf <- c(
#'   1, 5, 5, 5, 9, 9, 6, 6, 6, 6, 2, 7, 7, 11, 11, 7, 7,
#'   12, 12, 12, 2, 2, 3, 3, 4, 4, 4, 8, 8, 12, 1, 1, 6, 10, 10
#' )
#'
#' # Create edge data for network structure between classes
#' edges_data <- data.frame(
#'   "From Class (Parent) >>>" = c(
#'     1, 2, 3, 4, 5, 7, 2, 4, 6, 8, 10, 6, 6, 11, 8, 9, 12
#'   ),
#'   ">>> To Class (Child)" = c(
#'     2, 4, 5, 5, 6, 11, 3, 7, 9, 12, 12, 10, 8, 12, 12, 11, 13
#'   ),
#'   "At Field (Locus)" = c(
#'     1, 2, 2, 3, 4, 4, 5, 5, 5, 5, 5, 7, 8, 8, 9, 9, 12
#'   )
#' )
#'
#' # Save edge data to temporary CSV file
#' tmp_file <- tempfile(fileext = ".csv")
#' write.csv(edges_data, file = tmp_file, row.names = FALSE)
#'
#' # Fit Bicluster Network Model
#' result.BINET <- BINET(
#'   J35S515,
#'   ncls = 13, # Maximum class number from edges (13)
#'   nfld = 12, # Maximum field number from conf (12)
#'   conf = conf, # Field configuration vector
#'   adj_file = tmp_file # Path to the CSV file
#' )
#'
#' # Clean up temporary file
#' unlink(tmp_file)
#'
#' # Display model results
#' print(result.BINET)
#'
#' # Visualize different aspects of the model
#' plot(result.BINET, type = "Array") # Show bicluster structure
#' plot(result.BINET, type = "TRP") # Test Response Profile
#' plot(result.BINET, type = "LRD") # Latent Rank Distribution
#' plot(result.BINET,
#'   type = "RMP", # Rank Membership Profiles
#'   students = 1:9, nc = 3, nr = 3
#' )
#' plot(result.BINET,
#'   type = "FRP", # Field Reference Profiles
#'   nc = 3, nr = 2
#' )
#' plot(result.BINET,
#'   type = "LDPSR", # Locally Dependent Passing Student Rates
#'   nc = 3, nr = 2
#' )
#' }
#' @export

BINET <- function(U, Z = NULL, w = NULL, na = NULL,
                  conf = NULL, ncls = NULL, nfld = NULL,
                  g_list = NULL, adj_list = NULL, adj_file = NULL,
                  verbose = FALSE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (U$response.type != "binary") {
    response_type_error(U$response.type, "BINET")
  }

  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  if (is.null(ncls)) {
    stop("Please specify the appropriate number of classes")
  }
  if (is.null(nfld)) {
    stop("Please specify the appropriate number of fields.")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  ret.TS <- TestStatistics(tmp)
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

  # graph check
  if (is.null(g_list) && is.null(adj_list) && is.null(adj_file)) {
    stop("Specify the graph in either matrix form, CSV file, or as a graph object.")
  }

  FieldLabel <- sprintf("Class%02d", 1:ncls)

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
    colnames(g_csv) <- c("From", "To", "Field")
    g_csv$From <- sprintf("Class%02d", g_csv$From)
    g_csv$To <- sprintf("Class%02d", g_csv$To)
    for (i in 1:nfld) {
      adj_R <- g_csv[g_csv$Field == i, 1:2]
      g_tmp <- igraph::graph_from_data_frame(adj_R)
      adj_list[[i]] <- fill_adj(g_tmp, FieldLabel)
    }
    g_csv$Field <- sprintf("Field%02d", g_csv$Field)
  }

  all_adj <- Reduce("|", adj_list) * 1

  # get the Biclustering structure
  ret.Biclustering <- Biclustering(
    tmp,
    ncls = ncls,
    nfld = nfld,
    conf = conf,
    method = "BINET",
    verbose = FALSE
  )

  ### Adj mat check
  adjU <- all_adj + t(all_adj)
  simpleFLG <- ifelse(max(adjU) <= 1, 1, 0)
  acyclicFLG <- 0
  connectedFLG <- 0
  for (i in 1:(nfld - 1)) {
    acyclicFLG <- acyclicFLG + sum(diag(all_adj^i))
    connectedFLG <- connectedFLG + min(sum(U^i))
  }
  acyclicFLG <- ifelse(acyclicFLG == 0, 1, 0)
  connectedFLG <- ifelse(connectedFLG > 0, 1, 0)
  dag <- simpleFLG * acyclicFLG
  cdag <- dag * connectedFLG

  if (cdag + dag == 0) {
    stop("Your graph is not a DAG")
  }

  ### check the max parent
  maxnpa <- 2
  add_mat <- Reduce("+", adj_list)
  maxna_position <- which(add_mat == max(add_mat), arr.ind = TRUE)
  maxadj <- max(add_mat)
  if (maxadj > maxnpa) {
    warning(
      "[Caution!] The maximum number of fields per edge is ",
      maxnpa, ". Please check the edge from Class ",
      maxna_position[1], " to Class ", maxna_position[2]
    )
  }

  ### Estimation of parameter set
  flddist <- colSums(ret.Biclustering$FieldMembership)
  clsmemb <- ret.Biclustering$ClassMembership
  fldmemb <- ret.Biclustering$FieldMembership
  fld <- ret.Biclustering$FieldEstimated
  cls <- ret.Biclustering$ClassEstimated

  # Estimation for BINET -------------------------------------------------------
  gamp <- 1
  const <- 1e-10
  lls <- 0

  irp <- t(t(clsmemb) %*% tmp$U / colSums(clsmemb))
  Ccj <- t(clsmemb) %*% tmp$U
  Fcj <- t(clsmemb) %*% (tmp$Z * (1 - tmp$U))
  Ncj <- Ccj + Fcj
  Ccf <- Ccj %*% fldmemb
  Fcf <- Fcj %*% fldmemb
  Ncf <- Ccf + Fcf
  Pcf <- (Ccf + gamp - 1) / (Ncf + 2 * gamp - 2)
  Pcf[1, ] <- 0
  Pcf[NROW(Pcf), ] <- 1
  Pcf01 <- matrix(0, ncol = nfld, nrow = ncls)
  Pcf2 <- matrix(list(), nrow = ncls, ncol = nfld)
  Pcj <- matrix(0, nrow = ncls, ncol = testlength)

  param <- list()
  row <- 0

  ### RISP
  for (i in 1:ncls) {
    ## i is parent
    for (j in 1:ncls) {
      ## j is childen
      for (k in 1:nfld) {
        ## k is field
        if (adj_list[[k]][i, j] == 1) {
          Pcf01[j, k] <- 1
          row <- row + 1
          fld_item <- which(fldmemb[, k] == 1)
          paC <- Ccj[i, fld_item]
          paN <- Ncj[i, fld_item]
          pap <- (paC + gamp - 1) / (paN + 2 * gamp - 2)
          if (i == 1) {
            pap <- rep(0, length(pap))
          }
          chC <- Ccj[j, fld_item]
          chN <- Ncj[j, fld_item]
          chp <- (chC + gamp - 1) / (chN + 2 * gamp - 2)
          if (j == ncls) {
            chp <- rep(1, length(chp))
          }
          param[[row]] <- list(
            parent = i,
            child = j,
            field = k,
            fld = fld_item,
            pap = pap,
            chap = chp
          )
          Pcf2[j, k] <- list(chp)
        }
      }
    }
  }

  for (i in 1:nrow(Pcf2)) {
    for (j in 1:ncol(Pcf2)) {
      if (is.numeric(Pcf2[[i, j]][[1]])) {
        Pcf2[[i, j]] <- (1 - Pcf01[i, j]) * Pcf[i, j] + Pcf2[i, j][[1]]
      } else {
        if (is.null(Pcf2[[i, j]])[[1]]) {
          Pcf2[[i, j]] <- (1 - Pcf01[i, j]) * Pcf[i, j]
        }
      }
    }
  }

  for (i in 1:ncls) {
    for (j in 1:nfld) {
      fld_item <- which(fld == j)
      if (Pcf01[i, j] == 1) {
        for (k in 1:length(fld_item)) {
          tg <- fld_item[k]
          Pcj[i, tg] <- Pcf2[i, j][[1]][k]
        }
      } else {
        for (k in fld_item) {
          Pcj[i, k] <- Pcf[i, j]
        }
      }
    }
  }

  log_num_Zic <- matrix(NA, nrow = nobs, ncol = ncls)
  log_num_Zic <- tmp$U %*% log(t(Pcj) + const) +
    (tmp$Z * (1 - tmp$U)) %*% log(t(1 - Pcj) + const)
  num_Zic <- exp(log_num_Zic)
  num_Zic[, 1] <- 0
  num_Zic[, ncls] <- 0
  for (s in 1:nobs) {
    if (cls[s] == 1) {
      num_Zic[s, 1] <- 1
    }
    if (cls[s] == ncls) {
      num_Zic[s, ncls] <- 1
    }
  }

  clsmemb <- num_Zic / rowSums(num_Zic)
  cls <- apply(clsmemb, 1, which.max)
  cls01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
  Ccj <- t(clsmemb) %*% tmp$U
  Fcj <- t(clsmemb) %*% (tmp$Z * (1 - tmp$U))
  Ncj <- Ccj + Fcj
  llm <- sum(Ccj * log(Pcj + const) + Fcj * log(1 - Pcj + const))
  chim <- 2 * (lls - llm)
  df <- nobs * testlength - sum(sign(sign(unlist(Pcf2)) + 1))
  aic <- chim - 2 * df
  bic <- chim - df * log(nobs)
  indices <- list(llm = llm, chim = chim, df = df, aic = aic, bic = bic)

  # For output  --------------------------------------------------

  g_list <- list()
  for (i in 1:nfld) {
    g_list[[i]] <- igraph::graph_from_adjacency_matrix(adj_list[[i]])
  }
  all_g <- igraph::graph_from_data_frame(g_csv, directed = TRUE)

  ## Students Info
  next_stage <- matrix(NA, ncol = 3 + maxadj, nrow = nobs)
  colnames(next_stage) <-
    c("NextClass", "Membership", "Class-Up Odds", paste("Recommended Field", 1:maxadj))
  for (s in 1:nobs) {
    path_way <- lapply(adj_list, function(mat) {
      return(which(mat[cls[s], ] == 1))
    })
    path_way <- as.vector(unlist(path_way))
    if (length(path_way) == 0) {
      path_way <- NA
    } else {
      mat <- cbind(
        ns = path_way,
        prob = clsmemb[s, path_way]
      )
      sort_order <- order(mat[, 2], decreasing = TRUE)
      if (length(path_way) == 1) {
        candidate <- mat[1, ]
      } else {
        candidate <- mat[sort_order, ][1, ]
      }
      odds <- candidate[2] / clsmemb[s, cls[s]]
      recommend <- which(sapply(adj_list, function(mat) mat[cls[s], candidate[1]] == 1))
      if (length(recommend) < maxadj) {
        recommend <- c(recommend, rep(NA, maxadj - length(recommend)))
      }
      next_stage[s, ] <- c(candidate, odds, recommend)
    }
  }
  Students <- cbind(clsmemb, cls)
  NextStage <- next_stage
  rownames(Students) <- tmp$ID
  rownames(NextStage) <- tmp$ID
  colnames(Students) <- c(
    paste("Membership", 1:ncls), "Latent Class"
  )
  colnames(NextStage) <- c(
    "Next Class", "Membership in Next Class",
    "Class-up odds", paste("Recommmended Field", 1:maxadj)
  )
  clsdist <- colSums(cls01)
  clsmembdist <- colSums(clsmemb)

  ## PSRP matrix
  Dmax <- max(sapply(param, function(x) length(x$pap)))
  if (Dmax < max(sapply(param, function(x) length(x$chap)))) {
    Dmax <- max(sapply(param, function(x) length(x$chap)))
  }
  PSRP <- list()
  PSRP_tmp <- matrix(ncol = Dmax, nrow = ncls)
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      vec <- Pcf2[j, i][[1]]
      if (length(vec) < Dmax) {
        vec <- c(vec, rep(NA, Dmax - length(vec)))
      }
      PSRP_tmp[j, ] <- vec
    }
    rownames(PSRP_tmp) <- paste("Class", 1:ncls)
    colnames(PSRP_tmp) <- paste("PSRP", 1:Dmax)
    PSRP[[i]] <- PSRP_tmp
  }


  ### Local Dependence Passing Student Rate
  param <- param[order(sapply(param, function(x) x$field))]
  LDPSR_table <- data.frame(matrix(NA, nrow = length(param), ncol = Dmax * 3 + 3))
  names(LDPSR_table) <-
    c(
      "Field", paste("Field Item", 1:Dmax),
      "Parent Class", paste("Parent CCR", 1:Dmax),
      "Child Class", paste("Child CCR", 1:Dmax)
    )
  for (i in 1:length(param)) {
    LDPSR_table[i, 1] <- param[[i]]$field
    item_vec <- tmp$ItemLabel[param[[i]]$fld]
    if (length(item_vec) < Dmax) {
      item_vec <- c(item_vec, rep(NA, Dmax - length(item_vec)))
    }
    LDPSR_table[i, 2:(2 + Dmax - 1)] <- item_vec
    LDPSR_table[i, (2 + Dmax)] <- param[[i]]$parent
    pa_CCR <- param[[i]]$pap
    if (length(pa_CCR) < Dmax) {
      pa_CCR <- c(pa_CCR, rep(NA, Dmax - length(pa_CCR)))
    }
    LDPSR_table[i, (2 + Dmax + 1):(2 * (Dmax + 1))] <- pa_CCR
    LDPSR_table[i, 2 * (Dmax + 1) + 1] <- param[[i]]$child
    ch_CCR <- param[[i]]$chap
    if (length(ch_CCR) < Dmax) {
      ch_CCR <- c(ch_CCR, rep(NA, Dmax - length(ch_CCR)))
    }
    LDPSR_table[i, (2 * (Dmax + 1) + 2):(Dmax * 3 + 3)] <- ch_CCR
  }


  # Marginal Bicluster Ref mat ----------------------------------------------
  pifr <- t(Pcf)
  TRP <- colSums(pifr * flddist)
  LCD <- colSums(cls01)
  CMD <- colSums(clsmemb)


  # Fit indices for two null model ----------------------------------

  indices1 <- TestFit(
    U = tmp$U, Z = tmp$Z,
    ell_A = indices$llm,
    nparam = sum(sign(sign(unlist(Pcf2)) + 1))
  )
  indices2 <- TestFitSaturated(
    U = tmp$U,
    Z = tmp$Z,
    ell_A = indices$llm,
    nparam = sum(sign(sign(unlist(Pcf2)) + 1))
  )


  ret <- structure(list(
    U = U,
    testlength = testlength,
    nobs = nobs,
    Nclass = ncls,
    Nfield = nfld,
    crr = crr(U),
    ItemLabel = tmp$ItemLabel,
    FieldLabel = FieldLabel,
    all_adj = all_adj,
    all_g = all_g,
    adj_list = adj_list,
    params = param,
    FRP = pifr,
    PSRP = PSRP,
    LDPSR = LDPSR_table,
    LFD = flddist,
    LCD = LCD,
    CMD = CMD,
    TRP = TRP,
    FieldEstimated = fld,
    ClassEstimated = cls,
    Students = Students,
    NextStage = NextStage,
    MG_FitIndices = indices1,
    SM_FitIndices = indices2
  ), class = c("exametrika", "BINET"))
  return(ret)
}
