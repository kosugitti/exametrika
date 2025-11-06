#' @rdname LRA
#' @section Rated Data Method:
#' \code{LRA.rated} analyzes data with ratings assigned to each response, such as
#' partially-credited items or preference scales where response categories have different weights.
#'
#' @param minFreqRatio Minimum frequency ratio for response categories (default = 0).
#' Categories with occurrence rates below this threshold will be excluded from analysis.
#' For example, if set to 0.1, response categories that appear in less than 10% of
#' responses for an item will be omitted.
#'
#' @return
#' For rated data (\code{LRA.rated}), the returned list additionally includes:
#' \describe{
#' \item{msg}{A character string indicating the model type. }
#' \item{converge}{Logical value indicating whether the algorithm converged within maxiter iterations}
#' \item{ScoreReport}{Descriptive statistics of test performance, including sample size,
#'   test length, central tendency, variability, distribution characteristics, and reliability.}
#' \item{ItemReport}{Basic statistics for each item including category proportions and item-total correlations.}
#' \item{ICRP}{Item Category Reference Profile matrix showing probability of response in each category by rank.}
#' \item{ScoreRankCorr}{Spearman's correlation between test scores and estimated ranks.}
#' \item{RankQuantCorr}{Spearman's correlation between estimated ranks and quantile groups.}
#' \item{ScoreRank}{Contingency table of raw scores by estimated ranks.}
#' \item{ScoreMembership}{Expected rank memberships for each raw score.}
#' \item{RankQuantile}{Cross-tabulation of rank frequencies and quantile groups.}
#' \item{MembQuantile}{Cross-tabulation of rank membership probabilities and quantile groups.}
#' \item{ItemQuantileRef}{Reference values for each item across quantile groups.}
#' \item{CatQuant}{Response patterns across item categories and quantile groups.}
#' }
#'
#' @examples
#' \donttest{
#' # Rated data example
#' # Fit a Latent Rank Analysis model with 10 ranks to rated data
#' result.LRArated <- LRA(J35S5000, nrank = 10, mic = TRUE)
#'
#' # Plot score distributions
#' plot(result.LRArated, type = "ScoreFreq")
#' plot(result.LRArated, type = "ScoreRank")
#'
#' # Plot category response patterns for items 1-6
#' plot(result.LRArated, type = "ICRP", items = 1:6, nc = 3, nr = 2)
#' }
#'
#' @importFrom stats cor
#' @importFrom utils head
#' @export
#'
LRA.rated <- function(U,
                      nrank = 2,
                      mic = FALSE,
                      maxiter = 100,
                      trapezoidal = 0,
                      eps = 1e-4,
                      minFreqRatio = 0,
                      verbose = TRUE, ...) {
  ## check trapezoidal prior
  if (trapezoidal > 0) {
    if (trapezoidal > 1 / nrank) {
      stop("Trapezoidal prior hem value must be smaller than 1/nrank")
    }
  }

  ScoreReport <- ScoreReport(U)
  ItemReport <- ItemReport(U)
  score <- rowSums(U$Z * U$U)
  maxscore <- max(score)
  zzzTotal <- apply(U$Z, 2, sum)
  scoredist <- table(factor(score, levels = 0:maxscore))
  nobs <- NROW(U$Q)
  nitems <- NCOL(U$Q)
  correct_answer <- U$CA
  nquan <- nrank
  missing1error0 <- 0
  collapse_indicator <- 100
  const <- 1e-6

  # Making Filter
  Fil <- create_filter_matrix(nrank)


  # Prepare ---------------------------------------------------------


  # Calc Frequency
  category999 <- lapply(apply(U$Q, 2, unique), sort)
  # Calc Frequency excluding missing
  category <- lapply(
    category999,
    function(x) x[x != -1]
  )
  # number of categories
  ncat999 <- sapply(category999, length)
  # number of categories excluding missing
  ncat <- sapply(category, length)

  # Frequency table of categories
  catfreq999 <- apply(U$Q, 2, table)
  catfreq <- lapply(catfreq999, function(x) x[names(x) != "-1"])

  ## Count Correct Ans
  correctFreq <- rep(0, nitems)
  for (j in 1:nitems) {
    correctFreq[j] <- sum(U$Q[, j] == U$CA[j])
  }

  ### MinFreq Check
  usecategory <- list()
  for (j in 1:nitems) {
    temp <- sapply(1:ncat[j], function(k) {
      if (catfreq[[j]][k] >= nobs * minFreqRatio || catfreq[[j]][k] != U$CA[j]) {
        category[[j]][k]
      } else {
        collapse_indicator
      }
    })
    usecategory[[j]] <- sort(unique(temp))
  }

  usecategory999 <- list()
  for (j in 1:nitems) {
    if (-1 %in% category999[[j]]) {
      usecategory999[[j]] <- c(usecategory[[j]], -1)
    } else {
      usecategory999[[j]] <- usecategory[[j]]
    }
  }

  # useNcat,useNcat999
  useNcat <- sapply(usecategory, length)
  useNcat999 <- sapply(usecategory999, length)

  # Expand data matrix
  expand_Zij <- matrix(0, nrow = nobs, ncol = sum(useNcat))
  current_col <- 1

  for (j in 1:ncol(U$Z)) {
    for (k in 1:useNcat[j]) {
      expand_Zij[, current_col] <- U$Z[, j]
      current_col <- current_col + 1
    }
  }

  # use_Xij
  use_Xij <- matrix(0, nrow = nobs, ncol = nitems)
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      if (U$Q[i, j] %in% usecategory[[j]]) {
        use_Xij[i, j] <- U$Q[i, j]
      } else {
        use_Xij[i, j] <- collapse_indicator
      }
    }
  }

  usecatfreq <- list()
  for (j in 1:nitems) {
    usecatfreq[[j]] <- sapply(usecategory[[j]], function(cat) {
      sum(use_Xij[, j] == cat)
    })
  }

  Cijk999 <- array(NA, dim = c(nobs, nitems, max(ncat999)))
  for (j in 1:nitems) {
    for (k in 1:ncat999[j]) {
      Cijk999[, j, k] <- 1 * (U$Q[, j] == category999[[j]][k])
    }
  }


  use_Cijk <- array(0, dim = c(nobs, nitems, max(sapply(usecategory, length))))
  for (j in 1:nitems) {
    for (k in 1:useNcat[j]) {
      use_Cijk[, j, k] <- 1 * (use_Xij[, j] == usecategory[[j]][k])
    }
  }

  CijkMat <- matrix(0, nrow = nobs, ncol = sum(useNcat))
  for (i in 1:nobs) {
    tmp <- c()
    for (j in 1:nitems) {
      tmp <- c(tmp, use_Cijk[i, j, 1:useNcat[j]])
    }
    CijkMat[i, ] <- tmp
  }

  quantileScore <- quantile(score, probs = (1:(nquan - 1)) / nquan)
  quantileRank <- rowSums(outer(score, quantileScore, ">")) + 1
  quantileRefVec <- matrix(0, nrow = nitems, ncol = nquan)
  for (j in 1:nitems) {
    for (k in 1:nquan) {
      quantileRefVec[j, k] <- mean(U$U[quantileRank == k, j])
    }
  }

  QRVdf <- data.frame(quantileRefVec)
  names(QRVdf) <- paste0("Q", 1:nquan)
  rownames(QRVdf) <- U$ItemLabel

  ## Category Quantile Report
  SelectRatio <- NULL
  ref_mat <- matrix(NA, nrow = sum(ncat999), ncol = nquan)
  catquanRefbox999 <- list()
  quanRefbox <- list()
  for (j in 1:nitems) {
    catquanRefbox999[[j]] <- matrix(NA, nrow = ncat999[j], ncol = nquan)
    quanRefbox[[j]] <- matrix(NA, nrow = useNcat[j], ncol = nquan)
    place0 <- (cumsum(ncat999) - ncat999 + 1)[j]
    place1 <- cumsum(ncat999[1:j])[j]
    for (q in 1:nquan) {
      catquanRefbox999[[j]][, q] <- apply(Cijk999[quantileRank == q, j, ][, 1:ncat999[j]], 2, sum) / sum(U$Z[quantileRank == q, j])
      quanRefbox[[j]][, q] <- apply(use_Cijk[quantileRank == q, j, ][, 1:useNcat[j]], 2, sum) / sum(U$Z[quantileRank == q, j])
      ref_mat[place0:place1, q] <- catquanRefbox999[[j]][, q]
    }
    category_labels <- unlist(U$CategoryLabel[j])
    if (any(names(catfreq999[[j]]) == "-1")) {
      category_labels <- c(paste0(U$ItemLabel[j], "-X"), category_labels)
    }
    item_data <- data.frame(
      item = rep(U$ItemLabel[j], ncat999[j]),
      cateogry = category_labels,
      selRatio = catfreq999[[j]] / zzzTotal[j],
      ref_mat = ref_mat[place0:place1, ]
    )
    if (any(names(catfreq999[[j]]) == "-1")) {
      item_data <- rbind(
        item_data[names(catfreq999[[j]]) != "-1", ],
        item_data[names(catfreq999[[j]]) == "-1", ]
      )
    }
    SelectRatio <- rbind(SelectRatio, item_data[, -3])
  }
  rownames(SelectRatio) <- NULL
  names(SelectRatio) <- c("Item", "Category", "SLCT Ratio", paste0("Q", 1:nquan))



  # Algorithm -------------------------------------------------------

  ## Prior
  if (trapezoidal > 0) {
    log_prior <- c(
      trapezoidal,
      rep((1 - 2 * trapezoidal) / (nrank - 2), nrank - 2),
      trapezoidal
    )
  } else {
    log_prior <- rep(1, nrank)
  }
  lp2 <- logprior_NQmat <- matrix(rep(log(log_prior), nobs), byrow = TRUE, nrow = nobs)


  # Design Vector/Matrix
  designfunc <- function(x, y) 1

  create_upper_diagonal_matrix <- function(func, n) {
    matrix <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i <= j) matrix[i, j] <- func(i, j)
      }
    }
    return(matrix)
  }

  design0 <- sapply(1:nitems, function(j) sum(useNcat[1:j]))
  design1 <- cbind(head(c(0, design0) + 1, -1), design0)

  design2 <- lapply(1:nitems, function(j) {
    seq(design1[j, 1], design1[j, 2])
  })

  design3 <- do.call(rbind, lapply(1:nitems, function(j) {
    cbind(rep(j, useNcat[j]), design2[[j]])
  }))

  design4 <- matrix(0, nrow = nitems, ncol = sum(useNcat))
  for (i in 1:nrow(design3)) {
    design4[design3[i, 1], design3[i, 2]] <- 1
  }

  design5 <- do.call(rbind, lapply(1:nitems, function(j) {
    # design4の各行jを取得し、ncat[j]回繰り返す
    matrix(rep(design4[j, ], useNcat[j]), nrow = useNcat[j], byrow = TRUE)
  }))

  design6 <- rep(NA, nitems)
  for (j in 1:nitems) {
    pos <- which(usecategory[[j]] == correct_answer[j])
    design6[j] <- design2[[j]][pos]
  }
  # Saturation Model -------------------------------------------------
  correctCRPsatu <- rep(NA, nitems)
  for (i in 1:nitems) {
    correctCRPsatu[i] <- 0.1 + 0.8 / (1 + exp(-(i - nitems / 2) / 3))
  }

  catRefbox_satu <- vector("list", nitems)
  for (j in 1:nitems) {
    catRefbox_satu[[j]] <- matrix(NA, nrow = useNcat[j], ncol = length(correctCRPsatu))
    for (k in 1:useNcat[j]) {
      if (usecategory[[j]][[k]] == correct_answer[j]) {
        catRefbox_satu[[j]][k, ] <- correctCRPsatu
      } else {
        catRefbox_satu[[j]][k, ] <- (1 - correctCRPsatu) / (useNcat[j] - 1)
      }
    }
  }

  catRefmat_satu <- do.call(rbind, catRefbox_satu)
  ## EM Algorithm
  if (verbose) {
    message("Starting EM estimation for Saturation Model...")
  }

  ij_log_lik_satu <- -10
  iter_satu <- 0
  converge_satu <- TRUE
  FLG <- TRUE
  while (FLG) {
    old_log_lik_satu <- ij_log_lik_satu

    rankProf_num_satu <- exp(CijkMat %*% log(catRefmat_satu + const))
    rankProf_den_satu <- rowSums(rankProf_num_satu)
    rankProf_satu <- rankProf_num_satu / rankProf_den_satu

    refMatcore_satu <- t(CijkMat) %*% rankProf_satu

    catRefmat_satu <- refMatcore_satu / design5 %*% refMatcore_satu
    log_lik_satu <- sum(rankProf_satu * (log(rankProf_num_satu + const)))
    ij_log_lik_satu <- log_lik_satu / nitems / nobs

    iter_satu <- iter_satu + 1
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", iter_satu, " logLik ", format(ij_log_lik_satu, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }

    if (abs(old_log_lik_satu - ij_log_lik_satu) < eps * abs(old_log_lik_satu)) {
      FLG <- FALSE
    }
    if (iter_satu > maxiter) {
      converge_satu <- FALSE
      FLG <- FALSE
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
    }
  }

  first_sum <- sum(catRefmat_satu[design6, 1])
  last_sum <- sum(catRefmat_satu[design6, ncol(catRefmat_satu)])


  if (first_sum > last_sum) {
    catRefmat_satu <- catRefmat_satu[, ncol(catRefmat_satu):1]
  }

  catRefbox_satu <- vector("list", nitems)
  for (j in 1:nitems) {
    catRefbox_satu[[j]] <- catRefmat_satu[design1[j, 1]:design1[j, 2], ]
  }

  if (mic) {
    catRefbox_satu2 <- vector("list", nitems)
    for (j in 1:nitems) {
      target_row <- catRefmat_satu[design6[j], ]
      current_matrix <- catRefbox_satu[[j]]
      augmented_matrix <- t(rbind(target_row, current_matrix))
      sorted <- augmented_matrix[do.call(order, as.data.frame(augmented_matrix)), ]
      back_matrix <- t(sorted)
      catRefbox_satu2[[j]] <- back_matrix[-1, ]
    }
    catRefmat_satu <- do.call(rbind, catRefbox_satu2)
  }


  # Restricted Model ------------------------------------------------

  catRefbox <- quanRefbox
  catRefmat <- do.call(rbind, catRefbox)

  ij_log_lik <- -10
  iter <- 0
  if (verbose) {
    message("\nStarting EM estimation for Restricted Model...")
  }
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    old_log_lik <- ij_log_lik
    rankProf_num <- exp(CijkMat %*% log(catRefmat + const) + logprior_NQmat)
    rankProf_den <- rowSums(rankProf_num)
    rankProf <- rankProf_num / rankProf_den

    refMatcore <- t(CijkMat) %*% rankProf %*% Fil
    catRefmat <- refMatcore / design5 %*% refMatcore

    first_col_sum <- sum(catRefmat[design6, 1])
    last_col_sum <- sum(catRefmat[design6, ncol(catRefmat)])
    if (first_col_sum > last_col_sum) {
      catRefmat <- catRefmat[, ncol(catRefmat):1]
    }
    for (j in 1:nitems) {
      catRefbox[[j]] <- catRefmat[design1[j, 1]:design1[j, 2], ]
    }

    if (mic == 1) {
      for (j in 1:nitems) {
        sort_order <- order(catRefmat[design6[[j]], ])
        catRefbox[[j]] <- catRefbox[[j]][, sort_order]
      }
    }

    catRefmat <- do.call(rbind, catRefbox)

    log_lik <- sum(rankProf * log(rankProf_num))
    ij_log_lik <- log_lik / nitems / nobs

    iter <- iter + 1
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", iter, " logLik ", format(ij_log_lik, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }

    if (abs(old_log_lik - ij_log_lik) < eps * abs(old_log_lik)) {
      FLG <- FALSE
    }
    if (iter > maxiter) {
      converge <- FALSE
      FLG <- FALSE
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
    }
  }


  # results ---------------------------------------------------------

  category_report <- as.data.frame(catRefmat)
  colnames(category_report) <- paste0("rank", 1:nrank)

  cat_labelXXX <- vector("list", length(category))
  for (i in seq_along(category)) {
    original_labels <- U$CategoryLabel[[i]]
    used_cats <- usecategory[[i]]

    is_dropped <- used_cats == 100

    if (any(is_dropped)) {
      used_labels <- original_labels[used_cats[!is_dropped]]
      cat_labelXXX[[i]] <- c(used_labels, "CatX")
    } else {
      cat_labelXXX[[i]] <- original_labels
    }
  }
  category_report$ItemLabel <- rep(U$ItemLabel, U$categories)
  category_report$CategoryLabel <- unlist(U$CategoryLabel)
  ## Item-Category Reference Profile
  ICRP <- category_report[, c("ItemLabel", "CategoryLabel", paste0("rank", 1:nrank))]

  #
  testRefVec <- apply(catRefmat[design6, ], 2, sum)

  ## rank Profile
  rankProf_num <- exp(CijkMat %*% log(catRefmat + const) + logprior_NQmat)
  rankProf_den <- rowSums(rankProf_num)
  rankProf <- rankProf_num / rankProf_den

  rankmemb <- apply(rankProf, 1, which.max)
  rankmemb01 <- sign(rankProf - apply(rankProf, 1, max)) + 1
  rankdist <- colSums(rankmemb01)
  RMD <- colSums(rankProf)

  StudentRank <- rankProf
  RU <- ifelse(rankmemb + 1 > nrank, NA, rankmemb + 1)
  RD <- ifelse(rankmemb - 1 < 1, NA, rankmemb - 1)
  RUO <- StudentRank[cbind(1:nobs, RU)] / StudentRank[cbind(1:nobs, rankmemb)]
  RDO <- StudentRank[cbind(1:nobs, RD)] / StudentRank[cbind(1:nobs, rankmemb)]
  StudentRank <- cbind(StudentRank, score, rankmemb, quantileRank, RUO, RDO)
  colnames(StudentRank) <- c(
    paste("Membership", 1:nrank), "Score", "Estimate",
    "Quantile Rank",
    "Rank-Up Odds", "Rank-Down Odds"
  )
  rownames(StudentRank) <- U$ID

  rho1 <- cor(score, rankmemb, method = "spearman")

  ## score-rank dist
  scoreRankDist <- matrix(0, nrow = maxscore + 1, ncol = nrank)
  scoreMembDist <- matrix(0, nrow = maxscore + 1, ncol = nrank)
  rownames(scoreRankDist) <- rownames(scoreMembDist) <- 0:maxscore
  colnames(scoreRankDist) <- colnames(scoreMembDist) <- paste0("Rank", 1:nrank)
  for (s in 0:maxscore) {
    if (scoredist[s + 1] > 0) {
      ranks_s <- rankmemb[score == s]
      scoreRankDist[s + 1, ] <- sapply(1:nrank, function(r) sum(ranks_s == r))
      scoreMembDist[s + 1, ] <- colSums(rankProf[score == s, , drop = FALSE])
    }
  }
  rankQuanDist <- unname(table(rankmemb, quantileRank))
  membQuanDist <- matrix(0, nrow = nrank, ncol = nquan)
  rho2 <- cor(rankmemb, quantileRank, method = "spearman")
  for (q in 1:nquan) {
    membQuanDist[, q] <- colSums(rankProf[quantileRank == q, , drop = FALSE])
  }
  rownames(rankQuanDist) <- rownames(membQuanDist) <- paste0("Rank", 1:nrank)
  colnames(rankQuanDist) <- colnames(membQuanDist) <- paste0("Quantile", 1:nrank)

  # Fit indices -----------------------------------------------------
  itemdf <- (useNcat - 1) * (nitems - sum(diag(Fil)))
  testdf <- sum(itemdf)
  null_itemdf <- (useNcat - 1) * (nitems - 1)
  null_testdf <- sum(null_itemdf)

  rankProf_num_satu <- exp(CijkMat %*% log(catRefmat_satu + const))
  rankProf_den_satu <- rowSums(rankProf_num_satu)
  rankProf_satu <- rankProf_num_satu / rankProf_den_satu
  Rank_satu <- apply(rankProf_satu, 1, which.max)
  Rank_satu01 <- sign(rankProf_satu - apply(rankProf_satu, 1, max)) + 1

  # modelfff_jq1 <- t(dat$Z) %*% rankmemb01
  # modelggg_jq1 <- t(CijkMat) %*% rankmemb01
  # satufff_jq1 <- t(dat$Z) %*% Rank_satu01
  # satuggg_jq1 <- t(CijkMat) %*% Rank_satu01
  modelfff_jq2 <- t(U$Z) %*% rankProf
  modelggg_jq2 <- t(CijkMat) %*% rankProf
  satufff_jq2 <- t(U$Z) %*% rankProf_satu
  satuggg_jq2 <- t(CijkMat) %*% rankProf_satu

  # Model item log-likelihood
  # model_itemll1 <- design4 %*% colSums(t(modelggg_jq1 * log(catRefmat + const)))
  model_itemll2 <- design4 %*% colSums(t(modelggg_jq2 * log(catRefmat + const)))

  # Saturated item log-likelihood
  # satu_itemll1 <- design4 %*% colSums(t(satuggg_jq1 * log(catRefmat_satu + const)))
  satu_itemll2 <- design4 %*% colSums(t(satuggg_jq2 * log(catRefmat_satu + const)))

  null_itemll <- rep(0, nitems)
  zzzTotal <- apply(U$Z, 2, sum)
  for (j in 1:nitems) {
    null_itemll[j] <- sum(usecatfreq[[j]] * log(usecatfreq[[j]] / zzzTotal[j] + const))
  }


  # Model chi-square
  # model_itemchisq1 <- pmax(0.000001, pmin(2 * (satu_itemll1 - model_itemll1), 1000000000))
  model_itemchisq2 <- pmax(0.000001, pmin(2 * (satu_itemll2 - model_itemll2), 1000000000))

  # Null chi-square
  # null_itemchisq1 <- pmax(0.000001, pmin(2 * (satu_itemll1 - null_itemll), 1000000000)) + 0.000001
  null_itemchisq2 <- pmax(0.000001, pmin(2 * (satu_itemll2 - null_itemll), 1000000000)) + 0.000001

  # Item Fit Indices
  # ItemFitIndices1 <- calcFitIndices(model_itemchisq1, null_itemchisq1, itemdf, null_itemdf, colSums(dat$Z))
  ItemFits <- calcFitIndices(model_itemchisq2, null_itemchisq2, itemdf, null_itemdf, colSums(U$Z))
  ItemFitIndices <- c(
    list(
      model_Chi_sq = model_itemchisq2,
      null_Chi_sq = null_itemchisq2,
      model_df = itemdf,
      null_df = null_itemdf
    ),
    ItemFits
  )

  TestFits <- calcFitIndices(
    sum(model_itemchisq2),
    sum(null_itemchisq2) + const,
    sum(itemdf),
    sum(null_itemdf),
    sum(colSums(U$Z))
  )

  TestFitIndices <- c(
    list(
      ScoreRankCorr = rho1,
      RankQuantCorr = rho2,
      model_log_like = log_lik,
      null_log_like = log_lik_satu,
      model_Chi_sq = sum(model_itemchisq2),
      null_Chi_sq = sum(null_itemchisq2),
      model_df = sum(itemdf),
      null_df = sum(null_itemdf)
    ),
    TestFits
  )

  # output ----------------------------------------------------------

  ret <- structure(list(
    U = U,
    mic = mic,
    msg = "Rank",
    converge = converge,
    testlength = NCOL(U$Q),
    nobs = NROW(U$Q),
    Nrank = nrank,
    N_Cycle = iter,
    TRP = as.vector(testRefVec),
    LRD = rankdist,
    RMD = RMD,
    ICRP = ICRP,
    ScoreRankCorr = rho1,
    RankQuantCorr = rho2,
    Students = StudentRank,
    ScoreRank = scoreRankDist,
    ScoreMembership = scoreMembDist,
    RankQuantile = rankQuanDist,
    MembQuantile = membQuanDist,
    ItemFitIndices = ItemFitIndices,
    TestFitIndices = TestFitIndices,
    ScoreReport = ScoreReport,
    ItemReport = ItemReport,
    ItemQuantileRef = QRVdf,
    CatQuant = SelectRatio
  ), class = c("exametrika", "LRArated"))
  return(ret)
}
