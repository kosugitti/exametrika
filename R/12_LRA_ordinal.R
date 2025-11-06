#' @rdname LRA
#' @section Ordinal Data Method:
#' \code{LRA.ordinal} analyzes ordered categorical data with multiple thresholds,
#' such as Likert-scale responses or graded items.
#'
#' @param trapezoidal Specifies the height of both tails when using a trapezoidal
#' prior distribution. Must be less than 1/nrank. The default value is 0, which
#' results in a uniform prior distribution.
#' @param eps Convergence threshold for parameter updates. Default is 1e-4.
#'
#' @return
#' For ordinal data (\code{LRA.ordinal}), the returned list additionally includes:
#' \describe{
#' \item{msg}{A character string indicating the model type. }
#' \item{converge}{Logical value indicating whether the algorithm converged within maxiter iterations}
#' \item{ScoreReport}{Descriptive statistics of test performance, including sample size,
#'   test length, central tendency, variability, distribution characteristics, and reliability.}
#' \item{ItemReport}{Basic statistics for each item including category proportions and item-total correlations.}
#' \item{ICBR}{Item Category Boundary Reference matrix showing cumulative probabilities for rank-category combinations.}
#' \item{ICRP}{Item Category Reference Profile matrix showing probability of response in each category by rank.}
#' \item{ScoreRankCorr}{Spearman's correlation between test scores and estimated ranks.}
#' \item{RankQuantCorr}{Spearman's correlation between estimated ranks and quantile groups.}
#' \item{ScoreRank}{Contingency table of raw scores by estimated ranks.}
#' \item{ScoreMembership}{Expected rank memberships for each raw score.}
#' \item{RankQuantile}{Cross-tabulation of rank frequencies and quantile groups.}
#' \item{MembQuantile}{Cross-tabulation of rank membership probabilities and quantile groups.}
#' \item{CatQuant}{Response patterns across item categories and quantile groups.}
#' }
#'
#' @examples
#' \donttest{
#' # Ordinal data example
#' # Fit a Latent Rank Analysis model with 3 ranks to ordinal data
#' result.LRAord <- LRA(J15S3810, nrank = 3, mic = TRUE)
#'
#' # Plot score distributions
#' plot(result.LRAord, type = "ScoreFreq")
#' plot(result.LRAord, type = "ScoreRank")
#'
#' # Plot category response patterns for items 1-6
#' plot(result.LRAord, type = "ICBR", items = 1:6, nc = 3, nr = 2)
#' plot(result.LRAord, type = "ICRP", items = 1:6, nc = 3, nr = 2)
#' }
#'
#' @importFrom stats cor
#' @export
#'
LRA.ordinal <- function(U,
                        nrank = 2,
                        mic = FALSE,
                        maxiter = 100,
                        trapezoidal = 0,
                        eps = 1e-4,
                        verbose = TRUE, ...) {
  ## check trapezoidal prior
  if (trapezoidal > 0) {
    if (trapezoidal > 1 / nrank) {
      stop("Trapezoidal prior hem value must be smaller than 1/nrank")
    }
  }

  ScoreReport <- ScoreReport(U)
  ItemReport <- ItemReport(U)
  score <- rowSums(U$Z * U$Q)
  maxscore <- max(score)
  scoredist <- table(factor(score, levels = 0:maxscore))
  nobs <- nrow(U$Q)
  nitems <- ncol(U$Q)
  nquan <- nrank
  const <- 1e-6

  # Prepare ---------------------------------------------------------
  score <- rowSums(U$Z * U$Q)
  zzzTotal <- apply(U$Z, 2, sum)


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


  UU <- array(NA, dim = c(nobs, nitems, max(ncat)))
  YY <- array(0, dim = c(nobs, nitems, max(ncat) - 1))
  uuMat <- matrix(NA, nrow = nobs, ncol = nitems * max(ncat))
  for (i in 1:nobs) {
    for (j in 1:nitems) {
      for (k in 1:ncat[j]) {
        UU[i, j, k] <- ifelse(U$Q[i, j] == category[[j]][k], 1, 0)
      }
      for (k in 2:ncat[j]) {
        YY[i, j, k - 1] <- ifelse((U$Z[i, j] * U$Q[i, j]) >= category[[j]][k], 1, 0)
      }
    }
    uuMat[i, ] <- as.vector(t(UU[i, , ]))
  }



  quantileScore <- quantile(score, probs = (1:(nquan - 1)) / nquan)
  quantileRank <- rowSums(outer(score, quantileScore, ">")) + 1


  quanRefmat <- array(NA, dim = c(nitems, max(ncat) - 1, nquan))
  catquanRefmat <- array(NA, dim = c(nitems, max(ncat), nquan))

  for (j in 1:nitems) {
    for (q in 1:nquan) {
      quanRefmat[j, , q] <- apply(YY[quantileRank == q, j, ], 2, sum) / apply(U$Z[quantileRank == q, ], 2, sum)[j]
      catquanRefmat[j, , q] <- apply(UU[quantileRank == q, j, ], 2, sum) / apply(U$Z[quantileRank == q, ], 2, sum)[j]
    }
  }


  ## Category Quantile Report

  SelectRatio <- NULL
  ref_mat <- matrix(NA, nrow = sum(ncat), ncol = nquan)
  colnames(ref_mat) <- paste0("Q", 1:nquan)
  for (i in 1:nitems) {
    item_name <- U$ItemLabel[i]
    categories <- dimnames(catfreq[[i]])[[1]]
    ratios <- as.vector(catfreq[[i]] / zzzTotal[i])

    ref_mat[((i - 1) * ncat[i] + 1):((i * ncat[i])), ] <- catquanRefmat[i, , ]

    item_data <- data.frame(
      item = item_name,
      category = categories,
      selectRatio = ratios
    )

    SelectRatio <- rbind(SelectRatio, item_data)
  }

  SelectRatioTable <- cbind(SelectRatio, ref_mat)

  # Algorithm -------------------------------------------------------

  ## Filter
  Fil <- create_filter_matrix(nrank)

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
  logprior_NQmat <- matrix(rep(log(log_prior), nobs), byrow = TRUE, nrow = nobs)

  ## Design vector/Matrix
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

  design0 <- sapply(1:nitems, function(j) sum(ncat[1:j]))
  design1 <- cbind(design0 - ncat[1] + 1, design0)

  design2 <- lapply(1:nitems, function(j) {
    seq(design1[j, 1], design1[j, 2])
  })

  design3 <- do.call(rbind, lapply(1:nitems, function(j) {
    cbind(rep(j, ncat[j]), design2[[j]])
  }))

  design4 <- matrix(0, nrow = nitems, ncol = sum(ncat))
  for (i in 1:nrow(design3)) {
    design4[design3[i, 1], design3[i, 2]] <- 1
  }

  design5 <- do.call(rbind, lapply(1:nitems, function(j) {
    matrix(rep(design4[j, ], ncat[j]), nrow = ncat[j], byrow = TRUE)
  }))

  design6_matrix <- create_upper_diagonal_matrix(designfunc, sum(ncat))
  design6 <- design5 * design6_matrix


  # Saturation Model -------------------------------------------------
  ij_log_lik_satu <- -10
  iter_satu <- 0
  refbox_satu <- array(0, dim = c(nitems, max(ncat) - 1, nitems))
  for (j in 1:nitems) {
    for (k in 1:(ncat[j] - 1)) {
      for (q in 1:nitems) {
        refbox_satu[j, k, q] <- 0.1 + 0.8 / (1 + exp(-q + nitems * k / ncat[j]))
      }
    }
  }

  refbox111_satu <- array(1, dim = c(nitems, max(ncat), nitems))
  refbox000_satu <- array(0, dim = c(nitems, max(ncat), nitems))
  refbox111_satu[, 2:max(ncat), ] <- refbox_satu
  refbox000_satu[, 1:(max(ncat) - 1), ] <- refbox_satu
  catrefbox_satu <- refbox111_satu - refbox000_satu
  catRefMat_satu <- matrix(0, nrow = (nitems * max(ncat)), ncol = nitems)
  for (i in 1:nitems) {
    l <- (i - 1) * ncat[i] + 1
    catRefMat_satu[l:(l + ncat[i] - 1), ] <- catrefbox_satu[i, , ]
  }
  ## EM Algorithm
  if (verbose) {
    message("Starting EM estimation for Saturation Model...")
  }
  converge_satu <- TRUE
  FLG <- TRUE
  while (FLG) {
    old_log_like_satu <- ij_log_lik_satu
    ## Estep
    nume_satu <- exp(uuMat %*% log(catRefMat_satu + const))
    denom_satu <- rowSums(nume_satu)
    rankProf_satu <- nume_satu / denom_satu

    ## Mstep
    refMatcore_satu <- t(uuMat) %*% rankProf_satu
    refMat111_satu <- design6 %*% refMatcore_satu / design5 %*% refMatcore_satu
    delete_rows <- design0 - ncat[1] + 1
    refMat_satu <- refMat111_satu[-delete_rows, ]
    refMat000_satu <- rbind(refMat111_satu[-1, ], refMat111_satu[1, ])
    refMat000_satu[design0, ] <- 0
    catRefMat_satu <- refMat111_satu - refMat000_satu
    ### Log Lik for Saturation Model
    log_lik_satu <- sum(rankProf_satu * log(nume_satu + const))
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

    if (abs(old_log_like_satu - ij_log_lik_satu) < (eps * abs(old_log_like_satu))) {
      FLG <- FALSE
    }
    if (iter_satu > maxiter) {
      converge_satu <- FALSE
      FLG <- FALSE
      message("\nReached the maximum number of iterations.")
      message("Warning: Algorithm may not have converged. Interpret results with caution.")
    }
  }

  # monotonic increasing check
  col_sums <- colSums(refMat111_satu)
  if (col_sums[1] > col_sums[ncol(refMat111_satu)]) {
    refMat111_satu <- refMat111_satu[, ncol(refMat111_satu):1]
  }
  if (mic) {
    refMat111_satu <- t(apply(refMat111_satu, 1, sort))
  }

  delete_rows <- design0 - ncat[1] + 1
  refMat_satu <- refMat111_satu[-delete_rows, ]

  refMat000_satu <- rbind(refMat111_satu[-1, ], refMat111_satu[1, ])
  refMat000_satu[design0, ] <- 0

  catRefMat_satu <- refMat111_satu - refMat000_satu


  # Restricted Model ------------------------------------------------
  ij_log_lik <- -10
  iter <- 0
  refBox <- array(0, dim = c(nitems, max(ncat) - 1, nrank))

  for (j in 1:nitems) {
    for (k in 1:(ncat[j] - 1)) {
      refBox[j, k, ] <- sort(quanRefmat[j, k, ])
    }
  }


  refBox111 <- array(1, dim = c(nitems, max(ncat), nrank))
  refBox000 <- array(0, dim = c(nitems, max(ncat), nrank))
  refBox111[, 2:(max(ncat)), ] <- refBox
  refBox000[, 1:(max(ncat) - 1), ] <- refBox
  catRefBox <- refBox111 - refBox000

  refMat <- matrix(0, nrow = nitems * (max(ncat) - 1), nrank)
  refMat111 <- matrix(0, nrow = nitems * (max(ncat)), nrank)
  refMat000 <- matrix(0, nrow = nitems * (max(ncat)), nrank)
  catRefMat <- matrix(0, nrow = nitems * (max(ncat)), nrank)
  for (i in 1:nitems) {
    l <- (i - 1) * (max(ncat) - 1) + 1
    refMat[l:(l + max(ncat) - 2), ] <- refBox[i, , ]
    m <- (i - 1) * (max(ncat)) + 1
    refMat111[m:(m + max(ncat) - 1), ] <- refBox111[i, , ]
    refMat000[m:(m + max(ncat) - 1), ] <- refBox000[i, , ]
    catRefMat[m:(m + max(ncat) - 1), ] <- catRefBox[i, , ]
  }

  ## EM Algorithm
  if (verbose) {
    message("\nStarting EM estimation for Restricted Model...")
  }
  converge <- TRUE
  FLG <- TRUE
  while (FLG) {
    old_log_like <- ij_log_lik
    ## Estep
    nume <- exp(uuMat %*% log(catRefMat + const) + logprior_NQmat)
    denom <- rowSums(nume)
    rankProf <- nume / denom

    # Filtering
    refMatcore <- t(uuMat) %*% rankProf %*% Fil
    refMat111 <- design6 %*% refMatcore / design5 %*% refMatcore

    if (sum(refMat111[1, ]) > sum(refMat111[nrank, ])) {
      refMat111 <- refMat111[, ncol(refMat111):1]
    }

    if (mic == 1) {
      refMat111 <- t(apply(refMat111, 1, sort))
    }

    delete_rows <- sapply(1:nitems, function(j) design0[j] - ncat[1] + 1)
    refMat <- refMat111[-delete_rows, ]

    refMat000 <- rbind(refMat111[2:nrow(refMat111), ], rep(0, nrank))
    refMat000[design0, ] <- 0
    catRefMat <- refMat111 - refMat000

    log_lik <- sum(rankProf * log(nume))
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
    if (abs(old_log_like - ij_log_lik) < (eps * abs(old_log_like))) {
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
  nume <- exp(uuMat %*% log(catRefMat + const) + logprior_NQmat)
  denom <- rowSums(nume)
  rankProf <- nume / denom

  ## Item - Prob report
  boundary_report <- as.data.frame(refMat111)
  colnames(boundary_report) <- paste0("rank", 1:nrank)
  boundary_report$ItemLabel <- rep(U$ItemLabel, U$categories)
  boundary_report$CategoryLabel <- unlist(U$CategoryLabel)
  ## Item-Category Boundary Profile
  ICBR <- boundary_report[, c("ItemLabel", "CategoryLabel", paste0("rank", 1:nrank))]

  category_report <- as.data.frame(catRefMat)
  colnames(category_report) <- paste0("rank", 1:nrank)
  category_report$ItemLabel <- rep(U$ItemLabel, U$categories)
  category_report$CategoryLabel <- unlist(U$CategoryLabel)
  ## Item-Category Reference Profile
  ICRP <- category_report[, c("ItemLabel", "CategoryLabel", paste0("rank", 1:nrank))]

  testRefVec <- t(catRefMat) %*% unlist(category)

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

  # Fit Indices -----------------------------------------------------

  itemdf <- (ncat - 1) * (nitems - sum(diag(Fil)))
  testdf <- sum(itemdf)
  null_itemdf <- (ncat - 1) * (nitems - 1)
  null_testdf <- sum(null_itemdf)

  rankProf_satu_num <- exp(uuMat %*% log(catRefMat_satu + const))
  rankProf_satu_denom <- rowSums(rankProf_satu_num)
  rankProf_satu <- rankProf_satu_num / rankProf_satu_denom
  Rank_satu <- apply(rankProf_satu, 1, which.max)
  Rank_satu01 <- sign(rankProf_satu - apply(rankProf_satu, 1, max)) + 1


  # modelfff_jq1 <- t(dat$Z) %*% rankmemb01
  # modelggg_jq1 <- t(uuMat) %*% rankmemb01
  # satufff_jq1 <- t(dat$Z) %*% Rank_satu01
  # satuggg_jq1 <- t(uuMat) %*% Rank_satu01
  modelfff_jq2 <- t(U$Z) %*% rankProf
  modelggg_jq2 <- t(uuMat) %*% rankProf
  satufff_jq2 <- t(U$Z) %*% rankProf_satu
  satuggg_jq2 <- t(uuMat) %*% rankProf_satu

  # Model item log-likelihood
  # model_itemll1 <- design4 %*% colSums(t(modelggg_jq1 * log(catRefMat + const)))
  model_itemll2 <- design4 %*% colSums(t(modelggg_jq2 * log(catRefMat + const)))

  # Saturated item log-likelihood
  # satu_itemll1 <- design4 %*% colSums(t(satuggg_jq1 * log(catRefMat_satu + const)))
  satu_itemll2 <- design4 %*% colSums(t(satuggg_jq2 * log(catRefMat_satu + const)))

  # Null item log-likelihood
  catfreqMat <- matrix(unlist(catfreq), ncol = ncat, byrow = T)
  null_itemll <- colSums(t(catfreqMat * log(catfreqMat / zzzTotal + const)))

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

  # Test Fit Indices
  # TestFitindices1 <- calcFitIndices(
  #   sum(model_itemchisq1),
  #   sum(null_itemchisq1) + const,
  #   sum(itemdf),
  #   sum(null_itemdf),
  #   sum(colSums(dat$Z))
  # )

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
    testlength = NCOL(U$Q),
    msg = "Rank",
    converge = converge,
    nobs = NROW(U$Q),
    Nrank = nrank,
    N_Cycle = iter,
    TRP = as.vector(testRefVec),
    LRD = rankdist,
    RMD = RMD,
    ICBR = ICBR,
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
    CatQuant = SelectRatioTable
  ), class = c("exametrika", "LRAordinal"))
  return(ret)
}
