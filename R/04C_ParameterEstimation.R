#' @title Prior distribution function with respect to the slope.
#' @param a slope coefficient
#' @param m prior parameter to be set
#' @param s prior parameter to be set
#' @param const A very small constant
#'
slopeprior <- function(a, m, s, const = 1e-15) {
  -(log(max(a, const)) - m)^2 / (2 * s^2) - log(max(a, const)) - log(s)
}

#' @title Prior distribution function with guessing parameter
#' @param c guessing parameter
#' @param alp prior to be set
#' @param bet prior to be set
#'
asymprior <- function(c, alp, bet) {
  (alp - 1) * log(c) + (bet - 1) * log(1 - c)
}

#' @title Log-likelihood function used in the Maximization Step (M-Step).
#' @param model 2,3,or 4 PL
#' @param lambda item parameter vector
#' @param qjtrue correct resp pattern
#' @param qjfalse incorrect resp pattern
#' @param quadrature Pattern of a segmented normal distribution.

objective_function_IRT <- function(lambda, model, qjtrue, qjfalse, quadrature) {
  a <- lambda[1]
  b <- lambda[2]
  c <- ifelse(model > 2, lambda[3], 0)
  d <- ifelse(model > 3, lambda[4], 1)

  exloglike <- sum(
    qjtrue * log(LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)) +
      qjfalse * log(1 - LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature))
  )

  exloglike <- exloglike - ((b / 2)^2 / 2) + slopeprior(a, 0, 0.5)

  if (model >= 3) {
    exloglike <- exloglike + asymprior(c, 2, 5)
  }

  if (model == 4) {
    exloglike <- exloglike + asymprior(d, 10, 2)
  }

  return(exloglike)
}

#' @title internal functions for PSD of Item parameters
#' @param model 2,3,or 4PL
#' @param Lambda item parameters Matrix
#' @param quadrature quads
#' @param marginal_posttheta marginal post theta

PSD_item_params <- function(model, Lambda, quadrature, marginal_posttheta) {
  J <- NROW(Lambda)
  ret <- array(NA, dim = c(J, model))
  for (j in 1:J) {
    a <- Lambda[j, 1]
    b <- Lambda[j, 2]
    c <- Lambda[j, 3]
    d <- Lambda[j, 4]
    ### I_pr
    I_pr_lambda <- diag(rep(NA, model))
    I_pr_lambda[1, 1] <- (1 - 0.5^2 - log(a)) / (a^2 * 0.5^2)
    I_pr_lambda[2, 2] <- 1 / 2^2
    if (model > 2) {
      I_pr_lambda[3, 3] <- 1 / c^2 + 4 / (1 - c)^2
    }
    if (model > 3) {
      I_pr_lambda[4, 4] <- 9 / d^2 + 1 / (1 - d)^2
    }
    ## I_F
    p <- LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)
    q <- 1 - p
    I_F_lambda <- matrix(rep(NA, model * model), ncol = model)
    den <- (d - c)^2 * p * q
    ## aa
    num <- (quadrature - b)^2 * (p - c)^2 * (d - p)^2
    I_F_lambda[1, 1] <- sum((num / den) * marginal_posttheta)
    ## ba
    num <- a * (quadrature - b) * (p - c)^2 * (d - p)^2
    I_F_lambda[1, 2] <- I_F_lambda[2, 1] <- -1 * sum(num / den * marginal_posttheta)
    ## bb
    num <- a^2 * (p - c)^2 * (d - p)^2
    I_F_lambda[2, 2] <- sum((num / den) * marginal_posttheta)

    if (model > 2) {
      ## ca
      num <- (quadrature - b) * (p - c) * (d - p)^2
      I_F_lambda[1, 3] <- I_F_lambda[3, 1] <- sum((num / den) * marginal_posttheta)
      ## cb
      num <- a * (p - c) * (d - p)^2
      I_F_lambda[2, 3] <- I_F_lambda[3, 2] <- -1 * sum((num / den) * marginal_posttheta)
      ## cc
      num <- (d - p)^2
      I_F_lambda[3, 3] <- sum(num / den * marginal_posttheta)
    }

    if (model > 3) {
      ## da
      num <- (quadrature - b) * (p - c)^2 * (d - p)
      I_F_lambda[1, 4] <- I_F_lambda[4, 1] <- sum((num / den) * marginal_posttheta)
      ## db
      num <- a * (p - c)^2 * (d - p)
      I_F_lambda[2, 4] <- I_F_lambda[4, 2] <- -1 * sum((num / den) * marginal_posttheta)
      ## dc
      num <- (p - c) * (d - p)
      I_F_lambda[3, 4] <- I_F_lambda[4, 3] <- sum((num / den) * marginal_posttheta)
      ## dd
      num <- (p - c)^2
      I_F_lambda[4, 4] <- sum((num / den) * marginal_posttheta)
    }

    ##
    Ij <- I_F_lambda + I_pr_lambda
    ret[j, ] <- sqrt(diag(solve(Ij)))
    ## Return
  }
  return(PSD = ret)
}


#' @title Estimating Item parameters using EM algorithm
#' @description
#' A function for estimating item parameters using the EM algorithm.
#' @param model This argument takes the number of item parameters to be
#' estimated in the logistic model. It is limited to values 2, 3, or 4.
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the \code{\link{dataFormat}} function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param verbose logical; if TRUE, shows progress of iterations (default: TRUE)
#' @importFrom stats optim
#' @details
#' Apply the 2, 3, and 4 parameter logistic models to estimate the item and subject populations.
#' The 4PL model can be described as follows.
#' \deqn{P(\theta,a_j,b_j,c_j,d_j)= c_j + \frac{d_j -c_j}{1+exp\{-a_j(\theta - b_j)\}}}
#' \eqn{a_j, b_j, c_j}, and \eqn{d_j} are parameters related to item j, and are parameters that
#' adjust the logistic curve.
#' \eqn{a_j} is called the slope parameter, \eqn{b_j} is the location, \eqn{c_j} is the lower asymptote,
#' and \eqn{d_j} is the upper asymptote parameter.
#' The model includes lower models, and among the 4PL models, the case where \eqn{d=1} is the 3PL model,
#' and among the 3PL models, the case where \eqn{c=0} is the 2PL model.
#' @return
#' \describe{
#' \item{model}{number of item parameters you set.}
#' \item{testlength}{Length of the test. The number of items included in the test.}
#' \item{nobs}{Sample size. The number of rows in the dataset.}
#' \item{params}{Matrix containing the estimated item parameters}
#' \item{Q3mat}{Q3-matrix developed by Yen(1984)}
#' \item{itemPSD}{Posterior standard deviation of the item parameters}
#' \item{ability}{Estimated parameters of students ability}
#' \item{ItemFitIndices}{Fit index for each item.See also \code{\link{ItemFit}}}
#' \item{TestFitIndices}{Overall fit index for the test.See also \code{\link{TestFit}}}
#' }
#' @references Yen, W. M. (1984) Applied Psychological Measurement, 8, 125-145.
#' @importFrom stats rnorm
#' @examples
#' \donttest{
#' # Fit a 3-parameter IRT model to the sample dataset
#' result.IRT <- IRT(J15S500, model = 3)
#'
#' # Display the first few rows of estimated student abilities
#' head(result.IRT$ability)
#'
#' # Plot Item Response Function (IRF) for items 1-6 in a 2x3 grid
#' plot(result.IRT, type = "IRF", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot Item Information Function (IIF) for items 1-6 in a 2x3 grid
#' plot(result.IRT, type = "IIF", items = 1:6, nc = 2, nr = 3)
#'
#' # Plot the Test Information Function (TIF) for all items
#' plot(result.IRT, type = "TIF")
#' }
#' @export
#'

IRT <- function(U, model = 2, na = NULL, Z = NULL, w = NULL, verbose = TRUE) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  if (U$response.type != "binary") {
    response_type_error(U$response.type, "IRT")
  }

  U <- tmp$U * tmp$Z

  rho <- exametrika::ItemTotalCorr(U)
  tau <- exametrika::ItemThreshold(U)

  # initialize
  testlength <- NCOL(U)
  nobs <- NROW(U)
  slope <- 2 * rho
  loc <- 2 * tau
  if (model >= 3) {
    loasym <- rep(0.05, testlength)
  } else {
    loasym <- rep(0, testlength)
  }
  if (model >= 4) {
    upasym <- rep(0.95, testlength)
  } else {
    upasym <- rep(1, testlength)
  }

  paramset <- matrix(c(slope, loc, loasym, upasym), ncol = 4)
  quadrature <- seq(-3.2, 3.2, 0.4)
  const <- exp(-testlength)
  loglike <- -1 / const
  oldloglike <- -2 / const
  itemloglike <- rep(loglike / testlength, testlength)

  # EM algorithm
  emt <- 0
  maxemt <- 25
  FLG <- TRUE
  itemloglike <- array(NA, testlength)

  while (FLG) {
    if (abs(loglike - oldloglike) < 0.00001 * abs(oldloglike)) {
      FLG <- FALSE
    }
    emt <- emt + 1
    oldloglike <- loglike
    if (emt > maxemt) {
      FLG <- FALSE
    }
    ### Expectation
    lpj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
    for (j in 1:testlength) {
      lpj[j, ] <- log(LogisticModel(
        a = paramset[j, 1],
        b = paramset[j, 2],
        c = paramset[j, 3],
        d = paramset[j, 4],
        theta = quadrature
      ) + const)
    }

    lqj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
    for (j in 1:testlength) {
      lqj[j, ] <- log(1 - LogisticModel(
        a = paramset[j, 1],
        b = paramset[j, 2],
        c = paramset[j, 3],
        d = paramset[j, 4],
        theta = quadrature
      ) + const)
    }

    posttheta_numerator <- exp(
      (tmp$Z * tmp$U) %*% lpj +
        (tmp$Z * (1 - tmp$U)) %*% lqj -
        matrix(rep(quadrature^2 / 2, NROW(tmp$U)), nrow = NROW(tmp$U), byrow = T)
    )

    post_theta <- posttheta_numerator / rowSums(posttheta_numerator)

    marginal_posttheta <- colSums(post_theta)
    #
    qjtrue <- t(tmp$Z * tmp$U) %*% post_theta
    qjfalse <- t(tmp$Z * (1 - tmp$U)) %*% post_theta


    ### Maximize
    totalLogLike <- 0
    for (j in 1:testlength) {
      ## Initial perturbation and setting of upper and lower limits.
      initial_values <- paramset[j, 1:model] + rnorm(model, 0, 0.01)
      lowers <- c(-Inf, -5, 1e-10, 1e-10)[1:model]
      uppers <- c(Inf, 5, 1 - 1e-10, 1 - 1e-10)[1:model]
      ## optimization
      optim_flg <- FALSE
      warning_flg <- FALSE
      max_attempt <- 100
      attempt <- 0
      while (!optim_flg & attempt < max_attempt) {
        attempt <- attempt + 1
        tryCatch(
          {
            result <- optim(
              par = initial_values,
              fn = objective_function_IRT,
              method = "L-BFGS-B",
              lower = lowers,
              upper = uppers,
              control = list(fnscale = -1, factr = 1e-10),
              ### args for objective_function_IRT
              model = model,
              qjtrue = qjtrue[j, ],
              qjfalse = qjfalse[j, ],
              quadrature = quadrature
            )
            optim_flg <- TRUE
          },
          error = function(e) {
            initial_values <<- paramset[j, 1:model] + rnorm(model, 0, 0.1)
          }
        )
      }
      itemloglike[j] <- result$value
      totalLogLike <- totalLogLike + itemloglike[j]
      if (model == 2) {
        newparams <- c(result$par, 0, 1)
      } else if (model == 3) {
        newparams <- c(result$par, 1)
      } else if (model == 4) {
        newparams <- result$par
      }
      paramset[j, ] <- newparams
    }
    loglike <- totalLogLike
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "iter ", emt, " LogLik ", format(totalLogLike, digits = 6)
          )
        ),
        appendLF = FALSE
      )
    }
  }

  #### Warning
  if (sum(paramset[, 1] > 10) > 0) {
    warning("Some items have a discrimination parameter that exceeds 10.Please exercise caution in interpreting the model.")
  }

  ## Returns
  #### Item information
  item_model_loglike <- itemloglike + (paramset[, 2] / 2)^2 / 2 - slopeprior(paramset[, 1], 0, 0.5)
  if (model > 2) {
    item_model_loglike <- item_model_loglike - asymprior(paramset[, 3], 2, 5)
  }
  if (model > 3) {
    item_model_loglike <- item_model_loglike - asymprior(paramset[, 4], 10, 2)
  }
  itemPSD <- PSD_item_params(model, paramset, quadrature, marginal_posttheta)

  ### Ability Scores
  EAP <- post_theta %*% quadrature
  tmpA <- matrix(rep(quadrature, NROW(tmp$U)), nrow = NROW(tmp$U), byrow = T)
  tmpB <- matrix(rep(EAP, length(quadrature)), nrow = NROW(tmp$U), byrow = F)
  PSD <- sqrt(diag(post_theta %*% t((tmpA - tmpB)^2)))

  ### Model fit
  ell_A <- item_model_loglike
  FitIndices <- ItemFit(tmp$U, tmp$Z, ell_A, model)

  ### Q3mat
  pij <- matrix(nrow = nobs, ncol = testlength)
  for (j in 1:testlength) {
    pij[, j] <-
      LogisticModel(
        a = paramset[j, 1],
        b = paramset[j, 2],
        c = paramset[j, 3],
        d = paramset[j, 4],
        theta = EAP
      )
  }
  eij <- tmp$Z * (tmp$U - pij)
  eij_mean <- colSums(eij) / colSums(tmp$Z)
  eij_dev <- tmp$Z * (eij - eij_mean)
  eij_var <- colSums(eij_dev^2) / (colSums(tmp$Z) - 1)
  eij_sd <- sqrt(eij_var)
  eij_cov <- t(eij_dev) %*% eij_dev / (JointSampleSize(tmp) - 1)
  Q3mat <- eij_cov / (eij_sd %*% t(eij_sd))


  ## Formatting
  paramset <- as.data.frame(paramset)[1:model]
  itemPSD <- as.data.frame(itemPSD)[1:model]
  colnames(paramset) <- c("slope", "location", "lowerAsym", "upperAsym")[1:model]
  colnames(itemPSD) <- c("PSD(slope)", "PSD(location)", "PSD(lowerAsym)", "PSD(upperAsym)")[1:model]
  rownames(paramset) <- rownames(itemPSD) <- tmp$ItemLabel

  names(item_model_loglike) <- tmp$ItemLabel
  EAP <- data.frame(EAP)
  PSD <- data.frame(PSD)
  theta <- cbind(tmp$ID, EAP, PSD)
  colnames(theta) <- c("ID", "EAP", "PSD")

  ret <- structure(list(
    model = model,
    testlength = testlength,
    nobs = nobs,
    params = paramset,
    itemPSD = itemPSD,
    Q3mat = Q3mat,
    ability = theta,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("exametrika", "IRT"))
  return(ret)
}
