#' @title Structure Learning for LDLRA by PBIL algorithm
#' @description
#' Generating DAG list from data using Population-Based Incremental learning
#' @details
#' This function performs structural learning for each classes by using
#' the Population-Based Incremental Learning model(PBIL) proposed by
#' Fukuda et al.(2014) within the genetic algorithm framework.
#' Instead of learning the adjacency matrix itself, the 'genes of genes'
#' that generate the adjacency matrix are updated with each generation.
#' For more details, please refer to Fukuda(2014) and Section
#' 9.4.3 of the text(Shojima,2022).
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ncls number of latent class(rank). The default is 2.
#' @param seed seed for random.
#' @param method specify the model to analyze the data.Local dependence latent
#' class model is set to "C", latent rank model is set "R". The default is "R".
#' @param population Population size. The default is 20
#' @param Rs Survival Rate. The default is 0.5
#' @param Rm Mutation Rate. The default is 0.002
#' @param maxParents Maximum number of edges emanating from a single node. The default is 2.
#' @param maxGeneration Maximum number of generations.
#' @param successiveLimit Termination conditions. If the optimal individual does not change
#' for this number of generations, it is considered to have converged.
#' @param elitism Number of elites that remain without crossover when transitioning to
#' the next generation.
#' @param alpha Learning rate. The default is 0.05
#' @param estimate In PBIL for estimating the adjacency matrix, specify by number from the
#' following four methods: 1. Optimal adjacency matrix, 2. Rounded average of individuals in
#' the last generation, 3. Rounded average of survivors in the last generation, 4. Rounded
#' generational gene of the last generation. The default is 1.
#' @param filename Specify the filename when saving the generated adjacency matrix in CSV format.
#' The default is null, and no output is written to the file.
#' @param verbose verbose output Flag. default is TRUE
#' @param beta1 Beta distribution parameter 1 for prior density. Default is 2.
#' @param beta2 Beta distribution parameter 2 for prior density. Default is 2.
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
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
#' @references Fukuda, S., Yamanaka, Y., & Yoshihiro, T. (2014). A Probability-based evolutionary
#'  algorithm with mutations to learn Bayesian networks. International Journal of Artificial
#'  Intelligence and Interactive Multimedia, 3, 7–13. DOI: 10.9781/ijimai.2014.311
#' @importFrom stats runif rbinom
#' @examples
#' \donttest{
#' # Perform Structure Learning for LDLRA using PBIL algorithm
#' # This process may take considerable time due to evolutionary optimization
#' result.LDLRA.PBIL <- LDLRA_PBIL(J35S515,
#'   seed = 123, # Set random seed for reproducibility
#'   ncls = 5, # Number of latent ranks
#'   maxGeneration = 10,
#'   method = "R", # Use rank model (vs. class model)
#'   elitism = 1, # Keep best solution in each generation
#'   successiveLimit = 15 # Convergence criterion
#' )
#'
#' # Examine the learned network structure
#' # Plot Item Response Profiles showing item patterns across ranks
#' plot(result.LDLRA.PBIL, type = "IRP", nc = 4, nr = 3)
#'
#' # Plot Test Response Profile showing overall response patterns
#' plot(result.LDLRA.PBIL, type = "TRP")
#'
#' # Plot Latent Rank Distribution showing student distribution
#' plot(result.LDLRA.PBIL, type = "LRD")
#' }
#' @export
#'

LDLRA_PBIL <- function(U, Z = NULL, w = NULL, na = NULL,
                       seed = 123, ncls = 2, method = "R",
                       population = 20, Rs = 0.5, Rm = 0.002,
                       maxParents = 2, maxGeneration = 100,
                       successiveLimit = 5, elitism = 0,
                       alpha = 0.05, estimate = 1,
                       filename = NULL,
                       verbose = TRUE, beta1 = 2, beta2 = 2) {
  # data format
  if (!inherits(U, "exametrika")) {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  if (method == "C" | method == "Class") {
    if (verbose) message("local dependence latent Class model is chosen.")
    model <- 1
  } else if (method == "R" | method == "Rank") {
    if (verbose) message("local dependence latent Rank model is chosen.")
    model <- 2
  }

  # estimate type
  if (estimate < 1 || estimate > 4) {
    stop("Check the estimate type. It should be set between 1 to 4.")
  }

  RsI <- round(population * Rs)
  # Elitism check
  if (RsI < 0) {
    RsI <- 0
  } else if (RsI > population * Rs * 0.5) {
    RsI <- round(population * Rs * 0.5)
    warning("Too many survivers. Limit to ", RsI)
  }

  # Elitism check
  if (elitism < 0) {
    elitism <- 0
  } else if (elitism > population * Rs * 0.5) {
    elitism <- round(population * Rs * 0.5)
    warning("Too many elites. Limit to ", elitism)
  }

  # Initialize ------------------------------------------------------

  set.seed(seed)
  crr <- crr(tmp)
  sort_list <- order(crr, decreasing = TRUE)
  adj_sort <- data.frame(item = tmp$ItemLabel, crr = crr)
  adj <- matrix(0, ncol = testlength, nrow = testlength)
  adj[upper.tri(adj)] <- 1
  colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]
  gene_length <- sum(upper.tri(adj))

  gene <- array(0.5, dim = c(ncls, gene_length))
  adj_t <- array(0, dim = c(population, ncls, gene_length))

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

  fitness <- numeric(population)

  limit_count <- 0
  adj_list <- list()

  GA_FLG <- TRUE
  generation <- 0
  bestfit <- 1e+100

  # gen0
  for (i in 1:population) {
    for (j in 1:ncls) {
      vec <- rbinom(gene_length, 1, prob = gene[j, ])
      # Omit Null Model
      if (sum(vec) == 0) {
        pos <- sample(1:gene_length, 1)
        vec[pos] <- 1
      }
      adj_t[i, j, ] <- maxParents_penalty(vec, testlength, maxParents)
      adj[upper.tri(adj)] <- adj_t[i, j, ]
      adj_list[[j]] <- adj
    }
    # fitness
    ret.LDparam <- LD_param_est(tmp, adj_list, ret.emclus$classRefMat, ncls, smoothpost, beta1, beta2)
    fitness[i] <- ret.LDparam$FitIndices$BIC
  }

  sort_list <- order(fitness)
  adj_t <- adj_t[sort_list, , ]
  best_individual <- adj_t[1, , ]
  fitness <- sort(fitness)
  bestfit <- fitness[1]

  while (GA_FLG) {
    generation <- generation + 1
    for (i in 1:population) {
      if (i > elitism) {
        for (j in 1:ncls) {
          vec <- rbinom(gene_length, 1, prob = gene[j, ])
          # Omit Null Model
          if (sum(vec) == 0) {
            pos <- sample(1:gene_length, 1)
            vec[pos] <- 1
          }
          adj_t[i, j, ] <- maxParents_penalty(vec, testlength, maxParents)
          adj[upper.tri(adj)] <- adj_t[i, j, ]
          adj_list[[j]] <- adj
        }
        # fitness
        ret.LDparam <- LD_param_est(tmp, adj_list, ret.emclus$classRefMat, ncls, smoothpost, beta1, beta2)
        if (verbose) {
          message(
            sprintf(
              "\r%-80s",
              paste0(
                "Gen ", generation, " ID.", i,
                " BIC ", format(round(ret.LDparam$FitIndices$BIC, 3), nsmall = 3),
                " BEST ", format(round(bestfit, 3), nsmall = 3),
                " limit count ", limit_count
              )
            ),
            appendLF = FALSE
          )
        }
        fitness[i] <- ret.LDparam$FitIndices$BIC
      }
    }

    sort_list <- order(fitness)
    adj_t <- adj_t[sort_list, , ]

    ## update Gene
    ar <- round(colMeans(adj_t[1:RsI, , ]))
    gene <- gene + (alpha * (ar - gene))
    ## mutation
    u <- matrix(runif(gene_length * ncls, min = 0, max = 1), nrow = ncls)
    Bm <- matrix(rbinom(gene_length * ncls, 1, Rm), nrow = ncls)
    gene <- (1 - Bm) * gene + Bm * (gene + u) / 2

    # Termination check
    if (generation > maxGeneration) {
      if (verbose) message("The maximum generation has been reached")
      GA_FLG <- FALSE
    }
    if (all(best_individual == adj_t[1, , ])) {
      limit_count <- limit_count + 1
    } else {
      limit_count <- 0
      fitness <- sort(fitness)
      bestfit <- fitness[1]
      best_individual <- adj_t[1, , ]
    }
    if (limit_count >= successiveLimit) {
      if (verbose) message("The BIC has not changed for ", successiveLimit, " times.")
      GA_FLG <- FALSE
    }
  }

  # Estimate --------------------------------------------------------
  if (estimate == 1) {
    for (i in 1:ncls) {
      adj[upper.tri(adj)] <- adj_t[1, i, ]
      adj_list[[i]] <- adj
    }
  } else if (estimate == 2) {
    for (i in 1:ncls) {
      adj[upper.tri(adj)] <- round(colMeans(adj_t[, i, ]))
      adj_list[[i]] <- adj
    }
  } else if (estimate == 3) {
    for (i in 1:ncls) {
      adj[upper.tri(adj)] <- round(colMeans(adj_t[1:RsI, i, ]))
      adj_list[[i]] <- adj
    }
  } else {
    for (i in 1:ncls) {
      adj[upper.tri(adj)] <- round(gene[i, ])
      adj_list[[i]] <- adj
    }
  }

  # output file -----------------------------------------------------
  if (!is.null(filename)) {
    g.df <- data.frame()
    for (i in 1:ncls) {
      g <- igraph::graph_from_adjacency_matrix(adj_list[[i]])
      tmp <- igraph::as_data_frame(g)
      tmp$Rank <- i
      g.df <- rbind(g.df, tmp)
    }
    write.table(g.df,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      file = filename
    )
  }


  ret <- LDLRA(tmp, ncls = ncls, adj_list = adj_list)
  return(ret)
}
