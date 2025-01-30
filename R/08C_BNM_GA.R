#' @title Structure Learning for BNM by simple GA
#' @description
#' Generating a DAG from data using a genetic algorithm.
#' @details
#' This function generates a DAG from data using a genetic algorithm.
#' Depending on the size of the data and the settings, the computation may
#' take a significant amount of computational time. For details on the
#' settings or algorithm, see Shojima(2022), section 8.5
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param seed seed for random.
#' @param population Population size. The default is 20
#' @param Rs Survival Rate. The default is 0.5
#' @param Rm Mutation Rate. The default is 0.005
#' @param maxParents Maximum number of edges emanating from a single node. The default is 2.
#' @param maxGeneration Maximum number of generations.
#' @param successiveLimit Termination conditions. If the optimal individual does not change
#' for this number of generations, it is considered to have converged.
#' @param crossover Configure crossover using numerical values. Specify 0 for uniform
#' crossover, where bits are randomly copied from both parents. Choose 1 for single-point
#' crossover with one crossover point, and 2 for two-point crossover with two crossover points.
#' The default is 0.
#' @param elitism Number of elites that remain without crossover when transitioning to
#' the next generation.
#' @param filename Specify the filename when saving the generated adjacency matrix in CSV format.
#' The default is null, and no output is written to the file.
#' @param verbose verbose output Flag. default is TRUE
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph as_data_frame
#' @importFrom utils write.table
#' @importFrom utils combn
#' @return
#' \describe{
#'  \item{adj}{Optimal adjacency matrix}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{crr}{correct response ratio}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{adj}{Adjacency matrix}
#'  \item{param}{Learned Parameters}
#'  \item{CCRR_table}{Correct Response Rate tables}
#' }
#' @importFrom stats rbinom
#' @examples
#' \donttest{
#' # Perform Structure Learning for Bayesian Network Model using Genetic Algorithm
#' # Parameters are set for balanced exploration and computational efficiency
#' StrLearningGA_BNM(J5S10,
#'   population = 20, # Size of population in each generation
#'   Rs = 0.5, # 50% survival rate for next generation
#'   Rm = 0.002, # 0.2% mutation rate for genetic diversity
#'   maxParents = 2, # Maximum of 2 parent nodes per item
#'   maxGeneration = 100, # Maximum number of evolutionary steps
#'   crossover = 2, # Use two-point crossover method
#'   elitism = 2 # Keep 2 best solutions in each generation
#' )
#' }
#' @export

StrLearningGA_BNM <- function(U, Z = NULL, w = NULL, na = NULL,
                              seed = 123,
                              population = 20, Rs = 0.5, Rm = 0.005,
                              maxParents = 2, maxGeneration = 100,
                              successiveLimit = 5, crossover = 0,
                              elitism = 0, filename = NULL,
                              verbose = TRUE) {
  # data format
  if (class(U)[1] != "exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  # crossover type
  if (crossover < 0 || crossover > 2) {
    stop("Check the crossover type. It should be set as 0, 1 or 2.")
  }

  set.seed(seed)
  crr <- crr(tmp)
  sort_list <- order(crr, decreasing = TRUE)
  adj_sort <- data.frame(item = tmp$ItemLabel, crr = crr)
  adj <- matrix(0, ncol = testlength, nrow = testlength)
  adj[upper.tri(adj)] <- 1
  colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]
  gene_length <- sum(upper.tri(adj))


  # Initialize ------------------------------------------------------
  RsI <- round(population * Rs)

  adj <- matrix(0, ncol = testlength, nrow = testlength)
  colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]
  gene_list <- matrix(0, nrow = population, ncol = gene_length)
  for (j in 1:population) {
    adj_gene <- rbinom(gene_length, 1, 0.5)
    gene_list[j, ] <- maxParents_penalty(adj_gene, testlength, maxParents)
  }
  # Elitism check
  if (elitism < 0) {
    elitism <- 0
  } else if (elitism > population * Rs * 0.5) {
    elitism <- round(population * Rs * 0.5)
    warning("Too many elites. Limit to ", elitism)
  }

  bestfit <- 1e+100
  limit_count <- 0
  GA_FLG <- TRUE
  generation <- 0

  # simple GA -------------------------------------------------------

  while (GA_FLG) {
    generation <- generation + 1
    if (verbose) {
      message(
        "\rgen. ", generation, " best BIC ", format(bestfit, digits = 6),
        " limit count ", limit_count,
        appendLF = FALSE
      )
    }
    fitness <- numeric(population)
    for (i in 1:population) {
      # make adj from genes
      adj[upper.tri(adj)] <- gene_list[i, ]
      # Fitness
      ret <- BNM(tmp, adj_matrix = adj)
      fitness[i] <- ret$TestFitIndices$BIC
    }

    # Termination check
    if (generation > maxGeneration) {
      if (verbose) {
        message("The maximum generation has been reached")
      }
      GA_FLG <- FALSE
    }
    if (sort(fitness)[1] == bestfit) {
      limit_count <- limit_count + 1
    } else {
      limit_count <- 0
      bestfit <- sort(fitness)[1]
    }
    if (limit_count >= successiveLimit) {
      if (verbose) {
        message("The BIC has not changed for ", successiveLimit, " times.")
      }
      GA_FLG <- FALSE
    }

    # Selection
    new_gene_list <- matrix(NA, nrow = population, ncol = gene_length)
    survivers <- order(fitness)[1:RsI]
    new_gene_list[1:RsI, ] <- gene_list[survivers, ]

    # elitism
    if (elitism > 0) {
      new_gene_list[(RsI + 1):(RsI + elitism), ] <-
        new_gene_list[1:elitism, ]
    }
    # new gene
    for (i in (RsI + elitism + 1):population) {
      parents_num <- sample(1:(RsI + elitism), size = 2)
      parent1 <- new_gene_list[parents_num[1], ]
      parent2 <- new_gene_list[parents_num[2], ]
      # CrossOver
      if (crossover == 2) {
        # two-points crossover
        combinations <- combn(1:gene_length, 2)
        # Filter out combinations where the two numbers are adjacent
        combinations <- combinations[, abs(combinations[1, ] - combinations[2, ]) > 1]
        # Exclude combinations that start with 1 and 9
        combinations <- combinations[, combinations[1, ] != 1]
        combinations <- combinations[, combinations[2, ] != (gene_length)]
        cut_off <- combinations[, sample(1:ncol(combinations), size = 1)]
        child <- c(
          parent1[1:(cut_off[1] - 1)],
          parent2[cut_off[1]:cut_off[2]],
          parent1[(cut_off[2] + 1):gene_length]
        )
      } else if (crossover == 1) {
        # single point crossover
        cut_off <- sample(2:(gene_length - 1), 1)
        child <- c(parent1[1:cut_off], parent2[(cut_off + 1):gene_length])
      } else {
        # uniform crossover
        inherent <- sample(parents_num, size = gene_length, replace = TRUE)
        child <- numeric(gene_length)
        for (pos in 1:gene_length) {
          child[pos] <- new_gene_list[inherent[pos], pos]
        }
      }
      # Mutation
      Bm <- rbinom(gene_length, 1, Rm)
      child <- (1 - Bm) * child + Bm * (1 - child)

      # Omit Null Model
      if (sum(child) == 0) {
        pos <- sample(1:gene_length, 1)
        child[pos] <- 1
      }

      child <- maxParents_penalty(child, testlength, maxParents)

      ## new_gene
      new_gene_list[i, ] <- child
    }

    gene_list <- new_gene_list
    old_fitness <- fitness
  }
  adj_best <- gene_list[1, ]
  adj[upper.tri(adj)] <- adj_best
  GA_g <- graph_from_adjacency_matrix(adj)

  ret <- BNM(tmp, g = GA_g)

  if (!is.null(filename)) {
    write.table(igraph::as_data_frame(GA_g),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      file = filename
    )
  }

  return(ret)
}




#' @title Structure Learning for BNM by PBIL
#' @description
#' Generating a DAG from data using a Population-Based Incremental Learning
#' @details
#' This function performs structural learning using the Population-Based
#' Incremental Learning model(PBIL) proposed by Fukuda et al.(2014) within
#' the genetic algorithm framework. Instead of learning the adjacency matrix
#' itself, the 'genes of genes' that generate the adjacency matrix are updated
#' with each generation. For more details, please refer to Fukuda(2014) and Section
#' 8.5.2 of the text(Shojima,2022).
#' @param U U is either a data class of exametrika, or raw data. When raw data is given,
#' it is converted to the exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param seed seed for random.
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
#' @importFrom igraph as_adjacency_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph as_data_frame
#' @importFrom utils write.table
#' @importFrom utils combn
#' @return
#' \describe{
#'  \item{adj}{Optimal adjacency matrix}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{crr}{correct response ratio}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{param}{Learned Parameters}
#'  \item{CCRR_table}{Correct Response Rate tables}
#' }
#' @references Fukuda, S., Yamanaka, Y., & Yoshihiro, T. (2014). A Probability-based evolutionary
#'  algorithm with mutations to learn Bayesian networks. International Journal of Artificial
#'  Intelligence and Interactive Multimedia, 3, 7–13. DOI: 10.9781/ijimai.2014.311
#' @importFrom stats runif rbinom
#' @examples
#' \donttest{
#' # Perform Structure Learning for Bayesian Network Model using PBIL
#' # (Population-Based Incremental Learning)
#' StrLearningPBIL_BNM(J5S10,
#'   population = 20, # Size of population in each generation
#'   Rs = 0.5, # 50% survival rate for next generation
#'   Rm = 0.005, # 0.5% mutation rate for genetic diversity
#'   maxParents = 2, # Maximum of 2 parent nodes per item
#'   alpha = 0.05, # Learning rate for probability update
#'   estimate = 4 # Use rounded generational gene method
#' )
#' }
#' @export

StrLearningPBIL_BNM <- function(U, Z = NULL, w = NULL, na = NULL,
                                seed = 123,
                                population = 20, Rs = 0.5, Rm = 0.002,
                                maxParents = 2, maxGeneration = 100,
                                successiveLimit = 5, elitism = 0,
                                alpha = 0.05, estimate = 1,
                                filename = NULL,
                                verbose = TRUE) {
  # data format
  if (class(U)[1] != "exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

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

  gene <- rep(0.5, gene_length)
  adj_t <- matrix(0, nrow = population, ncol = gene_length)

  bestfit <- 1e+100
  limit_count <- 0

  GA_FLG <- TRUE
  generation <- 0
  for (i in 1:population) {
    adj_t[i, ] <- rbinom(gene_length, 1, prob = gene)
  }
  best_individual <- adj_t[1, ]


  # PBIL GA ---------------------------------------------------------
  while (GA_FLG) {
    generation <- generation + 1
    if (verbose) {
      message(
        sprintf(
          "\r%-80s",
          paste0(
            "gen. ", generation, " best BIC ", format(bestfit, digits = 6),
            " limit count ", limit_count
          )
        ),
        appendLF = FALSE
      )
    }
    fitness <- numeric(population)
    for (i in 1:population) {
      # out of elite
      if (i > elitism) {
        vec <- rbinom(gene_length, 1, prob = gene)
        # Omit Null Model
        if (sum(vec) == 0) {
          pos <- sample(1:gene_length, 1)
          vec[pos] <- 1
        }
        adj_t[i, ] <- maxParents_penalty(vec, testlength, maxParents)
      }
      adj[upper.tri(adj)] <- adj_t[i, ]
      # Fitness
      ret <- BNM(tmp, adj_matrix = adj)
      fitness[i] <- ret$TestFitIndices$BIC
    }
    sort_list <- order(fitness)
    adj_t <- adj_t[sort_list, ]

    ### Update Gne
    ar <- round(colMeans(adj_t[1:RsI, ]))
    gene <- gene + (alpha * (ar - gene))

    ### mutation
    u <- runif(gene_length, min = 0, max = 1)
    Bm <- rbinom(gene_length, 1, Rm)
    gene <- (1 - Bm) * gene + Bm * (gene + u) / 2

    # Termination check
    if (generation > maxGeneration) {
      if (verbose) {
        message("The maximum generation has been reached")
      }
      GA_FLG <- FALSE
    }
    if (all(best_individual == adj_t[1, ])) {
      limit_count <- limit_count + 1
    } else {
      fitness <- sort(fitness)
      bestfit <- fitness[1]
      best_individual <- adj_t[1, ]
      limit_count <- 0
    }
    if (limit_count >= successiveLimit) {
      if (verbose) {
        message("The BIC has not changed for ", successiveLimit, " times.")
      }
      GA_FLG <- FALSE
    }
  }

  # Estimate --------------------------------------------------------
  if (estimate == 1) {
    adj[upper.tri(adj)] <- adj_t[1, ]
  } else if (estimate == 2) {
    adj[upper.tri(adj)] <- round(colMeans(adj_t))
  } else if (estimate == 3) {
    adj[upper.tri(adj)] <- round(colMeans(adj_t[1:RsI, ]))
  } else {
    adj[upper.tri(adj)] <- round(gene)
  }

  GA_g <- graph_from_adjacency_matrix(adj)

  ret <- BNM(tmp, g = GA_g)

  if (!is.null(filename)) {
    write.table(igraph::as_data_frame(GA_g),
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      file = filename
    )
  }

  return(ret)
}
