% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/09_LDLRA.R
\name{LDLRA}
\alias{LDLRA}
\title{Local Dependence Latent Rank Analysis}
\usage{
LDLRA(
  U,
  Z = NULL,
  w = NULL,
  na = NULL,
  ncls = 2,
  method = "R",
  g_list = NULL,
  adj_list = NULL,
  adj_file = NULL,
  verbose = FALSE
)
}
\arguments{
\item{U}{U is either a data class of exametrika, or raw data. When raw data is given,
it is converted to the exametrika class with the \link{dataFormat} function.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{ncls}{number of latent class(rank). The default is 2.}

\item{method}{specify the model to analyze the data.Local dependence latent
class model is set to "C", latent rank model is set "R". The default is "R".}

\item{g_list}{A list compiling graph-type objects for each rank/class.}

\item{adj_list}{A list compiling matrix-type adjacency matrices for each rank/class.}

\item{adj_file}{A file detailing the relationships of the graph for each rank/class,
listed in the order of starting point, ending point, and rank(class).}

\item{verbose}{verbose output Flag. default is TRUE}
}
\value{
\describe{
\item{nobs}{Sample size. The number of rows in the dataset.}
\item{msg}{A character string indicating the model type. }
\item{testlength}{Length of the test. The number of items included in the test.}
\item{crr}{correct response ratio}
\item{adj_list}{adjacency matrix list}
\item{g_list}{graph list}
\item{referenceMatrix}{Learned Parameters.A three-dimensional array of patterns where
item x rank x pattern.}
\item{IRP}{Marginal Item Reference Matrix}
\item{IRPIndex}{IRP Indices which include Alpha, Beta, Gamma.}
\item{TRP}{Test Reference Profile matrix.}
\item{LRD}{latent Rank/Class Distribution}
\item{RMD}{Rank/Class Membership Distribution}
\item{TestFitIndices}{Overall fit index for the test.See also \link{TestFit}}
\item{Estimation_table}{Estimated parameters tables.}
\item{CCRR_table}{Correct Response Rate tables}
\item{Studens}{Student information. It includes estimated class
membership, probability of class membership, RUO, and RDO.}
}
}
\description{
performs local dependence latent lank analysis(LD_LRA) by Shojima(2011)
}
\details{
This function is intended to perform LD-LRA. LD-LRA is an analysis that
combines LRA and BNM, and it is used to analyze the network structure among
items in the latent rank. In this function, structural learning is not
performed, so you need to provide item graphs for each rank as separate files.
The file format for this is plain text CSV that includes edges (From, To) and
rank numbers.
}
\examples{
\donttest{
# Create sample DAG structure with different rank levels
# Format: From, To, Rank
DAG_dat <- matrix(c(
  "From", "To", "Rank",
  "Item01", "Item02", "1", # Simple structure for Rank 1
  "Item01", "Item02", "2", # More complex structure for Rank 2
  "Item02", "Item03", "2",
  "Item01", "Item02", "3", # Additional connections for Rank 3
  "Item02", "Item03", "3",
  "Item03", "Item04", "3"
), ncol = 3, byrow = TRUE)

# Method 1: Directly use graph and adjacency lists
g_list <- list()
adj_list <- list()

for (i in 1:3) {
  adj_R <- DAG_dat[DAG_dat[, 3] == as.character(i), 1:2, drop = FALSE]
  g_tmp <- igraph::graph_from_data_frame(
    d = data.frame(
      From = adj_R[, 1],
      To = adj_R[, 2]
    ),
    directed = TRUE
  )
  adj_tmp <- igraph::as_adjacency_matrix(g_tmp)
  g_list[[i]] <- g_tmp
  adj_list[[i]] <- adj_tmp
}

# Fit Local Dependence Latent Rank Analysis
result.LDLRA1 <- LDLRA(J12S5000,
  ncls = 3,
  g_list = g_list,
  adj_list = adj_list
)

# Plot Item Reference Profiles (IRP) in a 4x3 grid
# Shows the probability patterns of correct responses for each item across ranks
plot(result.LDLRA1, type = "IRP", nc = 4, nr = 3)

# Plot Test Reference Profile (TRP)
# Displays the overall pattern of correct response probabilities across ranks
plot(result.LDLRA1, type = "TRP")

# Plot Latent Rank Distribution (LRD)
# Shows the distribution of students across different ranks
plot(result.LDLRA1, type = "LRD")
}

}
