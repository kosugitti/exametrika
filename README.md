
# exametrika <img src="man/figures/exametrika.png" align="right" height="139" />


## Overview

The `exametrika` package provides comprehensive Test Data Engineering tools for analyzing educational test data. Based on the methods described in Shojima (2022), this package enables researchers and practitioners to:

- Analyze test response patterns and item characteristics
- Classify respondents using various psychometric models
- Investigate latent structures in test data
- Examine local dependencies between items
- Perform network analysis of item relationships

The package implements both traditional psychometric approaches and advanced statistical methods, making it suitable for various assessment and research purposes.

## Features

The package implements various psychometric models and techniques:

#### Classical Methods
- Classical Test Theory (CTT)
  - Item difficulty and discrimination
  - Test reliability and validity
- Item Response Theory (IRT)
  - 2PL, 3PL, and 4PL models
  - Item characteristic curves
  - Test information functions

#### Latent Structure Analysis
- Latent Class Analysis (LCA)
  - Class membership estimation
  - Item response profiles
- Latent Rank Analysis (LRA)
  - Ordered latent classes
  - Rank transition probabilities
- Biclustering and Ranklustering
  - Simultaneous clustering of items and examinees
  - Field-specific response patterns
- Infinite Relational Model (IRM)
  - Optimal class/field determination
  - Nonparametric clustering

#### Advanced Network Models
- Bayesian Network Analysis
  - Structure Learning
    - Genetic Algorithm approach
    - Population-Based Incremental Learning (PBIL)
  - Conditional probability estimation
- Local Dependence Models
  - Local Dependence Latent Rank Analysis (LDLRA)
  - Local Dependence Biclustering (LDB)
  - Bicluster Network Model (BINET)

## Model Overview

### Local Dependence Models

The package implements three complementary approaches to modeling local dependencies in test data:

1. **LDLRA (Local Dependence Latent Rank Analysis)**
   - Analyzes how item dependencies change across different proficiency ranks
   - Suitable when item relationships are expected to vary by student ability level
   - Combines the strengths of LRA and Bayesian Networks

2. **LDB (Local Dependence Biclustering)**
   - Focuses on relationships between item fields within each rank
   - Optimal when items naturally form groups (fields) with hierarchical relationships
   - Integrates biclustering with field-level dependency structures

3. **BINET (Bicluster Network Model)**
   - Examines class transitions within each field
   - Best for understanding complex patterns of class progression
   - Combines biclustering with class-level network analysis

## Background

Exametrika was originally developed and published as a Mathematica and Excel Add-in. For additional information about Exametrika, visit:

- [Test Data Engineering Website](http://sh0j1ma.stars.ne.jp/tde/index.htm)
- [Package News](NEWS.md)


## Installation

The development version of Exametrika can be installed from [GitHub](https://github.com/):

```{r install, eval=FALSE}
# Install devtools if not already installed
if (!require("devtools")) install.packages("devtools")

# Install Exametrika
devtools::install_github("kosugitti/exametrika")
```

### Dependencies

The package requires:

+ R (>= 4.1.0)
+ igraph (for network analysis)
+ Other dependencies are automatically installed

## Data Format and Usage

### Basic Usage

```{r setup-library, message=FALSE, warning=FALSE}
library(exametrika)
```

### Data Requirements

Exametrika accepts both binary and polytomous response data:

- Binary data (0/1)
  - 0: Incorrect answer
  - 1: Correct answer
- Polytomous data
  - Ordinal response categories
  - Multiple score levels
- Missing values
  - NA values supported
  - Custom missing value codes can be specified

### Input Data Specifications

The package accepts data in several formats with the following features:

1. **Data Structure**
   - Matrix or data.frame format
   - Response data (binary or polytomous)
   - Flexible handling of missing values
   - Support for various data types and structures

2. **Optional Components**
   - Examinee ID column (default: first column)
   - Item weights (default: all weights = 1)
   - Item labels (default: sequential numbers)
   - Missing value indicator matrix

Note: Some analysis methods may have specific data type requirements. Please refer to each function's documentation for detailed requirements.

### Data Formatting

The `dataFormat` function processes input data before analysis:

- **Functions**
  - Extracts and validates ID vectors if present
  - Processes item labels or assigns sequential numbers
  - Creates response data matrix U
  - Generates missing value indicator matrix Z
  - Handles item weights
  - Converts data to appropriate format for analysis

Example:
```{r example-data-format}
# Format raw data for analysis
data <- dataFormat(J15S500) # Using sample dataset
str(data) # View structure of formatted data
```

### Sample Datasets

The package includes various sample datasets from Shojima (2022) for testing and learning:

- **Naming Convention**: JxxSxxx format
  - J: Number of items (e.g., J15 = 15 items)
  - S: Sample size (e.g., S500 = 500 examinees)

Available datasets:

- J5S10: Very small dataset (5 items, 10 examinees)
  - Useful for quick testing and understanding basic concepts
- J12S5000: Large sample dataset (12 items, 5000 examinees)
  - Suitable for LDLRA and other advanced analyses
- J14S500: Medium dataset (14 items, 500 examinees)
- J15S500: Medium dataset (15 items, 500 examinees)
  - Often used in IRT and LCA examples
- J20S400: Medium dataset (20 items, 400 examinees)
- J35S515: Large item dataset (35 items, 515 examinees)
  - Used in Biclustering and network model examples
- J15S3810: Ordinal scale dataset (15 items with 4-point scale, 3810 examinees)
  - Used in ordinal latent rank model examples
- J35S5000: Multiple-choice dataset (35 items, 5000 examinees)
  - Includes both response categories and correct answer data
  - Used in nominal scale latent rank model examples

## Examples

### Test Statistics

```{r results-test-statistics, message=FALSE, warning=FALSE}
TestStatistics(J15S500)
```

### ItemStatistics

```{r results-item-statistics, message=FALSE, warning=FALSE}
ItemStatistics(J15S500)
```

### CTT

```{r results-ctt, message=FALSE, warning=FALSE}
CTT(J15S500)
```

### IRT

The IRT function estimates the number of parameters using a logistic model, which can be specified using the `model` option. It supports 2PL, 3PL, and 4PL models.

```{r model-irt, message=FALSE, warning=FALSE}
result.IRT <- IRT(J15S500, model = 3)
result.IRT
```

The estimated population of subjects is included in the returned object.
```{r results-irt-ability, message=FALSE, warning=FALSE}
head(result.IRT$ability)
```


The plots offer options for Item Response Function(also known as Item Characteristic Curves (ICC)),Test Response Function, Item Information Curves (IIC), and Test Information Curves (TIC), which can be specified through options. Items can be specified using the `items` argument, and if not specified, plots will be drawn for all items. The number of rows and columns for dividing the plotting area can be specified using `nr` and `nc`, respectively.

```{r plot-irt-curves, fig.width=7, fig.height=5, message=FALSE, warning=FALSE}
plot(result.IRT, type = "IRF", items = 1:6, nc = 2, nr = 3)
plot(result.IRT, type = "IRF", overlay = TRUE)
plot(result.IRT, type = "IIC", items = 1:6, nc = 2, nr = 3)
plot(result.IRT, type = "TRF")
plot(result.IRT, type = "TIC")
```

### GRM: IRT for Polytomous Cases

The Graded Response Model (Samejima, 1969) can be considered an extension of IRT to polytomous response models. In this package, it can be implemented using the GRM function. However, the estimation accuracy is somewhat inferior to packages such as ltm, so it might be better to use different packages for more sophisticated analyses.

```{r GRM, message=FALSE,warning = FALSE}
result.GRM <- GRM(J5S1000)
result.GRM
```

Similar output to IRT is also possible.

```{r GRM plot}
plot(result.GRM, type = "IRF")
plot(result.GRM, type = "IIF")
plot(result.GRM, type = "TIF")
```

### LCA

Latent Class Analysis requires specifying the dataset and the number of classes.

```{r model-lca, message=FALSE, warning=FALSE}
LCA(J15S500, ncls = 5)
```

The returned object contains the Class Membership Matrix, which indicates which latent class each subject belongs to. The Estimate includes the one with the highest membership probability.

```{r results-lca-membership, message=FALSE, warning=FALSE}
result.LCA <- LCA(J15S500, ncls = 5)
head(result.LCA$Students)
```

The plots offer options for IRP, CMP, TRP, and LCD. For more details on each, please refer to Shojima (2022).

```{r plot-lca, message=FALSE, warning=FALSE}
plot(result.LCA, type = "IRP", items = 1:6, nc = 2, nr = 3)
plot(result.LCA, type = "CMP", students = 1:9, nc = 3, nr = 3)
plot(result.LCA, type = "TRP")
plot(result.LCA, type = "LCD")
```

### LRA

Latent Rank Analysis requires specifying the dataset and the number of classes.

```{r model-lra, message=FALSE, warning=FALSE}
LRA(J15S500, nrank = 6)
```

The estimated subject rank membership probabilities and plots are almost the same as those in LCA (Latent Class Analysis). Since a ranking is assumed for the latent classes, rank-up odds and rank-down odds are calculated.

```{r results-lra-membership, message=FALSE, warning=FALSE}
result.LRA <- LRA(J15S500, nrank = 6)
head(result.LRA$Students)
```

```{r plot-lra, message=FALSE, warning=FALSE}
plot(result.LRA, type = "IRP", items = 1:6, nc = 2, nr = 3)
plot(result.LRA, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.LRA, type = "TRP")
plot(result.LRA, type = "LRD")
```

### LRA for ordinal data

LRA can also be applied to ordinal scale data. The sample dataset J15S3810 contains responses to 15 items on a 4-point scale, which we'll classify into 3 ranks. The mic option enforces monotonic increasing constraints.

```{r model-lra-ordinal}
result.LRAord <- LRA(J15S3810, nrank = 3, mic = TRUE)
```

We can visualize the relationship between total scores from the ordinal scale and estimated ranks. ScoreFreq plots a frequency polygon of scores with rank thresholds, while ScoreRank shows the relationship between scores and rank membership probabilities as a heatmap.

```{r plot-lra-ordinal-score}
plot(result.LRAord, type = "ScoreFreq")
plot(result.LRAord, type = "ScoreRank")
```

The relationship between items and ranks can be visualized in two complementary ways using ICBR and ICRP plots. These visualizations help understand how items function across different ranks:

+ ICBR (Item Category Boundary Reference) shows the cumulative probability curves for each category threshold. For each item, these lines represent the probability of scoring at or above each category boundary across ranks.
+ ICRP (Item Category Response Profile) displays the probability of selecting each response category across ranks. These lines show how response patterns change as rank increases.

```{r ICBR/ICRP plot}
plot(result.LRAord, type = "ICBR", items = 1:4, nc = 2, nr = 2)
plot(result.LRAord, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

Similar to binary data output, we can examine individual examinee characteristics through rank membership probability plots. This visualization shows the probability distribution of rank membership for each examinee, allowing us to understand the certainty of rank classifications. For the first 15 examinees in the dataset:

```{r plot-lra-ordinal-rmp}
plot(result.LRAord, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

Note: The layout parameters nc = 3 and nr = 5 control the arrangement of plots in a 3-column by 5-row grid, making it easier to compare multiple items or examinees simultaneously.

### LRA for rated data

If you have data where respondents select the correct answer from multiple choices, like in a multiple-choice test (nominal scale level), you can analyze it using LRA.

```{r LRArated}
result.LRArated <- LRA(J35S5000, nrank = 10, mic = TRUE)
```

You can visualize the relationship between scores and ranks, just like with ordinal scale data.

```{r LRArated ScoreFreq}
plot(result.LRArated, type = "ScoreFreq")
plot(result.LRArated, type = "ScoreRank")
```

You can also visualize the relationship between latent ranks and items, or the probability of subjects belonging to certain ranks.

```{r LRAratedplot}
plot(result.LRArated, type = "ICRP", items = 1:4, nc = 2, nr = 2)
```

```{r LRAratedplot2 RMP}
plot(result.LRAord, type = "RMP", students = 1:9, nc = 3, nr = 3)
```

### Biclustering/Ranklustering

Biclustering and Ranklustering algorithms are almost the same, differing only in whether they include a filtering matrix or not. The difference is specified using the `method` option in the `Biclustering()` function. For more details, please refer to the help documentation.

```{r model-biclustering}
## Biclustering
Biclustering(J35S515, nfld = 5, ncls = 6, method = "B")
```

```{r model-ranklustering}
## Ranklustering
result.Ranklustering <- Biclustering(J35S515, nfld = 5, ncls = 6, method = "R")
plot(result.Ranklustering, type = "Array")
plot(result.Ranklustering, type = "FRP", nc = 2, nr = 3)
plot(result.Ranklustering, type = "RRV")
plot(result.Ranklustering, type = "RMP", students = 1:9, nc = 3, nr = 3)
plot(result.Ranklustering, type = "LRD")
```


#### Finding optimal number of classes and fields

##### Grid Search function

The Grid Search function performs systematic exploration of optimal numbers of classes and fields based on statistical fit indices. This approach evaluates multiple parameter combinations and selects the best-fitting model according to specified criteria such as AIC, BIC, or other goodness-of-fit measures.

```{r gr}
result <- GridSearch(J35S515, method = "R", max_ncls = 10, max_nfld = 10, index = "BIC")
result$optimal_ncls
result$optimal_nfld
result$optimal_result
```

##### Infinite Relational Model

The Infinite Relational Model uses the Chinese Restaurant Process to explore the optimal number of fields and classes automatically. This model is particularly useful when you don't know the appropriate number of latent structures beforehand.

**Important notes:**
- Execution can be time-consuming, especially with large datasets
- The model tends to detect larger numbers of classes and fields
- Use with caution and consider computational resources

```{r model-irm}
result.IRM <- Biclustering_IRM(J35S515, gamma_c = 1, gamma_f = 1, verbose = TRUE)
plot(result.IRM, type = "Array")
plot(result.IRM, type = "FRP", nc = 3)
plot(result.IRM, type = "TRP")
```

Additionally, supplementary notes on the derivation of the Infinite Relational Model with Chinese restaurant process is [here](https://kosugitti.github.io/kosugitti10/notes/IRM_memo.pdf).

#### Biclustering for Polytomous data

Biclustering and Rankclustering can also be performed on data with ordinal or nominal polytomous responses. The plot output is limited to the "Array" type only. Color palettes can be customized using the `colors` option to specify different color schemes.

```{r bic_poly}
result.B.poly <- Biclustering(J15S3810, ncls = 3, nfld = 4)
result.B.poly
# Custom color palette example
plot(result.B.poly, type = "Array", 
colors = c("#FF1493", "#00FF00", "#FF4500", "#9932CC", "#FFD700"))
```

### Bayesian Network Model

The Bayesian network model is a model that represents the conditional probabilities between items in a network format based on the pass rates of the items. By providing a Directed Acyclic Graph (DAG) between items externally, it calculates the conditional probabilities based on the specified graph. The igraph package is used for the analysis and representation of the network.

There are three ways to specify the graph. You can either pass a matrix-type DAG to the argument adj_matrix, pass a DAG described in a CSV file to the argument adj_file, or pass a graph-type object g used in the igraph package to the argument g.

The methods to create the matrix-type adj_matrix and the graph object g are as follows:

```{r setup-igraph, message=FALSE, warning=FALSE}
library(igraph)
DAG <-
  matrix(
    c(
      "Item01", "Item02",
      "Item02", "Item03",
      "Item02", "Item04",
      "Item03", "Item05",
      "Item04", "Item05"
    ),
    ncol = 2, byrow = T
  )
## graph object
g <- igraph::graph_from_data_frame(DAG)
g
## Adjacency matrix
adj_mat <- as.matrix(igraph::get.adjacency(g))
print(adj_mat)
```

A CSV file with the same information as the graph above in the following format. The first line contains column names (headers) and will not be read as data.

```{r print-dag, echo=FALSE, message=FALSE, warning=FALSE}
cat("From,To\n")
for (i in 1:nrow(DAG)) {
  cat(sprintf("%s,%s\n", DAG[i, 1], DAG[i, 2]))
}
```

While only one specification is sufficient, if multiple specifications are provided, they will be prioritized in the order of file, matrix, and graph object.

An example of executing BNM by providing a graph structure (DAG) is as follows:

```{r model-bnm, message=FALSE, warning=FALSE}
result.BNM <- BNM(J5S10, adj_matrix = adj_mat)
result.BNM
```

#### Structure Learning for Bayesian network with GA

The function searches for a DAG suitable for the data using a genetic algorithm. A best DAG is not necessarily identified. Instead of exploring all combinations of nodes and edges, only the space topologically sorted by the pass rate, namely the upper triangular matrix of the adjacency matrix, is explored. For interpretability, the number of parent nodes should be limited. A null model is not proposed. Utilize the content of the items and the experience of the questioner to aid in interpreting the results. For more details, please refer to Section 8.5 of the text(Shojima,2022).

Please note that the GA may take a considerable amount of time, depending on the number of items and the size of the population.

```{r model-ga-bnm, message=FALSE, warning=FALSE}
BNM_GA(J5S10,
  population = 20, Rs = 0.5, Rm = 0.002, maxParents = 2,
  maxGeneration = 100, crossover = 2, elitism = 2
)
```

The method of Population-Based incremental learning proposed by Fukuda (2014) can also be used for learning. This method has several variations for estimating the optimal adjacency matrix at the end, which can be specified as options. See help or text Section 8.5.2.

```{r model-pbil-bnm, message=FALSE, warning=FALSE}
BNM_PBIL(J5S10,
  population = 20, Rs = 0.5, Rm = 0.005, maxParents = 2,
  alpha = 0.05, estimate = 4
)
```

### Local Dependence Latent Rank Analysis

LD-LRA is an analysis that combines LRA and BNM, and it is used to analyze the network structure among items in the latent rank. In this function, structural learning is not performed, so you need to provide item graphs for each rank as separate files.

For each class, it is necessary to specify a graph, and there are three ways to do so. You can either pass a matrix-type DAG for each class or a list of graph-type objects used in the igraph package to the arguments adj_list or g_list, respectively, or you can provide a DAG described in a CSV file. The way to specify it in a CSV file is as follows.

```{r setup-dag-data, message=FALSE, warning=FALSE}
DAG_dat <- matrix(c(
  "From", "To", "Rank",
  "Item01", "Item02", 1,
  "Item04", "Item05", 1,
  "Item01", "Item02", 2,
  "Item02", "Item03", 2,
  "Item04", "Item05", 2,
  "Item08", "Item09", 2,
  "Item08", "Item10", 2,
  "Item09", "Item10", 2,
  "Item08", "Item11", 2,
  "Item01", "Item02", 3,
  "Item02", "Item03", 3,
  "Item04", "Item05", 3,
  "Item08", "Item09", 3,
  "Item08", "Item10", 3,
  "Item09", "Item10", 3,
  "Item08", "Item11", 3,
  "Item02", "Item03", 4,
  "Item04", "Item06", 4,
  "Item04", "Item07", 4,
  "Item05", "Item06", 4,
  "Item05", "Item07", 4,
  "Item08", "Item10", 4,
  "Item08", "Item11", 4,
  "Item09", "Item11", 4,
  "Item02", "Item03", 5,
  "Item04", "Item06", 5,
  "Item04", "Item07", 5,
  "Item05", "Item06", 5,
  "Item05", "Item07", 5,
  "Item09", "Item11", 5,
  "Item10", "Item11", 5,
  "Item10", "Item12", 5
), ncol = 3, byrow = TRUE)

# save csv file
edgeFile <- tempfile(fileext = ".csv")
write.csv(DAG_dat, edgeFile, row.names = FALSE, quote = TRUE)
```

Here, it is shown an example of specifying with matrix-type and graph objects using the aforementioned CSV file. While only one specification is sufficient, if multiple specifications are provided, they will be prioritized in the order of file, matrix, and graph object.

```{r setup-graph-conversion, message=FALSE, warning=FALSE}
g_csv <- read.csv(edgeFile)
colnames(g_csv) <- c("From", "To", "Rank")
adj_list <- list()
g_list <- list()
for (i in 1:5) {
  adj_R <- g_csv[g_csv$Rank == i, 1:2]
  g_tmp <- igraph::graph_from_data_frame(adj_R)
  adj_tmp <- igraph::get.adjacency(g_tmp)
  g_list[[i]] <- g_tmp
  adj_list[[i]] <- adj_tmp
}
## Example of graph list
g_list
```

```{r results-adj-list, message=FALSE, warning=FALSE}
### Example of adjacency list
adj_list
```

The example of running the LDLRA function using this CSV file would look like this.

```{r model-ldlra, message=FALSE, warning=FALSE}
result.LDLRA <- LDLRA(J12S5000,
  ncls = 5,
  adj_file = edgeFile
)
result.LDLRA
```

Of course, it also supports various types of plots.
```{r plot-ldlra, message=FALSE, warning=FALSE}
plot(result.LDLRA, type = "IRP", nc = 4, nr = 3)
plot(result.LDLRA, type = "TRP")
plot(result.LDLRA, type = "LRD")
```

```{r cleanup-ldlra, include=FALSE}
# Clean up temporary file
unlink(edgeFile)
```

#### Structure Learning for LDLRA with GA(PBIL)

You can learn item-interaction graphs for each rank using the PBIL algorithm. In addition to various options, the learning process requires a very long computation time. It's also important to note that the result is merely one of the feasible solutions, and it's not necessarily the optimal solution.

```{r model-pbil-ldlra, message=FALSE, warning=FALSE, eval=T}
result.LDLRA.PBIL <- LDLRA_PBIL(J35S515,
  seed = 123,
  ncls = 5,
  method = "R",
  elitism = 1,
  successiveLimit = 15
)
result.LDLRA.PBIL
```

### Local Dependence Biclustering

Local Dependence Biclustering combines biclustering and Bayesian network models. The model requires three main components:

- Number of latent classes/ranks
- Field assignments for items
- Network structure between fields at each rank

Here's an example implementation:

```{r setup-ldb}
# Create field configuration vector (assign items to fields)
conf <- c(1, 6, 6, 8, 9, 9, 4, 7, 7, 7, 5, 8, 9, 10, 10, 9, 9, 10, 10, 10, 2, 2, 3, 3, 5, 5, 6, 9, 9, 10, 1, 1, 7, 9, 10)

# Create edge data for network structure between fields
edges_data <- data.frame(
  "From Field (Parent) >>>" = c(
    6, 4, 5, 1, 1, 4, # Class/Rank 2
    3, 4, 6, 2, 4, 4, # Class/Rank 3
    3, 6, 4, 1, # Class/Rank 4
    7, 9, 6, 7 # Class/Rank 5
  ),
  ">>> To Field (Child)" = c(
    8, 7, 8, 7, 2, 5, # Class/Rank 2
    5, 8, 8, 4, 6, 7, # Class/Rank 3
    5, 8, 5, 8, # Class/Rank 4
    10, 10, 8, 9 # Class/Rank 5
  ),
  "At Class/Rank (Locus)" = c(
    2, 2, 2, 2, 2, 2, # Class/Rank 2
    3, 3, 3, 3, 3, 3, # Class/Rank 3
    4, 4, 4, 4, # Class/Rank 4
    5, 5, 5, 5 # Class/Rank 5
  )
)

# Save edge data to temporary file
edgeFile <- tempfile(fileext = ".csv")
write.csv(edges_data, file = edgeFile, row.names = FALSE)
```

```{r setup-ldb-conf, include=FALSE, message=FALSE, warning=FALSE}
# Fit Local Dependence Biclustering model
result.LDB <- LDB(
  U = J35S515,
  ncls = 5, # Number of latent classes
  conf = conf, # Field configuration vector
  adj_file = edgeFile # Network structure file
)

# Display model results
print(result.LDB)
```


Additionally, as mentioned in the text (Shojima, 2022), it is often the case that seeking the network structure exploratively does not yield appropriate results, so it has not been implemented.
```{r model-ldb}
result.LDB <- LDB(U = J35S515, ncls = 5, conf = conf, adj_file = edgeFile)
result.LDB
```

```{r cleanup-ldb, include=FALSE}
# Clean up temporary file
unlink(edgeFile)
```

Of course, it also supports various types of plots.

```{r plot-ldb, message=FALSE, warning=FALSE}
# Show bicluster structure
plot(result.LDB, type = "Array")
# Test Response Profile
plot(result.LDB, type = "TRP")
# Latent Rank Distribution
plot(result.LDB, type = "LRD")
# Rank Membership Profiles for first 9 students
plot(result.LDB, type = "RMP", students = 1:9, nc = 3, nr = 3)
# Field Reference Profiles
plot(result.LDB, type = "FRP", nc = 3, nr = 2)
```

In this model, you can draw a Field PIRP Profile that visualizes the correct answer count for each rank and each field.

```{r plot-ldb-fieldpirp, fig.width=7, fig.height=5, message=FALSE, warning=FALSE}
plot(result.LDB, type = "FieldPIRP")
```

### Bicluster Network Model

Bicluster Network Model: BINET is a model that combines the Bayesian 
network model and Biclustering. BINET is very similar to LDB and LDR.

The most significant difference is that in LDB, the nodes represent the fields, whereas in BINET, they represent the class. BINET explores the local dependency structure among latent classes at each latent field, where each field is a locus.

To execute this analysis, in addition to the dataset, the same field correspondence file used during exploratory Biclustering is required, as well as an adjacency matrix between classes. 

```{r setup-binet}
# Create field configuration vector for item assignment
conf <- c(1, 5, 5, 5, 9, 9, 6, 6, 6, 6, 2, 7, 7, 11, 11, 7, 7, 12, 12, 12, 2, 2, 3, 3, 4, 4, 4, 8, 8, 12, 1, 1, 6, 10, 10)

# Create edge data for network structure between classes
edges_data <- data.frame(
  "From Class (Parent) >>>" = c(
    1, 2, 3, 4, 5, 7, # Dependencies in various fields
    2, 4, 6, 8, 10,
    6, 6, 11, 8, 9, 12
  ),
  ">>> To Class (Child)" = c(
    2, 4, 5, 5, 6, 11, # Target classes
    3, 7, 9, 12, 12,
    10, 8, 12, 12, 11, 13
  ),
  "At Field (Locus)" = c(
    1, 2, 2, 3, 4, 4, # Field locations
    5, 5, 5, 5, 5,
    7, 8, 8, 9, 9, 12
  )
)

# Save edge data to temporary file
edgeFile <- tempfile(fileext = ".csv")
write.csv(edges_data, file = edgeFile, row.names = FALSE)
```

The model requires three components:

1. Field assignments for items (vector from configuration file)
2. Network structure between classes for each field
3. Number of classes and fields

```{r model-binet, message=FALSE, warning=FALSE}
# Fit Bicluster Network Model
result.BINET <- BINET(
  U = J35S515,
  ncls = 13, # Maximum class number from edges (13)
  nfld = 12, # Maximum field number from conf (12)
  conf = conf, # Field configuration vector
  adj_file = edgeFile # Network structure file
)

# Display model results
print(result.BINET)
```

```{r cleanup-binet, include=FALSE}
# Clean up temporary file
unlink(edgeFile)
```

Of course, it also supports various types of plots.

```{r plot-binet, message=FALSE, warning=FALSE}
# Show bicluster structure
plot(result.BINET, type = "Array")
# Test Response Profile
plot(result.BINET, type = "TRP")
# Latent Rank Distribution
plot(result.BINET, type = "LRD")
# Rank Membership Profiles for first 9 students
plot(result.BINET, type = "RMP", students = 1:9, nc = 3, nr = 3)
# Field Reference Profiles
plot(result.BINET, type = "FRP", nc = 3, nr = 2)
```

LDPSR plot shows all Passing Student Rates for all locally dependent classes compared with their respective parents.


```{r plot-binet-ldpsr, message=FALSE, warning=FALSE}
# Locally Dependent Passing Student Rates
plot(result.BINET, type = "LDPSR", nc = 3, nr = 2)
```


### Available Output Types by Model

#### Pattern Analysis

| Model | IRP | FRP | TRP | ICRP | C/R RV |
|-------|:---:|:---:|:---:|:----:|:----:|
| IRT | | | | | |
| LCA | ✓ | ✓ | ✓ | | |
| LRA | ✓ | ✓ | ✓ | | |
| LRAordinal | | | | ✓ | |
| LRArated | | | | ✓ | |
| Biclustering |  | ✓ | ✓ | |✓ | |
| IRM | | ✓ | ✓ | | |
| LDLRA | ✓ | | | | |
| LDB | | ✓ | ✓ | | |
| BINET | | ✓ | ✓ | | |

#### Diagnostics & Visualization

| Model | LCD/LRD | CMP/RMP | Array | Other |
|-------|:--------:|:--------:|:-----:|-------|
| IRT | | | | IIC, ICC, TIC |
| LCA | ✓ | ✓ | | |
| LRA | ✓ | ✓ | | |
| LRAordinal |  | ✓ | | | ICBR,ScoreFreq, ScoreRank |
| LRArated |  | ✓ | | | ScoreFreq, ScoreRank |
| Biclustering | ✓ | ✓ | ✓ | |
| IRM | | | ✓ | |
| LDLRA | ✓ | ✓ | | |
| LDB | ✓ | ✓ | ✓ | FieldPIRP |
| BINET | ✓ | ✓ | ✓ | LDPSR |

Note: ✓ indicates available output type for the model.

## Community and Support

We welcome community involvement and feedback to improve `exametrika`. Here's how you can participate and get support:

### Reporting Issues

If you encounter bugs or have suggestions for improvements:

- Open an issue on [GitHub Issues](https://github.com/kosugitti/exametrika/issues)
- Provide a minimal reproducible example
- Include your R session information (`sessionInfo()`)

### Discussions and Community

Join our [GitHub Discussions](https://github.com/kosugitti/exametrika/discussions):

- Ask questions
- Share your use cases
- Discuss feature requests
- Exchange tips and tricks
- Get updates about package development

### Contributing

We appreciate contributions from the community:

- Bug reports and feature requests through Issues
- Usage examples and tips through Discussions
- Code improvements through Pull Requests

Please check our existing Issues and Discussions before posting to avoid duplicates.

## Reference

+ Shojima, Kojiro (2022) Test Data Engineering: Latent Rank Analysis, Biclustering, and Bayesian Network (Behaviormetrics: Quantitative Approaches to Human Behavior, 13),Springer.
+ Samejima, F. (1969). Estimation of latent ability using a response pattern of graded scores. Psychometrika, 34(S1), 1-97.


Follow our [GitHub repository](https://github.com/kosugitti/exametrika) and join the [Discussions](https://github.com/kosugitti/exametrika/discussions) to stay updated on development progress and provide feedback on desired features.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11529926.svg)](https://doi.org/10.5281/zenodo.11529926)
