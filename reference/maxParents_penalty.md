# Utility function for searching DAG

Function to limit the number of parent nodes

## Usage

``` r
maxParents_penalty(vec, testlength, maxParents)
```

## Arguments

- vec:

  gene Vector corresponding to the upper triangular of the adjacency
  matrix

- testlength:

  test length. In this context it means a number of nodes.

- maxParents:

  Upper limit of number of nodes.

## Details

When generating an adjacency matrix using GA, the number of edges coming
from a single node should be limited to 2 or 3. This is because if there
are too many edges, it becomes difficult to interpret in practical
applications. This function works to adjust the sampling of the randomly
generated adjacency matrix so that the column sum of the upper
triangular elements fits within the set limit.
