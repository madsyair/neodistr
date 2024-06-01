library(testthat)
library(neodistr)

# use fixed random seed for all tests for reproducibility
set.seed(42)

test_check("neodistr")
