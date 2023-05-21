library(testthat)
library(bayesEO)
if (bayes_run_tests()) {
    test_check("bayesEO")
}
