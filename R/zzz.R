# On load
.onAttach <- function(lib, pkg) {
    packageStartupMessage("bayesEO - Bayesian Smoothing of Remote Sensing Image Classification.")
    packageStartupMessage(
        sprintf(
            "Loaded bayesEO v%s.
             See ?bayesEO for help, citation(\"bayesEO\") for use in publication.",
             utils::packageDescription("sits")$Version
        )
    )
}
.onLoad <- function(lib, pkg) {
    bayes_colors()
}
# Creates a package environment to store global variables
bayesEO_env <- new.env()
#' @importFrom Rcpp sourceCpp
#' @importFrom dplyr .data
#' @useDynLib bayesEO, .registration = TRUE
NULL
