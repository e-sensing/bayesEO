#' @title Informs if tests should run
#'
#' @name bayes_run_tests
#'
#' @description
#' This function informs if tests should run.
#' To run the examples, set "BAYES_RUN_TESTS" environment
#' variable to "YES" using
#' Sys.setenv("BAYES_RUN_TESTS" = "YES")
#' To come back to the default behaviour, please unset
#' the enviroment variable
#' Sys.unsetenv("BAYES_RUN_TESTS")
#' @return TRUE/FALSE
#'
#' @export
bayes_run_tests <- function() {
    return(!Sys.getenv("BAYES_RUN_TESTS") %in% c("", "NO", "FALSE", "OFF"))
}

#' @title Informs if  examples should run
#'
#' @name bayes_run_examples
#'
#' @description
#' This function informs if examples should run.
#' To run the examples, set "BAYES_RUN_EXAMPLES" environment
#' variable to "YES" using
#' Sys.setenv("BAYES_RUN_EXAMPLES" = "YES")
#' To come back to the default behaviour, please unset
#' the enviroment variable
#' Sys.unsetenv("BAYES_RUN_EXAMPLES")
#'
#' @return A logical value
#' @export
bayes_run_examples <- function() {
    return(!Sys.getenv("BAYES_RUN_EXAMPLES") %in% c("", "NO", "FALSE", "OFF"))
}
