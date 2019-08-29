##' @title Use devtools::check running testthat::skip_on_cran() tests
##' @param ... arguments passed to devtools::check
##' @param run_tests value of the NOT_CRAN environment variable. This should be
##' either 'true' or 'false'
##' @return results from devtools::check
##' @author Jorge Cimentada
##' @export
adapted_check <- function(..., run_tests = 'true') {
  on.exit(Sys.setenv("NOT_CRAN" = Sys.getenv("NOT_CRAN")))
  Sys.setenv("NOT_CRAN" = run_tests)
  devtools::check(...)
}

##' @title Use devtools::test running testthat::skip_on_cran() tests
##' @param ... arguments passed to devtools::test
##' @param run_tests value of the NOT_CRAN environment variable. This should be
##' either 'true' or 'false'
##' @return results from devtools::test
##' @author Jorge Cimentada
##' @export
adapted_test <- function(..., run_tests = 'true') {
  on.exit(Sys.setenv("NOT_CRAN" = Sys.getenv("NOT_CRAN")))
  Sys.setenv("NOT_CRAN" = run_tests)
  devtools::test(...)
}

##' @title Use devtools::test_file running testthat::skip_on_cran() tests
##' @param ... arguments passed to devtools::test_file
##' @param run_tests value of the NOT_CRAN environment variable. This should be
##' either 'true' or 'false'
##' @return results from devtools::test_file
##' @author Jorge Cimentada
##' @export
adapted_test_file <- function(..., run_tests = 'true') {
  on.exit(Sys.setenv("NOT_CRAN" = Sys.getenv("NOT_CRAN")))
  Sys.setenv("NOT_CRAN" = run_tests)
  devtools::test_file(...)
}

##' @title Use covr::codecov while running testthat::skip_on_cran() tests
##' @param ... arguments passed to covr::codecov
##' @param run_tests value of the NOT_CRAN environment variable. This should be
##' either 'true' or 'false'
##' @return results from covr::codecov
##' @author Jorge Cimentada
##' @export
adapted_covr <- function(..., run_tests = 'true') {
  on.exit(Sys.setenv("NOT_CRAN" = Sys.getenv("NOT_CRAN")))
  Sys.setenv("NOT_CRAN" = run_tests)
  covr::codecov(...)
}
