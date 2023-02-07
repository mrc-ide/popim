#' Function to test if the argument is scalar
#'
#' This function tests whether the argument is a scalar, i.e. not a
#' list or dataframe, is a vector of length 1 and is numeric. Returns
#' TRUE if this is the case, FALSE otherwise.
#'
#' @param x argument to be tested for scalar-ness.
#' @return logical - TRUE for a scalar, FALSE otherwise.
#' 
is_scalar <- function(x) {
    is.atomic(x) && length(x) == 1L && is.numeric(x) 
}


##' Function to test if the argument is a scalar integer
##'
##' This function tests whether the argument is a scalar integer,
##' i.e., a scalar as defined by is.scalar, and an integer.
##' 
##' @param x argument to be tested for scalar integer-ness.
##' @return logical - TRUE for a scalar integer, FALSE otherwise.
##' @author Tini Garske
##' 
is_scalar_integer <- function(x) {
    is_scalar(x) && is_wholenumber(x)
}

##' Function to test if the argument is a whole number
##'
##' Doing what one would expect is.integer to do...
##'
##' @param x argument to be tested for whole-ness.
##' @param tol tolerance for deviation from a whole number, by default
##'     set to the square root of Machine precision.
##' @return logical (of the same dimensions as the input)
##' @author R-help for is.integer
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5)
    abs(x - round(x)) < tol

assert_population <- function(x) {

    all(class(x) == c("vip_population", "data.frame"))

}
