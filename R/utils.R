#' Function to test if the argument is scalar
#'
#' This function tests whether the argument is a scalar, i.e. not a
#' list or dataframe, is a vector of length 1 and is numeric. Returns
#' TRUE if this is the case, FALSE otherwise.
#'
#' @param x argument to be tested for scalar-ness.
#' @return logical - TRUE for a scalar, FALSE otherwise.
#' 
is.scalar <- function(x) {
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
is.scalar.integer <- function(x) {
    is.scalar(x) && x == round(x)
}
