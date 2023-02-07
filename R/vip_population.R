##' Low-level constructor of an object of the "vip_population" class
##'
##' This function is intended for developer use only and therefore
##' only performs minimal type checks on input parameters.
##' 
##' @param year_min integer, first year to be considered.
##' @param year_max integer, last year to be considered.
##' @param age_min integer, youngest age to be considered.
##' @param age_max integer, oldest age to be considered.
##' @return S3 object of class "vip_population": a dataframe with
##'     columns year, age, cohort, immunity where year and age cover
##'     the ranges given by the input parameters. Cohort = year - age
##'     and gives the year of birth. It is redundant but included for
##'     ease of handling. Immunity is initialised as 0 throughout the
##'     whole population.
##' @author Tini Garske
new_population <- function(year_min = integer(), year_max = integer(),
                           age_min = integer(), age_max = integer()) {

    stopifnot(is_scalar_integer(year_min))
    stopifnot(is_scalar_integer(year_max))
    stopifnot(is_scalar_integer(age_min))
    stopifnot(is_scalar_integer(age_max))

    df <- expand.grid(year = year_min:year_max, age = age_min:age_max) |>
        as.data.frame()

    ## easier handling via cohorts, defined by birth year:
    df$cohort <- df$year - df$age

    ## starting with a fully susceptible population:
    df$immunity <- 0

    class(df) <- "vip_population"

    df
}
    
#' Function to generate an initial population object of S3 class
#' "vip_population"
#'
#' The "vip_population" object is a dataframe used to track
#' vaccination coverage by age through time for a single location.
#'
#' This function generates a dataframe used to track the vaccination
#' status of a population through time. Each age and year is tracked
#' in a row and the proportion of the population that is immune is
#' recorded. Immunity is initialised as 0 throughout by this function.
#' 
#' @param year_min integer, determines the first year considered.
#' @param year_max integer, determines the last year considered.
#' @param age_min integer, determines the youngest age (in years)
#'     considered. Defaults to 0
#' @param age_max integer, determines the oldest age (in years)
#'     considered. Defaults to 100.
#' @return vip_population object: a dataframe containing the columns
#'     year, age, cohort, immunity. Year and age are defined by the
#'     input parameters, cohort = year - age, immunity is set to a
#'     starting value of 0 throughout.
#' @export
#' 
vip_population <- function(year_min, year_max, age_min = 0, age_max = 100) {
    
    stopifnot(is_scalar_integer(year_min),
              is_scalar_integer(year_max),
              is_scalar_integer(age_min),
              is_scalar_integer(age_max))

    ## test if the ranges given make sense
    stopifnot(year_min <= year_max,
              age_min >= 0,
              age_min <= age_max)

    new_population(year_min = year_min, year_max = year_max,
                    age_min = age_min,   age_max = age_max)
}

