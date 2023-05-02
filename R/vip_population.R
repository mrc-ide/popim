##' Constructor of an object of the "vip_population" class
##'
##' The "vip_population" object is a dataframe used to track
##' vaccination coverage by age through time for a single
##' location. The dataframe has at least the 4 columns year, age,
##' cohort and immunity.
##'
##' This function generates a such a dataframe.  Each age and year is
##' tracked in a row and the proportion of the population that is
##' immune is recorded. Immunity is initialised as 0 throughout by
##' this function.
##' 
##' @param region character vector, list of regions considered.
##' @param year_min integer, first year to be considered.
##' @param year_max integer, last year to be considered.
##' @param age_min integer, youngest age to be considered, defaults to 0.
##' @param age_max integer, oldest age to be considered, defaults to 100.
##' @return S3 object of class "vip_population": a dataframe with
##'     columns region, year, age, cohort, immunity where year and age
##'     cover the ranges given by the input parameters. Cohort = year
##'     - age and gives the year of birth. It is redundant but
##'     included for ease of handling. Immunity is initialised as 0
##'     throughout the whole population.
##' @export
##' @author Tini Garske
vip_population <- function(region = character(),
                           year_min = integer(), year_max = integer(),
                           age_min = 0, age_max = 100) {

    assert_character(region)

    assert_scalar_wholenumber(year_min)
    assert_scalar_wholenumber(year_max)
    assert_non_negative(year_max - year_min)

    assert_scalar_wholenumber(age_min)
    assert_scalar_wholenumber(age_max)
    assert_non_negative(age_min)
    assert_non_negative(age_max - age_min)

    df <- expand.grid(region = region, year = year_min:year_max,
                      age = age_min:age_max, stringsAsFactors = FALSE)

    ## easier handling via cohorts, defined by birth year:
    df$cohort <- df$year - df$age

    ## starting with a fully susceptible population:
    df$immunity <- 0

    df <- structure(
        df,
        region = region,
        year_min = year_min,
        year_max = year_max,
        age_min = age_min,
        age_max = age_max,
        class = c("vip_population", "data.frame")
    )

    df
}
