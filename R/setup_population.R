#' Function to generate an initial population dataframe to track
#' vaccination coverage by age through time for a single location.
#'
#' This function generates a dataframe used to track the vaccination
#' status of a population through time. Each age and year
#' is tracked in a row and the proportion of the population that is
#' immune is recorded. Immunity is set to 0 throughout by this
#' function.
#' 
#' @param year_min integer, determines the first year considered.
#' @param year_max integer, determines the last year considered.
#' @param age_min integer, determines the youngest age (in years)
#'     considered. Defaults to 0
#' @param age_max integer, determines the oldest age (in years)
#'     considered. Defaults to 100.
#' @return dataframe containing the columns year, age, cohort,
#'     immunity. Year and age are defined by the input parameters,
#'     cohort = year - age, immunity is set to a starting value of 0
#'     throughout.
#' @export
#' 
setup_population <- function(year_min, year_max, age_min = 0, age_max = 100) {
    
    stopifnot(is_scalar_integer(year_min),
              is_scalar_integer(year_max),
              is_scalar_integer(age_min),
              is_scalar_integer(age_max))

    ## test if the ranges given make sense
    stopifnot(year_min <= year_max,
              age_min >= 0,
              age_min <= age_max)

    df <- as.data.frame(expand.grid(year = year_min:year_max,
                                    age = age_min:age_max))
    
    # easier handling via cohorts, defined by birth year:
    # eg. cohort 2015 is those born in 2015, so year-age = 2015.
    df$cohort = df$year - df$age
    # starting with a fully susceptible population:
    df$immunity = 0

    return(df)    
}
