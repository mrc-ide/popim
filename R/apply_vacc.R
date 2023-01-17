##' Function to apply a single vaccination activity to the population
##'
##' @param pop_df population dataframe object such as created by
##'     function setup_population
##' @param year year of the vaccination activity
##' @param age_first age of the youngest age group targeted
##' @param age_last age of the oldest age group targeted
##' @param coverage proportion of the population to be vaccinated in
##'     the activity
##' @param skew parameter to determine how successive activities are
##'     targeted: see function calc_new_coverage for more details
##' @return pop_df: the supplied population dataframe object with
##'     updated immunity to reflect the vaccination activity.
##' @export
##' @author Tini Garske
apply_vacc <- function(pop_df, year, age_first = 0, age_last = Inf,
                       coverage = 0, skew = 0) {
    stopifnot(is_population(pop_df))

    stopifnot(is_scalar_integer(age_first),
              is_scalar_integer(age_last),
              age_first >=0, age_last >= age_first)
    stopifnot(is_scalar(coverage), coverage >= 0, coverage <= 1)
    stopifnot(skew %in% c(0, 1, -1))

    cohorts <- year - (age_last:age_first)

    for(j in cohorts) {
        i_vec <- which(pop_df$cohort == j & pop_df$year > year)
        ## vaccination is implemented at the end of the year, so
        ## doesn't change immunity until the next year.
        for(i in i_vec) {
            pop_df$immunity[i] <-
                calc_new_coverage(pop_df$immunity[i], coverage, skew = skew)
        }
    }
    return(pop_df)
}
