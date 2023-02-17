##' Function to apply a single vaccination activity to the population
##'
##' @param pop_df population dataframe object such as created by
##'     function setup_population
##' @param year year of the vaccination activity
##' @param age_first age of the youngest age group targeted
##' @param age_last age of the oldest age group targeted
##' @param coverage proportion of the population to be vaccinated in
##'     the activity
##' @param target character to determine how successive activities are
##'     targeted. Valid values are "random", "correlated", "targeted".
##'     For target = "random" (the default), allocation of vaccine is
##'     random within the population, therefore the resulting coverage
##'     will be smaller than the sum of the coverages. For target =
##'     "correlated", there is a 100% correlation between who will get
##'     the vaccine in either vaccination activitiy, the resulting
##'     coverage is simply the larger of the two inputs. For target =
##'     "targeted", doses are targeted at unvaccinated people,
##'     resulting in the sum of both coverages, though capped by 1
##'     (full coverage).
##' @return pop_df: the supplied population dataframe object with
##'     updated immunity to reflect the vaccination activity.
##' @export
##' @author Tini Garske
apply_vacc <- function(pop_df, year, age_first = 0, age_last = Inf,
                       coverage = 0, target = "random") {

    stopifnot(is_population(pop_df))

    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)
    assert_scalar_0_to_1(coverage)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    assert_valid_target(target)

    cohorts <- year - (age_last:age_first)

    for(j in cohorts) {
        i_vec <- which(pop_df$cohort == j & pop_df$year > year)
        ## vaccination is implemented at the end of the year, so
        ## doesn't change immunity until the next year.
        if(length(i_vec) > 0) {
            pop_df$immunity[i_vec] <-
                calc_new_coverage(coverage, pop_df$immunity[i_vec],
                                  target = target)
        }
    }

    pop_df
}
