##' Function to apply a single vaccination activity to the population
##'
##' @param pop_df population dataframe object such as created by
##'     function 'vip_population'
##' @param region region of the vaccination activity
##' @param year year of the vaccination activity
##' @param age_first age of the youngest age group targeted
##' @param age_last age of the oldest age group targeted
##' @param coverage proportion of the population to be vaccinated in
##'     the activity
##' @param targeting character to determine how successive activities are
##'     targeted. Valid values are "random", "correlated", "targeted".
##'     For targeting = "random" (the default), allocation of vaccine is
##'     random within the population, therefore the resulting coverage
##'     will be smaller than the sum of the coverages. For targeting =
##'     "correlated", there is a 100% correlation between who will get
##'     the vaccine in either vaccination activitiy, the resulting
##'     coverage is simply the larger of the two inputs. For targeting =
##'     "targeted", doses are targeted at unvaccinated people,
##'     resulting in the sum of both coverages, though capped by 1
##'     (full coverage).
##' @return pop_df: the supplied population dataframe object with
##'     updated immunity to reflect the vaccination activity.
##' @export
##' @author Tini Garske
apply_vacc <- function(pop_df, region, year, age_first = 0, age_last = Inf,
                       coverage = 0, targeting = "random") {

    stopifnot(is_population(pop_df))

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)
    assert_scalar_0_to_1(coverage)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    assert_valid_targeting(targeting)

    cohorts <- year - (age_last:age_first)

    for(j in cohorts) {
        i_vec <- which(pop_df$region == region & pop_df$cohort == j &
                       pop_df$year > year)
        ## vaccination is implemented at the end of the year, so
        ## doesn't change immunity until the next year.
        if(length(i_vec) > 0) {
            pop_df$immunity[i_vec] <-
                calc_new_coverage(coverage, pop_df$immunity[i_vec],
                                  targeting = targeting)
        }
    }
    pop_df
}
##' Function to apply vaccination activities to a population
##'
##' Takes the vaccination activities listed in the input object
##' vaccs_df (a data.frame object of class vip_vacc_activities), and
##' applies them to the vip_population object pop_df, then returns
##' that updated object.
##'
##' @param pop_df object of class vip_population such as created by
##'     function 'vip_population'
##' @param vaccs_df object of class 'vip_vacc_activities' such as
##'     created by reading from file with function
##'     'read_vacc_activities'
##' @return pop_df: the supplied population dataframe object with
##'     updated immunity to reflect the vaccination activities
##'     supplied in vaccs_df.
##' @export
##' @author Tini Garske
apply_vaccs <- function(pop_df, vaccs_df) {
    for(i in seq_len(nrow(vaccs_df))) {

        pop_df <- apply_vacc(pop_df, vaccs_df$region[i],
                             vaccs_df$year[i],
                             age_first = vaccs_df$age_first[i],
                             age_last = vaccs_df$age_last[i],
                             coverage = vaccs_df$coverage[i],
                             targeting = vaccs_df$targeting[i])
    }
    pop_df
}

##' Calculate the coverage of a vaccination activity achieved with a
##' given number of doses
##'
##' The coverage for a vaccination activity is calculated as the
##' proportion of the target population that can be vaccinated by the
##' given number of doses. The target population is the sum of the
##' population of the targeted age groups in the targeted region(s).
##'
##' @param pop_df object of class vip_population such as created by
##'     function `vip_population`
##' @param doses number of vaccine doses available for the vaccination
##'     activity
##' @param region region to be targeted
##' @param year year of vaccination activity
##' @param age_first youngest age group to be targeted
##' @param age_last oldest age group to be targeted
##' @return coverage: a number between 0 and 1 indicating what
##'     proportion of the target population can be covered with the
##'     given number of doses.
##' @author Tini Garske
##' @importFrom rlang .data
coverage_from_doses <- function(pop_df, doses, region, year, age_first = 0,
                                age_last = Inf) {

    stopifnot(is_population(pop_df))

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)
    assert_scalar_non_negative(doses)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    target_pop <- pop_df |>
        dplyr::filter(.data$region %in% region, .data$year == year,
                      .data$age >= age_first, .data$age <= age_last) |>
        dplyr::select(tidyselect::all_of("pop_size")) |>
        sum()

    coverage <- min(doses / target_pop, 1)
    assert_scalar_0_to_1(coverage)

    coverage
}
