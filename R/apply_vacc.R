##' Function to apply a single vaccination activity to the population
##'
##' If the vaccination activity has non-missing entries for both
##' coverage and doses, the coverage is used - there is no test
##' whether the coverage and doses information are consistent with the
##' target population size in the vip_population object to which the
##' vaccination activity is applied.
##'
##' @param pop_df population dataframe object such as created by
##'     function 'vip_population'
##' @param region region of the vaccination activity
##' @param year year of the vaccination activity
##' @param age_first age of the youngest age group targeted
##' @param age_last age of the oldest age group targeted
##' @param coverage proportion of the population to be vaccinated in
##'     the activity
##' @param doses number of doses available to vaccinate the target population
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
                       coverage = double(), doses = NA_real_,
                       targeting = "random") {

    stopifnot(is_population(pop_df))

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)
    assert_scalar_0_to_1_or_missing(coverage)
    assert_scalar_non_negative_or_missing(doses)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    assert_valid_targeting(targeting)

    cohorts <- year - (age_last:age_first)

    if(is.na(coverage)) {
        coverage = coverage_from_doses(pop_df, doses, region, year,
                                       age_first, age_last)
    }

    for(j in cohorts) {
        i_vec <- which(pop_df$region == region & pop_df$cohort == j &
                       pop_df$year > year)
        ## vaccination is implemented at the end of the year, so
        ## doesn't change immunity until the next year.
        if(length(i_vec) > 0) {
            pop_df$immunity[i_vec] <-
                calc_new_immunity(coverage, pop_df$immunity[i_vec],
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
                             doses = vaccs_df$doses[i],
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

    assert_scalar_non_negative(doses)

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    if(age_last == Inf) age_last <- max(pop_df$age)

    ## check that pop_df contains relevant info for the proposed
    ## vaccination activity:
    pop_targeted <- expand.grid(region = region, year = year,
                                age = age_first:age_last) |>
        dplyr::left_join(pop_df, by = c("region", "year", "age"))

    target_pop <- sum(pop_targeted$pop_size)
    if(is.na(target_pop))
        stop("pop_df does is missing pop_size information for at least some targeted cohorts.")

    coverage <- min(doses / target_pop, 1)
    assert_scalar_0_to_1(coverage)

    coverage
}

##' Calculate the number of doses needed to achieve a given coverage
##' in a vaccination activity
##'
##' The number of doses needed to achieve the given coverage is
##' calculated as coverage * target population. The target population
##' is the sum of the population of the targeted age groups in the
##' targeted region(s).
##'
##' @param pop_df object of class vip_population such as created by
##'     function `vip_population`
##' @param coverage proportion of the target population to be covered,
##'     0 <= coverage <= 1
##' @param region region to be targeted
##' @param year year of vaccination activity
##' @param age_first youngest age group to be targeted
##' @param age_last oldest age group to be targeted
##' @return doses: a non-negative number between indicating how many
##'     doses will be needed to achieve the given coverage in the
##'     target population.
##' @author Tini Garske
##' @importFrom rlang .data
doses_from_coverage <- function(pop_df, coverage, region, year, age_first = 0, age_last = Inf) {

    stopifnot(is_population(pop_df))

    assert_scalar_0_to_1(coverage)

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    if(age_last == Inf) age_last <- max(pop_df$age)

    ## check that pop_df contains relevant info for the proposed
    ## vaccination activity:
    pop_targeted <- expand.grid(region = region, year = year,
                                age = age_first:age_last) |>
        dplyr::left_join(pop_df, by = c("region", "year", "age"))

    target_pop <- sum(pop_targeted$pop_size)
    if(is.na(target_pop))
        stop("pop_df does is missing pop_size information for at least some targeted cohorts.")

    doses <- coverage*target_pop
    assert_scalar_non_negative(doses)

    doses
}
