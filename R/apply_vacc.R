##' Apply a single vaccination activity to a `popim_population` object
##'
##' The vaccination activity is defined by the input parameters of the
##' function.
##'
##' If the vaccination activity has non-missing entries for both
##' coverage and doses, the coverage is used - there is no test
##' whether the coverage and doses information are consistent with the
##' target population size in the popim_population object to which the
##' vaccination activity is applied.
##'
##' @param pop `popim_population` dataframe object such as created
##'     by function [popim_population()]
##' @param region character, region of the vaccination activity
##' @param year integer, year of the vaccination activity
##' @param age_first,age_last non-negative integer with `age_last` >=
##'     `age_first`. Age range targeted
##' @param coverage numeric, 0 <= `coverage` <= 1. Proportion of the
##'     target population to be vaccinated in the activity.
##' @param doses numeric, non-negative. Number of doses available to
##'     vaccinate the target population.
##' @param targeting character, determines how successive activities
##'     are targeted. Valid values are "random", "correlated",
##'     "targeted".  For `targeting` = "random" (the default),
##'     allocation of vaccine is random within the population,
##'     therefore the resulting coverage will be smaller than the sum
##'     of the coverages. For `targeting` = "correlated", there is a
##'     100% correlation between who will get the vaccine in either
##'     vaccination activitiy, the resulting coverage is simply the
##'     larger of the two inputs. For `targeting` = "targeted", doses
##'     are targeted at unvaccinated people, resulting in the sum of
##'     both coverages, though capped by 1 (full coverage).
##' @return The supplied population dataframe object with
##'     updated immunity to reflect the vaccination activity.
##' @noRd
##' @author Tini Garske
apply_vacc <- function(pop, region, year, age_first = 0, age_last = Inf,
                       coverage = double(), doses = NA_real_,
                       targeting = "random") {

    stopifnot(is_population(pop))

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
        coverage = coverage_from_doses(pop, doses, region, year,
                                       age_first, age_last)
    }

    for(j in cohorts) {
        i_vec <- which(pop$region == region & pop$cohort == j &
                       pop$year > year)
        ## vaccination is implemented at the end of the year, so
        ## doesn't change immunity until the next year.
        if(length(i_vec) > 0) {
            pop$immunity[i_vec] <-
                calc_new_immunity(coverage, pop$immunity[i_vec],
                                  targeting = targeting)
        }
    }
    pop
}
##' Apply vaccination activities to a `popim_population` object
##'
##' Takes the vaccination activities listed in the input object
##' vaccs_df (a data.frame object of class popim_vacc_activities), and
##' applies them to the popim_population object pop, then returns
##' that updated object.
##'
##' @param pop An object of class `popim_population` such as
##'     created by [popim_population()].
##' @param vaccs_df An object of class `popim_vacc_activities` such as
##'     created by [popim_vacc_activities()].
##' @return The input `popim_population` class object `pop` with
##'     updated column `immunity` to reflect the vaccination
##'     activities supplied in `vaccs_df`.
##' @export
##' @author Tini Garske
apply_vaccs <- function(pop, vaccs_df) {
    for(i in seq_len(nrow(vaccs_df))) {

        pop <- apply_vacc(pop, vaccs_df$region[i],
                             vaccs_df$year[i],
                             age_first = vaccs_df$age_first[i],
                             age_last = vaccs_df$age_last[i],
                             coverage = vaccs_df$coverage[i],
                             doses = vaccs_df$doses[i],
                             targeting = vaccs_df$targeting[i])
    }
    pop
}

##' Calculate the coverage of a vaccination activity achieved with a
##' given number of doses
##'
##' The coverage for a vaccination activity is calculated as the
##' proportion of the target population that can be vaccinated by the
##' given number of doses. The target population is the sum of the
##' population of the targeted age groups in the targeted region(s).
##'
##' @param pop object of class popim_population such as created by
##'     function `popim_population`
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
##' @noRd
coverage_from_doses <- function(pop, doses, region, year, age_first = 0,
                                age_last = Inf) {

    stopifnot(is_population(pop))

    assert_scalar_non_negative(doses)

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    if(age_last == Inf) age_last <- max(pop$age)

    ## check that pop contains relevant info for the proposed
    ## vaccination activity:
    pop_targeted <- expand.grid(region = region, year = year,
                                age = age_first:age_last) |>
        dplyr::left_join(pop, by = c("region", "year", "age"))

    target_pop <- sum(pop_targeted$pop_size)
    if(is.na(target_pop))
        stop("pop does is missing pop_size information for at least some targeted cohorts.")

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
##' @param pop object of class popim_population such as created by
##'     [popim_population()].
##' @param coverage proportion of the target population to be covered,
##'     0 <= `coverage` <= 1.
##' @param region region to be targeted.
##' @param year year of vaccination activity.
##' @param age_first youngest age group to be targeted.
##' @param age_last oldest age group to be targeted.
##' @return A non-negative number indicating how many doses will be
##'     needed to achieve the given coverage in the target population.
##' @author Tini Garske
##' @noRd
doses_from_coverage <- function(pop, coverage, region, year, age_first = 0, age_last = Inf) {

    stopifnot(is_population(pop))

    assert_scalar_0_to_1(coverage)

    assert_character(region)
    assert_scalar_wholenumber(age_first)
    assert_scalar_wholenumber(age_last)

    assert_non_negative(age_first)
    assert_non_negative(age_last - age_first)

    if(age_last == Inf) age_last <- max(pop$age)

    ## check that pop contains relevant info for the proposed
    ## vaccination activity:
    pop_targeted <- expand.grid(region = region, year = year,
                                age = age_first:age_last) |>
        dplyr::left_join(pop, by = c("region", "year", "age"))

    target_pop <- sum(pop_targeted$pop_size)
    if(is.na(target_pop))
        stop("pop does is missing pop_size information for at least some targeted cohorts.")

    doses <- coverage*target_pop
    assert_scalar_non_negative(doses)

    doses
}
