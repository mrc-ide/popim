#' Combine two coverages for the same cohort
#'
#' This function calculates the resulting coverage if coverages of
#' two separate vaccination activities are combined, and returns this
#' single value.
#'
#' @param coverage numeric between 0 and 1. Coverage value of the new
#'     campaign as a proportion of the target population.
#' @param prev_immunity numeric between 0 and 1. Pre-existing immunity
#'     of the cohort to be vaccinated as a proportion. Can be a vector
#'     (e.g. for calculation of resulting immunity in the same cohort
#'     forward in time).
#' @param targeting char to determine the assumption used to combine
#'     coverages. Valid values are "random", "correlated", "targeted".
#'     For targeting = "random" (the default), allocation of vaccine
#'     is random within the population, therefore the resulting
#'     coverage will be smaller than the sum of the coverages. For
#'     targeting = "correlated", there is a 100% correlation between
#'     who will get the vaccine in either vaccination activitiy, the
#'     resulting coverage is simply the larger of the two inputs. For
#'     targeting = "targeted", doses are targeted at unvaccinated
#'     people, resulting in the sum of both coverages, though capped
#'     by 1 (full coverage).
#' @return A scalar: combined coverage
#' @noRd
calc_new_immunity <- function(coverage, prev_immunity, targeting) {

    assert_scalar(coverage)
    assert_0_to_1(coverage)
    assert_0_to_1(prev_immunity)
    assert_1d(prev_immunity)
    ## technically, a matrix would work - but it probably wouldn't be
    ## sensible, so more likely an error...
    assert_valid_targeting(targeting)
    
    if(targeting == "random") {
        return(coverage + prev_immunity - coverage*prev_immunity)
    }

    if(targeting == "correlated") {
        return(sapply(prev_immunity,
                      function(x) max(x, coverage, na.rm = TRUE)))
    }

    if(targeting == "targeted") {
        return(sapply(prev_immunity,
                      function(x) min(1, coverage + x, na.rm = TRUE)))
    }
}

assert_valid_targeting <- function(x, name = deparse(substitute(x))) {
    assert_character(x)
    if(!(x %in% c("random", "correlated", "targeted"))) {
        stop(sprintf("'%s' must be one of 'random', 'correlated', 'targeted'",
                     name), call. = FALSE)
    }
}

##' Aggregate the population immunity over age
##'
##' Calculate the overall population immunity (aggregating over age)
##' from the supplied `popim_population` object.
##'
##' @param pop A `popim_population` object for which the population size and
##'     immunity will be aggregated over age.
##' @return A dataframe containing the `popim_population` aggregated by age.
##' @author Tini Garske
##' @export
calc_pop_immunity <- function(pop) {

    assert_population(pop)

    pop_no_age <- pop |>
        dplyr::mutate(n_immune = .data$pop_size * .data$immunity) |>
        dplyr::group_by(.data$region, .data$year) |>
        dplyr::summarise(pop_size = sum(.data$pop_size),
                         n_immune = sum(.data$n_immune)) |>
        dplyr::mutate(immunity = .data$n_immune / .data$pop_size) |>
        dplyr::select(!tidyselect::any_of("n_immune"))

    pop_no_age
}
