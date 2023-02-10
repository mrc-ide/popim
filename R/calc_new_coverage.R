#' Function to combine two coverages for the same cohort
#'
#' This function calculates the resulting coverage if coverages of
#' two separate vaccination activities are combined, and returns this
#' single value.
#'
#' @param coverage coverage value of the new campaign as a proportion
#'     - needs to be between 0 and 1.
#' @param prev_immunity pre-existing immunity of the cohort to be
#'     vaccinated as a proportion. Can be a vector (e.g. for
#'     calculation of resulting immunity in the same cohort forward in
#'     time). All entries need to be between 0 and 1.
#' @param skew char to determine the assumption used to combine
#'     coverages. Valid values are "random", "correlated", "targeted".
#'     For skew = "random" (the default), allocation of vaccine is
#'     random within the population, therefore the resulting coverage
#'     will be smaller than the sum of the coverages. For skew =
#'     "correlated", there is a 100% correlation between who will get
#'     the vaccine in either vaccination activitiy, the resulting
#'     coverage is simply the larger of the two inputs. For skew =
#'     "targeted", doses are targeted at unvaccinated people,
#'     resulting in the sum of both coverages, though capped by 1
#'     (full coverage).
#' @return A scalar: combined coverage
#'
calc_new_coverage <- function(coverage, prev_immunity, skew) {

    assert_scalar(coverage)
    assert_0_to_1(coverage)
    assert_0_to_1(prev_immunity)
    assert_1d(prev_immunity)
    ## technically, a matrix would work - but it probably wouldn't be
    ## sensible, so more likely an error...
    assert_valid_skew(skew)
    
    if(skew == "random") {
        return(coverage + prev_immunity - coverage*prev_immunity)
    }

    if(skew == "correlated") {
        return(sapply(prev_immunity,
                      function(x) max(x, coverage, na.rm = TRUE)))
    }

    if(skew == "targeted") {
        return(sapply(prev_immunity,
                      function(x) min(1, coverage + x, na.rm = TRUE)))
    }
}

assert_valid_skew <- function(x, name = deparse(substitute(x))) {
    assert_character(x)
    if(!(x %in% c("random", "correlated", "targeted"))) {
        stop(sprintf("'%s' must be one of 'random', 'correlated', 'targeted'",
                     name), call. = FALSE)
    }
}
