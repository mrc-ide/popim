##' Infer vaccination activities from population
##'
##' Given a `popim_population` object and an assumption of how vaccine
##' is targeted in a partially immune population, this function infers
##' the vaccination activities that have given rise to the specified
##' population immunity. When there is ambiguity (e.g., due to
##' subsequent campaigns achieving full coverage), the minimum
##' coverage/doses needed will be returned.
##'
##' Default is the targeting option "random", which assumes that
##' individuals receive vaccine independently of vaccination status,
##' resulting in some double vaccination if there is pre-existing
##' immunity in the population. The option "targeted" gives the most
##' effective vaccine distribution, vaccinating unvaccinated people
##' first, while the option "correlated" models a situation of
##' inequalities in access to vaccination - in this extreme cases, all
##' previously vaccinated individuals will be vaccinated first, before
##' any remaining doses are given to unvaccinated individuals.
##'
##' Note that this function will not return any potential vaccination
##' activities that don't change the immunity, for instance if
##' `targeting` is set to "correlated" and the coverage is too small
##' to increase population immunity, or if the immunity prior to the
##' activity was already at 1.
##'
##' The vaccination of the oldest age group in the population will
##' also never be picked up as this age group will have aged out of
##' the population (i.e., died) before the immunity is updated in the
##' next year.
##' @param pop `popim_population` object for which vaccination activities
##'     are to be inferred.
##' @param targeting character, determines the assumption of how doses
##'     are allocated. Valid options are "random", "correlated",
##'     "targeted".
##' @param n_digits number of digits to which the coverage is to be
##'     rounded, defaults to 10.
##' @return popim_vacc_activites object
##' @export
##' @author Tini Garske
vacc_from_immunity <- function(pop, targeting = "random", n_digits = 10) {

    assert_population(pop)
    assert_valid_targeting(targeting)

    pop <- add_immunity_rate(pop)

    vacc <- pop |>
        dplyr::mutate(coverage =
                          coverage_from_immunity_diff(.data$immunity,
                                                      .data$immunity_diff,
                                                      targeting = targeting,
                                                      n_digits = n_digits)) |>
        dplyr::mutate(doses = .data$pop_size * .data$coverage) |>
        dplyr::filter(.data$coverage > 0) |>
        dplyr::mutate(age_first = .data$age, age_last = .data$age,
                      targeting = targeting) |>
        dplyr::select(tidyselect::all_of(c("region", "year", "age_first",
                                           "age_last", "coverage", "doses",
                                           "targeting"))) |>
        dplyr::arrange(.data$region, .data$year, .data$targeting,
                       round(.data$coverage, n_digits), .data$age_first)

    class(vacc) <- c("popim_vacc_activities", "data.frame")
    if(nrow(vacc) > 1)
        vacc <- aggregate_vacc_activities(vacc, n_digits = n_digits)
    vacc <- complete_vacc_activities(vacc, pop)
    validate_vacc_activities(vacc)

    vacc
}

##' Add the rate of immunity change to a `popim_population` object
##'
##' @param pop `popim_population` object
##' @return the input `popim_population` object with an added column
##'     `immunity_diff` that holds the difference in immunity between
##'     the current and next year.
##' @author Tini Garske
##' @noRd
add_immunity_rate <- function(pop) {

    a <- attributes(pop)

    pop <- pop |> dplyr::select(!tidyselect::any_of("immunity_diff"))

    pop_next <- pop |>
        dplyr::rename(year_next = tidyselect::all_of("year"),
                      immunity_next = tidyselect::all_of("immunity")) |>
        dplyr::select(tidyselect::all_of(c("region", "year_next", "cohort",
                                           "immunity_next")))

    pop <- pop |> dplyr::mutate(year_next = .data$year + 1)

    pop_out <- dplyr::left_join(pop, pop_next,
                                by = c("region", "cohort", "year_next")) |>
        dplyr::mutate(immunity_diff = .data$immunity_next - .data$immunity) |>
        dplyr::select(!tidyselect::all_of(c("year_next", "immunity_next")))

    a$names <- names(pop_out)
    attributes(pop_out) <- a

    pop_out
}

coverage_from_immunity_diff <- function(imm_now, imm_diff, targeting,
                                        n_digits = 10) {

    assert_valid_targeting(targeting)

    if(targeting == "random") {
        coverage <- (imm_diff) / (1 - imm_now)
    } else if(targeting == "targeted") {
        coverage <- imm_diff
    } else if(targeting == "correlated") {
        coverage <- ifelse(imm_diff > 0, imm_now + imm_diff, 0)
    }

    coverage <- round(coverage, digits = n_digits)
    coverage
}

##' Expand the age range given by age_first and age_last into an
##' integer vector giving each individual age group
##'
##' If the `popim_vacc_activities` object contains more than one row the
##' rows are assumed to belong to the same campaign, but detail
##' different age groups. The function will expand the age ranges to a
##' vector that gives every single age cohort targeted.
##'
##' @param va object of class `popim_vacc_activities`
##' @return integer vector of variable length
##' @author Tini Garske
##' @noRd
get_all_ages <- function(va) {

    assert_vacc_activities(va)
    ## va is a set of vaccination activities that could be combined into
    ## one, as they share the region, year, targeting and coverage, but
    ## target potentially different ages.
    sapply(seq_len(nrow(va)), function(i) va$age_first[i]:va$age_last[i]) |>
        unlist() |> sort()
}

##' Summarise the individual ages into one or more consecutive age ranges
##'
##' @param ages non-negative integer vector of individual ages to be
##'     targeted in a vaccination activity.
##' @return list of age ranges: The list elements are integer vectors
##'     of length 2, giving the age_first and age_last of the age
##'     range into which the ages can be aggregated. The length of the
##'     list depends on the complexity of the input ages.
##' @author Tini Garske
##' @noRd
get_consecutive_range <- function(ages) {

    assert_non_negative(ages)
    assert_wholenumber(ages)

    ages <- sort(ages)

    consecutive_start <- function(ages) {
        ## ages is a non-negative integer vector of ages that are targeted
        ## in a vaccination activity, as output by function get_all_ages.

        ## This function checks if these ages are consecutive, so can be
        ## expressed as age_min:age_max

        age_min <- min(ages)
        age_max <- max(ages)

        tt <- table(factor(ages, levels = age_min:age_max))

        a_mis <- which(tt == 0)[1]
        if(is.na(a_mis)) {
            my_range <- c(age_min, age_max)
            tt <- tt - 1

        } else {

            my_range <- c(age_min, tt[a_mis-1] |> names() |> as.numeric())
            tt[1:(a_mis-1)] <- tt[1:(a_mis-1)] - 1
        }

        ages_left <- sapply(seq_along(tt), function(i)
            rep(names(tt)[i] |> as.numeric(), tt[i])) |>
            unlist()

        list(range = my_range, ages_res = ages_left)
    }

    age_ranges <- list()
    while(length(ages) > 0) {
        tmp <- consecutive_start(ages)
        age_ranges[[length(age_ranges) + 1]] <- tmp$range
        ages <- tmp$ages_res
    }

    age_ranges
}

##' Aggregate matching vaccination activities across age groups
##'
##' The vaccination activities contained in the input dataset are
##' matched by region, year, coverage and targeting method, and for
##' any that match on these variables, the age range will be
##' compressed into a (or several) consecutive age range, such that
##' the same information is coded in fewer lines.
##'
##' @param vacc popim_vacc_activities object to be aggregated
##' @param n_digits number of digits to which coverage is rounded
##'     before coverages from different activities are matched.
##' @return popim_vacc_activities object containing the same vaccination
##'     activities as the input object, but where possible aggregated
##'     into fewer lines
##' @author Tini Garske
##' @noRd
aggregate_vacc_activities <- function(vacc, n_digits = 10) {

    assert_vacc_activities(vacc)

    if(nrow(vacc) < 2) {
        message(sprintf("vacc has %d rows, no aggregation performed.",
                        nrow(vacc)))
        return(vacc)
    }

    vacc <- vacc |>
        dplyr::mutate(coverage = round(.data$coverage, n_digits)) |>
        dplyr::group_by(.data$region, .data$year, .data$targeting,
                        .data$coverage) |>
        dplyr::arrange(.data$region, .data$year, .data$targeting,
                       .data$coverage) |>
        dplyr::mutate(id = dplyr::cur_group_id())

    va_agg <- vacc |>
        dplyr::summarise(doses = sum(.data$doses), id = mean(.data$id)) |>
        dplyr::ungroup()
    class(vacc) <- c("popim_vacc_activities", "data.frame")

    vacc_ids <- vacc$id |> unique()
    ages_list <- lapply(vacc_ids,
                        function(i) get_all_ages(vacc |>
                                                 dplyr::filter(.data$id == i)))

    ranges_list <- lapply(seq_along(ages_list),
                          function(i) get_consecutive_range(ages_list[[i]]))
    if((depth <- list_depth(ranges_list)) != 2)
        stop("ranges_list should have depth 2, but here it has %d.", depth)

    n_ranges <- vapply(seq_along(ranges_list),
                       function(i) length(ranges_list[[i]]), integer(1))

    ranges_df <- data.frame(id = vacc_ids, n_ranges = n_ranges) |>
        tidyr::uncount(n_ranges, .remove = FALSE, .id = "id2")

    ranges_df <- do.call(rbind, unlist(ranges_list, recursive = FALSE)) |>
        as.data.frame() |>
        dplyr::rename(age_first = tidyselect::all_of("V1"),
                      age_last = tidyselect::all_of("V2")) |>
        cbind(ranges_df)

    va_agg$n_ranges <- n_ranges
    va_agg <- va_agg |>
        dplyr::mutate(doses = ifelse(n_ranges == 1, .data$doses, NA)) |>
        tidyr::uncount(.data$n_ranges, .id = "id2") |>
        dplyr::left_join(ranges_df, by = c("id", "id2")) |>
        dplyr::select(tidyselect::all_of(c("region", "year", "age_first",
                                           "age_last", "coverage", "doses",
                                           "targeting")))
    class(va_agg) <- c("popim_vacc_activities", "data.frame")

    va_agg
}
