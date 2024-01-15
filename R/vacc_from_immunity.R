##' Infer vaccination activities from population
##'
##' Given a vip_population object and an assumption of how vaccine is
##' targeted in a partially immune population, this function infers
##' the vaccination activities that returns a vip_vacc_activities
##' object that details the vaccination activities that have given
##' rise to the specified population immunity.
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
##' @param pop vip_population object for which vaccination activities
##'     are to be inferred
##' @param targeting string to determine the assumption of how doses
##'     are allocated. Valid options are "random", "correlated",
##'     "targeted". 
##' @return vip_vacc_activites object
##' @export
##' @author Tini Garske
##' @importFrom rlang .data
vacc_from_immunity <- function(pop, targeting = "random") {

    assert_valid_targeting(targeting)
    stopifnot(targeting == "random")
    
    pop_next <- pop |>
        dplyr::rename(year_next = .data$year, pop_size_next = .data$pop_size,
               immunity_next = .data$immunity) |>
        dplyr::select(!tidyselect::any_of("age"))

    pop <- pop |> dplyr::mutate(year_next = .data$year + 1)

    vaccs <- dplyr::left_join(pop, pop_next,
                              by = c("region", "cohort", "year_next")) |>
        dplyr::select(!tidyselect::any_of("year_next")) |>
        dplyr::mutate(immunity_diff = .data$immunity_next - .data$immunity,
                      pop_size_diff = .data$pop_size_next - .data$pop_size)|>
        ## for random targeting:
        dplyr::mutate(coverage = (.data$immunity_next - .data$immunity) /
                          (1 - .data$immunity)) |>
        dplyr::mutate(doses = .data$pop_size * .data$coverage) |>
        dplyr::filter(.data$coverage > 0) |>
        dplyr::mutate(age_first = .data$age, age_last = .data$age,
                      targeting = "random") |>
        dplyr::select(tidyselect::all_of(c("region", "year", "age_first",
                                           "age_last", "coverage", "doses",
                                           "targeting"))) |>
        dplyr::arrange(.data$region, .data$year, .data$age_first)

    class(vaccs) <- c("vip_vacc_activities", "data.frame")
    validate_vacc_activities(vaccs)

    vaccs
    ## this vip_vacc_activities object has individual entries for each
    ## age group. To do: implement an aggregation function that
    ## aggregates compatible activities using the age_first and
    ## age_last option.
}


get_all_ages <- function(va) {

    assert_vacc_activities(va)
    ## va is a set of vaccination activities that could be combined into
    ## one, as they share the region, year, targeting and coverage, but
    ## target potentially different ages.
    sapply(seq_len(nrow(va)), function(i) va$age_first[i]:va$age_last[i]) |>
        unlist() |> sort()
}
