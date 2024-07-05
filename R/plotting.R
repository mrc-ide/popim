##' Plot the immunity or population size of a `popim_population` object
##'
##' @details The population is displayed in a grid showing the cohorts
##'     through time. Time is shown on the x-axis, age on the y-axis,
##'     such that a particular cohort tracks along diagonally from
##'     bottom left to top right. If there are several regions, these
##'     are shown as separate facets.
##'
##' The colour in each cell corresponds to:
##' * for `plot_immunity()`: the proportion of each cohort that is
##'   immune, therefore varying between 0 and 1.
##' * for `plot_pop_size()`: the size of each cohort.
##'
##' As the returned object is a regular `ggplot` object, it can be
##' further modified with the ususal `ggplot2` syntax.
##'
##'
##' @param pop `popim_population` object such as created by
##'     [popim_population()].
##' @return A ggplot object.
##' @name plotting
NULL

##' @rdname plotting
##' @export
##' @author Tini Garske
##' @examples
##' ## set up population and vaccination activities:
##' pop <- popim_population(region = "UK", year_min = 2000, year_max = 2005,
##'                         age_min = 0, age_max = 10)
##' vacc <- popim_vacc_activities(region = "UK", year = c(2001, 2002),
##'                               age_first = 0, age_last = 0,
##'                               coverage = 0.8, doses = NA,
##'                               targeting = "random")
##'
##' ## update the population immunity based on the vaccination activities:
##' pop <- apply_vacc(pop, vacc)
##'
##' ## plot the population size by age and time:
##' plot_pop_size(pop)
##'
##' ## plot the population immunity by age and time:
##' plot_immunity(pop)
##'
plot_immunity <- function(pop) {

    assert_population(pop)
    
    pal <- MetBrewer::met.brewer("VanGogh3", 5, "discrete")
    ## might want to get rid of this dependency and implement the
    ## option to pass a colour scheme

    g <- ggplot2::ggplot(pop) +
        ggplot2::aes(x = .data$year, y = .data$age, fill = .data$immunity) +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~region) +
        ggplot2::scale_fill_gradient(low = "white",
                                     high = pal[5],
                                     limits = c(0,1)) +
        ggplot2::labs(x = "year", y = "age", fill = "immunity") +
        ggplot2::theme_minimal()
    
    return(g)
}

##' @rdname plotting
##' @author Tini Garske
##' @export
plot_pop_size <- function(pop) {
    assert_population(pop)
    
    pal <- MetBrewer::met.brewer("VanGogh3", 5, "discrete")
    ## might want to get rid of this dependency and implement the
    ## option to pass a colour scheme

    g <- ggplot2::ggplot(pop) +
        ggplot2::aes(x = .data$year, y = .data$age, fill = .data$pop_size) +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~region) +
        ggplot2::scale_fill_gradient(low = "white",
                                     high = pal[5]) +
        ggplot2::labs(x = "year", y = "age", fill = "population size") +
        ggplot2::theme_minimal()
    
    return(g)
}
