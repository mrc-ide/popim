##' Plot the vaccine-derived immunity of a `popim_population` object
##'
##' @details The population is displayed in a grid showing the cohorts
##'     through time. Time is shown on the x-axis, age on the y-axis,
##'     such that a particular cohort tracks along diagonally from
##'     bottom left to top right. If there are several regions, these
##'     are shown as separate facets. The colour in each cell
##'     corresponds to the proportion of the cohort that is immune.
##'
##' @param pop_df `popim_population` object such as created by
##'     [popim_population()]
##' @return ggplot object
##' @export
##' @author Tini Garske
plot_immunity <- function(pop_df) {

    assert_population(pop_df)
    
    pal <- MetBrewer::met.brewer("VanGogh3", 5, "discrete")
    ## might want to get rid of this dependency and implement the
    ## option to pass a colour scheme

    g <- ggplot2::ggplot(pop_df) +
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

##' Plot the population size of a `popim_population` object
##'
##' @details The population is displayed in a grid showing the cohorts
##'     through time. Time is shown on the x-axis, age on the y-axis,
##'     such that a particular cohort tracks along diagonally from
##'     bottom left to top right. If there are several regions, these
##'     are shown as separate facets. The colour in each cell
##'     corresponds to the number of people in the cohort. 
##' @param pop_df `popim_population` object such as created by
##'     [popim_population()]
##' @return ggplot object
##' @author Tini Garske
##' @export
plot_pop_size <- function(pop_df) {
    assert_population(pop_df)
    
    pal <- MetBrewer::met.brewer("VanGogh3", 5, "discrete")
    ## might want to get rid of this dependency and implement the
    ## option to pass a colour scheme

    g <- ggplot2::ggplot(pop_df) +
        ggplot2::aes(x = .data$year, y = .data$age, fill = .data$pop_size) +
        ggplot2::geom_tile() +
        ggplot2::facet_wrap(~region) +
        ggplot2::scale_fill_gradient(low = "white",
                                     high = pal[5],
                                     limits = c(0,1)) +
        ggplot2::labs(x = "year", y = "age", fill = "population size") +
        ggplot2::theme_minimal()
    
    return(g)
}
