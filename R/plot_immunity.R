##' Function to plot the vaccine-derived immunity of a population
##'
##' @param pop_df population dataframe object such as created by
##'     function setup_population
##' @return ggplot object
##' @export
##' @author Tini Garske
##' @importFrom rlang .data
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
