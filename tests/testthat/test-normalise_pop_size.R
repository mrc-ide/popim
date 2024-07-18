test_that("normalise_pop_size works correctly", {
    
    ## single region, constant pop_size through all ages and years:
    age_min <- 0
    age_max <- 5
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2004,
                            age_min = age_min, age_max = age_max)
    pop$pop_size <- 1

    rel_size_exp <- rep(1/(age_max - age_min + 1), nrow(pop))
    expect_equal(normalise_pop_size(pop)$pop_rel, rel_size_exp)

    ## population size varying through the years:
    age_min <- 0
    age_max <- 4
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                            age_min = age_min, age_max = age_max)

    pop$pop_size <- pop$cohort - 1990

    pop_tot <- pop |> dplyr::group_by(.data$year) |>
        dplyr::summarise(tot = sum(.data$pop_size))
    pop_rel <- pop$pop_size / max(pop_tot$tot)

    expect_equal(pop_rel,
                 normalise_pop_size(pop)$pop_rel)

    ## setting up a 2-region population:
    pop2 <- pop
    pop2$region <- "FRA"
    pop2$pop_size <- pop2$pop_size * 2 ## double the population size,
                                       ## but the age structure is
                                       ## still the same, so relative
                                       ## population size will be the
                                       ## same.

    pop2_tot <- pop2 |> dplyr::group_by(.data$year) |>
        dplyr::summarise(tot = sum(.data$pop_size))
    
    pop_agg <- rbind(pop, pop2)
    pop_rel_agg <- c(pop_rel, pop_rel)

    expect_equal(pop_rel_agg,
                 normalise_pop_size(pop_agg)$pop_rel)
    
})
