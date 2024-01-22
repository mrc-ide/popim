test_that("add_immunity_rate adds a column immunity_diff correctly", {

    ## no immunity:
    year_min <- 2000
    year_max <- 2005
    age_min <- 0
    age_max <- 4
    pop <- vip_population(region = "UK", year_min, year_max,
                          age_min, age_max)
    pop$pop_size <- 100

    ## manual:
    pop_1 <- pop |> dplyr::mutate(immunity_diff = 0) |>
        dplyr::mutate(immunity_diff =
                          ifelse(year == year_max, NA, immunity_diff)) |>
        dplyr::mutate(immunity_diff = ifelse(age == age_max, NA, immunity_diff))

    a <- attributes(pop)
    a$names <- names(pop_1)
    attributes(pop_1) <- a

    
    pop_2 <- add_immunity_rate(pop)

    expect_equal(pop_1, pop_2)

    ## with some immunity:
    year_vacc <- 2002
    pop <- pop |> dplyr::mutate(immunity = ifelse(year > year_vacc, 0.5, 0))

    pop_1 <- pop |>
        dplyr::mutate(immunity_diff = ifelse(year == year_vacc, 0.5, 0)) |>
        dplyr::mutate(immunity_diff =
                          ifelse(year == year_max, NA, immunity_diff)) |>
        dplyr::mutate(immunity_diff = ifelse(age == age_max, NA, immunity_diff))

    a <- attributes(pop)
    a$names <- names(pop_1)
    attributes(pop_1) <- a

    pop_2 <- add_immunity_rate(pop)

    expect_equal(pop_1, pop_2)

})

test_that("add_immunity_rate works when pop already has an immunity_diff column", {

    year_min <- 2000
    year_max <- 2005
    age_min <- 0
    age_max <- 4
    pop <- vip_population(region = "UK", year_min, year_max,
                          age_min, age_max)

    pop_1 <- add_immunity_rate(pop)
    pop_2 <- add_immunity_rate(pop_1)

    expect_equal(add_immunity_rate(pop_1), pop_1)

    ## what if pop has another non-standard column:
    pop$spurious = 1:nrow(pop)
    pop_1 <- pop_1_0 <- add_immunity_rate(pop)
    pop_2 <- add_immunity_rate(pop_1)

    pop_1_0$immunity_diff <- NULL

    expect_equal(pop, pop_1_0)
    expect_equal(pop_1, pop_2)

})
