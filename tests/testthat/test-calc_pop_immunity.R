test_that("calc_pop_immunity aggregates correctly over age", {
    ## a sample population with missing pop_size
    pop <- popim_population(region = "UK", year_min = 2001, year_max = 2003,
                          age_min = 0, age_max = 4)

    pop_exp <- data.frame(region = "UK", year = 2001:2003,
                          pop_size = NA_real_, immunity = NA_real_)
    expect_equal(pop |> calc_pop_immunity() |> as.data.frame(), pop_exp)
    

    ## a sample population with 0 immunity
    pop <- popim_population(region = "UK", year_min = 2001, year_max = 2003,
                          age_min = 0, age_max = 4)
    pop$pop_size <- 100

    pop_exp <- data.frame(region = "UK", year = 2001:2003,
                          pop_size = 500, immunity = 0)

    expect_equal(pop |> calc_pop_immunity() |> as.data.frame(), pop_exp)

    expect_equal(pop_exp, as.data.frame(calc_pop_immunity(pop)))
    
    ## a sample population with some non-zero immunity
    pop <- apply_vacc_1(pop, region = "UK", year = 2001,
                      age_first = 0, age_last = 0,
                      doses = NA, coverage = 1)
    pop_exp <- data.frame(region = "UK", year = 2001:2003, pop_size = 500,
                          immunity = c(0, 0.2, 0.2))

    expect_equal(pop |> calc_pop_immunity() |> as.data.frame(), pop_exp)

    ## a sample population with 2 regions
    pop <- popim_population(region = c("FRA", "UK"),
                          year_min = 2000, year_max = 2000,
                          age_min = 0, age_max = 4)
    pop$pop_size = 100

    pop <- apply_vacc_1(pop, region = "UK", year = 1999,
                      age_first = 0, age_last = 0,
                      coverage = 1)
    pop_exp <- data.frame(region = c("FRA", "UK"), year = 2000,
                          pop_size = 500, immunity = c(0, 0.2))
    
    expect_equal(pop |> calc_pop_immunity() |> as.data.frame(), pop_exp)

})


