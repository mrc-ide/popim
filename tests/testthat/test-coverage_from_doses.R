test_that("coverage_from_doses returns the correct coverage for sensible inputs", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    expect_equal(0.25, coverage_from_doses(pop = pop, doses = 50,
                                           region = "UK", year = 2001,
                                           age_first = 0, age_last = 1))

    ## more doses than the target population:
    expect_equal(1, coverage_from_doses(pop = pop, doses = 500,
                                        region = "UK", year = 2001,
                                        age_first = 0, age_last = 1))

    ## targeting several regions at once:
    expect_equal(0.5, coverage_from_doses(pop = pop, doses = 400,
                                          region = c("UK", "FRA"),
                                          year = 2001, age_first = 0,
                                          age_last = 3))

    ## using the default age_first/age_last entries
    expect_equal(0.25, coverage_from_doses(pop = pop, doses = 100,
                                           region = "UK", year = 2001))
})

test_that("coverage_from_doses fails if the population object doesn't contain all targeted cohorts", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    ## region not covered in pop:
    expect_error(coverage_from_doses(pop = pop, doses = 100, region = "GER",
                                     year = 2001, age_first = 0, age_last = 100))
    ## year not covered in pop:
    expect_error(coverage_from_doses(pop = pop, doses = 100, region = "UK",
                                     year = 1998, age_first = 0, age_last = 1))
    ## target age group(s) not covered in pop:
    expect_error(coverage_from_doses(pop = pop, doses = 100, region = "UK",
                                     year = 2001, age_first = 5, age_last = 5))
})

test_that("coverage_from_doses fails for silly input values", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    expect_error(coverage_from_doses(pop = pop, doses = 50, region = "UK",
                                     year = 2001, age_first = -1, age_last = 1))
    expect_error(coverage_from_doses(pop = pop, doses = 50, region = "UK",
                                     year = 2001, age_first = 2, age_last = 1))
})
