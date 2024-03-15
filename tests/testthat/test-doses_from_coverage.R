test_that("doses_from_coverage returns the correct number of doses for sensible inputs", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    expect_equal(50, doses_from_coverage(pop = pop,
                                         coverage = 0.25,
                                         region = "UK", year = 2001,
                                         age_first = 0, age_last = 1))
    ## targeting several regions at once:
    expect_equal(400,
                 doses_from_coverage(pop = pop, coverage = 0.5,
                                     region = c("UK", "FRA"), year = 2001,
                                     age_first = 0, age_last = 3))

    ## using the default age_first/age_last entries:
    expect_equal(100, doses_from_coverage(pop = pop, coverage = 0.25,
                                          region = "UK", year = 2001))
    })

test_that("doses_from_coverage fails if the population object doesn't contain all targeted cohorts", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    ## region not covered in pop:
    expect_error(doses_from_doses(pop = pop, coverage = 0.5, region = "GER",
                                     year = 2001, age_first = 0, age_last = 100))
    ## year not covered in pop:
    expect_error(doses_from_coverage(pop = pop, coverage = 0.5, region = "UK",
                                     year = 1998, age_first = 0, age_last = 1))
    ## target age group(s) not covered in pop:
    expect_error(doses_from_coverage(pop = pop, coverage = 0.5, region = "UK",
                                     year = 2001, age_first = 5, age_last = 5))
})

test_that("coses_from_coverage fails for silly input values", {
    pop <- popim_population(region = c("UK", "FRA"),
                          year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 3)
    pop$pop_size <- 100

    expect_error(doses_from_coverage(pop = pop, coverage = 0.5, region = "UK",
                                     year = 2001, age_first = -1, age_last = 1))
    expect_error(doses_from_coverage(pop = pop, coverage = 0.5, region = "UK",
                                     year = 2001, age_first = 2, age_last = 1))
})
