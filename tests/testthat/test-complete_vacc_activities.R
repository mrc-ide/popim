test_that("complete_vacc_activities adds appropriate doses information for a single activity", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000, age_first = 0,
                                 age_last = 0, coverage = 0.5, doses = NA,
                                 targeting = "random")

    vacc <- complete_vacc_activities(vacc = vacc, pop = pop)

    expect_equal(50, vacc$doses)
})

test_that("complete_vacc_activities adds appropriate doses information for several activities", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000:2002, age_first = 0,
                                 age_last = 0, coverage = 0.5, doses = NA,
                                 targeting = "random")

    vacc <- complete_vacc_activities(vacc = vacc, pop = pop)

    expect_equal(rep(50, 3), vacc$doses)
})

test_that("complete_vacc_activities adds appropriate coverage information for a single activity", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000, age_first = 0,
                                 age_last = 0, coverage = NA, doses = 100,
                                 targeting = "random")

    vacc <- complete_vacc_activities(vacc = vacc, pop = pop)

    expect_equal(1, vacc$coverage)
})

test_that("complete_vacc_activities adds appropriate coverage information for several activities", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000:2002, age_first = 0,
                                 age_last = 0, coverage = NA, doses = 100,
                                 targeting = "random")

    vacc <- complete_vacc_activities(vacc = vacc, pop = pop)

    expect_equal(rep(1, 3), vacc$coverage)
})

test_that("complete_vacc_activities flags inconsistend coverage and doses information for a single activity", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000, age_first = 0,
                                 age_last = 0, coverage = 0.5, doses = 100,
                                 targeting = "random")

    expect_error(vacc <- complete_vacc_activities(vacc = vacc, pop = pop))
})

test_that("complete_vacc_activities adds appropriate coverage information for several activities", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000:2002, age_first = 0,
                                 age_last = 0, coverage = c(NA, 0.5,0.5),
                                 doses = c(50, 50, NA),
                                 targeting = "random")

    vacc <- complete_vacc_activities(vacc = vacc, pop = pop)

    expect_equal(rep(0.5, 3), vacc$coverage)
    expect_equal(rep(50, 3), vacc$doses)
})

test_that("complete_vacc_activities fails conflicting coverage/doses information for several activities", {
    
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2002,
                          age_min = 0, age_max = 5)
    pop$pop_size <- 100

    vacc <- popim_vacc_activities(region = "UK", year = 2000:2002, age_first = 0,
                                 age_last = 0, coverage = c(NA, 0.8,0.5),
                                 doses = c(50, 50, NA),
                                 targeting = "random")

    expect_error(vacc <- complete_vacc_activities(vacc = vacc, pop = pop))
})
