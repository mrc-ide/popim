test_that("get_all_ages works with valid inputs", {
    ## single age 
    vaccs <- new_vacc_activities(region = "UK", year = 2000,
                                 age_first = 0, age_last = 0,
                                 coverage = 1, doses = NA, targeting = "random")
    expect_equal(get_all_ages(vaccs), 0)

    ## single age range
    vaccs <- new_vacc_activities(region = "UK", year = 2000,
                                 age_first = 0, age_last = 9,
                                 coverage = 1, doses = NA, targeting = "random")
    expect_equal(get_all_ages(vaccs), 0:9)

    ## several overlapping and disjoint age ranges
    vaccs <- new_vacc_activities(region = "UK", year = 2000,
                                 age_first = c(0, 2, 7), age_last = c(3, 5, 10),
                                 coverage = 1, doses = NA, targeting = "random")
    expect_equal(get_all_ages(vaccs), sort(c(0:3, 2:5, 7:10)))
    
})
