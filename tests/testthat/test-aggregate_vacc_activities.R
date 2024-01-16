test_that("aggregate_vacc_activities aggregates correctly", {
    ## returns the same if there isn't anything to aggregate:
    v <- new_vacc_activities(region = "UK", year = 2000,
                             age_first = 0, age_last = 0,
                             coverage = 0.5, doses = 100, targeting = "random")
    expect_equal(v, aggregate_vacc_activities(v))

   
    v <- new_vacc_activities(region = "UK", year = c(2000, 2002),
                             age_first = 0, age_last = c(0, 10),
                             coverage = 0.5, doses = 100, targeting = "random")
    expect_equal(v, aggregate_vacc_activities(v))

    ## aggregate a single simple campaign
    v <- new_vacc_activities(region = "UK", year = 2000,
                             age_first = 0:5, age_last = 0:5,
                             coverage = 0.5, doses = 100, targeting = "random")
    v1 <- new_vacc_activities(region = "UK", year = 2000,
                             age_first = 0, age_last = 5,
                             coverage = 0.5, doses = 600, targeting = "random")
    expect_equal(v1, aggregate_vacc_activities(v))

    ## a couple of different campaigns:
    v <- new_vacc_activities(region = "UK", year = 2000,
                             age_first = 0:5, age_last = 0:5,
                             coverage = c(0.5, rep(0.7, 5)), doses = 100,
                             targeting = "random")
    v1 <- new_vacc_activities(region = "UK", year = 2000,
                              age_first = c(0, 1), age_last = c(0, 5),
                              coverage = c(0.5, 0.7), doses = c(100, 500),
                              targeting = "random")
    expect_equal(v1, aggregate_vacc_activities(v))

})
