test_that("new_vacc_activities generates a valid popim_vacc_activities object for valid inputs", {
    
    x <- new_vacc_activities(region = "UK", year = 2000,
                             age_first = 0, age_last = 5,
                             coverage = 0.5, doses = NA, targeting = "random")

    expect_equal(nrow(x), 1)
    expect_no_error(validate_vacc_activities(x))

    x <- new_vacc_activities(region = "UK", year = 2000:2005,
                             age_first = 0, age_last = 5,
                             coverage = 0.5, doses = NA, targeting = "random")
    ## dodgy generation - but data.frame allows this, therefore
    ## new_vacc_activities does, too.

    expect_equal(nrow(x), 6)
    expect_no_error(validate_vacc_activities(x))
})


test_that("new_vacc_activities fails for dodgy inputs", {
    
    expect_error(new_vacc_activities(region = "UK", year = 2000:2004,
                                     age_first = 0, age_last = c(5, 10),
                                     coverage = c(0, 0.7), doses = NA,
                                     targeting = "random"))
    ## data.frame() doesn't allow this, so failure.

})
