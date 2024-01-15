test_that("get_consecutive_range works well with valid input", {

    ## single age 
    ages <- 0
    expect_equal(get_consecutive_range(ages), list(c(0,0)))

    ## consecutive age range
    ages <- 0:10
    expect_equal(get_consecutive_range(ages), list(c(0, 10)))

    ## age range with gap
    ages <- c(0:10, 15:20)
    expect_equal(get_consecutive_range(ages), list(c(0, 10), c(15, 20)))

    ## some doubled up ages
    ages <- c(0:10, 5:10)
    expect_equal(get_consecutive_range(ages), list(c(0, 10), c(5, 10)))
})
