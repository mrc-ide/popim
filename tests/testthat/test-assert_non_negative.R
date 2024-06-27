test_that("non-negative or missing arguments pass", {
    x <- 0:2
    expect_no_error(assert_non_negative(x))
    x <- c(0, 1, NA)
    expect_no_error(assert_non_negative(x))
    x <- c(NA_real_, NA_real_, NA_real_)
    expect_no_error(assert_non_negative(x))
})

test_that("arguments with negative entries fail", {
    x <- -1:1
    expect_error(assert_non_negative(x))
    x <- c(-1, NA)
    expect_error(assert_non_negative(x))    
})
