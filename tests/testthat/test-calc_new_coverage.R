test_that("calc_new_coverage combines two coverages correctly", {
    expect_equal(calc_new_coverage(0.5, 0.5), 0.75)
    expect_equal(calc_new_coverage(0.3, 0.5, 1), 0.5)
    expect_equal(calc_new_coverage(0.3, 0.5, -1), 0.8)
})

test_that("calc_new_coverage works for an input vector of prev_immunity", {
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), skew = -1),
                 c(0.8, 1, 1))
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), skew = 0),
                 c(0.65, 0.75, 0.9))
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), skew = 1),
                 c(0.5, 0.5, 0.8))
})

test_that("calc_new_coverage fails for undefined skew value", {
    expect_error(calc_new_coverage(0.5, 0.5, -0.5))
})

test_that("calc_new_coverage fails for coverage values outside of (0,1)", {
    expect_error(calc_new_coverage(0.5, 1.2, 0))
    expect_error(calc_new_coverage(-0.3, 0.75, 0))
})

test_that("calc_new_coverage fails for non_skalar input coverage", {
    expect_error(calc_new_coverage(c(0.5, 0.5), 0.3, 0))
})

test_that("calc_new_coverage fails for matrix or data.frame input as prev_immunity", {
    expect_error(calc_new_coverage(0.5, matrix(0, nrow = 3, ncol = 3)))
    expect_error(calc_new_coverage(0.5, data.frame(year = 2000:2002,
                                                   age = 0,
                                                   immunity = 0)))
})
