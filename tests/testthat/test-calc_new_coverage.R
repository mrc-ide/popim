test_that("calc_new_coverage combines two coverages correctly", {
    expect_equal(calc_new_coverage(0.5, 0.5, "random"), 0.75)
    expect_equal(calc_new_coverage(0.3, 0.5, "correlated"), 0.5)
    expect_equal(calc_new_coverage(0.3, 0.5, "targeted"), 0.8)
})

test_that("calc_new_coverage works for an input vector of prev_immunity", {
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), target = "targeted"),
                 c(0.8, 1, 1))
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), target = "random"),
                 c(0.65, 0.75, 0.9))
    expect_equal(calc_new_coverage(0.5, c(0.3, 0.5, 0.8), target = "correlated"),
                 c(0.5, 0.5, 0.8))
})

test_that("calc_new_coverage fails for undefined target value", {
    expect_error(calc_new_coverage(0.5, 0.5, -0.5))
    expect_error(calc_new_coverage(0.5, 0.5, "blue"))
})

test_that("calc_new_coverage fails for coverage values outside of (0,1)", {
    expect_error(calc_new_coverage(0.5, 1.2, "random"))
    expect_error(calc_new_coverage(-0.3, 0.75, "random"))
})

test_that("calc_new_coverage fails for non_skalar input coverage", {
    expect_error(calc_new_coverage(c(0.5, 0.5), 0.3, "random"))
})

test_that("calc_new_coverage fails for matrix or data.frame input as prev_immunity", {
    expect_error(calc_new_coverage(0.5, matrix(0, nrow = 3, ncol = 3),
                                   "random"))
    expect_error(calc_new_coverage(0.5, data.frame(year = 2000:2002,
                                                   age = 0,
                                                   immunity = 0), "random"))
})
