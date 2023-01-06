test_that("calc_new_coverage combines two coverages", {
    expect_equal(calc_new_coverage(0.5,0.5), 0.75)
    expect_equal(calc_new_coverage(0.3, 0.5, 1), 0.5)
    expect_equal(calc_new_coverage(0.3, 0.5, -1), 0.8)
})

test_that("calc_new_coverage fails for undefined skew value", {
    expect_error(calc_new_coverage(0.5, 0.5, -0.5))
})

test_that("calc_new_coverage fails for coverage values outside of (0,1)", {
    expect_error(calc_new_coverage(0.5, 1.2, 0))
})

test_that("calc_new_coverage fails for non_skalar input coverage", {
    expect_error(calc_new_coverage(c(0.5, 0.5), 0))
})
