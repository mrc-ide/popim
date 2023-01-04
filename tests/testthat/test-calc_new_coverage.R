test_that("calc_new_coverage combines two coverages", {
  expect_equal(calc_new_coverage(0.5,0.5,0), 0.75)
})
