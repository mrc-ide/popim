test_that("setup_population returns a dataframe of expected dimensions", {
  expect_equal(dim(setup_population(2000, 2005)), c(606, 4))
})

test_that("setup_population fails for invalid inputs", {
    expect_error(setup_population(18, 10, 0, 10))
    expect_error(setup_population(2000:2005, 0, 100))
    expect_error(setup_population(2000, 2005, -1, 5))
})
