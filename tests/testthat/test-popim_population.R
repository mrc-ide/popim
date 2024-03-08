test_that("popim_population returns a dataframe of expected dimensions", {

    pop <- popim_population("UK", 2000, 2005)
    expect_equal(dim(pop), c(606, 6))
})

test_that("popim_population fails for invalid inputs", {
    expect_error(popim_population(2000, 2005))
    expect_error(popim_population("UK", 18, 10, 0, 10))
    expect_error(popim_population("UK", 2000:2005, 0, 100))
    expect_error(popim_population("UK", 2000, 2005, -1, 5))
})
