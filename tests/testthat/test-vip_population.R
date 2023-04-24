test_that("vip_population returns a dataframe of expected dimensions", {

    pop <- vip_population("UK", 2000, 2005)
    expect_equal(dim(pop), c(606, 5))
})

test_that("vip_population fails for invalid inputs", {
    expect_error(vip_population(2000, 2005))
    expect_error(vip_population("UK", 18, 10, 0, 10))
    expect_error(vip_population("UK", 2000:2005, 0, 100))
    expect_error(vip_population("UK", 2000, 2005, -1, 5))
})
