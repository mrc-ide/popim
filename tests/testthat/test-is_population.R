test_that("vip_population generates a df that passes the is_population check", {
    expect_equal(is_population(vip_population(2000, 2005)), TRUE)
    expect_equal(is_population(vip_population(0, 100, 5, 10)), TRUE)
})

test_that("is_population rejects df with missing columns", {

    df <- vip_population(2000, 2010)
    df$cohort <- NULL

    expect_false(is_population(df))
})

test_that("is_population rejects df with incorrectly typed columns", {

    df <- vip_population(2000, 2010, 0, 10)
    df$year <- "a"

    expect_false(is_population(df))
})

test_that("is_population rejects df with negative ages", {

    df <- vip_population(2000, 2005, 0, 5)
    df$age <- df$age - 2

    expect_false(is_population(df))
})

test_that("is_population rejects df with immunity outside (0, 1)", {

    df <- vip_population(2000, 2005, 0, 5)
    df$immunity[5] <- -1

    expect_false(is_population(df))

    df <- vip_population(2000, 2005, 0, 5)
    df$immunity[5] <- 1.3
    expect_false(is_population(df))
})
        
        
