test_that("popim_population generates a df that passes the is_population check", {
    expect_true(is_population(popim_population("UK", 2000, 2005)))
    expect_true(is_population(popim_population("UK", 0, 100, 5, 10)))
})

test_that("is_population rejects df with missing columns", {

    df <- popim_population("UK", 2000, 2010)
    df$cohort <- NULL

    expect_false(is_population(df))
})

test_that("is_population rejects df with incorrectly typed columns", {

    df <- popim_population("UK", 2000, 2010, 0, 10)
    df$year <- "a"

    expect_false(is_population(df))
})

test_that("is_population rejects df with negative ages", {

    df <- popim_population("UK", 2000, 2005, 0, 5)
    df$age <- df$age - 2

    expect_false(is_population(df))
})

test_that("is_population rejects df with immunity outside (0, 1)", {

    df <- popim_population("UK", 2000, 2005, 0, 5)
    df$immunity[5] <- -1

    expect_false(is_population(df))

    df <- popim_population("UK", 2000, 2005, 0, 5)
    df$immunity[5] <- 1.3
    expect_false(is_population(df))
})

test_that("is_population rejects when attributes are missing", {
    df <- popim_population("UK", 2000, 2003, 0, 2)

    attr(df, "region") <- NULL
    expect_false(is_population(df))

})
        
        
