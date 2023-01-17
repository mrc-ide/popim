test_that("setup_population generates a df that passes the is_population check", {
    expect_equal(is_population(setup_population(2000, 2005)), TRUE)
    expect_equal(is_population(setup_population(0, 100, 5, 10)), TRUE)
})

test_that("is_population rejects df with missing columns", {
    expect_false({
        df <- setup_population(2000, 2010)
        df$cohort <- NULL
        is_population(df)})
})

test_that("is_population rejects df with incorrectly typed columns", {
    expect_false({
        df <- setup_population(2000, 2010, 0, 10)
        df$year <- "a"
        is_population(df)})
})

test_that("is_population rejects df with negative ages", {
    expect_false({
        df <- setup_population(2000, 2005, 0, 5)
        df$age <- df$age - 2
        is_population(df)
    })
})

test_that("is_population rejects df with immunity outside (0, 1)", {
    expect_false({
        df <- setup_population(2000, 2005, 0, 5)
        df$immunity[5] <- -1
        is_population(df)
    })
    expect_false({
        df <- setup_population(2000, 2005, 0, 5)
        df$immunity[5] <- 1.3
        is_population(df)
    })
})
        
        
