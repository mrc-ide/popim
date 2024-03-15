test_that("as_popim_pop successfully converts a good dataframe", {

    df <- expand.grid(region = c("UK", "FRA"), age = 0:5, year = 2000:2002,
                      stringsAsFactors = FALSE)
    df$pop_size <- 0

    expect_no_error(pop <- as_popim_pop(df))
    expect_true(is_population(pop))

})

test_that("as_popim_pop fails or warns when given a bad dataframe", {

    ## df with missing column:
    df <- expand.grid(region = c("UK", "FRA"), age = 0:5,
                      stringsAsFactors = FALSE)
    df$pop_size <- 0

    expect_error(as_popim_pop(df))


    ## df with different time scope for different regions:
    df1 <- expand.grid(region = "UK", age = 0:5, year = 2000:2002,
                       stringsAsFactors = FALSE)
    df2 <- expand.grid(region = "FRA", age = 0:5, year = 2000:2003,
                       stringsAsFactors = FALSE)
    df <- rbind(df1, df2)
    df$pop_size <- 0

    expect_warning(pop <- as_popim_pop(df))


    ## df with duplicated rows:
    df <- expand.grid(region = c("UK", "UK", "FRA"), age = 0:5, year = 2000:2002,
                      stringsAsFactors = FALSE)
    df$pop_size <- 1:nrow(df)
    expect_error(pop <- as_popim_pop(df))

})
