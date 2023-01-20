test_that("apply_vacc modifies the correct rows", {

    df <- setup_population(2000, 2005, 0, 5)
    df <- apply_vacc(df, year = 2000, age_first = 0, age_last = 0,
                     coverage = 0.2, skew = 0)

    ## first expected value:
    ee1 <- data.frame(year = 2001:2005, age = 1:5, cohort = 2000, immunity = 0.2)

    expect_true(all(df[df$immunity == 0.2,] == ee1))

    ## adding a previous campaign on top:
    df <- apply_vacc(df, year = 1995, age_first = 0, age_last = 5,
                     coverage = 0.5, skew = 0)

    ee2 <- list(year = 2000, age = 5, cohort = 1995, immunity = 0.5)

    expect_true(all(df[df$immunity == 0.5,] == ee2))
    expect_true(all(df[df$immunity == 0.2,] == ee1)) ## no change to
                                                       ## the other
                                                       ## vaccinated
                                                       ## cohorts

})
