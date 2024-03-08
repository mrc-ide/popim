test_that("apply_vacc modifies the correct rows", {

    df <- popim_population("UK", 2000, 2005, 0, 5)
    df <- apply_vacc(df, region = "UK", year = 2000,
                     age_first = 0, age_last = 0,
                     coverage = 0.2, doses = NA, targeting = "random")

    ## first expected value:
    ee1 <- data.frame(region = "UK", year = 2001:2005, age = 1:5, cohort = 2000,
                      immunity = 0.2, pop_size = NA_real_)

    expect_equal(df[df$immunity > 0,], ee1, ignore_attr = TRUE)

    ## adding a previous campaign on top:
    df <- apply_vacc(df, region = "UK", year = 1996,
                     age_first = 0, age_last = 5,
                     coverage = 0.5, doses = NA, targeting = "random")

    ee2 <- data.frame(region = "UK",
                      year = c(2000, 2000, 2001), age = c(4, 5, 5),
                      cohort = c(1996, 1995, 1996),
                      immunity = 0.5, pop_size = NA_real_)

    expect_equal(df[df$immunity == 0.5,], ee2, ignore_attr = TRUE)
    ## no change to the other vaccinated cohorts:
    expect_equal(df[df$immunity == 0.2,], ee1, ignore_attr = TRUE)

})
