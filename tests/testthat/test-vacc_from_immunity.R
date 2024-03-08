test_that("vacc_from_immunity returns the correct vacc_activities for random targeting", {
    
    pop0 <- popim_population(region = "UK", year_min = 2000, year_max = 2005,
                          age_min = 0, age_max = 4)
    pop0$pop_size <- 100
    
    ## single vaccination activity, 1 age group, random targeting,
    ## going into completely naive population:
    vaccs <- new_vacc_activities(region = "UK", year = 2001,
                                 age_first = 0, age_last = 0,
                                 coverage = 0.5, doses = NA,
                                 targeting = "random")
    vaccs <- complete_vacc_activities(vaccs, pop0)

    pop <- apply_vaccs(pop0, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "random", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## same vaccination activity into partially immune population:
    pop <- pop0
    pop$immunity <- 0.5

    pop <- apply_vaccs(pop, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "random", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## a campaign targeting all ages, no pre-existing immunity:
    
    ## setting age_last to the population's (age_max - 1), as
    ## vaccination of the last age group can never be detected: by the
    ## time the immunity is updated (next year), this age group has
    ## aged out of the population.
    vaccs <- new_vacc_activities(region = "UK", year = 2001,
                                 age_first = 0, age_last = 3,
                                 coverage = 0.5, doses = NA,
                                 targeting = "random")
    vaccs <- complete_vacc_activities(vaccs, pop0)
    
    pop <- apply_vaccs(pop0, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "random", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## same campaign, but on pre-existing immunity:
    pop <- pop0
    pop$immunity <- 0.3

    pop <- apply_vaccs(pop, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "random", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## a campaign and routine in the same year:
    vaccs <- new_vacc_activities(region = "UK", year = 2001,
                                 age_first = c(0, 2), age_last = c(0, 3),
                                 coverage = 0.5, doses = NA,
                                 targeting = "random")
    vaccs <- complete_vacc_activities(vaccs, pop0)
    
    pop <- apply_vaccs(pop0, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "random", n_digits = 10)

    expect_equal(va_out, vaccs)

})

test_that("vacc_from_immunity returns the correct vacc_activities for non-random targeting", {

    pop0 <- popim_population(region = "UK", year_min = 2000, year_max = 2005,
                           age_min = 0, age_max = 4)
    pop0$pop_size <- 100
    
    ## single vaccination activity, 1 age group, targeted targeting,
    ## going into completely naive population:
    vaccs <- new_vacc_activities(region = "UK", year = 2001,
                                 age_first = 0, age_last = 0,
                                 coverage = 0.3, doses = NA,
                                 targeting = "targeted")
    vaccs <- complete_vacc_activities(vaccs, pop0)

    pop <- apply_vaccs(pop0, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "targeted", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## same vaccination activity into partially immune population:
    pop <- pop0
    pop$immunity <- 0.3

    pop <- apply_vaccs(pop, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "targeted", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## single vaccination activity, 1 age group, correlated targeting,
    ## going into completely naive population:
    vaccs <- new_vacc_activities(region = "UK", year = 2001,
                                 age_first = 0, age_last = 0,
                                 coverage = 0.3, doses = NA,
                                 targeting = "correlated")
    vaccs <- complete_vacc_activities(vaccs, pop0)

    pop <- apply_vaccs(pop0, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "correlated", n_digits = 10)

    expect_equal(va_out, vaccs)

    ## same vaccination activity into partially immune population:
    pop <- pop0
    pop$immunity <- 0.2

    pop <- apply_vaccs(pop, vaccs)
    va_out <- vacc_from_immunity(pop, targeting = "correlated", n_digits = 10)

    expect_equal(va_out, vaccs)
    
})

test_that("vacc_from_immunity behaves sensibly with no change in immunity", {

    pop0 <- popim_population(region = "UK", year_min = 2000, year_max = 2005,
                           age_min = 0, age_max = 4)
    pop0$pop_size <- 100

    va_out <- vacc_from_immunity(pop0, targeting = "random")

    expect_no_error(validate_vacc_activities(va_out))
    expect_equal(nrow(va_out), 0)
})
