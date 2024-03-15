test_that("apply_vaccs correctly uses apply_vacc for a single vacc_activity", {
    ## get a population object
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2010,
                          age_min = 0, age_max = 10)
    ## get a vaccine object
    vacc <- popim_vacc_activities(region = "UK", year = 2001,
                                age_first = 0, age_last = 0,
                                coverage = 0.5, doses = NA, targeting = "random")

    ## applying a single vacc activity - does apply_vaccs give the
    ## same as apply_vacc?
    pop_sing <- apply_vacc(pop = pop, region = "UK", year = vacc$year,
                           age_first = vacc$age_first,
                           age_last = vacc$age_last,
                           coverage = vacc$coverage,
                           doses = NA,
                           targeting = vacc$targeting)
    pop_multi <- apply_vaccs(pop = pop, vaccs_df = vacc)
    
    expect_equal(pop_sing, pop_multi)
})
test_that("apply_vaccs gives the same result as apply_vacc used twice for two activities", {
    ## get a population object
    pop <- popim_population(region = "UK", year_min = 2000, year_max = 2010,
                          age_min = 0, age_max = 10)
    ## get a vaccine object
    vacc <- popim_vacc_activities(region = "UK", year = c(2001, 2003),
                                age_first = 0, age_last = 0,
                                coverage = 0.5, doses = NA, targeting = "random")

    pop_sing <- apply_vacc(pop = pop, region = "UK", year = vacc$year[1],
                           age_first = vacc$age_first[1],
                           age_last = vacc$age_last[1],
                           coverage = vacc$coverage[1],
                           doses = vacc$doses[1],
                           targeting = vacc$targeting[1])
    pop_sing <- apply_vacc(pop = pop_sing, region = "UK", year = vacc$year[2],
                           age_first = vacc$age_first[2],
                           age_last = vacc$age_last[2],
                           coverage = vacc$coverage[2],
                           doses = vacc$doses[2],
                           targeting = vacc$targeting[2])
    pop_multi <- apply_vaccs(pop = pop, vaccs_df = vacc)
    
    expect_equal(pop_sing, pop_multi)
   })
