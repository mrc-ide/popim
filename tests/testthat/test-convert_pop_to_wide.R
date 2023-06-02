test_that("convert_pop_to_wide fails for invalid inputs", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pw <- vip_pop_wide(region = my_regions,
                       year_min = year_min, year_max = year_max,
                       age_min = age_min, age_max = age_max)
    expect_error(convert_pop_to_wide(pw))

    pl <- vip_population(region = my_regions,
                         year_min = year_min, year_max = year_max,
                         age_min = age_min, age_max = age_max)
    class(pl) <- "data.frame" ## stripping the vip_population class attribute

    expect_error(convert_pop_to_wide(pl))
})

test_that("convert_pop_to_wide converts a vip_population initialised to 0 correctly", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- vip_population(region = my_regions,
                         year_min = year_min, year_max = year_max,
                         age_min = age_min, age_max = age_max)

    pw <- vip_pop_wide(region = my_regions,
                       year_min = year_min, year_max = year_max,
                       age_min = age_min, age_max = age_max)

    expect_equal(pw, convert_pop_to_wide(pl)) 
})

test_that("convert_pop_to_wide converts a vip_population with non-zero data correctly", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- vip_population(region = my_regions,
                         year_min = year_min, year_max = year_max,
                         age_min = age_min, age_max = age_max)

    pl$immunity <- (1:length(pl$region))/100
    pl$pop_size <- 1:length(pl$region)

    pw <- convert_pop_to_wide(pl)

    for(test_region in my_regions)
        for(test_year in year_min:year_max)
            for(test_age in age_min:age_max) {
                expect_equal(pl$immunity[pl$region == test_region &
                                         pl$year == test_year &
                                         pl$age == test_age],
                             pw$vacc_data[test_region,
                                          as.character(test_year),
                                          as.character(test_age)])
                expect_equal(pl$pop_size[pl$region == test_region &
                                         pl$year == test_year &
                                         pl$age == test_age],
                             pw$pop_data[test_region,
                                          as.character(test_year),
                                          as.character(test_age)])
    }
})
