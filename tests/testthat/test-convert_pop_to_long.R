test_that("convert_pop_to_long fails for invalid inputs", {

    my_regions <- c("FRA", "UK")
    year_min = 2001
    year_max = 2003
    age_min = 0
    age_max = 3

    pl <- vip_population(region = my_regions,
                         year_min = year_min, year_max = year_max,
                         age_min = age_min, age_max = age_max)
    expect_error(convert_pop_to_long(pl))


    pw <- vip_pop_wide(region = my_regions,
                       year_min = year_min, year_max = year_max,
                       age_min = age_min, age_max = age_max)

    class(pw) <- "data.frame" ## stripping the vip_population class attribute

    expect_error(convert_pop_to_long(pw))
})

test_that("convert_pop_to_long converts a vip_pop_wide initialised to 0 correctly", {

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

    expect_equal(pl, convert_pop_to_long(pw)) 
})

test_that("successive conversion from long to wide to long give the original", {

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
    pl_out <- convert_pop_to_long(pw)

    expect_equal(pl, pl_out)
})
