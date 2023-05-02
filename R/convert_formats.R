## convert the population structure between Keith's wide and my long format

## constructor for an empty population in the wide format:
## this is exactly Keith's format, including the idiosynchratic names.
vip_pop_wide <- function(region = character(),
                         year_min = integer(), year_max = integer(),
                         age_min = 0, age_max = 100) {

    assert_character(region)
    
    assert_scalar_wholenumber(year_min)
    assert_scalar_wholenumber(year_max)
    assert_non_negative(year_max - year_min)

    assert_scalar_wholenumber(age_min)
    assert_scalar_wholenumber(age_max)
    assert_non_negative(age_min)
    assert_non_negative(age_max - age_min)

    age <- age_min:age_max
    year <- year_min:year_max

    my_dims <- c(length(region), length(year), length(age))
    my_dimnames <- list(region = region, year = year, age = age)

    vacc_data <- array(data = rep(NA, length(region)*length(year)*length(age)),
                       dim = my_dims, dimnames = my_dimnames)
    pop_data <- array(data = rep(NA, length(region)*length(year)*length(age)),
                      dim = my_dims, dimnames = my_dimnames)
                       
    pop_wide <- list(region_labels = region,
                     years_labels = year,
                     age_labels = age,
                     vacc_data = vacc_data,
                     pop_data = pop_data)

    class(pop_wide) <- "vip_population_wide"

    pop_wide
}

## convert_pop_to_wide <- function(pop_long) {

## }
