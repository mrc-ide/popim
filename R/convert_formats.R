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

    ## the arrays containing the immunity and population data are
    ## initialised to NA.
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

## adds dependencies on dplyr and tidyr.
convert_pop_to_wide <- function(pop_long) {

    assert_population(pop_long)

    ## setting up the structure:
    pop_wide <- vip_pop_wide(region = attributes(pop_long)$region,
                             year_min = attributes(pop_long)$year_min,
                             year_max = attributes(pop_long)$year_max,
                             age_min = attributes(pop_long)$age_min,
                             age_max = attributes(pop_long)$age_max)

    ## filling the vacc_data array:
    for(reg in attributes(pop_long)$region) {

        pop_wide$vacc_data[reg,,] <-
        pop_long |> dplyr::filter(region == reg) |>
            dplyr::select(year, age, immunity) |>
            tidyr::pivot_wider(names_from = age, values_from = immunity) |>
            dplyr::arrange(year) |>
            dplyr::select(-year) |> as.matrix()

    }
    ## filling in the pop_data array if pop_long has a pop_size column:
    if("pop_size" %in% names(pop_long)) {
        for(reg in attributes(pop_long)$region) {
            pop_wide$pop_data[reg,,] <-
                pop_long |> dplyr::filter(region == reg) |>
                dplyr::select(year, age, pop_size) |>
                tidyr::pivot_wider(names_from = age, values_from = pop_size) |>
                dplyr::arrange(year) |>
                dplyr::select(-year) |> as.matrix()
        }
    }
    pop_wide
}
