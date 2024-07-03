
## get some data from the unwpp api at url
"https://population.un.org/dataportalapi/api/v1/data/indicators/47/locations/566/start/1995/end/2000
## indicator 47: population by 1 year age groups and by sex"
## location 566: Nigeria

## copy json data from website and save as file "inst/extdata/2000.json".

js <- jsonlite::fromJSON("inst/extdata/2000.json")
js_2 <- jsonlite::fromJSON("inst/extdata/2000_2.json")
js_3 <- jsonlite::fromJSON("inst/extdata/2000_3.json")
js_4 <- jsonlite::fromJSON("inst/extdata/2000_4.json")

## the data is in js$data:
names(js$data)

df <- rbind(js$data, js_2$data, js_3$data, js_4$data) |>
    dplyr::select(location, iso3, timeLabel, sex, ageLabel, value) |>
    dplyr::filter(sex == "Both sexes") |> dplyr::select(-sex) |>
    dplyr::rename(region = iso3, year = timeLabel, age = ageLabel,
                  pop_size = value) |>
    dplyr::mutate(age = dplyr::case_match(age, "100+" ~ "100",
                                          .default = age)) |>
    dplyr::mutate(year = as.numeric(year), age = as.numeric(age))

df <- as_popim_pop(df)
