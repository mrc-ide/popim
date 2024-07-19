
## get some data from the unwpp api at url
base_url <- "http://population.un.org/dataportalapi/api/v1"
target <- paste0(base_url,
                 "/data/indicators/47/locations/566/start/1995/end/2000")
## js <- jsonlite::fromJSON(target)
## indicator 47: population by 1 year age groups and by sex"
## location 566: Nigeria

## copy json data from website and save as file "inst/extdata/2000.json".

## js <- jsonlite::fromJSON("inst/extdata/2000.json")
## js_2 <- jsonlite::fromJSON("inst/extdata/2000_2.json")
## js_3 <- jsonlite::fromJSON("inst/extdata/2000_3.json")
## js_4 <- jsonlite::fromJSON("inst/extdata/2000_4.json")

## ## the data is in js$data:
## names(js$data)

## df <- rbind(js$data, js_2$data, js_3$data, js_4$data) |>
##     dplyr::select(location, iso3, timeLabel, sex, ageLabel, value) |>
##     dplyr::filter(sex == "Both sexes") |> dplyr::select(-sex) |>
##     dplyr::rename(region = iso3, year = timeLabel, age = ageLabel,
##                   pop_size = value) |>
##     dplyr::mutate(age = dplyr::case_match(age, "100+" ~ "100",
##                                           .default = age)) |>
##     dplyr::mutate(year = as.numeric(year), age = as.numeric(age))

## df <- as_popim_pop(df)


## library(jsonlite)
## library(httr)

## base_url <- "https://population.un.org/dataportalapi/api/v1"
## target <- paste0(base_url, "/indicators/")
## target <- paste0(base_url, "/locations/")

## response <- fromJSON(target)

## data1 <- fromJSON("https://api.github.com/users/hadley/orgs")
## ## fromJSON works with an appropriate url.

## ## URL for a server ssl protocol scan:
## ## https://www.ssllabs.com/ssltest/
## ## https://www.ssllabs.com/ssltest/analyze.html?d=population.un.org

## ## maybe this is to do with the openssl version I've got?
## ## in the shell: openssl version
