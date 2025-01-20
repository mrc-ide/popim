## File downloaded on 20 Jan 2025 from
## "https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=CSV%20format"
dat <- readr::read_csv("data-raw/WPP2024_TotalPopulationBySex.csv.gz", na = c(""))
## need to specify na.strings as "NA" is the ISO2 code for Namibia,
## which I don't want to be interpreted as NA.

regions <- dat |> dplyr::select(ISO3_code:Location) |> unique()

## What countries are available?
unwpp_countries <- regions |> dplyr::filter(LocTypeID == 4) |>
    dplyr::select(ISO3_code, ISO2_code, SDMX_code, Location) |>
    dplyr::arrange(SDMX_code) |>
    dplyr::relocate(Location)

usethis::use_data(unwpp_countries, overwrite = TRUE)
