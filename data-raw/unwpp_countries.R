dat <- read.csv("inst/extdata/hide/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv",
                stringsAsFactors = FALSE, na.strings = "<NA>",
                fileEncoding = "UTF-8", encoding = "UTF-8")
## need to specify na.strings as "NA" is the ISO2 code for Namibia,
## which I don't want to be interpreted as NA.

regions <- dat |> dplyr::select(ISO3_code:Location) |> unique()

## What countries are available?
unwpp_countries <- regions |> dplyr::filter(LocTypeID == 4) |>
    dplyr::select(ISO3_code, ISO2_code, SDMX_code, Location) |>
    dplyr::arrange(SDMX_code) |>
    dplyr::relocate(Location)

usethis::use_data(unwpp_countries, overwrite = TRUE)
