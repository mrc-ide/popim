library(dplyr)
library(tidyr)

indir <- "inst/extdata/data-raw/"
outdir <- "inst/extdata/"

dat <- read.csv(
    paste0(indir, "vaccinationCampaigns_GAVI impact_Nov2013 - Copy.csv"),
    stringsAsFactors = FALSE)


out <- dat |> filter(country.code == "NGA", scenario == "base") |>
    group_by(vac.id) |>
    mutate(doses = as.numeric(doses)) |> 
    summarise(year = mean(year), doses = mean(doses)) |>
    select(!vac.id) |>
    mutate(region = "NGA", targeting = "random", age_first = 0, age_last = 100,
           coverage = NA) |>
    relocate(region, year, age_first, age_last, coverage, doses, targeting) |>
    arrange(year) |>
    filter(doses > 1e6)

write.csv(out, paste0(outdir, "vacc_campaigns_NGA.csv"), row.names = FALSE)



dat2 <- read.csv(
    paste0(indir, "vaccination_coverage_by_year_country_GAVINov13A_skew0.csv"),
    stringsAsFactors = FALSE)

dat2 <- dat2 |> rename(region = X) |>
    pivot_longer(!region, names_to = "year", values_to = "coverage") |>
    filter(region == "NGA") |>
    mutate(year = gsub("X", "", year) |> as.numeric()) |>
    mutate(age_first = 0, age_last = 0, doses = NA, targeting = "random") |>
    filter(year >= 1990, year <= 2013) |>
    relocate(region, year, age_first, age_last, coverage, doses, targeting) |>
    arrange(year)

write.csv(dat2, paste0(outdir, "vacc_routine_NGA.csv"), row.names = FALSE)

