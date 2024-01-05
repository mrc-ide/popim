library(dplyr)

dat <- read.csv("vaccinationCampaigns_GAVI impact_Nov2013 - Copy.csv", stringsAsFactors = FALSE)


out <- dat |> filter(country.code == "NGA", scenario == "base") |>
    group_by(vac.id) |>
    mutate(doses = as.numeric(doses)) |> 
    summarise(year = mean(year), doses = mean(doses)) |>
    select(!vac.id) |>
    mutate(region = "NGA", targeting = "random", age_first = 0, age_last = 100,
           coverage = NA) |>
    relocate(region, year, age_first, age_last, coverage, doses, targeting) |>
    arrange(year)

write.csv(out, "../vacc_campaigns_NGA.csv", row.names = FALSE)

