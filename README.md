
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vip

<!-- badges: start -->

<!-- badges: end -->

Vaccine Immunity Propagation

The goal of vip is to …

This repository contains functions to run a demographic model of vaccine
exposure over time, tracking the vaccine-derived immunity by age through
time, following implementation of one or more vaccination activities
that can target the population as a whole or selectively target specific
age groups.

## Installation

You can install the development version of vip from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/vip")
```

## Example

The population is the central object, a dataframe with at least the
columns year, age, cohort (the year of birth for the individuals tracked
in this row), and immunity (as a proportion of this cohort). Year, age
and cohort are tracked as annual time steps and age groups.

A basic, totally naive population dataframe can be setup using the
function “setup\_population”:

``` r
library(vip)

pop <- setup_population(year_min = 2000, year_max = 2010,
                        age_min = 0, age_max = 10)
```

Next we apply a number of vaccination activities to this population.
Note that the first one takes place before we started tracking the
population - but the effects to individuals vaccinated in this initial
campaign is still tracked onwards through time.

``` r
library(dplyr) ## needed for the pipe %>%
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

pop <- pop %>%
    apply_vacc(year = 1998, age_first = 0, age_last = 10, coverage = 0.5) %>%
    apply_vacc(year = 2005, age_first = 0, age_last = 0, coverage = 0.5) %>%
    apply_vacc(year = 2006, age_first = 0, age_last = 0, coverage = 0.6) %>%
    apply_vacc(year = 2007, age_first = 0, age_last = 0, coverage = 0.7) %>%
    apply_vacc(year = 2008, age_first = 0, age_last = 0, coverage = 0.8) %>%
    apply_vacc(year = 2009, age_first = 0, age_last = 0, coverage = 0.9) %>%
    apply_vacc(year = 2010, age_first = 0, age_last = 0, coverage = 0.95)
```

The vaccine-derived immunity of the population can be visualised with
the function “plot\_population()”.

``` r
plot_population(pop)
```

<img src="man/figures/README-plot_population-1.png" width="100%" />

<!-- You'll still need to render `README.Rmd` regularly, to keep -->

<!-- `README.md` up-to-date. `devtools::build_readme()` is handy for -->

<!-- this. You could also use GitHub Actions to re-render `README.Rmd` -->

<!-- every time you push. An example workflow can be found here: -->

<!-- <https://github.com/r-lib/actions/tree/v1/examples>. -->

<!-- You can also embed plots, for example: -->

<!-- ```{r pressure, echo = FALSE} -->

<!-- plot(pressure) -->

<!-- ``` -->

<!-- In that case, don't forget to commit and push the resulting figure -->

<!-- files, so they display on GitHub and CRAN. -->
