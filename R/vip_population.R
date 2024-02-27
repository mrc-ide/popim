##' Constructor of an object of the "vip_population" class
##'
##' The "vip_population" object is a dataframe that models an
##' age-structured population through time, tracking population size
##' and vaccine-induced immunity in the population.
##'
##' This constructor sets up the population as fully susceptible
##' (i.e., `immunity = 0`, with missing population size (i.e.,
##' `pop_size = NA_real_`) throughout. The parameters passed to the
##' constructor are retained as attributes to the dataframe object.
##'
##' A population with non-missing population size can be read in from
##' a suitable file using [vip_pop_from_file()], while vaccine induced
##' immunity can be generated through applying vaccination activities
##' to the population with [apply_vaccs()].
##' 
##' @param region character vector, list of regions considered.
##' @param year_min integer, first year to be considered.
##' @param year_max integer, last year to be considered.
##' @param age_min integer, youngest age to be considered, defaults to
##'     0. Must be non-negative.
##' @param age_max integer, oldest age to be considered, defaults to
##'     100. Must be non-negative, and >= age_min.
##' @return S3 object of class "vip_population": a dataframe with
##'     columns region, year, age, cohort, immunity and pop_size,
##'     where year and age cover the ranges given by the input
##'     parameters. Cohort = year - age and gives the year of
##'     birth. It is redundant but included for ease of
##'     handling. Immunity and pop_size are initialised as 0 and NA,
##'     respectively, throughout the whole population.
##' @export
##' @author Tini Garske
##' @examples
##' pop <- vip_population(region = "UK", year_min = 2000, year_max = 2010)
##'
##' pop <- vip_population(region = c("FRA", "UK"),
##'                       year_min = 2000, year_max = 2010,
##'                       age_min = 0, age_max = 80)
##'
vip_population <- function(region = character(),
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

    df <- expand.grid(region = region, year = year_min:year_max,
                      age = age_min:age_max,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    ## easier handling via cohorts, defined by birth year:
    df$cohort <- df$year - df$age

    ## starting with a fully susceptible population:
    df$immunity <- 0
    df$pop_size <- NA_real_

    df <- structure(
        df,
        region = region,
        year_min = year_min,
        year_max = year_max,
        age_min = age_min,
        age_max = age_max,
        class = c("vip_population", "data.frame")
    )

    df
}

##' Reading vip_population data from a .csv file
##'
##' Reads a population data from a .csv file, checks if
##' the data fulfils the requirements for a "vip_population"
##' object, and if so returns this object.
##'
##' The requirements are that the data contain the columns `region`,
##' `age`, `year` which will be coerced to character, integer,
##' integer. Columns `pop_size` and `immunity` are optional; they will
##' be coerced to numeric, and if missing will be initialised to 0 and
##' NA, respectively be set to NA and 0, respectively.
##'
##' Limitations for values:
##' * `age` must be non-negative integer
##' * `immunity` must be between 0 and 1 (inclusive)
##' * `pop_size` must be non-negative
##'
##' @param file Name of the .csv file from which the population data
##'     are to be read. If it does not contain an absolute path, the
##'     file name is relative to the current working directory.
##' @return An object of class "vip_population", a dataframe with one row
##'     per birth cohort/year/region, with columns "region", "year",
##'     "age", "cohort", "immunity", "pop_size".
##' @seealso [vip_population()] for details of the S3 class, and
##'     [utils::read.csv()] which handles the reading of the .csv
##'     file.
##' @author Tini Garske
##' @export
vip_pop_from_file <- function(file) {

    ## assert_file_exists(file)
    if(!file.exists(file))
        stop(sprintf("%s does not exist", file), call. = FALSE)

    df <- utils::read.csv(file, stringsAsFactors = FALSE)

    pop <- vip_pop_from_df(df)
    pop
}

##' Generate a "vip_population" object from a dataframe
##'
##' Checks if the dataframe is suitable (i.e., contains appropriate
##' columns and data ranges), and if so converts it to a
##' "vip_population" object and returns this.
##'
##' The input dataframe has to have at least the columns region, age,
##' and year. The output vip_population object is generated via
##' expand.grid to have consecutive year and age ranges that are
##' identical for all regions.
##'
##' If the input dataframe contains a column `pop_size`, this must be
##' numeric and non-negative. If it is missing, this column is
##' generated and initialised to NA.
##'
##' If the input dataframe contains a colum `immunity`, this must be
##' numeric, with values between 0 and 1. If it is missing, this
##' column is generated and initialised to 0.
##'
##' Any further colunms are simply carried over into the vip_population object.
##'
##' @param df a dataframe with at least columns region, age, year and pop_size.
##' @return an object of class vip_population
##' @author Tini Garske
##' @noRd
vip_pop_from_df <- function(df) {

    assert_column_exists(df, "region")
    assert_column_exists(df, "age")
    assert_column_exists(df, "year")

    if(!("pop_size" %in% names(df))) {
        df$pop_size <- NA
    }

    if(!("immunity" %in% names(df))) {
        df$immunity <- 0
    }

    assert_character(df$region)

    assert_wholenumber(df$age)
    assert_non_negative(df$age)

    assert_wholenumber(df$year)

    assert_non_negative(df$pop_size)

    assert_0_to_1(df$immunity)

    ## check for duplicated rows in input dataframe:
    n_dup_rows <- df |>
        dplyr::select(tidyselect::all_of(c("region", "year", "age"))) |>
        duplicated() |> sum()
    if(n_dup_rows > 0)
        stop(sprintf("Input dataframe has %d duplicated rows with respect to columns region, year, age.", n_dup_rows))


    ## establish the extent of the population:
    region <- df$region |> unique()

    age_min <- min(df$age)
    age_max <- max(df$age)

    year_min <- min(df$year)
    year_max <- max(df$year)

    df <- df |> dplyr::relocate(tidyselect::all_of(
                           c("region", "year", "age", "immunity", "pop_size")))


    ## here I'm generating a consecutive population from year_min to
    ## year_max, age_min to age_max in all regions. However, there is
    ## no guarantee that the input data are fully consecutive across
    ## all regions. If they aren't, this will generate missing data in
    ## the pop_size and immunity columns.
    out <- vip_population(region, year_min, year_max, age_min, age_max)
    a <- attributes(out)

    out <- out |>
        dplyr::select(!tidyselect::any_of(c("pop_size", "immunity"))) |>
        dplyr::left_join(df, by = c("region", "year", "age"))
    attributes(out) <- a

    if(nrow(out) > nrow(df)) {
        warning(sprintf("Input dataframe has fewer rows than the vip_population generated from it (%d vs %d). This may be due to non-consecutive years or ages, or different year/age ranges for different regions.", nrow(df), nrow(out)))
    } else if(nrow(out) < nrow(df)) {
        stop(sprintf("Input dataframe has more rows than the vip_population generated from it (%d vs %d).", nrow(df), nrow(out)))
    }

    if(!is_population(out)) {
        stop(sprintf("cannot generate valid vip_population from file '%s'",
                     file))
    }

    out
}
