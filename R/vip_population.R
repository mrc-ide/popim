##' Constructor of an object of the "vip_population" class
##'
##' The "vip_population" object is a dataframe used to track
##' vaccination coverage by age through time for a single
##' location. The dataframe has at least the 5 columns region, year,
##' age, cohort and immunity.
##'
##' This function generates a such a dataframe.  Each age and year is
##' tracked in a row and the proportion of the population that is
##' immune is recorded. Immunity is initialised as 0 throughout by
##' this function, and pop_size to NA_real_.
##' 
##' @param region character vector, list of regions considered.
##' @param year_min integer, first year to be considered.
##' @param year_max integer, last year to be considered.
##' @param age_min integer, youngest age to be considered, defaults to
##'     0.
##' @param age_max integer, oldest age to be considered, defaults to
##'     100.
##' @return S3 object of class "vip_population": a dataframe with
##'     columns region, year, age, cohort, immunity and pop_size,
##'     where year and age cover the ranges given by the input
##'     parameters. Cohort = year - age and gives the year of
##'     birth. It is redundant but included for ease of
##'     handling. Immunity and pop_size are initialised as 0
##'     throughout the whole population.
##' @export
##' @author Tini Garske
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
                      age = age_min:age_max, stringsAsFactors = FALSE)
    attr(df, "out.attrs") <- NULL ## removing the spurious attributes
                                  ## set by expand.grid

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

##' Reading population data from a .csv file
##'
##' Reads a population data from a .csv file, checks if
##' the data fulfils the requirements for a "vip_population"
##' object, and if so returns this object.
##'
##' The requirements are that the data contain the columns "region",
##' "age", "year". Columns "pop_size" and "immunity" are optional. The
##' columns will be coerced to character, integer, integer, numeric,
##' double, respectively.  If the column "immunity" exists, it will be
##' used to initialise the immunity data, if it doesn't exist,
##' immunity will be initialised to zero. If colunn pop_size doesn't
##' exist, it will be initialised to NA.
##'
##' 0 <= age
##' 0 <= immunity <= 1
##' 0 <= pop_size
##'
##' @param file Name of the file from which the population data are to
##'     be read. If it does not contain an absolute path, the file
##'     name is relative to the current working directory.
##' @return object of class "vip_population", a dataframe with one row
##'     per birth cohort/year/region, with columns "region", "year",
##'     "age", "cohort", "immunity", "pop_size".
##' @author Tini Garske
##' @export
read_population <- function(file) {

    ## assert_file_exists(file)
    if(!file.exists(file))
        stop(sprintf("%s does not exist", file), call. = FALSE)

    df <- utils::read.csv(file, stringsAsFactors = FALSE)

    pop <- convert_df_to_pop(df)
    pop
}

##' Convert a dataframe to a vip_population object
##'
##' `convert_df_to_pop` converts a suitable dataframe to a vip
##' population object and returns this object.
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
##' @return an object of class vip_populaiton
##' @author Tini Garske
convert_df_to_pop <- function(df) {

    assert_column_exists(df, "region")
    assert_column_exists(df, "age")
    assert_column_exists(df, "year")
    assert_column_exists(df, "pop_size")

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
    out <- vip_population(region, year_min, year_max, age_min, age_max) |>
        dplyr::select(!tidyselect::any_of(c("pop_size", "immunity"))) |>
        dplyr::left_join(df, by = c("region", "year", "age"))

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
