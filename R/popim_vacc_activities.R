##' Constructor of an object of the `popim_vacc_activites` class
##'
##' The `popim_vacc_activities` object is a dataframe that holds
##' information on vaccination activities that are typically meant to
##' be applied to a `popim_population` object.
##'
##' The input parameters are the columns of the `popim_vacc_activities`
##' object to be returned, they should be vectors of the same length,
##' or will be recycled as appropriate by [data.frame()].
##'
##' @param region character vector: specifies the geographic region to
##'     which each vaccination activity is to be administered.
##' @param year integer vector: specifies the year in which each
##'     vaccination activity takes place.
##' @param age_first,age_last non-negative integer vectors: specify
##'     the age range targeted in the vaccination
##'     activity. `age_first` <= `age_last` for all entries.
##' @param coverage numeric vector, 0 <= `coverage` <= 1 for all
##'     entries. Specifies the proportion of the target population to
##'     be immunised.
##' @param doses numeric vector, non-negative. Specifies the number of
##'     doses to be used in each vaccination activity.
##' @param targeting character vector, permissible entries are
##'     "random", "correlated", "targeted". Defines how vaccine is
##'     allocated if there is pre-existing immunity in the population:
##'     For "random" targeting individuals are vaccinated irrespective
##'     of immunity status, so if prior to the vaccination activity
##'     the proportion immune was x, then a proportion x of the
##'     vaccine will be administered to already vaccinated individuals
##'     and therefore be wasted. For "correlated" targeting vaccine is
##'     administered first to those already immune before any
##'     susceptible individuals receive vaccine. This option models
##'     the case of unequal access to vaccination. For "targeted"
##'     targeting, vaccine will be given first to as yet non-immune
##'     individuals. This is the most effective use of vaccine. It may
##'     be realistic in the case of multi-year campaigns targeting
##'     different areas within the geographical region specified.
##' @return S3 object of class `popim_vacc_activities`: a dataframe with
##'     columns `region`, `year`, `age_first`, `age_last`, `coverage`,
##'     `doses`, `targeting`.
##' @author Tini Garske
##' @export
##' @examples
##' # setting up an empty class `popim_vacc_activities` object of the
##' # correct structure:
##' vacc <- popim_vacc_activities()
##'
##' # setting up a couple of specific vaccination activities
##' vacc <- popim_vacc_activities(region = c("UK", "FRA"),
##'                             year = c(2010, 2005),
##'                             age_first = c(0, 0),
##'                             age_last = c(0, 10),
##'                             coverage = c(0.8, 0.5),
##'                             doses = NA,
##'                             targeting = "random")
##'
popim_vacc_activities <- function(region = character(), year = integer(),
                                age_first = integer(), age_last = integer(),
                                coverage = double(), doses = double(),
                                targeting = character()) {

    x <- data.frame(region = region, year = year,
                    age_first = age_first, age_last = age_last,
                    coverage = coverage, doses = doses, targeting = targeting)
    class(x) <- c("popim_vacc_activities", "data.frame")

    x
}

validate_vacc_activities <- function(x, name = deparse(substitute(x))) {
    assert_vacc_activities(x)

    assert_column_exists(x, "region")
    assert_column_exists(x, "year")
    assert_column_exists(x, "age_first")
    assert_column_exists(x, "age_last")
    assert_column_exists(x, "coverage")
    assert_column_exists(x, "doses")
    assert_column_exists(x, "targeting")

    assert_character(x$region)
    assert_wholenumber(x$year)
    assert_wholenumber(x$age_first)
    assert_wholenumber(x$age_last)
    assert_non_negative(x$age_first)
    assert_non_negative(x$age_last - x$age_first)
    assert_0_to_1_or_missing(x$coverage[!is.na(x$coverage)])
    assert_non_negative_or_missing(x$doses)
    assert_character(x$targeting)

    ## assert at most one of coverage and doses is missing
    if(any(is.na(x$coverage) & is.na(x$doses)))
        stop(sprintf("coverage and doses must not both be missing"))

    is_valid_targeting <- function(x) {
        ifelse(x %in% c("random", "correlated", "targeted"), TRUE, FALSE)
    }

    if(!all(vapply(x$targeting, function(tar) is_valid_targeting(tar), TRUE)))
        stop(sprintf("%s has an invalid entry in column 'targeting'", name))

}

##' Read vaccination activities from a .csv file
##'
##' Reads a list of vaccination activities from a .csv file, checks if
##' the data fulfils the requirements for a `popim_vacc_activities`
##' object, and if so returns this object.
##'
##' The requirements are that the data contain the columns `region`,
##' `year`, `age_first`, `age_last`, `coverage` and `targeting`, of
##' types character, integer, integer, integer, double, character,
##' respectively.
##'
##' 0 <= age_first <= age_last
##' 0 <= coverage <= 1
##' targeting must be one of "random", "correlated", "targeted".
##'
##' Column `region` details the (geographical) region to which each
##' vaccination activity is to be administered.
##'
##' Column `year` details the year in which each vaccination activity
##' occurs.
##'
##' Columns `age_first` and `age_last` give the age range targeted in
##' each vaccination activity.
##'
##' Column `coverage` gives the proportion of the target population
##' that will be vaccinated, while column `doses` gives the absolute
##' number of doses used in the vaccination activity. For a given
##' population size (which is not recorded in the
##' `popim_vacc_activities` object generated here), these can be
##' converted into each other, when both are given, they may be
##' inconsistent with each other once applied to a specific
##' `popim_population` object. The consistency between these two colums
##' cannot be confirmed in without reference to a popim_population
##' object, but this function requires that at least one of these is
##' non-missing in each row.
##'
##' The column `targeting` defines how vaccine is allocated if there
##' is pre-existing immunity in the population: For "random" targeting
##' individuals are vaccinated irrespective of immunity status, so if
##' prior to the vaccination activity the proportion immune was x,
##' then a proportion x of the vaccine will be administered to already
##' vaccinated individuals and therefore be wasted.
##'
##' For "correlated" targeting vaccine is administered first to those
##' already immune before any susceptible individuals receive
##' vaccine. This option models the case of unequal access to
##' vaccination.
##'
##' For "targeted" targeting, vaccine will be given first to as yet
##' non-immune individuals. This is the most effective use of
##' vaccine. It may be realistic in the case of multi-year campaigns
##' targeting different areas within the geographical region
##' specified.
##' 
##' @param file Name of the file from which the vaccination activities
##'     are to be read from. If it does not contain an absolute path,
##'     the file name is relative to the current working directory.
##' @return object of class `popim_vacc_activities`, a dataframe with
##'     one row per vaccination activity, with columns `year`,
##'     `age_first`, `age_last`, `coverage`, `targeting`.
##' @author Tini Garske
##' @seealso [popim_vacc_activities()] for details of the S3 class, and
##'     [utils::read.csv()] which handles the reading of the .csv
##'     file.
##' @export
read_vacc_activities <- function(file) {

    ## assert_file_exists(file)
    if(!file.exists(file))
        stop(sprintf("%s does not exist", file), call. = FALSE)

    df <- utils::read.csv(file, stringsAsFactors = FALSE)
    df <- as_vacc_activities(df)
    df
}

##' Generate a `popim_vacc_activites` object from a dataframe
##'
##' Checks if the dataframe is suitable (i.e., contains appropriate
##' columns and data ranges), and if so converts it to a
##' `popim_vacc_activities` object and returns this.
##'
##' The input dataframe has to have at least the columns `region`,
##' `year`, `age_first`, `age_last`, `coverage`, `doses` and
##' `targeting`. Any further columns are simply carried over into the
##' `popim_vacc_activities` object.
##'
##' @param df dataframe to be converted to class
##'     `popim_vacc_activities`
##' @return an object of class `popim_vacc_activities`
##' @author Tini Garske
##' @seealso [popim_vacc_activities()] for details of the S3 class.
##' @export
as_vacc_activities <- function(df) {

    class(df) <- c("popim_vacc_activities", "data.frame")
    validate_vacc_activities(df)
    df
}

##' Add coverage or doses information to the input
##' `popim_vacc_activities` object
##'
##' For each line in the `popim_vacc_activities` object the given
##' information of coverage is converted to doses, or vice versa,
##' using the target population size implied by the `popim_population`
##' object supplied. If both coverage and doses are given for any
##' activity, the function checks if they are consistent with the
##' population size, and fails if there are any inconsistencies.
##'
##' @param vacc `popim_vacc_activities` object
##' @param pop `popim_population` object
##' @return The supplied object of class `popim_vacc_activities`,
##'     updated to have both `doses` and `coverage` information.
##' @author Tini Garske
##' @export
complete_vacc_activities <- function(vacc, pop) {
    validate_vacc_activities(vacc)
    stopifnot(is_population(pop))

    ## double check coverage and doses are compatible:
    ii <- which(!is.na(vacc$coverage) & !is.na(vacc$doses))
    if(length(ii) > 0) {
        new_doses <- sapply(ii, function(i)
            doses_from_coverage(pop, vacc$coverage[i], vacc$region[i],
                                vacc$year[i],
                                vacc$age_first[i], vacc$age_last[i]))

        stopifnot(isTRUE(all.equal(vacc$doses[ii], new_doses)))
    }
    ## missing coverage:
    ii <- which(is.na(vacc$coverage))
    if(length(ii) > 0) {
        vacc$coverage[ii] <- sapply(ii, function(i)
            coverage_from_doses(pop, vacc$doses[i], vacc$region[i],
                                vacc$year[i],
                                vacc$age_first[i], vacc$age_last[i]))
    }

    ## missing doses:
    ii <- which(is.na(vacc$doses))
    if(length(ii) > 0) {
        vacc$doses[ii] <- sapply(ii, function(i)
            doses_from_coverage(pop, vacc$coverage[i], vacc$region[i],
                                vacc$year[i],
                                vacc$age_first[i], vacc$age_last[i]))
    }

    vacc
}
