## low level constructor for a new "vip_vacc_activities" object from
## the inputs provided. This is for internal use only, so doesn't
## currently do any checks on the inputs - it may therefore fail
## ungracefully, or generate an invalid object.
new_vacc_activities <- function(region = character(), year = integer(),
                                age_first = integer(), age_last = integer(),
                                coverage = double(), doses = double(),
                                targeting = character()) {

    x <- data.frame(region = region, year = year,
                    age_first = age_first, age_last = age_last,
                    coverage = coverage, doses = doses, targeting = targeting)
    class(x) <- c("vip_vacc_activities", "data.frame")

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

##' Reading vaccination activities from a .csv file
##'
##' Reads a list of vaccination activities from a .csv file, checks if
##' the data fulfils the requirements for a "vip_vacc_activities"
##' object, and if so returns this object.
##'
##' The requirements are that the data contain the columns "year",
##' "age_first", "age_last", "coverage" and "targeting", of types
##' integer, integer, integer, double, character, respectively.
##'
##' 0 <= age_first <= age_last
##' 0 <= coverage <= 1
##' targeting must be one of "random", "correlated", "targeted".
##' 
##' @param file Name of the file from which the vaccination activities
##'     are to be read from. If it does not contain an absolute path,
##'     the file name is relative to the current working directory.
##' @return object of class "vip_vacc_activities", a dataframe with
##'     one row per vaccination activity, with columns "year",
##'     "age_first", "age_last", "coverage", "targeting".
##' @author Tini Garske
##' @export
read_vacc_activities <- function(file) {

    ## assert_file_exists(file)
    if(!file.exists(file))
        stop(sprintf("%s does not exist", file), call. = FALSE)

    df <- utils::read.csv(file, stringsAsFactors = FALSE)
    class(df) <- c("vip_vacc_activities", "data.frame")

    validate_vacc_activities(df)

    df
}

##' Adding coverage or doses information (whichever is missing) to the
##' vip_vacc_activities object
##'
##' For each line in the vip_vaccination_activities object the given
##' information of coverage is converted to doses, or vice versa,
##' using the target population size implied by the vip_population
##' object supplied. If both coverage and doses are given for any
##' activity, the function checks if they are consistent with the
##' population size, and fails if there are any inconsistencies.
##'
##' @param vaccs vip_vaccination_activities object
##' @param pop_df vip_population object
##' @return vip_vaccination_activities object, updated to have both
##'     doses and coverage information
##' @author Tini Garske
##' @export
complete_vacc_activities <- function(vaccs, pop_df) {
    validate_vacc_activities(vaccs)
    stopifnot(is_population(pop_df))

    ## double check coverage and doses are compatible:
    ii <- which(!is.na(vaccs$coverage) & !is.na(vaccs$doses))
    if(length(ii) > 0) {
        new_doses <- sapply(ii, function(i)
            doses_from_coverage(pop_df, vaccs$coverage[i], vaccs$region[i],
                                vaccs$year[i],
                                vaccs$age_first[i], vaccs$age_last[i]))

        stopifnot(isTRUE(all.equal(vaccs$doses[ii], new_doses)))
    }
    ## missing coverage:
    ii <- which(is.na(vaccs$coverage))
    if(length(ii) > 0) {
        vaccs$coverage[ii] <- sapply(ii, function(i)
            coverage_from_doses(pop_df, vaccs$doses[i], vaccs$region[i],
                                vaccs$year[i],
                                vaccs$age_first[i], vaccs$age_last[i]))
    }

    ## missing doses:
    ii <- which(is.na(vaccs$doses))
    if(length(ii) > 0) {
        vaccs$doses[ii] <- sapply(ii, function(i)
            doses_from_coverage(pop_df, vaccs$coverage[i], vaccs$region[i],
                                vaccs$year[i],
                                vaccs$age_first[i], vaccs$age_last[i]))
    }

    vaccs
}
