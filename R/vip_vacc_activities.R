## low level constructor for a new "vip_vacc_activities" object from
## the inputs provided. This is for internal use only, so doesn't
## currently do any checks on the inputs - it may therefore fail
## ungracefully, or generate an invalid object.
new_vacc_activities <- function(year = integer(),
                                age_first = integer(), age_last = integer(),
                                coverage = double(), target = character()) {

    x <- data.frame(year = year,
                    age_first = age_first, age_last = age_last,
                    coverage = coverage, target = target)
    class(x) <- c("vip_vacc_activities", "data.frame")

    x
}

validate_vacc_activities <- function(x, name = deparse(substitute(x))) {
    assert_vacc_activities(x)

    assert_column_exists(x, "year")
    assert_column_exists(x, "age_first")
    assert_column_exists(x, "age_last")
    assert_column_exists(x, "coverage")
    assert_column_exists(x, "target")

    assert_wholenumber(x$year)
    assert_wholenumber(x$age_first)
    assert_wholenumber(x$age_last)
    assert_non_negative(x$age_first)
    assert_non_negative(x$age_last - x$age_first)
    assert_0_to_1(x$coverage)
    assert_character(x$target)

    is_valid_target <- function(x) {
        ifelse(x %in% c("random", "correlated", "targeted"), TRUE, FALSE)
    }

    if(!all(vapply(x$target, function(tar) is_valid_target(tar), TRUE)))
        stop(sprintf("%s has an invalid entry in column 'target'", name))

}



##' Reading vaccination activities from a .csv file
##'
##' Reads a list of vaccination activities from a .csv file, checks if
##' the data fulfils the requirements for a "vip_vacc_activities"
##' object, and if so returns this object.
##'
##' The requirements are that the data contain the columns "year",
##' "age_first", "age_last", "coverage" and "target", of types
##' integer, integer, integer, double, character, respectively.
##'
##' 0 <= age_first <= age_last
##' 0 <= coverage <= 1
##' target must be one of "random", "correlated", "targeted". 
##' 
##' @param file Name of the file from which the vaccination activities
##'     are to be read from. If it does not contain an absolute path,
##'     the file name is relative to the current working directory.
##' @return object of class "vip_vacc_activities", a dataframe with
##'     one row per vaccination activity, with columns "year",
##'     "age_first", "age_last", "coverage", "target".
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
