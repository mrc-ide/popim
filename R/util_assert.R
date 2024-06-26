assert_scalar <- function(x, name = deparse(substitute(x))) {
  if (length(x) != 1) {
    stop(sprintf("'%s' must be a scalar", name), call. = FALSE)
  }
}

assert_1d <- function(x, name = deparse(substitute(x))) {
  if (!is.null(dim(x))) {
    stop(sprintf("'%s' must not be 2d or more", name), call. = FALSE)
  }
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if (!is.character(x)) {
    stop(sprintf("'%s' must be character", name), call. = FALSE)
  }
}

assert_numeric <- function(x, name = deparse(substitute(x))) {
  if (!is.numeric(x)) {
    stop(sprintf("'%s' must be numeric", name), call. = FALSE)
  }
}

assert_logical <- function(x, name = deparse(substitute(x))) {
  if (!is.logical(x)) {
    stop(sprintf("'%s' must be logical", name), call. = FALSE)
  }
}

assert_hash <- function(x, name = deparse(substitute(x))) {
  if (!all(grepl("^[[:xdigit:]]{32}$", x))) {
    stop(sprintf("'%s' must be a hash", name), call. = FALSE)
  }
}

assert_wholenumber <- function(x, name = deparse(substitute(x)),
                               tol = .Machine$double.eps^0.5) {
    assert_numeric(x, name)
    if(any(is.finite(x) & !(abs(x - round(x)) < tol))) {
        stop(sprintf("'%s' must be a (vector of) whole number(s)", name),
             call. = FALSE)
    }
}

assert_0_to_1 <- function(x, name = deparse(substitute(x))) {
    assert_numeric(x, name)
    if(any(x < 0 | x > 1)) {
        stop(sprintf("'%s' must be between 0 and 1", name), call. = FALSE)
    }
}

assert_0_to_1_or_missing <- function(x, name = deparse(substitute(x))) {
    xval <- x[!is.na(x)]
    if(length(xval) > 0) assert_0_to_1(xval, name)
}

assert_non_negative <- function(x, name = deparse(substitute(x))) {
    assert_numeric(x, name)
    y <- any(x < 0, na.rm = TRUE)
    if((is.na(y) || y)) {
        stop(sprintf("'%s' must be non-negative", name), call. = FALSE)
    }
}

assert_non_negative_or_missing <- function(x, name = deparse(substitute(x))) {
    xval <- x[!is.na(x)]
    if(length(xval) > 0) assert_non_negative(xval, name)
}

assert_scalar_character <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_character(x, name)
}

assert_scalar_numeric <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_numeric(x, name)
}

assert_scalar_wholenumber <- function(x, name = deparse(substitute(x))) {
    assert_scalar(x, name)
    assert_wholenumber(x, name)
}

assert_scalar_0_to_1 <- function(x, name = deparse(substitute(x))) {
    assert_scalar(x, name)
    assert_0_to_1(x, name)
}

assert_scalar_0_to_1_or_missing <- function(x, name = deparse(substitute(x))) {
    assert_scalar(x, name)
    assert_0_to_1_or_missing(x, name)
}

assert_scalar_non_negative <- function(x, name = deparse(substitute(x))) {
    assert_scalar(x, name)
    assert_non_negative(x, name)
}

assert_scalar_non_negative_or_missing <-
    function(x, name = deparse(substitute(x))) {
    assert_scalar(x, name)
    assert_non_negative_or_missing(x, name)
}

assert_scalar_logical <- function(x, name = deparse(substitute(x))) {
  assert_scalar(x, name)
  assert_logical(x, name)
}

assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}

assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}

assert_population <- function(x, name = deparse(substitute(x))) {
    if(!inherits(x, "popim_population"))
        stop(sprintf("'%s' must be of class 'popim_population'", name),
             call. = FALSE)
}

assert_vacc_activities <- function(x, name = deparse(substitute(x))) {
    if(!inherits(x, "popim_vacc_activities"))
        stop(sprintf("'%s' must be of class 'popim_vacc_activities'", name),
             call. = FALSE)
}

assert_file_exists <- function(x, check_case = TRUE, workdir = NULL,
                               name = "File") {
  err <- !file.exists(x, check_case = check_case, workdir = workdir)
  if (any(err)) {
    i <- attr(err, "incorrect_case")
    if (!is.null(i)) {
      msg_case <- x[i]
      msg_totally <- x[err & !i]
      if (length(msg_case) > 0L) {
        correct_case <- attr(err, "correct_case")
        msg_case <- sprintf("'%s' (should be '%s')",
                            names(correct_case), correct_case)
      }
      msg <- c(msg_case, squote(msg_totally))
    } else {
      msg <- squote(x[err])
    }
    stop(sprintf("%s does not exist: %s", name, paste(msg, collapse = ", ")),
         call. = FALSE)
  }
}

assert_is_directory <- function(x, check_case = TRUE, workdir = NULL,
                                name = "File") {
  assert_file_exists(x, check_case, workdir, name)
  path <- if (is.null(workdir)) x else file.path(workdir, x)
  if (!is_directory(path)) {
    stop(sprintf("%s exists but is not a directory: %s",
                 name, paste(x, collapse = ", ")),
         call. = FALSE)
  }
}

match_value <- function(arg, choices, name = deparse(substitute(arg))) {
  assert_scalar_character(arg)
  if (!(arg %in% choices)) {
    stop(sprintf("%s must be one of %s",
                 name, paste(squote(choices), collapse = ", ")),
         call. = FALSE)
  }
  arg
}

assert_type <- function(x, type, name = deparse(substitute(x))) {
  switch(type,
         logical = assert_scalar_logical(x, name),
         numeric = assert_scalar_numeric(x, name),
         character = assert_scalar_character(x, name))
}

assert_column_exists <- function(x, col, name = deparse(substitute(x))) {
    if(!(col %in% names(x)))
        stop(sprintf("%s is missing column %s", name, col),
             call. = FALSE)
}
