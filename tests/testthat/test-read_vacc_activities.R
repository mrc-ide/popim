test_that("read_vacc_activities reads well", {
    infile <- system.file("extdata/vacc_activities.csv", package = "popim")

    ## reading a good file correctly
    vaccs <- read_vacc_activities(infile)
    expect_null(validate_vacc_activities(vaccs))

    ## error for reading a bad file
    vaccs$targeting <- NULL
    write.csv(vaccs, "tmp.csv")

    expect_error(vaccs <- read_vacc_activities("tmp.csv"))
    file.remove("tmp.csv")

    ## error for reading non-existing file
    infile_bad <- paste0(infile, "bad")
    expect_error(vaccs <- read_vacc_acitivies(infile_bad))

})
