test_that("read_population reads well", {
    infile <- system.file("extdata/pop_sample.csv", package = "vip")

    ## reading a good file correctly
    pop <- read_population(infile)
    expect_true(is_population(pop))

    ## error for reading a bad file
    pop$region <- NULL
    write.csv(pop, "tmp.csv")

    expect_error(pop <- read_population("tmp.csv"))
    file.remove("tmp.csv")

    ## error for reading non-existing file
    infile_bad <- paste0(infile, "bad")
    expect_error(pop <- read_population(infile_bad))


})
