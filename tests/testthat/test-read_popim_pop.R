test_that("read_popim_pop() reads well", {
    infile <- system.file("extdata/pop_sample.csv", package = "popim")

    ## reading a good file correctly
    pop <- read_popim_pop(infile)
    expect_true(is_population(pop))

    ## error for reading a bad file
    pop$region <- NULL
    write.csv(pop, "tmp.csv")

    expect_error(pop <- read_popim_pop("tmp.csv"))
    file.remove("tmp.csv")

    ## error for reading non-existing file
    infile_bad <- paste0(infile, "bad")
    expect_error(pop <- read_popim_pop(infile_bad))

})
