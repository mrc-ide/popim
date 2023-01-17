test_that("is_scalar returns TRUE for a simple scalar", {
    expect_equal(is_scalar(0), TRUE)
    expect_equal(is_scalar(pi), TRUE)
})

test_that("is_scalar returns FALSE for vectors and other structures", {
    expect_equal(is_scalar(c(1, 2)), FALSE)
    expect_equal(is_scalar(data.frame(x = 1:3, y = 4:6)), FALSE)
    expect_equal(is_scalar(list(3)), FALSE)
})

test_that("is_scalar returns FALSE for a character", {
    expect_equal(is_scalar("c"), FALSE)
})
