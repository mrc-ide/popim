test_that("is_scalar_integer is TRUE for integer input", {
    expect_equal(is_scalar_integer(0), TRUE)
    expect_equal(is_scalar_integer(1L), TRUE)
    expect_equal(is_scalar_integer(-1000), TRUE)
})

test_that("is_scalar_integer is FALSE for non-integer scalar input", {
    expect_equal(is_scalar_integer(pi), FALSE)
    expect_equal(is_scalar_integer(10.5), FALSE)
})

test_that("is_scalar_integer is FALSE for non-scalar input", {
    expect_equal(is_scalar_integer(matrix(1:3, nrow = 3)), FALSE)
    expect_equal(is_scalar_integer(list(1, 2, 3)), FALSE)
})
