test_that("list_depth returns the correct depth of a nested list", {

    l1 <- list(1, 2, 3)
    l2 <- list(1, 2, l1, 4)
    l3 <- list(1, l1, l2, 5)

    expect_equal(list_depth(l1), 1)
    expect_equal(list_depth(l2), 2)
    expect_equal(list_depth(l3), 3)
})
