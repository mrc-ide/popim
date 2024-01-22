test_that("coverage_from_immunity calculates correct coverage", {

    imm_now <- 0
    coverage <- 0
    
    imm_next <- calc_new_immunity(coverage = coverage,
                                  prev_immunity = imm_now,
                                  targeting = "random")
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "random"), coverage)
        
    imm_next <- calc_new_immunity(coverage = coverage,
                                  prev_immunity = imm_now,
                                  targeting = "targeted")
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "targeted"), coverage)
    
    imm_next <- calc_new_immunity(coverage = coverage,
                                  prev_immunity = imm_now,
                                  targeting = "correlated")
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "correlated"), coverage)

    imm_now <- c(0, 0.5, 0.4)
    coverage <- c(0, 0, 0.5)

    imm_next <- vapply(seq_along(imm_now), function(i)
                       calc_new_immunity(coverage = coverage[i],
                                  prev_immunity = imm_now[i],
                                  targeting = "random"), numeric(1))
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "random"),
                 coverage)

    imm_next <- vapply(seq_along(imm_now), function(i)
                       calc_new_immunity(coverage = coverage[i],
                                  prev_immunity = imm_now[i],
                                  targeting = "correlated"), numeric(1))
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "correlated"),
                 coverage)

    imm_next <- vapply(seq_along(imm_now), function(i)
                       calc_new_immunity(coverage = coverage[i],
                                  prev_immunity = imm_now[i],
                                  targeting = "targeted"), numeric(1))
    expect_equal(coverage_from_immunity_diff(imm_now, imm_next - imm_now,
                                             targeting = "targeted"),
                 coverage)


})
