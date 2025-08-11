test_that("make_md_data works.", {
  exp1 <- c(2, 1, 0)
  exp2 <- c(1, 1, 1)
  smpl <- c("1", "2", "3")
  meas_mean <- c(1.5, 1, 0.5)
  meas_diff <- c(-1, 0, 1)
  expect_md_data <- tibble::tibble(smpl, meas1 = exp1, meas2 = exp2, meas_mean, meas_diff)

  # happy path
  expect_equal(make_md_data(smpl = smpl, exp1 = exp1, exp2 = exp2, pot = FALSE), expect_md_data)

  # exp1 and exp2 must both be numeric
  expect_error(make_md_data(smpl, exp1, exp2 = smpl, pot = FALSE))

  # generate smpl data and message if smpl is NULL
  expect_message(make_md_data(exp1 = exp1, exp2 = exp2, pot = FALSE))

  # confirm generated smpl is sequential character vector
  expect_equal(make_md_data(exp1 = exp1, exp2 = exp2, pot = FALSE), expect_md_data)

  # smpl must be a character vector
  expect_error(make_md_data(smpl = TRUE, exp1 = exp1, exp2 = exp2, pot = FALSE))

  # smpl, exp1, and exp2 must all be the same length
  expect_error(make_md_data(smpl = smpl, exp1 = c(1:4), exp2 = exp2, pot = FALSE))

  # confirm log10 transformation of data when pot = TRUE
  expect_equal(
    make_md_data(smpl = smpl, exp1 = c(100, 10, 1), exp2 = c(10, 10, 10), pot = TRUE),
    expect_md_data
  )
})
