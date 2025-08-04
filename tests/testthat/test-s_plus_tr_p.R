test_that("s_plus_tr_p returns correct reinforcement probability when sig = TRUE", {
  expect_equal(
    s_plus_tr_p(TRUE, 10, 20, 0.5, 0.8),
    0.5  # 0.5/10 = 0.05 > 0.8/20 = 0.04
  )

  expect_equal(
    s_plus_tr_p(TRUE, 10, 20, 0.2, 0.8),
    0.8  # 0.2/10 = 0.02 < 0.8/20 = 0.04
  )
})

test_that("s_plus_tr_p returns NA when sig = FALSE", {
  expect_true(is.na(s_plus_tr_p(FALSE, 10, 20, 0.5, 0.8)))
  expect_type(s_plus_tr_p(FALSE, 10, 20, 0.5, 0.8), "double")
})

test_that("s_plus_tr_p returns NA when both tr_p1 and tr_p2 are 0", {
  expect_true(is.na(s_plus_tr_p(TRUE, 10, 20, 0, 0)))
  expect_type(s_plus_tr_p(TRUE, 10, 20, 0, 0), "double")
})

test_that("s_plus_tr_p returns NA when either duration is 0", {
  expect_true(is.na(s_plus_tr_p(TRUE, 0, 20, 0.5, 0.8)))
  expect_true(is.na(s_plus_tr_p(TRUE, 10, 0, 0.5, 0.8)))
  expect_type(s_plus_tr_p(TRUE, 0, 20, 0.5, 0.8), "double")
  expect_type(s_plus_tr_p(TRUE, 10, 0, 0.5, 0.8), "double")
})

test_that("s_plus_tr_p is vectorised", {
  result <- s_plus_tr_p(
    sig      = c(TRUE, FALSE, TRUE),
    tl_dur1  = c(10, 10, 0),
    tl_dur2  = c(20, 20, 20),
    tr_p1    = c(0.5, 0.5, 0.5),
    tr_p2    = c(0.8, 0.8, 0.8)
  )
  expect_equal(result, c(0.5, NA, NA))
})
