test_that("s_delta returns tl_dur when tr_p is not 0", {
  expect_equal(s_delta(10, 0.5, 1), 10)
  expect_equal(s_delta(0, 0.5, 1), 0)
})

test_that("s_delta returns tl_dur when tl_dur is 0 even if tr_p is 0", {
  expect_equal(s_delta(0, 0, 5), 0)
})

test_that("s_delta returns s_delta when tl_dur != 0 and tr_p == 0", {
  expect_equal(s_delta(10, 0, 3), 3)
})

test_that("s_delta works with vectorised input", {
  expect_equal(
    s_delta(c(10, 0, 5), c(0, 0.5, 0), c(1, 1, 1)),
    c(1, 0, 1)
  )
})

test_that("s_delta warns with uneven vectors", {
  expect_warning(s_delta(c(10, 0), c(0, 0.5, 0), c(1, 1, 1)))
})

test_that("s_delta handles mixed conditions correctly", {
  tl_dur <- c(0, 10, 5, 0)
  tr_p <- c(0, 0, 0, 0.5)
  s_d <- c(1, 2, 3, 4)
  expect_equal(
    s_delta(tl_dur, tr_p, s_d),
    c(0, 2, 3, 0)
  )
})
