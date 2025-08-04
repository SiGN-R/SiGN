test_that("common_il_dur = TRUE with both VI schedules computes combined VI timing via additive rates", {
  result <- r(10, 10, 5, 1, "VI", "VI", common_il_dur = TRUE)
  expect_equal(result, 0.1)
})

test_that("common_il_dur = TRUE but one schedule not VI returns NA", {
  expect_true(is.na(r(10, 10, 5, 1, "VI", "FR", TRUE)))
  expect_true(is.na(r(10, 10, 5, 1, "FR", "VI", TRUE)))
  expect_true(is.na(r(10, 10, 5, 1, "FR", "FR", TRUE)))
})

test_that("common_il_dur = FALSE uses il_dur1 regardless of schedule", {
  expect_equal(r(10, 20, 5, 1, "FR", "FR", FALSE), 1 / (10 + 5))
  expect_equal(r(10, 20, 5, 1, "VI", "FR", FALSE), 1 / (10 + 5))
})

test_that("zero durations are handled correctly", {
  expect_equal(r(0, 10, 5, 1, "VI", "VI", FALSE), 1 / 5)
  expect_equal(r(10, 10, 0, 1, "VI", "VI", FALSE), 1 / 10)
})

test_that("function handles vectorised inputs correctly", {
  res <- r(
    il_dur1 = c(10, 10, 10, 10),
    il_dur2 = c(20, 10, 10, 10),
    tl_dur1 = c(5, 5, 5, 5),
    tr_p = c(1, 0.5, -0.01, 1.01),
    il_sched1 = c("VI", "FR", "VI", "FR"),
    il_sched2 = c("VI", "FR", "VI", "FR"),
    common_il_dur = TRUE
  )
  expect_equal(length(res), 4)
  expect_true(is.na(res[2]))
  expect_equal(round(res[1], 4), round(1 / (1 / (1 / 10 + 1 / 20) + 5), 4))
  expect_true(is.na(res[3]))
  expect_true(is.na(res[4]))
  expect_warning(r(10:12, 10:11, 0, 1, "VI", "VI", TRUE))
})
