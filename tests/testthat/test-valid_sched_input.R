test_that("valid_sched_input produces correct outputs", {
  expect_equal(
    valid_sched_input(list(c("VI", "VI", "FR"), c("VI", "VI", "FR"))),
    TRUE)
  expect_equal(
    valid_sched_input(list(c("FR", "VI", "FR"), c("VI", "VI", "FR"))),
    FALSE)
})
