test_that("SiGN returns object of class 'SiGN'", {
  params <- choice_params(profile = "zentall")
  result <- SiGN(params)
  expect_s3_class(result, "SiGN")
})

test_that("SiGN returns a numeric choice prediction", {
  params <- choice_params(profile = "zentall")
  result <- SiGN(params)
  expect_type(result$cp, "double")
  expect_length(result$cp, 1)
  expect_true(result$cp >= 0 && result$cp <= 1)
})

test_that("SiGN returns details as a data frame", {
  params <- choice_params(profile = "zentall")
  result <- SiGN(params)
  expect_s3_class(result$details, "data.frame")
  expect_true(all(c("cp", "r_a", "r_b", "cr_a", "cr_b") %in% names(result$details)))
})

test_that("SiGN throws error if non-list is passed", {
  expect_error(SiGN("not a list"))
})

test_that("SiGN can handle vectorised parameter sets", {
  params <- choice_params(
    il_dur_a = c(10, 10),
    il_dur_b = c(10, 10),
    tl_dur_a1 = c(10, 20),
    tl_dur_a2 = c(10, 20),
    tl_dur_b1 = c(10, 10),
    tl_dur_b2 = c(10, 10),
    tl_p_a1 = c(0.5, 0.5),
    tl_p_a2 = c(0.5, 0.5),
    tl_p_b1 = c(0.5, 0.5),
    tl_p_b2 = c(0.5, 0.5),
    tr_p_a1 = c(1, 1),
    tr_p_a2 = c(0, 0),
    tr_p_b1 = c(1, 1),
    tr_p_b2 = c(0, 0),
    il_sched_a = "VI",
    il_sched_b = "VI"
  )
  result <- SiGN(params)
  expect_length(result$cp, 2)
})
