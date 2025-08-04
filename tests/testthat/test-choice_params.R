test_that("choice_params outputs a viable list", {

  eval1 <- suppressMessages(choice_params("fantino", il_dur_a = 1:90))
  expect_equal(length(eval1), 19)
  expect_equal(sum(lengths(eval1) == 90), 19)
  expect_error(choice_params("fantino", il_dur_a = "A"))
  expect_error(choice_params("fantino", il_dur_a = 0))
  expect_error(choice_params("fantino", tl_dur_a1 = -1))
  expect_error(
    choice_params("fantino", tl_dur_a2 = 1, tl_p_a1 = 1, tl_p_a2 = 0)
  )
  expect_error(
    choice_params("fantino", tl_dur_a2 = 1, tl_p_a1 = 0.9, tl_p_a2 = 0.15)
  )
  expect_error(choice_params("fantino", il_dur_a = NA))
})
