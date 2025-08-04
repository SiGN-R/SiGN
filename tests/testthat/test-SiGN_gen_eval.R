test_that("SiGN_gen_eval returns expected structure and class", {
  params <- do.call(choice_params, as.list(subopt_avian[9:24]))

  # Run function
  result <- SiGN_gen_eval(params, subopt_avian$cp, b = 1, k_r = 1, k_d = 1)

  # Test class
  expect_s3_class(result, "SiGN_gen_eval")

  # Test structure
  expect_named(result, c(
    "param_est", "desc_stats", "info_criteria",
    "mod_info", "phi_optim_result", "residuals", "details"
  ))

  # Test types
  expect_true(is.data.frame(result$param_est))
  expect_true(is.data.frame(result$desc_stats))
  expect_true(is.data.frame(result$info_criteria))
  expect_true(is.numeric(result$residuals))

  # Check values are finite
  expect_true(all(is.finite(unlist(result$param_est))))
  expect_true(all(is.finite(unlist(result$desc_stats))))
  expect_true(all(is.finite(unlist(result$info_criteria))))
})

test_that("SiGN_gen_eval errors with mismatched input lengths", {
  params <- do.call(choice_params, as.list(subopt_avian[9:24]))
  obs <- subopt_avian$cp[1:10]  # Too short
  expect_error(SiGN_gen_eval(params, obs))
})

test_that("SiGN_gen_eval errors when observed values are out of bounds", {
  params <- do.call(choice_params, as.list(subopt_avian[9:24]))
  obs <- subopt_avian$cp + 1  # Last value = 1.0 (invalid)
  expect_error(SiGN_gen_eval(params, obs))
})
