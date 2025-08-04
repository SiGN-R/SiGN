test_that("entry_p_sums computes correct row sums", {
  expect_equal(
    entry_p_sums(list(
      a = c(0.2, 0.5, 1),
      b = c(0.8, 0.5, 0),
      c = c(0.2, 0.5, 1),
      d = c(0.8, 0.5, 1)
    )),
    c(2, 2, 3)
  )
})

test_that("entry_p_sums errors on malformed input", {
  expect_warning(entry_p_sums(list(c(0.5, 0.5, 0.5))))
  expect_warning(entry_p_sums(list(c(1, 2, 3, 4, 5))))
  expect_error(entry_p_sums(list("a", "b", "c", "d")))
})
