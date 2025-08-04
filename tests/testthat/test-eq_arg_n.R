test_that("eq_arg_n returns TRUE when all elements have equal length", {
  expect_true(eq_arg_n(list(1:3, 4:6)))
  expect_true(eq_arg_n(list(c("a", "b"), c("c", "d"))))
  expect_true(eq_arg_n(list(numeric(0), numeric(0))))  # Both length 0
})

test_that("eq_arg_n returns FALSE when elements have unequal length", {
  expect_false(eq_arg_n(list(1:3, 4:5)))
  expect_false(eq_arg_n(list(c("a", "b"), "c")))
  expect_false(eq_arg_n(list(1, 1:2, 1:3)))
})

test_that("eq_arg_n handles edge cases", {
  expect_true(eq_arg_n(list()))  # empty list – vacuously true
  expect_true(eq_arg_n(list(1:5)))  # single element
})
