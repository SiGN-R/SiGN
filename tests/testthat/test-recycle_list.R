test_that("recycle_list correctly recycles elements to max length", {
  input <- list(a = 1:3, b = 5)
  output <- recycle_list(input)

  expect_equal(lengths(output), c(a = 3, b = 3))
  expect_equal(output$a, 1:3)
  expect_equal(output$b, rep(5, 3))
})

test_that("recycle_list preserves names and types", {
  input <- list(x = c("A", "B"), y = 1:4)
  output <- recycle_list(input)

  expect_named(output, c("x", "y"))
  expect_equal(output$x, c("A", "B", "A", "B"))
  expect_equal(output$y, 1:4)
})

test_that("recycle_list handles single-element list", {
  input <- list(z = 42)
  output <- recycle_list(input)

  expect_equal(length(output), 1)
  expect_equal(output$z, 42)
})

test_that("recycle_list handles empty list", {
  expect_warning(recycle_list(list()))
  expect_equal(suppressWarnings(recycle_list(list())), list())
})
