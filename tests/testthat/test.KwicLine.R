context("Class")

test_that("Class", {
  data(dickensl)

  k <- kwic(dickensl, "the", unit = "char")
  expect_true(class(k) == "KwicLine")

  k <- kwic(dickensl, "the", unit = "token")
  expect_true(class(k) == "KwicToken")
})

