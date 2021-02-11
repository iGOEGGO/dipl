library(stringr)

test_that("change type to Posixct", {
  dataset <- read.csv("testfile.csv")
  temp <- function(x) {
    val <- class(x)
    is.element(val[1], 'POSIXct')
  }
  test <- sapply(dataset, temp)
  expect_false(any(is.element(test, TRUE)))
  dataset <- convertDate(dataset)
  test <- sapply(dataset, temp)
  expect_true(any(is.element(test, TRUE)))
})
