library(stringr)

test_that("change type of Date-Columns to Posixct", {
  dataset <- read.csv("testfile_1.csv")
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

test_that("change type of Time-Columns to hms", {
  dataset <- read.csv("testfile_1.csv")
  temp <- function(x) {
    val <- class(x)
    is.element(val[1], 'hms')
  }
  test <- sapply(dataset, temp)
  expect_false(any(is.element(test, TRUE)))
  dataset <- convertTime(dataset)
  test <- sapply(dataset, temp)
  expect_true(any(is.element(test, TRUE)))
})

test_that("change type of Time- and Date-Columns to POSIXct", {
  dataset <- read.csv("testfile_1.csv")
  temp <- function(x) {
    val <- class(x)
    is.element(val[1], 'POSIXct')
  }
  test <- sapply(dataset, temp)
  expect_false(any(is.element(test, TRUE)))
  dataset <- convertDateAndTime(dataset)
  test <- sapply(dataset, temp)
  # there are no columns with date and time in the same field in this dataset
  expect_false(any(is.element(test, TRUE)))
})

test_that("change type of Time- and Date-Columns to POSIXct - actual column", {
  dataset <- read.csv("testfile_2.csv")
  temp <- function(x) {
    val <- class(x)
    is.element(val[1], 'POSIXct')
  }
  test <- sapply(dataset, temp)
  expect_false(any(is.element(test, TRUE)))
  dataset <- convertDateAndTime(dataset)
  test <- sapply(dataset, temp)
  # there is 1 column with date and time in the same field in this dataset
  expect_true(any(is.element(test, TRUE)))
})
