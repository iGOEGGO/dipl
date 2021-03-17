library(stringr)

test_that("convert factor-Columns", {
  dataset <- read.csv("data_STROKE.csv", fileEncoding = "latin1")
  n1 <- names(Filter(is.factor, dataset))
  dataset <- resetFactors(dataset)
  n2 <- names(Filter(is.factor, dataset))
  # Daten werden hier auch gelöscht -> noch strings
  print("Hallo")
  print(length(n1))
  print(length(n2))
  expect_true(isTRUE(all.equal(length(n1), length(n2))))
})

test_that("delete String-Columns", {
  dataset <- read.csv("testfile_1.csv")
  dataset <- filterStrings(dataset)
  # Daten werden hier auch gelöscht -> noch strings
  expect_equal(length(colnames(dataset)), 8)
})
