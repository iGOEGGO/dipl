library(stringr)

test_that("delete String-Columns", {
  dataset <- read.csv("testfile_1.csv")
  dataset <- filterStrings(dataset)
  # Daten werden hier auch gelöscht -> noch strings
  expect_equal(length(colnames(dataset)), 7)
})
