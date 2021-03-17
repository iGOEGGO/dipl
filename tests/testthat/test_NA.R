library(stringr)

test_that("delete Columns with only NA values", {
  dataset <- read.csv("testfile_1.csv")
  dataset <- removeNAColumns(dataset)
  expect_equal(length(colnames(dataset)), 7)
})

test_that("fill empty cells with NA", {
  dataset <- read.csv("testfile_1.csv")
  indices <- c(0)
  for (i in 1:ncol(dataset)) {
    temp <- length(which(dataset[,i]==''))
    indices <- c(indices, temp)
  }
  expect_true(sum(indices)>0)
  dataset <- fillNAs(dataset)
  indices <- c(0)
  for (i in 1:ncol(dataset)) {
    temp <- length(which(dataset[,i]==''))
    indices <- c(indices, temp)
  }
  expect_true(sum(indices)==0)
})
