context("dbR6_add_table.R")

test_that("dbR6_add_table works", {
  data(mtcars2)
  tmp <- tempfile()
  write.table(mtcars, tmp, quote = FALSE)
  my_db <- dbR6$new()
  my_db$add_table(mtcars,"mtcarsdb")
  expect_true(all(dim(my_db$get_table("mtcarsdb")) == c(32, 11)))
})
