context("dbR6_cbind.R")

test_that("left join works", {
  data(mtcars2)
  tmp <- tempfile()
  write.table(mtcars, tmp, quote = FALSE)
  my_db <- dbR6$new()
  my_db$add_table(mtcars,"mtcarsdb")
  my_db$create_index("mtcarsdb", column = "row_names")
  my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb")
  expect_true(all(dim(my_db$get_table("out")) == c(32, 22)))
})

test_that("inner join works", {
  data(mtcars2)
  tmp <- tempfile()
  write.table(mtcars, tmp, quote = FALSE)
  my_db <- dbR6$new()
  my_db$add_table(mtcars,"mtcarsdb")
  my_db$create_index("mtcarsdb", column = "row_names")
  my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb", join = "inner")
  expect_true(all(dim(my_db$get_table("out")) == c(32, 22)))
})


test_that("natural join works", {
  data(mtcars2)
  tmp <- tempfile()
  write.table(mtcars, tmp, quote = FALSE)
  my_db <- dbR6$new()
  my_db$add_table(mtcars, "mtcarsdb")
  my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb", join = "natural")
  expect_true(all(dim(my_db$get_table("out")) == c(32, 11)))
})

test_that("cross join works", {
  data(mtcars2)
  tmp <- tempfile()
  write.table(mtcars, tmp, quote = FALSE)
  my_db <- dbR6$new()
  my_db$add_table(mtcars, "mtcarsdb")
  my_db$cbind("out", using_what = "row_names", "mtcarsdb", "mtcarsdb", join = "cross")
  expect_true(all(dim(my_db$get_table("out")) == c(32, 11)))
})
