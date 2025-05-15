test_that("thin.time() retains one observation per interval", {
  set.seed(123)
  dat <- data.frame(
    datetime = as.POSIXct("2023-01-01 00:00:00") + sort(sample(0:7200, 100, replace = TRUE)),
    site = rep("A", 100)
  )
  thinned <- thin.time(dat, datetime.column = "datetime", site.column = "site", minutes.between = 30, progress.bar = FALSE)
  
  # Check that returned object is a data.frame
  expect_s3_class(thinned, "data.frame")
  
  # Check that no two records are within 30 minutes of each other
  deltas <- diff(sort(thinned$datetime))
  expect_true(all(deltas >= 30))
})

test_that("thin.time() handles multiple sites and species correctly", {
  dat <- data.frame(
    datetime = as.POSIXct("2023-01-01 00:00:00") + rep(c(0, 300, 600, 900, 1800), 6),
    site = rep(c("A", "B"), each = 15),
    species = rep(c("cat", "dog", "fox"), each = 5, times = 2)
  )
  thinned <- thin.time(dat, datetime.column = "datetime", site.column = "site",
                       species.column = "species", minutes.between = 15, progress.bar = FALSE)
  
  expect_true(all(c("datetime", "site", "species") %in% colnames(thinned)))
  expect_s3_class(thinned, "data.frame")
})

test_that("thin.time() removes NAs in datetime column", {
  dat <- data.frame(
    datetime = as.POSIXct(c("2023-01-01 00:00:00", NA, "2023-01-01 01:00:00")),
    site = c("A", "A", "A")
  )
  thinned <- thin.time(dat, datetime.column = "datetime", site.column = "site", progress.bar = FALSE)
  expect_equal(nrow(thinned), 2)
})

test_that("thin.time() errors on incorrect input types", {
  dat <- data.frame(datetime = as.character(Sys.time()), site = "A")
  
  expect_error(thin.time(1:5, "datetime", "site"), "data must be a data.frame")
  expect_error(thin.time(dat, "missing_column", "site"), "datetime.column not found in data")
  expect_error(thin.time(dat, "datetime", "missing_site"), "site.column not found in data")

  expect_error(thin.time(dat, "datetime", "site"), "datetime.column must be a POSIXct or POSIXt object")
})



test_that("thin.time() works with species.column = NULL", {
  dat <- data.frame(
    datetime = as.POSIXct("2023-01-01 00:00:00") + c(0, 60, 1200, 2000),
    site = rep("A", 4)
  )
  out <- thin.time(dat, datetime.column = "datetime", site.column = "site", minutes.between = 15, progress.bar = FALSE)
  
  expect_s3_class(out, "data.frame")
  expect_true(all(diff(out$datetime) >= 15))
})

test_that("thin.time() errors with wrong inputs on progress.bar", {
  dat <- data.frame(
    datetime = as.POSIXct("2023-01-01 00:00:00") + c(0, 60, 1200, 2000),
    site = rep("A", 4)
  )
  expect_error(thin.time(dat, "datetime", "site", progress.bar = "yes"), "progress.bar must be either TRUE or FALSE")
})
test_that("thin.time() errors with negative minutes", {
  dat <- data.frame(
    datetime = as.POSIXct("2023-01-01 00:00:00") + c(0, 60, 1200, 2000),
    site = rep("A", 4)
  )
  expect_error(thin.time(dat, "datetime", "site", minutes.between = -5), "`minutes.between` must be a single positive number")
})

test_that("thin.time() returns expected result with repeated timestamps", {
  dat <- data.frame(
    datetime = as.POSIXct(rep("2023-01-01 00:00:00", 5)),
    site = rep("A", 5)
  )
  result <- thin.time(dat, datetime.column = "datetime", site.column = "site", progress.bar = FALSE)
  expect_equal(nrow(result), 1)
})