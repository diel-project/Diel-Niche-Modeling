test_that("bin.diel.times() errors when bin.type.list is wrong class", {
  dummy_data <- data.frame(
    dt = as.POSIXct(
      c("2022-06-21 12:00:00",
        "2022-06-21 12:00:00"), tz = "UTC"),
    lat = 41.8781,
    lon = -87.6298
  )
  expect_error(
    bin.diel.times(dummy_data, datetime.column = "dt",
                   lat.column = "lat", lon.column = "lon",
                   bin.type.list = list()),
    "must be of class diel.bin.list"
  )
})

test_that("bin.diel.times() errors when datetime column is not POSIXct", {
  dummy_data <- data.frame(
    dt = as.character(c("2022-06-21 12:00:00", "2022-06-21 12:00:00")),
    lat = 41.8781,
    lon = -87.6298
  )
  expect_error(
    bin.diel.times(dummy_data, datetime.column = "dt",
                   lat.column = "lat", lon.column = "lon"),
    "must be of class POSIXct"
  )
})

test_that("bin.diel.times() errors when datetime column has no timezone", {
  dt <- as.POSIXct(c("2022-06-21 12:00:00","2022-06-21 12:00:00"))
  
  dummy_data <- data.frame(
    dt = dt,
    lat = 41.8781,
    lon = -87.6298
  )
  expect_error(
    bin.diel.times(dummy_data, datetime.column = "dt",
                   lat.column = "lat", lon.column = "lon"),
    "The timezone could not be pulled from the datetime.column in data. Please set it explicitly."
  )
})

test_that("bin.diel.times() returns a data.frame with dielBin column", {
  dummy_data <- data.frame(
    dt = as.POSIXct(c("2022-06-21 12:00:00", "2022-06-21 12:00:20"), tz = "US/Central"),
    lat = 41.8781,
    lon = -87.6298
  )
  result <- bin.diel.times(
    data = dummy_data,
    datetime.column = "dt",
    lat.column = "lat",
    lon.column = "lon"
  )
  expect_true("dielBin" %in% colnames(result))
  expect_equal(nrow(result), 2)
})

test_that("bin.diel.times() removes rows with NAs if na_vals = 'remove'", {
  bad_data <- data.frame(
    dt = as.POSIXct(c("2022-12-21 23:00:00","2022-12-21 23:00:00"), tz = "UTC"),
    lat = 89.0,  # extreme latitude, likely to cause NA in suncalc
    lon = 135.0
  )

  expect_error(
    bin.diel.times(
      data = bad_data,
      datetime.column = "dt",
      lat.column = "lat",
      lon.column = "lon",
      na_vals = "remove"
    )
  )
})

test_that("bin.diel.times() errors on NA if na_vals = 'error'", {
  bad_data <- data.frame(
    dt = as.POSIXct(c("2022-12-21 12:00:00", "2022-12-21 12:00:00"), tz = "UTC"),
    lat = 89.0,
    lon = 135.0
  )
  expect_error(
    bin.diel.times(
      data = bad_data,
      datetime.column = "dt",
      lat.column = "lat",
      lon.column = "lon",
      na_vals = "error"
    ),
    "NA values detected when calculating sunlight times"
  )
})

