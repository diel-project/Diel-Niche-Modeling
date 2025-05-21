test_that("make.diel.bin.list() returns diel.bin.list with twilight = TRUE", {
  bin_list <- make.diel.bin.list(
    twilight = TRUE,
    plot.bins = FALSE,
    date = "2020-06-21",
    lat = 41.8781,
    lon = -87.6298
  )
  
  expect_s3_class(bin_list, "diel.bin.list")
  expect_equal(names(bin_list), c("night", "day"))
})

test_that("make.diel.bin.list() returns diel.bin.list with twilight = FALSE", {
  bin_list <- make.diel.bin.list(
    twilight = FALSE,
    plot.bins = FALSE,
    night = "x <= nightEnd | x >= night",
    dawn = "x > nightEnd & x <= sunriseEnd",
    day  = "x > sunriseEnd & x <= sunset",
    date = "2020-06-21",
    lat = 41.8781,
    lon = -87.6298
  )
  
  expect_s3_class(bin_list, "diel.bin.list")
  expect_equal(names(bin_list), c("night", "day", "dawn"))
})

test_that("make.diel.bin.list() fails if too many bins with twilight = TRUE", {
  expect_error(
    make.diel.bin.list(
      twilight = TRUE,
      plot.bins = FALSE,
      day = "x >= sunriseEnd & x < sunset",
      night = "x < nightEnd | x >= night",
      dawn = "x >= nightEnd & x < sunriseEnd"
    ),
    "If twilight = TRUE then only 2 diel bins need to be defined with equalities."
  )
})

test_that("make.diel.bin.list() fails if too few bins with twilight = FALSE", {
  expect_error(
    make.diel.bin.list(
      twilight = FALSE,
      plot.bins = FALSE,
      night = "x < nightEnd | x >= night",
      day = "x >= sunriseEnd & x < sunset"
    ),
    "If twilight = FALSE, then only three diel periods need to be defined"
  )
})

test_that("make.diel.bin.list() fails with bad transition names", {
  expect_error(
    make.diel.bin.list(
      twilight = FALSE,
      plot.bins = FALSE,
      night = "x < notARealTransition | x >= night",
      dawn = "x >= nightEnd & x < sunriseEnd",
      day  = "x >= sunriseEnd & x < sunset"
    ),
    "do not match what is available within suncalc::getSunlightTimes"
  )
})

test_that("make.diel.bin.list() fails on overlapping bins", {
  expect_error(
    make.diel.bin.list(
      twilight = FALSE,
      plot.bins = FALSE,
      night = "x < nightEnd | x >= night",
      dawn = "x >= nightEnd & x <= sunriseEnd",
      day = "x >= sunriseEnd & x < sunset"
    ),
    "generated overlap"
  )
})

test_that("make.diel.bin.list() fails with NA sunlight times", {
  # Use high latitude + winter date for likely NA
  expect_error(
    make.diel.bin.list(
      twilight = FALSE,
      plot.bins = FALSE,
      night = "x < nightEnd | x >= night",
      dawn = "x >= nightEnd & x < sunriseEnd",
      day = "x >= sunriseEnd & x < sunset",
      lat = 69.6496,
      lon = 18.9560,
      date = "2020-12-21"
    ),
    "resulted in NA sun times"
  )
  
  test_that("character checks for diel period expressions work", {
    expect_error(make.diel.bin.list(day = 5), 
                 "day.*must be a character string")
    
    expect_error(make.diel.bin.list(night = "time < 5"), 
                 "night.*must include the variable name 'x'")
  })
  
  test_that("twilight and plot.bins must be logical", {
    expect_error(make.diel.bin.list(twilight = "yes"), 
                 "twilight.*must be a logical")
    
    expect_error(make.diel.bin.list(plot.bins = 1), 
                 "plot.bins.*must be a logical")
  })
  
  test_that("date must be character or Date", {
    expect_error(make.diel.bin.list(date = 20200312), 
                 "date.*must be a character string or a Date object")
    
    expect_error(make.diel.bin.list(date = "March 12, 2020"), 
                 "date.*must be in format")
  })
  
  test_that("lat and lon must be numeric and within range", {
    expect_error(make.diel.bin.list(lat = "41.87"), 
                 "lat.*must be a numeric")
    
    expect_error(make.diel.bin.list(lat = 200), 
                 "lat.*between -90 and 90")
    
    expect_error(make.diel.bin.list(lon = -200), 
                 "lon.*between -180 and 180")
  })
  test_that("tz must be a valid Olson timezone", {
    expect_error(make.diel.bin.list(tz = 1), 
                 "tz.*must be a character")
    
    expect_error(make.diel.bin.list(tz = "Mars/StandardTime"), 
                 "tz.*must be a valid timezone")
  })
  
  test_that("function works with all default inputs", {
    expect_s3_class(make.diel.bin.list(), "diel.bin.list")
  })
})