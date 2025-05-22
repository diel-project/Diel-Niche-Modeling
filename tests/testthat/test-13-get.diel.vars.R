test_that("get.diel.vars extracts variable names correctly", {
  f <-  list(
    night = "x < nightEnd | x >= night",
    day   = "x >= sunriseEnd & x < sunset"
    # twilight is inferred as: x >= nightEnd & x < sunriseEnd OR x >= sunset & x < night
  )
  expect_equal(
    Diel.Niche:::get.diel.vars(f),
    c("nightEnd", "night", "sunriseEnd", "sunset")
  )
  
  f2 <- list( thing = "x < dawn & (x > sunrise | x < noon)")
  expect_equal(
    Diel.Niche:::get.diel.vars(f2),
    c("dawn", "sunrise", "noon")
  )
  
  
})