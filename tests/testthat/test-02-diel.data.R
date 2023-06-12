test_that(
  "diel.data",{
  #load the data
    data("diel.data")

  expect_true(
    is.data.frame(diel.data)
  )
  
  }
)
