test_that(
  "prob.overlap",{
    # generate some data
    library(overlap)
    data("kerinci")
    tiger <- kerinci[kerinci$Sps == "tiger",]
    tiger$rads <- tiger$Time * 2 * pi
    tiger_kde <- overlap::densityFit(
      tiger$rads,
      grid = seq(
        0, 
        2 * pi,
        length.out = 10000
      ),
      getBandWidth(tiger$rads,3)
    )
    tiger_kde <- tiger_kde / (24 / (2 * pi))
    tiger_kde <- data.frame(
      x = seq(0,24,length.out = 10000),
      y = tiger_kde
    )
    expect_true( 
      is.matrix(
        prob.overlap(
          tiger_kde,
          dawn = c(5.2,6),
          dusk = c(18.8, 19.6)
        )
      )
    )
    expect_error(
      prob.overlap(
        as.matrix(tiger_kde)
      )
    )
    error_cols <- tiger_kde
    colnames(error_cols) <- c("time", "prob")
    expect_error(
      prob.overlap(
        error_cols,
        dawn = c(5.2,6),
        dusk = c(18.8, 19.6)
      )
    )
    expect_error(
      prob.overlap(
        tiger_kde,
        dawn = c(6,5),
        dusk = c(18.8, 19.6)
      )
    )
    
    expect_error(
      prob.overlap(
        tiger_kde,
        dawn = c("5","6"),
        dusk = c(18.8, 19.6)
      )
    )
    expect_error(
      prob.overlap(
        tiger_kde,
        dawn = c(5,6),
        dusk = c("18.8", "19.6")
      )
    )
    # do it again with fewer points
    tiger_kde2 <- overlap::densityFit(
      tiger$rads,
      grid = seq(
        0, 
        2 * pi,
        length.out = 100
      ),
      getBandWidth(tiger$rads,3)
    )
    tiger_kde2 <- tiger_kde2 / (24 / (2 * pi))
    tiger_kde2 <- data.frame(
      x = seq(0,24,length.out = 100),
      y = tiger_kde2
    )
    
    expect_warning(
      prob.overlap(
        tiger_kde2,
        dawn = c(5,6),
        dusk = c(18.8, 19.6)
      )
    )

  }
)