test_that(
  "posthoc.niche",{
    prob_vec <- c(0.3,0.6,0.1)
    # warns when a vector is supplied
    expect_warning(
      posthoc.niche(
        prob_vec,
        "D"
      )
    )
    # supply a matrix
    expect_true(
      is.data.frame(
        posthoc.niche(
          t(prob_vec),
          "D"
        )
      )
    )
    # row does not sum to 1
    expect_error(
      posthoc.niche(
        t(prob_vec) - 0.05,
        "D"
      )
    )
    # Wrong name
    expect_error(
      posthoc.niche(
        t(prob_vec),
        "Diurnal"
      )
    )
    expect_error(
      posthoc.niche(
        t(prob_vec),
        "D",
        "blarg"
      )
    )
     
  }
)