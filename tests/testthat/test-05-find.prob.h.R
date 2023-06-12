test_that(
  "find.hyp",{
    expect_error(
      find.hyp.prob(hyp = "Fake diel phenotype")
    )
    
    expect_true(
      is.matrix(
        find.prob.hyp("D")
      )
    )
    expect_error(
      find.prob.hyp()
    )
    
    expect_error(
      find.prob.hyp(
        "D", fast = "yo"
      )
    )
    expect_error(
      find.prob.hyp(
        "D", "yo"
      )
    )
  }
)