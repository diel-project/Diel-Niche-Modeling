test_that(
  "diel.ineq",{
    
    expect_true(
      all(
        class(diel.ineq()) %in% c("list", "diel")
      )
    )
    
    expect_error(
      diel.ineq(
        xi = "yo"
      )
    )
    expect_error(
      diel.ineq(
        e = "yo"
      )
    )
    expect_error(
      diel.ineq(
        xi = c(-1,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e = c(0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e.D = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e.N = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e.CR = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e.EC = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        e.AV = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi.D = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi.N = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi.CR= c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi.C = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        xi.EC = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        p.avail = c(0.5,0.5,0.5)
      )
    )
    expect_error(
      diel.ineq(
        p.avail = c(0.5,0.8)
      )
    )
    expect_warning(
      diel.ineq(
        xi = c(0.8)
      )
    )
    
  }
)