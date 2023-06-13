test_that(
  "sim.diel",{
    sims <- sim.diel(n.sim=1,reps=1,n.sample=100,hyp="D.th")
    
    expect_true(
      is.list(
        sims
      )
    )
    expect_true(
      all(
        names(
          sims
        ) %in% 
          c("y", "p", "p.error", "sd.error")
      )
    )
    expect_true(
      is.matrix(
        sims$y
      )
    )
    expect_true(
      is.matrix(
        sims$p
      )
    )
    expect_true(
      is.matrix(
        sims$p.error
      )
    )
    expect_true(
      is.numeric(
        sims$sd.error
      )
    )
    expect_error(
      sim.diel(1, 1, 100, "D", sd.error = -1)
    )
    expect_error(
      sim.diel(1, "1", 100, "D", sd.error = 0)
    )
    expect_error(
      sim.diel(1.5, 1, 100, "D", sd.error = 0)
    )
    expect_error(
      sim.diel(1, 1, 100, "Diurnal", sd.error = 1)
    )
    expect_error(
      sim.diel(-1, 1, 100, "D", sd.error = 0)
    )
  }
)