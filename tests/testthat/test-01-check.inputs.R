test_that(
  "check.inputs",{
    f <- function(
    y,
    hyp.set,
    prior,
    bf.fit,
    diel.setup
    ){
      to_return <- check.inputs(
        y = y,
        hyp.set = hyp.set,
        prior = prior,
        bf.fit = bf.fit,
        diel.setup = diel.setup
      )
      return(to_return)
    }
    # This one works
    expect_null(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # non-matrix supplied
    expect_error(
      f(
        c(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # rows instead of columns
    expect_error(
      f(
        rbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # hyp.sets not used
    expect_error(
      f(
        cbind(10,10,10),
        "Traditional",
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # works if you just provide the hypotheses
    expect_null(
      f(
        cbind(10,10,10),
        c("D",  "N",  "CR", "C" ),
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # priors dont sum to 1
    expect_error(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        c(0.25, 0.25, 0.5, 0.25),
        TRUE,
        diel.ineq()
      )
    )
    # fewer priors than hypotheses
    expect_error(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        c(0.25, 0.25, 0.5),
        TRUE,
        diel.ineq()
      )
    )
    # more priors than hypotheses
    expect_error(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        rep(0.2,5),
        TRUE,
        diel.ineq()
      )
    )
    # double number of hypotheses, needs to be fixed
    expect_error(
      f(
        y = cbind(10,10,10),
        hyp.set = rep(Diel.Niche::hyp.sets("Traditional"),2),
        prior = rep(1/8,8),
        bf.fit = TRUE,
        diel.setup = diel.ineq()
      )
    )
    # Y needs to be number
    expect_error(
      f(
        cbind("tomato","tomato","tomato"),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        TRUE,
        diel.ineq()
      )
    )
    # works, no BF
    expect_null(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        FALSE,
        diel.ineq()
      )
    )
    #
    expect_null(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        bf.fit = NULL,
        diel.ineq()
      )
    )
    
    # RETRY TEST AFTER FIX
    expect_error(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        c(0.5,0.5,-1,1),
        TRUE,
        diel.ineq()
      )
    )
    
    expect_null(
      f(
        cbind(10,10,10),
        Diel.Niche::hyp.sets("Traditional"),
        NULL,
        bf.fit = FALSE,
        diel.ineq()
      )
    )
    

  }
)