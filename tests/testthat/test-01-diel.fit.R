test_that(
  "diel.fit",{
    f <- function(
    hyp,
    n.sample,
    hyp.set,
    bf.fit,
    post.fit,
    diel.setup,
    prior,
    n.chains,
    n.mcmc,
    burnin,
    prints,
    alt.optim,
    delta,
    seed
    ){
      set.seed(seed = seed)
      y <- sim.diel(
        n.sample = n.sample,
        hyp = hyp
      )$y
    
      result <- diel.fit(
        y = y,
        hyp.set = hyp.set,
        bf.fit = bf.fit,
        post.fit = post.fit,
        diel.setup = diel.setup,
        prior = prior,
        n.chains = n.chains,
        n.mcmc = n.mcmc,
        burnin = burnin,
        prints = prints,
        alt.optim = alt.optim,
        delta = delta
      )
      return(result)
    }
    
    tmp <- f(
      "D",
      n.sample = 500,
      hyp.set = Diel.Niche::hyp.sets("Traditional"),
      bf.fit = TRUE,
      post.fit = FALSE,
      diel.setup = NULL,
      prior = NULL,
      n.chains = 4,
      n.mcmc = 1500,
      burnin = 500,
      prints = FALSE,
      alt.optim = FALSE,
      delta = NULL,
      seed = 5
    )
    tmpbf <- tmp$bf.table
    D_val <-  tmpbf[
      grep("D",row.names(tmpbf)),
      grep("Post", colnames(tmpbf))
    ]
    top_val <- max(tmpbf[,grep("Post", colnames(tmpbf))])
    
    # check if "D" is the top model
    expect_true(
      D_val == top_val
    )
    
    # We did not include posterior samples so there
    #  is no gelman diagnostic.
    expect_null(
      tmp$gelm.diag
    )
    expect_null(
      tmp$post.samp
    )
    expect_null(
      tmp$ppc
    )
    expect_null(
      tmp$ms.ppc
    )
    expect_null(
      tmp$post.samp.ms.model
    )
    # input wrong name for generating data
    expect_error(
      f(
        "Diurnal",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = FALSE,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = FALSE,
        alt.optim = FALSE,
        delta = NULL,
        seed = 5
      )
    )
    # have non-logical
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = "TRUE",
        post.fit = FALSE,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = FALSE,
        alt.optim = FALSE,
        delta = NULL,
        seed = 5
      )
    )
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = 3,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = FALSE,
        alt.optim = FALSE,
        delta = NULL,
        seed = 5
      )
    )
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = 3,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = "jk",
        alt.optim = FALSE,
        delta = NULL,
        seed = 5
      )
    )
    
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = FALSE,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = FALSE,
        alt.optim = FALSE,
        delta = "yo",
        seed = 5
      )
    )
    
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = FALSE,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = "1500",
        burnin = 500,
        prints = FALSE,
        alt.optim = FALSE,
        delta = NULL,
        seed = 5
      )
    )
    expect_error(
      f(
        "D",
        n.sample = 500,
        hyp.set = Diel.Niche::hyp.sets("Traditional"),
        bf.fit = TRUE,
        post.fit = FALSE,
        diel.setup = NULL,
        prior = NULL,
        n.chains = 4,
        n.mcmc = 1500,
        burnin = 500,
        prints = FALSE,
        alt.optim = "butter",
        delta = NULL,
        seed = 5
      )
    )


  }
)
