test_that(
  "diel.plot",{
    
    out <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp="D",
        post.fit=TRUE,
        prints = FALSE
      )
    )
    # make sure it works
    expect_true(
      all(class(diel.plot(out)) %in% c("plotly", "htmlwidget"))
    )
    # and now don't have a post.fit
    out <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp="D",
        prints = FALSE
      )
    )
    expect_error(
      diel.plot(out)
    )
    # ignore hypes
    out <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp=hyp.sets("Traditional"),
        prints = FALSE,
        post.fit = TRUE
      )
    )
    expect_warning(
      diel.plot(
        out,
        hyp = "CR"
      )
    )
    out <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp=hyp.sets("Traditional"),
        prints = FALSE,
        post.fit = TRUE
      )
    )
    expect_warning(
      diel.plot(
        out,
        posteriors = out$post.samp
      )
    )
  }

)
