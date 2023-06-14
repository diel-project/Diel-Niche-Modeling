test_that(
  "plot.diel",{
    
    # make sure it works
    expect_true(
      all(
        class(
          plot.diel(
            suppressWarnings(
              diel.fit(
                y=cbind(11,87,2),
                hyp="D",
                post.fit=TRUE,
                prints = FALSE
              )
            )
          )
        ) %in% c("plotly", "htmlwidget")
      )
    )

    # and now don't have a post.fit
    out2 <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp="D",
        prints = FALSE
      )
    )
    expect_error(
      plot.diel(out2)
    )
    rm(out2)
    # ignore hypes
    out3 <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp=hyp.sets("Traditional"),
        prints = FALSE,
        post.fit = TRUE
      )
    )
    expect_warning(
      plot.diel(
        out3,
        hyp = "CR"
      )
    )
    rm(out3)
    out4 <- suppressWarnings(
      diel.fit(
        y=cbind(11,87,2),
        hyp=hyp.sets("Traditional"),
        prints = FALSE,
        post.fit = TRUE
      )
    )
    expect_warning(
      plot.diel(
        out4,
        posteriors = out4$post.samp
      )
    )
    rm(out4)
  }

)
