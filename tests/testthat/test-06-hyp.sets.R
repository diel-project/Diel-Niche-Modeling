test_that(
  "hyp.sets",{
    
    expect_true(
      is.character(hyp.sets("Traditional"))
    )
    expect_true(
      all(class(hyp.sets("list")) %in% c("list", "diel"))
    )
    expect_true(
      is.character(hyp.sets("Selection"))
    )
    expect_true(
      is.character(hyp.sets("Variation"))
    )
    expect_true(
      is.character(hyp.sets("Maximizing"))
    )
    expect_true(
      is.character(hyp.sets("Threshold"))
    )
    expect_true(
      is.character(hyp.sets("General"))
    )
    expect_error(
      is.character(hyp.sets("traditional"))
    )
    expect_error(
      hyp.sets(c("Traditional", "General"))
    )
    expect_error(
      hyp.sets(3)
    )
  }
)