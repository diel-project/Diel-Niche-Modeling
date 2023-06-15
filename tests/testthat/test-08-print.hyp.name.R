test_that(
  "full.hyp.name",{
    expect_match(
      full.hyp.name(
        "D"
      ),
    "Diurnal \\(Traditional\\)"
    )
    
    expect_match(
      full.hyp.name(
        "N"
      ),
      "Nocturnal \\(Traditional\\)"
    )
    expect_match(
      full.hyp.name(
        "CR"
      ),
      "Crepuscular \\(Traditional\\)"
    )
    expect_match(
      full.hyp.name(
        "C"
      )
      ,"Cathemeral \\(Traditional\\)"
    )
    expect_match(
      full.hyp.name(
        "C2"
      )
      ,"Cathemeral \\(General\\)"
    )
    expect_match(
      full.hyp.name(
        "D.CR"
      )
      ,"Diurnal-Crepuscular \\(General\\)"
    )
    expect_match(
      full.hyp.name(
        "D.N"
      ),
      "Diurnal-Nocturnal \\(General\\)"
    )
    expect_match(
      full.hyp.name(
        "CR.N"
      )
      ,"Crepuscular-Nocturnal \\(General\\)"
    )
    expect_match(
      full.hyp.name(
        "D.max"
      )
      ,"Diurnal \\(Maximization\\)"
    )
    expect_match(
      full.hyp.name(
        "N.max"
      )
      ,"Nocturnal \\(Maximization\\)"
    )
    expect_match(
      full.hyp.name(
        "CR.max"
      )
      ,"Crepuscular \\(Maximization\\)"
    )
    expect_match(
      full.hyp.name(
        "D.var"
      )
      ,"Diurnal \\(Variation\\)"
    )
    expect_match(
      full.hyp.name(
        "N.var"
      )
      ,"Nocturnal  \\(Variation\\)"
    )
    expect_match(
      full.hyp.name(
        "CR.var"
      )
      ,"Crepuscular  \\(Variation\\)"
    )
    expect_match(
      full.hyp.name(
        "C.var"
      )
      ,"Cathemeral  \\(Variation\\)"
    )
    expect_match(
      full.hyp.name(
        "A.AV.var"
      )
      ,"Available \\(Variation\\)"
    )
    expect_match(
      full.hyp.name(
        "D.avail"
      )
      ,"Day \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "CR.avail"
      )
      ,"Twilight \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "N.avail"
      )
      ,"Night \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "D.TW.avail"
      )
      ,"Day-Twilight \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "N.TW.avail"
      )
      ,"Night-Twilight \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "D.N.avail"
      )
      ,"Day-Night \\(Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "EQ.avail"
      )
      ,"Available Equality \\(No Selection\\)"
    )
    expect_match(
      full.hyp.name(
        "EC"
      )
      ,"Even Cathemeral Equality"
    )
    expect_match(
      full.hyp.name(
        "C.Max"
      )
      ,"Even Cathemeral Equality Point"
    )
    expect_match(
      full.hyp.name(
        "Uncon"
      )
      ,"Unconstrained"
    )
    expect_warning(
      full.hyp.name(
        "shakabrah"
      )
    )
    
  }
)