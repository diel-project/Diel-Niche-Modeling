test_that(
  "what.hyp",{
    my_vals <- c("D","N","CR","C",
                 "C2","D.CR","D.N","CR.N",
                 "D.th","N.th","CR.th","C.th","EC.th",
                 "D.max","N.max","CR.max",
                 "D.var","N.var","CR.var","C.var","AV.var",
                 "Uncon","C.max","EC","EQ.avail",
                 "D.avail","CR.avail","N.avail","D.CR.avail","N.CR.avail","D.N.avail"
    )
    my_answers <- c("Traditional/General Diurnal", "Traditional/General Nocturnal", "Traditional/General Crepuscular","Traditional Cathemeral",
                    "General Cathemeral", "General Crepuscular-Diurnal", "General Diurnal-Nocturnal", "General Crepuscular-Nocturnal",
                    "Diurnal Threshold", "Nocturnal Threshold", "Crepuscular Threshold", "Cathemeral Threshold", "Even Cathemeral Threshold",
                    "Diurnal Maximizing", "Nocturnal Maximizing", "Crepuscular Maximizing",
                    "Diurnal Variation", "Nocturnal Variation", "Crepuscular Variation", "Cathemeral Variation", "Use is equivalent to Available Variation",
                    "Unconditional", "Cathemeral probabilities that are equivalent and exclude from maximizing hyps", "Even Cathemeral (Equivalent probabilities)", "Use is equivalent to availability",
                    "Day Selection", "Twilight Selection", "Night Selection", "Day-Twilight Selection", "Night-Twilight Selection","Day-Night Selection"
    )
    responses <- rep(NA, length(my_answers))
    for(i in 1:length(responses)){
      expect_output(
        what.hyp(my_vals[i])
      )
    }
    expect_output(
      what.hyp()
    )
    expect_error(
      what.hyp("shakabrah")
    )
  }
)