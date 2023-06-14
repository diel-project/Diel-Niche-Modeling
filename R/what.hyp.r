#' Hypothesis Codes
#'
#' Call defined hypotheses sets
#' @param hyp.in hypothesis code name or NULL.
#' @return Full name of hypothesis if code name is provided. If NULL a general description of hypotheses is provided. 
#' @examples 
#' what.hyp()
#' what.hyp("D.th")
#' @export

what.hyp=function(hyp.in=NULL){
if(!is.null(hyp.in)){
  
  mat.print=data.frame(hyp.opts=c("D","N","CR","C",
                                 "C2","D.CR","D.N","CR.N",
                                 "D.th","N.th","CR.th","C.th","EC.th",
                                 "D.max","N.max","CR.max",
                                 "D.var","N.var","CR.var","C.var","AV.var",
                                 "Uncon","C.max","EC","EQ.avail",
                                 "D.avail","TW.avail","N.avail","D.TW.avail","N.TW.avail","D.N.avail"
                                 ),
                       
                       print.desc=c("Traditional/General Diurnal", "Traditional/General Nocturnal", "Traditional/General Crepuscular","Traditional Cathemeral",
                                  "General Cathemeral", "General Crepuscular-Diurnal", "General Diurnal-Nocturnal", "General Crepuscular-Nocturnal",
                                  "Diurnal Threshold", "Nocturnal Threshold", "Crepuscular Threshold", "Cathemeral Threshold", "Even Cathemeral Threshold",
                                  "Diurnal Maximizing", "Nocturnal Maximizing", "Crepuscular Maximizing",
                                  "Diurnal Variation", "Nocturnal Variation", "Crepuscular Variation", "Cathemeral Variation", "Use is equivalent to Available Variation",
                                  "Unconditional", "Cathemeral probabilities that are equivalent and exclude from maximizing hyps", "Even Cathemeral (Equivalent probabilities)", "Use is equivalent to availability",
                                  "Day Selection", "Twilight Selection", "Night Selection", "Day-Twilight Selection", "Night-Twilight Selection","Day-Night Selection"
                                  )
  )
  
  index=match(hyp.in,mat.print$hyp.opts)
  print.out=mat.print[index,2]
  if(is.na(print.out)){
    stop("Hypothesis input does not match hypotheses codes")
  }
  print(print.out)

  
}  
  
  if(is.null(hyp.in)){cat("Traditional : ", paste(hyp.sets("list")[[1]], collapse = ' ')," \n\n",
                      "General : ", paste(hyp.sets("list")[[2]], collapse = ' '),"\n \n",
                      "Threshold : ", paste(hyp.sets("list")[[3]], collapse = ' '),"\n \n",
                      "Maximizing: ", paste(hyp.sets("list")[[4]], collapse = ' '),"\n \n",
                      "Variation: ", paste(hyp.sets("list")[[5]], collapse = ' '),"\n \n",
                      "Selection: ", paste(hyp.sets("list")[[6]], collapse = ' '),"\n \n",
                      sep=" ")}
  
  
  }
