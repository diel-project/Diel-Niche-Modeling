#' Hypothesis Codes
#'
#' Call defined hypotheses sets
#' @param hyp.in hypothesis code name, NULL, or ?
#' @return Full name of hypothesis if code name is provided. If NULL a general description of hypotheses is provided. If ? then hypotheses code names are provided.
#' @examples 
#' what.hyp()
#' what.hyp("D.th")
#' what.hyp("?")
#' @export

what.hyp=function(hyp.in=NULL){
if(!is.null(hyp.in)){  
  if(hyp.in=="D"){print("Traditional/General Diurnal")}
  if(hyp.in=="N"){print("Traditional/General Nocturnal")}
  if(hyp.in=="CR"){print("Traditional/General Crepuscular")}
  if(hyp.in=="C"){print("Traditional Cathemeral")}
  
  if(hyp.in=="C2"){print("General Cathemeral")}
  if(hyp.in=="D.CR"){print("General Crepuscular-Diurnal")}
  if(hyp.in=="D.N"){print("General Diurnal-Nocturnal")}
  if(hyp.in=="CR.N"){print("General Crepuscular-Nocturnal")}
  
  if(hyp.in=="D.th"){print("Diurnal Threshold")}
  if(hyp.in=="N.th"){print("Nocturnal Threshold")}
  if(hyp.in=="CR.th"){print("Crepuscular Threshold")}
  if(hyp.in=="C.th"){print("Cathemeral Threshold")}
  if(hyp.in=="EC.th"){print("Even Cathemeral Threshold")}

  if(hyp.in=="D.max"){print("Diurnal Maximizing")}
  if(hyp.in=="N.max"){print("Nocturnal Maximizing")}
  if(hyp.in=="CR.max"){print("Crepuscular Maximizing")}

  if(hyp.in=="D.var"){print("Diurnal Variation")}
  if(hyp.in=="N.var"){print("Nocturnal Variation")}
  if(hyp.in=="CR.var"){print("Crepuscular Variation")}
  if(hyp.in=="C.var"){print("Cathemeral Variation")}
  if(hyp.in=="AV.var"){print("Use is equivalent to Available Variation")}
  
  if(hyp.in=="Uncon"){print("Unconditional")}
  if(hyp.in=="C.max"){print("Cathemeral probabilities that are equivalent and exclude from maximizing hyps")}
  if(hyp.in=="EC"){print("Even Cathemeral (Equivalent probabilities)")}
  if(hyp.in=="AV.EQ"){print("Use is equivalent to availability)")}

  
}  
  
  if(is.null(hyp.in)){cat("Traditional : ", paste(hyp.sets("list")[[1]], collapse = ' ')," \n\n",
                      "General : ", paste(hyp.sets("list")[[2]], collapse = ' '),"\n \n",
                      "Threshold : ", paste(hyp.sets("list")[[3]], collapse = ' '),"\n \n",
                      "Maximizing: ", paste(hyp.sets("list")[[4]], collapse = ' '),"\n \n",
                      "Variation: ", paste(hyp.sets("list")[[5]], collapse = ' '),"\n \n",
                      sep=" ")}
  
  
  }
