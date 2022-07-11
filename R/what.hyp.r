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
  if(hyp.in=="D.th"){print("General: Diurnal Threshold")}
  if(hyp.in=="N.th"){print("General: Nocturnal Threshold")}
  if(hyp.in=="CR.th"){print("General: Crepuscular Threshold")}
  if(hyp.in=="C.th"){print("General: Cathemeral Threshold (> 0.2)")}
  if(hyp.in=="EC"){print("General: Even Cathemeral (Equivalent probabilities)")}
  if(hyp.in=="EC.th"){print("General: Even Cathemeral Threshold")}
#  if(hyp.in=="AC.th"){print("General: Available (Probabilities equivalent to availabilitiy)")}
  if(hyp.in=="D.max"){print("General: Diurnal Maximizing")}
  if(hyp.in=="N.max"){print("General: Nocturnal Maximizing")}
  if(hyp.in=="CR.max"){print("General: Crepuscular Maximizing")}
  if(hyp.in=="D.var"){print("General: Diurnal Variation")}
  if(hyp.in=="N.var"){print("General: Nocturnal Variation")}
  if(hyp.in=="CR.var"){print("General: Crepuscular Variation")}
  if(hyp.in=="EC.var"){print("General: Even Cathemeral Variation")}
  if(hyp.in=="C.var"){print("General: Cathemeral Variation")}
  if(hyp.in=="AC.var"){print("General: Available Variation")}

  if(hyp.in=="Dn.th"){print("Primary-secondary: Diurnal-nocturnal threshold")}
  if(hyp.in=="Dc.th"){print("Primary-secondary: Diurnal-cathemeral threshold")}
  if(hyp.in=="Dcr.th"){print("Primary-secondary: Diurnal-crepuscular threshold")}
  if(hyp.in=="Nd.th"){print("Primary-secondary: Nocturnal-diurnal threshold")}
  if(hyp.in=="Nc.th"){print("Primary-secondary: Nocturnal-cathemeral threshold")}
  if(hyp.in=="Ncr.th"){print("Primary-secondary: Nocturnal-crepuscular threshold")}
  if(hyp.in=="CRd.th"){print("Primary-secondary: Crepuscular-diurnal threshold")}
  if(hyp.in=="CRn.th"){print("Primary-secondary: Crepuscular-nocturnal threshold")}
  if(hyp.in=="CRc.th"){print("Primary-secondary: Crepuscular-cathemeral threshold")}
  
  if(hyp.in=="Dn.max"){print("Primary-secondary: Diurnal-nocturnal maximizing")}
  if(hyp.in=="Dcr.max"){print("Primary-secondary: Diurnal-crepuscular maximizing")}
  if(hyp.in=="Nd.max"){print("Primary-secondary: Nocturnal-diurnal maximizing")}
  if(hyp.in=="Ncr.max"){print("Primary-secondary: Nocturnal-crepuscular maximizing")}
  if(hyp.in=="CRd.max"){print("Primary-secondary: Crepuscular-diurnal maximizing")}
  if(hyp.in=="CRn.max"){print("Primary-secondary: Crepuscular-nocturnal maximizing")}
  
  if(hyp.in=="Dn.var"){print("Primary-secondary: Diurnal-nocturnal variation")}
  if(hyp.in=="Dcr.var"){print("Primary-secondary: Diurnal-crepuscular variation")}
  if(hyp.in=="Nd.var"){print("Primary-secondary: Nocturnal-diurnal variation")}
  if(hyp.in=="Ncr.var"){print("Primary-secondary: Nocturnal-crepuscular variation")}
  if(hyp.in=="CRd.var"){print("Primary-secondary: Crepuscular-diurnal variation")}
  if(hyp.in=="CRn.var"){print("Primary-secondary: Crepuscular-nocturnal variation")}

  if(hyp.in=="Dn.th.wk"){print("Primary-secondary: Diurnal-nocturnal threshold (weak constraint)")}
  if(hyp.in=="Dcr.th.wk"){print("Primary-secondary: Diurnal-crepuscular threshold (weak constraint)")}
  if(hyp.in=="Nd.th.wk"){print("Primary-secondary: Nocturnal-diurnal threshold (weak constraint)")}
  if(hyp.in=="Ncr.th.wk"){print("Primary-secondary: Nocturnal-crepuscular threshold (weak constraint)")}
  if(hyp.in=="CRd.th.wk"){print("Primary-secondary: Crepuscular-diurnal threshold (weak constraint)")}
  if(hyp.in=="CRn.th.wk"){print("Primary-secondary: Crepuscular-nocturnal threshold (weak constraint)")}
  
  if(hyp.in=="Dn.var.wk"){print("Primary-secondary: Diurnal-nocturnal variation (weak constraint)")}
  if(hyp.in=="Dcr.var.wk"){print("Primary-secondary: Diurnal-crepuscular variation (weak constraint)")}
  if(hyp.in=="Nd.var.wk"){print("Primary-secondary: Nocturnal-diurnal variation (weak constraint)")}
  if(hyp.in=="Ncr.var.wk"){print("Primary-secondary: Nocturnal-crepuscular variation (weak constraint)")}
  if(hyp.in=="CRd.var.wk"){print("Primary-secondary: Crepuscular-diurnal variation (weak constraint)")}
  if(hyp.in=="CRn.var.wk"){print("Primary-secondary: Crepuscular-nocturnal variation (weak constraint)")}

  #if(hyp.in=="Nc.max"){print("Primary-secondary: Nocturnal-cathemeral maximizing")}
  #if(hyp.in=="Dc.max"){print("Primary-secondary: Diurnal-cathemeral maximizing")}
  #if(hyp.in=="Dc.var"){print("Primary-secondary: Diurnal-cathemeral variation")}
  #if(hyp.in=="Nc.var"){print("Primary-secondary: Nocturnal-cathemeral variation")}
  #if(hyp.in=="CRc.var"){print("Primary-secondary: Crepuscular-cathemeral variation")}
  #if(hyp.in=="CRc.max"){print("Primary-secondary: Crepuscular-cathemeral maximizing")}
  
  
  

  if(hyp.in=="?"|hyp.in==0){cat("General threshold: ", paste(hyp.sets("list")[[1]], collapse = ' ')," \n\n",
                      "General threshold 2: ", paste(hyp.sets("list")[[2]], collapse = ' ')," \n\n",
                      "General maximizing: ", paste(hyp.sets("list")[[3]], collapse = ' '),"\n \n",
                      "General maximizing 2: ", paste(hyp.sets("list")[[4]], collapse = ' '),"\n \n",
                      "General variation: ", paste(hyp.sets("list")[[5]], collapse = ' '),"\n \n",
                      "General variation 2: ", paste(hyp.sets("list")[[6]], collapse = ' '),"\n \n",
                      "Primary-secondary threshold: ", paste(hyp.sets("list")[[7]], collapse = ' '),"\n\n",
                      "Primary-secondary maximizing: ", paste(hyp.sets("list")[[8]], collapse = ' '),"\n\n",
                      "Primary-secondary variation: ", paste(hyp.sets("list")[[9]], collapse = ' '),"\n\n",
                      "Primary-secondary threshold (weak): ", paste(hyp.sets("list")[[10]], collapse = ' '),"\n\n",
                      "Primary-secondary variation (weak): ", paste(hyp.sets("list")[[11]], collapse = ' '),
                      sep=" ")}
  
}  
  
  if(is.null(hyp.in)){cat("Hypotheses are either General or Primary-secondary (P-s).",
                        "General hypotheses are defined by primary use in one diel period without specifics about the other diel periods.", 
                        "P-s hypotheses are defined by primary and secondary diel periods.",
                        "P-s hypotheses can have a strong or weak constrain on the third probability. Strong constraints force the probability to be zero, and weak constraints do not.",
                        "Hypotheses are speicified by a maximazation, threshold, or variation type",
                        "Maximization is defined by the ordering of probabilities.",
                        "Threshold uses inputted xi value to define how large or small probabilities can be as well ordering",
                        "Variation uses ordering and epsilon values to define the range of upper and lower probabilities",
                        fill=30,sep="\n")}
  
  
  }
