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
  if(hyp.in=="D.th"){print("General: Diurnal Threshold Hypothesis")}
  if(hyp.in=="N.th"){print("General: Nocturnal Threshold Hypothesis")}
  if(hyp.in=="CR.th"){print("General: Crepuscular Threshold Hypothesis")}
  if(hyp.in=="EC.th"){print("General: Even Cathemeral Threshold Hypothesis")}
  if(hyp.in=="D.max"){print("General: Diurnal Maximizing Hypothesis")}
  if(hyp.in=="N.max"){print("General: Nocturnal Maximizing Hypothesis")}
  if(hyp.in=="CR.max"){print("General: Crepuscular Maximizing Hypothesis")}
  if(hyp.in=="D.var"){print("General: Diurnal Variation Hypothesis")}
  if(hyp.in=="N.var"){print("General: Nocturnal Variation Hypothesis")}
  if(hyp.in=="CR.var"){print("General: Crepuscular Variation Hypothesis")}
  if(hyp.in=="EC.var"){print("General: Even Cathemeral Variation Hypothesis")}
  if(hyp.in=="C.var"){print("General: Cathemeral Variation Hypothesis")}

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
  if(hyp.in=="Dc.max"){print("Primary-secondary: Diurnal-cathemeral maximizing")}
  if(hyp.in=="Dcr.max"){print("Primary-secondary: Diurnal-crepuscular maximizing")}
  if(hyp.in=="Nd.max"){print("Primary-secondary: Nocturnal-diurnal maximizing")}
  if(hyp.in=="Nc.max"){print("Primary-secondary: Nocturnal-cathemeral maximizing")}
  if(hyp.in=="Ncr.max"){print("Primary-secondary: Nocturnal-crepuscular maximizing")}
  if(hyp.in=="CRd.max"){print("Primary-secondary: Crepuscular-diurnal maximizing")}
  if(hyp.in=="CRn.max"){print("Primary-secondary: Crepuscular-nocturnal maximizing")}
  if(hyp.in=="CRc.max"){print("Primary-secondary: Crepuscular-cathemeral maximizing")}
  
  if(hyp.in=="Dn.var"){print("Primary-secondary: Diurnal-nocturnal variation")}
  if(hyp.in=="Dc.var"){print("Primary-secondary: Diurnal-cathemeral variation")}
  if(hyp.in=="Dcr.var"){print("Primary-secondary: Diurnal-crepuscular variation")}
  if(hyp.in=="Nd.var"){print("Primary-secondary: Nocturnal-diurnal variation")}
  if(hyp.in=="Nc.var"){print("Primary-secondary: Nocturnal-cathemeral variation")}
  if(hyp.in=="Ncr.var"){print("Primary-secondary: Nocturnal-crepuscular variation")}
  if(hyp.in=="CRd.var"){print("Primary-secondary: Crepuscular-diurnal variation")}
  if(hyp.in=="CRn.var"){print("Primary-secondary: Crepuscular-nocturnal variation")}
  if(hyp.in=="CRc.var"){print("Primary-secondary: Crepuscular-cathemeral variation")}

  hyp.set=vector("list",6)
  hyp.set[[1]]=c("D.th","N.th","CR.th","EC.th")     #General Hypotheses
  hyp.set[[2]]=c("D.max","N.max","CR.max")          #General Hypotheses
  hyp.set[[3]]=c("D.var","N.var","CR.var","EC.var") #General Hypotheses
  hyp.set[[4]]=c("Dn.th","Dc.th","Dcr.th","Nd.th","Nc.th","Ncr.th","CRd.th","CRn.th","CRc.th")          #Primary-secondary Hypotheses
  hyp.set[[5]]=c("Dn.max","Dc.max","Dcr.max","Nd.max","Nc.max","Ncr.max","CRd.max","CRn.max","CRc.max") #Primary-secondary Hypotheses
  hyp.set[[6]]=c("Dn.var","Dc.var","Dcr.var","Nd.var","Nc.var","Ncr.var","CRd.var","CRn.var","CRc.var") #Primary-secondary Hypotheses

  if(hyp.in=="?"|hyp.in==0){cat("General maximizing: ", paste(hyp.set[[2]], collapse = ' ')," \n\n",
                      "General threshold: ", paste(hyp.set[[1]], collapse = ' ')," \n\n",
                      "General variation: ", paste(hyp.set[[3]], collapse = ' '),"\n \n",
                      "Primary-secondary maximizing: ", paste(hyp.set[[5]], collapse = ' '),"\n\n",
                      "Primary-secondary threshold: ", paste(hyp.set[[4]], collapse = ' '),"\n\n",
                      "Primary-secondary variation: ", paste(hyp.set[[6]], collapse = ' '),
                      sep=" ")}
  
}  
  
  if(is.null(hyp.in)){cat("Hypotheses are either General or Primary-secondary.",
                        "General hypotheses are defined by primary use in one diel period without specifics about the other diel periods.", 
                         "Primary-secondary hypotheses are defined by primary and secondary diel periods.",
                        "Each hypothesis is speicified by a maximazation, threshold, or variation type",
                        "Maximization is defined by the ordering of probabilities.",
                        "Threshold uses inputted xi value to define how large or small probabilities can be as well ordering",
                        "Variation uses ordering and epsilon values to define the range of upper and lower probabilities",
                        fill=30,sep="\n")}
  
  
  }
