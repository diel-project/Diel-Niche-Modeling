#' Kernel Overlap density integration
#'
#' Gets full name of hypothesis
#' @param ms.model most supported model/hypothesis
#' @return Internal list
#' @export
#' @keywords internal
#' 

full.hyp.name=function(ms.model){
np=NULL
if(ms.model=="D"){np="Diurnal (Traditional)"}
if(ms.model=="N"){np="Nocturnal (Traditional)"}
if(ms.model=="CR"){np="Crepuscular (Traditional)"}
if(ms.model=="C"){np="Cathemeral (Traditional)"}
if(ms.model=="C2"){np="Cathemeral (General)"}
if(ms.model=="D.CR"){np="Diurnal-Crepuscular (General)"}
if(ms.model=="D.N"){np="Diurnal-Nocturnal (General)"}
if(ms.model=="CR.N"){np="Crepuscular-Nocturnal (General)"}
if(ms.model=="D.max"){np="Diurnal (Maximization)"}
if(ms.model=="N.max"){np="Nocturnal (Maximization)"}
if(ms.model=="CR.max"){np="Crepuscular (Maximization)"}

if(ms.model=="D.var"){np="Diurnal (Variation)"}
if(ms.model=="N.var"){np="Nocturnal  (Variation)"}
if(ms.model=="CR.var"){np="Crepuscular  (Variation)"}
if(ms.model=="C.var"){np="Cathemeral  (Variation)"}
if(ms.model=="A.AV.var"){np="Available (Variation)"}

if(ms.model=="D.avail"){np="Day (Selection)"}
if(ms.model=="CR.avail"){np="Twilight (Selection)"}
if(ms.model=="N.avail"){np="Night (Selection)"}
if(ms.model=="D.CR.avail"){np="Day-Twilight (Selection)"}
if(ms.model=="N.CR.avail"){np="Night-Twilight (Selection)"}
if(ms.model=="D.N.avail"){np="Day-Night (Selection)"}
if(ms.model=="EQ.avail"){np="Available Equality (No Selection)"}


if(ms.model=="EC"){np="Even Cathemeral Equality"}
if(ms.model=="C.Max"){np="Even Cathemeral Equality Point"}
if(ms.model=="Uncon"){np="Unconstrained"}

if(is.null(np)){warning("Hypothesis code is not recognized \n")}

np

}#End function

