#' Kernel Overlap density integration
#'
#' Gets full name of hypothesis
#' @param ms.model most supported model/hypothesis
#' @return Internal list
#' @export
#' @keywords internal
#' 

print.hyp.name=function(ms.model){
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
if(ms.model=="N.max"){np="Diurnal (Maximization)"}
if(ms.model=="CR.max"){np="Diurnal (Maximization)"}


if(is.null(np)){warning("Hypothesis code is not recognized")}

np

}#End function

