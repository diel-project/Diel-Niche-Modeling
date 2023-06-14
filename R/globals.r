#' Global Variables (internal)
#'
#' Plots the diel niche space and posterior distribution of a fitted model.
#' @importFrom utils globalVariables
#' @keywords internal
#' @name globalvars
#' @noRd

IsSmallerOrEqual <- function(a,b) {   
#if (   class(all.equal(a, b)) == "logical" && (a<b | all.equal(a, b))) { return(TRUE)
  if ( inherits(all.equal(a, b),"logical") && (a<b | all.equal(a, b))) { return(TRUE)
 } else if (a < b) { return(TRUE)
     } else { return(FALSE) }
}

utils::globalVariables(c("p.options", "p.options2", "non.linear.data"))

