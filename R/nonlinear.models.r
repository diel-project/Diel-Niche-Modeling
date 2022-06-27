#' Model Function for non-linear inequalities (Internal)
#'
#' Prepares data to plot hypotheses
#' @param x evaluated by multinomineq
#' @param hyp the name of the non-linear model to use
#' @return Internal list
#' @export
#' @keywords internal

nonlinear.model <- function(hyp){
  model.name=paste0("non.linear.data$",hyp)
  min(apply(abs(eval(parse(text=model.name))-x),1,sum))<0.01
}  
#Returns a TRUE or FALSE depending on model parameter combinations from the specific model

