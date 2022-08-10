#' Check on inputs
#'
#' A function to check the inputs provided through the function 'diel.fit'
#' @param y vector of frequencies
#' @param hyp.set the hypotheses to use
#' @param prior Prior used for bayes factors. NULL indicates equal weights.
#' @param bf.fit Indicator whether to fit bayes factors
#' @param diel.setup provided by user or used default as 'diel.setup=diel.ineq()'
#' @return Internal list
#' @export
#' @keywords internal

check.inputs=function(y,hyp.set,prior,bf.fit,diel.setup){
  
  if(!is.null(prior) & isTRUE(bf.fit)){
    if(length(prior)!=length(hyp.set)){
      stop("The prior is not the same length as hyp.set  \n")
    }
    if(sum(prior)!=1){
      stop("The sum of the prior vector has to be 1  \n")
    }
    if(!is.numeric(prior)){
     stop("The prior is not numeric  \n")
    }
    if(!is.vector(prior)){
      stop("The prior is not a vector  \n")
    }
  }
  
  
  if(!is.numeric(y)){
    stop("The data y needs to be numeric  \n")
  }
  
  if(all(hyp.set%in%names(diel.setup))!=TRUE){
    stop("Check that hyp.set match names in diel.setup \n",
         "Cannot find ",paste(hyp.set[which(hyp.set%in%names(diel.setup)==FALSE)], collapse = ' '), "\n")
    
    
  }
  
  if(!is.character(hyp.set) | !is.vector(hyp.set)){
    stop("hyp.set is not a vector or are not characters  \n")
  }
  
  if(!is.matrix(y)){
    stop("y needs to be a matrix  \n")
  }
  
  if(ncol(y)!=3){
    stop("y needs to have three columns \n")
  }
############################################  
  #Need to check all the inputs for inequalities
  if(diel.setup$inputs$e.D<0.01 | diel.setup$inputs$e.D>0.25){
    stop("diel.setup$inputs$e.D needs to be between 0.01-0.25 \n")
  }
  if(diel.setup$inputs$e.N<0.01 | diel.setup$inputs$e.N>0.25){
    stop("diel.setup$inputs$e.N needs to be between 0.01-0.25 \n")
  }
  if(diel.setup$inputs$e.CR<0.01 | diel.setup$inputs$e.CR>0.25){
    stop("diel.setup$inputs$e.CR needs to be between 0.01-0.25 \n")
  }
  if(diel.setup$inputs$e.EC<0.01 | diel.setup$inputs$e.EC>0.25){
    stop("diel.setup$inputs$e.EC needs to be between 0.01-0.25 \n")
  }
  if(diel.setup$inputs$e.AV<0.01 | diel.setup$inputs$e.AV>0.25){
    stop("diel.setup$inputs$e.AV needs to be between 0.01-0.25 \n")
  }
  if(any(diel.setup$inputs$xi.D<0) | any(diel.setup$inputs$xi.D>1)){
    stop("diel.setup$inputs$xi.D needs to be between 0-1 \n")
  }
  if(any(diel.setup$inputs$xi.EC<0) | any(diel.setup$inputs$xi.EC>1)){
    stop("diel.setup$inputs$xi.EC needs to be between 0-1 \n")
  }
  if(any(diel.setup$inputs$xi.CR<0) | any(diel.setup$inputs$xi.CR>1)){
    stop("diel.setup$inputs$xi.CR needs to be between 0-1 \n")
  }
  if(any(diel.setup$inputs$xi.N<0) | any(diel.setup$inputs$xi.N>1)){
    stop("diel.setup$inputs$xi.N needs to be between 0-1 \n")
  }
  if(diel.setup$inputs$xi.D[2]-diel.setup$inputs$e.D < 0 |
     diel.setup$inputs$xi.D[2]+diel.setup$inputs$e.D >1){
    stop("xi.D[2] \u00B1 e.D has to be between 0-1 \n")
  }
  if(diel.setup$inputs$xi.CR[2]-diel.setup$inputs$e.CR < 0 |
     diel.setup$inputs$xi.CR[2]+diel.setup$inputs$e.CR >1){
    stop("xi.CR[2] \u00B1 e.CR has to be between 0-1 \n")
  }
  if(diel.setup$inputs$xi.N[2]-diel.setup$inputs$e.N < 0 |
     diel.setup$inputs$xi.N[2]+diel.setup$inputs$e.N >1){
    stop("xi.N[2] \u00B1 e.N has to be between 0-1 \n")
  }
  if(diel.setup$inputs$xi.EC[1]-diel.setup$inputs$e.EC < 0 |
     diel.setup$inputs$xi.EC[1]+diel.setup$inputs$e.EC >1){
    stop("xi.EC[3] \u00B1 e.EC has to be between 0-1 \n")
  }
  if(diel.setup$inputs$xi.min.dom[1]<=0.5){
    stop("xi.min.dom has to be greater that 0.5 \n")
  }
  
}#End function