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
    stop("The data y needs to be only numeric  \n")
  }
  
  if(all(hyp.set%in%names(diel.setup))!=TRUE){
    stop("Check that hyp.set match names in diel.setup \n")
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
}#End function