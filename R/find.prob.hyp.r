#' Finding probabilities for a given hypothesis
#'
#' Function that inputs a given hypothesis and outputs as many possible
#' probability sets that match the diel hypothesis (i.e., satisfies the inequality constrints). Allows for inequalities, equalities, and non-linear inputs.
#' @param hyp hypothesis name, for example, D.max
#' @param diel.setup A list of created by diel.ineq(). Contains matrices A and vector b, or matrices A and C, and vectors b and d.
#' @param fast Default is TRUE, which uses p.options2 instead of p.options3. Does not apply to equality hypotheses.
#' @return A matrix of probabilities that match hypothesis specified by hyp
#' @examples 
#' find.prob.hyp(hyp ="D.max")
#' @export

find.prob.hyp=function(hyp, diel.setup = NULL,fast=TRUE){

# If diel.setup is not provided, the create it
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  if(
    !all(
      class(diel.setup) %in% c("list", "diel")
    )
  ){
    stop("diel.setup must be created via diel.ineq()")
  }

# Which set of data to load for bf_multinom hypotheses  
  if(isFALSE(fast)){p.opts=p.options3}
  if(isTRUE(fast)){p.opts=p.options2}

#Check hyp matching    
check.inputs(y=cbind(0,0,0),hyp.set=hyp,bf.fit=FALSE,prior=NULL,diel.setup=diel.setup,post.fit=FALSE,n.chains=1,
               n.mcmc=1,burnin=1,prints=FALSE,alt.optim=FALSE,delta=0)  
  
if(!is.logical(fast)){stop("fast needs to be logical")}
  
# Find hypotheses in diel.setup
  index.models=match(hyp,names(diel.setup))

# Inequality models  
  if(diel.setup[[index.models]]$func=="bf_multinom"){
    A=round(diel.setup[[index.models]][[2]],digits=5)
    b=round(diel.setup[[index.models]][[3]],digits=5)
  
    #Find all A %*% theta combinations
    p.ineq= round(matrix(apply(p.opts[1:2,],2,FUN=function(x){A%*%x}),nrow=nrow(A)),digits=4)
    #find if that is <= b
    p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  

    #Find where they are true
    index=which(p.ineq.logical)

    #These are the combinations of p's that match the constraints
    probs.out=t(p.opts[,index])
  }#End if

# Equality models      
  if(diel.setup[[index.models]]$func=="bf_equality"){  
    A=diel.setup[[index.models]][[2]]
    b=diel.setup[[index.models]][[3]]
    C=diel.setup[[index.models]][[4]]
    d=diel.setup[[index.models]][[5]]
  
    #Find all A %*% theta combinations
    p.ineq=  round(matrix(apply(p.options3[1:2,],2,FUN=function(x){A%*%x}),nrow=nrow(A)),digits=4)
    #find if that is <= b
    p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  
  
    #Find all C %*% theta combinations
    p.ineq2=  round(matrix(apply(p.options3[1:2,],2,FUN=function(x){C%*%x}),nrow=nrow(C)),digits=4)
  
    #find if abs(C*theta -d) < delta
    delta=0.001
    p.ineq.logical2= apply(p.ineq2,2,FUN=function(x){all(abs(x-d)<delta)})  
    p.ineq.logical=apply(data.frame(p.ineq.logical,p.ineq.logical2),1,FUN=function(x){all(x)})
  
    #Find where they are true
    index=which(p.ineq.logical)

 #These are the combinations of p's that match the constraints
  probs.out=t(p.options3[,index])
  
}

# Non-linear models - the data is stored in diel.setup  
  if(diel.setup[[index.models]]$func=="bf_nonlinear"){  
    probs.out=as.matrix(diel.setup[[index.models]]$data,ncol=3)
    #probs.out=as.matrix(diel.setup[[index.models]]$data,ncol=2)
    #probs.out=cbind(probs.out,1-apply(probs.out,1,sum))
    #probs.out=matrix(probs.out,ncol=3)
  }
  
# output  
  probs.out

} #end function