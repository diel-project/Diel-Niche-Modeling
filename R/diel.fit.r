#' Diel Modeling 
#'
#' Diel model hypotheses evaluation and parameter estimation.
#' This is essentially a wrapper function for functions provided by the package multinomineq.
#' @param y a matrix of frequencies of animal detections. Each row is a replicate dataset. Rows should be limited when using P-s hyps (1 or 2). The matrix should always be three columns in this order: twilight, day, night.
#' @param hyp.set Vector of diel hypotheses names representing hypotheses set or individual hypotheses.
#' @param bf.fit If TRUE, will calculate bayes factors for the model sit. Default is TRUE.
#' @param prior Prior probabilities for models used in bayes factors. Defaults to equal among models. 
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'. If not provided, it will use the defaults of the diel.ineq function.
#' @param n.mcmc Number of mcmc iterations.
#' @param burnin Burn-in number of mcmc iterations.
#' @param prints Whether to print messages about model fitting.
#' @param alt.optim Default is FALSE. If TRUE, uses an alternative approach to derive the bayes factors. It can be more stable, but takes a bit longer.
#' @return A list of outputs, including bayes factors for a model set, model bayes factor inputs, posterior samples, warning indicator, and posterior predictive checks.

#' @return A list of outputs
#' \item{bf.table}{Bayes factor for hyopthesis set}
#' \item{bf}{A list of ordered individual model bayes factor inputs}  
#' \item{post.samp}{A list of ordered matrices for model posterior distributions}  
#' \item{ms.model}{The name of the most supported model detrerminded by the maximum probability of support from the bayes factors}  
#' \item{ppc}{A list of ordered model posterior predictive check output}
#' \item{ms.ppc}{Posterior predictive check output from the most supported model}    
#' \item{post.samp.ms.model}{Posterior distributions of the most supported model}    
#' @examples 
#' diel.hypotheses.func(y=t(matrix(c(10,100,10))),hyp.set=hyp.sets("hyp.th"))
#'                     
#' Required libraries:   multinomineq, retry, MASS
#' @importFrom MASS fractions
#' @importFrom retry retry
#' @import multinomineq
#' @export

diel.fit=function(y,
                  hyp.set,
                  bf.fit=TRUE,
                  diel.setup=NULL,
                  prior=NULL,
                  n.mcmc=50000,
                  burnin=10000,
                  prints=TRUE,
                  alt.optim=FALSE){

###################################
#NOTES 
#multinomineq uses n.cpu number of threads. But it changes the data structure
#into lists. fix this for now and flexibility later.
  n.cpu=1
###################################    
#Define variables
  n.mcmc=as.integer(round(n.mcmc,digits=0))
  burnin=as.integer(round(burnin,digits=0))
###################################    
# If there is no diel setup then use defaults
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
###################################    
#Check the inputs  
  Diel.Niche:::check.inputs(y=y,hyp.set=hyp.set,bf.fit=bf.fit,prior=prior,diel.setup=diel.setup)
###################################    
#setup data for model fitting
    reps= nrow(y)
    y.vec=c(t(y))
    names(y.vec)=paste(rep(c("p_crep","p_day","p_night"), times = reps), rep(1:(reps),each=3), sep = "_")
###################################      
  
#These are the list items of A and b that need to be fit based on hyp.set
  idx.mod=match(hyp.set,names(diel.setup))
  names(idx.mod)=hyp.set
#########################################  
#Function for bayes factors
if(isTRUE(bf.fit) & length(idx.mod)>1){  
  bf.out=Diel.Niche:::diel.bf(y=y.vec,idx.mod=idx.mod,
             reps=reps,diel.setup=diel.setup,
             prior=prior,
             n.mcmc=n.mcmc,burnin=burnin,n.cpu=n.cpu,
             alt.optim=alt.optim)
  
  bf.table=bf.out$prior.postbf.hyp
  bf=bf.out$bf
  indicator=bf.out$indicator
}else{
  bf.table=NULL
  bf=NULL
  indicator=rep(0,length(idx.mod))
}
#########################################  
#fit models
  post.samples=Diel.Niche:::diel.post(y=y.vec,idx.mod,diel.setup=diel.setup,reps=reps,
                         n.mcmc=n.mcmc,burnin=burnin,n.cpu=n.cpu,
                         indicator=indicator)
#########################################    

  if(length(hyp.set)>1 & isTRUE(bf.fit) & !is.na(bf.out$idx.high.bf.model)){
    ms.model=bf.out$idx.high.bf.model
    idx.ms=which(names(post.samples$sampling.mcmc) %in% ms.model)
    post.samp.ms.model=post.samples$sampling.mcmc[[idx.ms]]
    idx.ms2=which(names(post.samples$ppc.list) %in% ms.model)
    ms.ppc=post.samples$ppc.list[[idx.ms2]]
    
  }else{
    post.samp.ms.model=NULL
    idx.ms=NULL
    ms.ppc=NULL
    ms.model=NULL
  }    

  
  if(prints==TRUE & isTRUE(bf.fit)){
    cat("The most supported model is: \n", ms.model,"\n")
    if(sum(indicator)>0){
      warning("Models that were not fit: \n",
              paste(hyp.set[indicator==1],rep("\n",length(hyp.set[indicator==1]))),call.=FALSE)
    }
  }

    
  #Output list
  list(bf.table=bf.table,
       post.samp=post.samples$sampling.mcmc,
       ms.model=ms.model,
       ppc=post.samples$ppc,ms.ppc=ms.ppc,
       post.samp.ms.model=post.samp.ms.model,
       y=y,y.vec=y.vec)
  
}#end function
