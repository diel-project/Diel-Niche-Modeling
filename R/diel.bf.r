#' Calculate Bayes factor (internal function)
#'
#' Wrapper for multinomineq::bf_multinom 
#' @import multinomineq
#' @param y vector of frequencies
#' @param idx.mod vector of indices indicating which hypotheses to use from diel.setup
#' @param diel.setup provided by user or used default as diel.setup=diel.ineq()
#' @param reps The number of replicate sets of three frequencies
#' @param prior Prior used for bayes factors. NULL indicates equal weights.
#' @param n.mcmc The number of mcmc iterations
#' @param burnin Burnin for mcmc algorithim
#' @param n.cpu Currently fixed at 1
#' @param alt.optim Alternative optimization for bayes factors
#' @param prints Whether to print information
#' @param delta vector of error tolerances for equality thresholds
#' @return Internal list
#' @export
#' @keywords internal
#' @noRd
#' 
diel.bf=function(y,
                 idx.mod,
                 diel.setup,
                 reps,
                 prior,
                 n.mcmc,
                 burnin,
                 n.cpu,
                 alt.optim,
                 prints,
                 delta){
  

  #Define hypotheses set
  hyp.set=names(idx.mod)
  soft.zero=0.001
  #Empty Storage lists
  indicator=rep(0,length(idx.mod))
  bf=vector("list",length(idx.mod)); names(bf)=hyp.set

  if(isTRUE(prints)){message(paste0("Calculating Bayes Factors..."))}      
  #Loop through models/hyps that need to be fit
  for(i in 1:length(idx.mod)){
    
    #if bf_equality do this function
    if(diel.setup[[idx.mod[i]]]$func=="bf_equality"){
      #Get A matrix and b vector
      A=diel.setup[[idx.mod[i]]][[2]]
      b=diel.setup[[idx.mod[i]]][[3]]
      C=diel.setup[[idx.mod[i]]][[4]]
      d=diel.setup[[idx.mod[i]]][[5]]
      #Need to repeat A matrix the number of reps  
      A=do.call("cbind", rep(list(A), reps))
      C=do.call("cbind", rep(list(C), reps))

      bf[[i]]= try(
                   multinomineq::bf_equality(k=y,
                              options=rep(3,reps),
                              A=A,
                              b=b,
                              C=C,
                              d=d,
                              prior = rep(1,length(y)),
                              M1 = n.mcmc,
                              M2 = n.mcmc,
                              burnin=burnin,
                              delta = delta,
                              return_Ab = FALSE,
                              progress = FALSE)  
               ,silent=TRUE)
    
    if(any(grepl( "Error", bf[[i]], fixed = TRUE))){
        A=rbind(A,C)
        b=c(b,d)
        b[length(b)]=soft.zero
        
        bf[[i]]= try(
                      multinomineq::bf_multinom(k=y,options =  rep(3,reps),
                                  A=A, 
                                  b=b,
                                  M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                  prior = rep(1,length(y)),progress = FALSE)
        ,silent=TRUE)

    } #End if Error stament
    
    }#END IF bf_equality
    
    if(diel.setup[[idx.mod[i]]]$func=="bf_multinom"){
    #Get A matrix and b vector
    A=diel.setup[[idx.mod[i]]][[2]]
    b=diel.setup[[idx.mod[i]]][[3]]
    
    #Need to repeat A matrix the number of reps  
    A=do.call("cbind", rep(list(A), reps))
    #Calculate bayes factor
    if(isFALSE(alt.optim)){
      bf[[i]]= try(
        multinomineq::bf_multinom(k=y,options =  rep(3,reps),
                                  A=A, 
                                  b=b,
                                  M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                  prior = rep(1,length(y)),progress = FALSE)
        ,silent=TRUE)
      
      
    #IF two values in bf are na then automatically do the alterntaive model fitting  
    if(length(which(is.finite(bf[[i]][,1])))<2){
      if(isTRUE(prints)){
          options(warn=1)
          warning("Trying alternative model fitting process...please be patient")
          options(warn=0)
      }
             count.model=multinomineq::count_multinom(k=y,options = rep(3,reps),
                                               A=A, 
                                               b=b,
                                               steps=1:nrow(A),
                                               M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                               progress = FALSE)
      
      count.model.prior=multinomineq::count_multinom(k=0,options = rep(3,reps),
                                                     A=A, 
                                                     b=b,
                                                     steps=1:nrow(A),
                                                     M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                                     progress = FALSE)
      
      
      bf[[i]]= multinomineq::count_to_bf(posterior=count.model,prior=count.model.prior)
      
      
      
    }
      
    }else{
      #Alternative process to calculate bayes factors
      count.model=multinomineq::count_multinom(k=y,options = rep(3,reps),
                                               A=A, 
                                               b=b,
                                               steps=1:nrow(A),
                                               M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                               progress = FALSE)
      
      count.model.prior=multinomineq::count_multinom(k=0,options = rep(3,reps),
                                                     A=A, 
                                                     b=b,
                                                     steps=1:nrow(A),
                                                     M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                                     progress = FALSE)
      
      
      bf[[i]]= multinomineq::count_to_bf(posterior=count.model,prior=count.model.prior)
    }
    }#end bf_multinom IF
    
    #bf_nonlinear
    if(diel.setup[[idx.mod[i]]]$func=="bf_nonlinear"){
      bf[[i]]= try(
                    multinomineq::bf_nonlinear(k=y,options =  rep(3,reps),
                                  inside=diel.setup[[idx.mod[i]]]$inside,
                                  M=n.mcmc,cpu=n.cpu,
                                  prior = rep(1,length(y)),progress = FALSE)
                    ,silent=TRUE)
    if((is.infinite(bf[[i]][2,1]))){indicator[i]=2}
      }
      
    if(grepl("Error", bf[[i]][[1]])| all(is.na(bf[[i]][,1]))){indicator[i]=1}
    
  } #End model for loop
  
  ##############################
  #Create input for bayes factor calculation using text string
  #Remove models from bf that did not fit

  prior.postbf.hyp=construct.post.prob(bf,hyp.set,prior,indicator)
  indicator[which(indicator==2)]=1
  
#output results as list  
list(prior.postbf.hyp=prior.postbf.hyp$prior.postbf.hyp,
     bf=bf,
     idx.high.bf.model=prior.postbf.hyp$idx.high.bf.model,
     indicator=indicator)  

} #End function 
