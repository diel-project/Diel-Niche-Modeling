#' Samples posterior distributions
#'
#' Wrapper for multinomineq::sampling_multinom
#' @import multinomineq
#' @param y, vector of frequencies
#' @param idx.mod vector of indices indicating which hypotheses to use from diel.setup
#' @param diel.setup provided by user or used default as 'diel.setup=diel.ineq()'
#' @param bf.Ab.new Modified A matrix and b vector for those equality/inequality hypotheses
#' @param reps The number of replicate sets of three frequencies
#' @param n.chains The number of chains for posterior sampling
#' @param n.mcmc The number of mcmc iterations
#' @param burnin Burnin for mcmc algorithim
#' @param n.cpu Currently fixed at 1
#' @param indicator Indicates failed bayes factor
#' @param prints Whether to print information
#' @return Internal list
#' @export
#' @keywords internal
#' @noRd

diel.post=function(y,idx.mod,diel.setup,bf.Ab.new,reps,
                   n.chains,n.mcmc,burnin,n.cpu,
                   indicator,prints){

#the names of the models to use  
  hyp.set=names(idx.mod)
#Empty Storage lists  
  ppc.list=vector("list",length(idx.mod)); names(ppc.list)=hyp.set
  sampling.mcmc=gelm.diag=ppc.list

if(isTRUE(prints)){message(paste0("Posterior Sampling..."))}    
#Loop through models/hyps that need to be fit
  for(i in 1:length(idx.mod)){
    if(indicator[i]==0){
    if(diel.setup[[idx.mod[i]]]$func!="bf_nonlinear"){
      A=bf.Ab.new[[i]]$A
      b=bf.Ab.new[[i]]$b
    
      
        #sample posterior distributions
        sampling.mcmc[[i]]= try(
                              replicate(n = n.chains, simplify = FALSE,
                                        expr = multinomineq::sampling_multinom(k=y,options = rep(3,reps),
                                                                               A=A,b=b,
                                                                               M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                                                               progress = FALSE
                                                                               ))
                                  ,silent=TRUE)
      }
    
      if(diel.setup[[idx.mod[i]]]$func=="bf_nonlinear"){
                sampling.mcmc[[i]]= try(
                   replicate(n = n.chains, simplify = FALSE,
                                        expr = multinomineq::sampling_nonlinear(k=y,options = rep(3,reps),
                                                     inside=diel.setup[[idx.mod[i]]]$inside,                          
                                                      M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                                      progress = FALSE,eps = 1e-06))
                  
                  
                            
                                  ,silent=TRUE)
                                  

        
      }  
      
      
      #Process output as long as no error
      if(!is.null(sampling.mcmc[[i]]) & 
         isFALSE(any(grepl("Error", sampling.mcmc[[i]][[1]]))) &
         length(which(is.na( sampling.mcmc[[i]])))<100){
            sampling.mcmc[[i]]=coda::as.mcmc.list(sampling.mcmc[[i]])
            if(n.chains>1){gelm.diag[[i]]=coda::gelman.diag(sampling.mcmc[[i]], confidence = 0.95,multivariate = FALSE)}
        
            #Calculate posterior predictive check
            #skips this if y is all zero, indicating to simualte from the prior
            if(sum(y)>0){ppc.list[[i]]=multinomineq::ppp_multinom(sampling.mcmc[[i]],k=y,options=rep(3,reps))}
        
          for(l in 1:n.chains){
            #First, find the paired reps and assign them the same column name
            colnames(sampling.mcmc[[i]][[l]])=rep(1:100, each=2)[1:(reps*2)]
            
            #Sum each paired columns and subtract the sum from one to get the third probability for each pair
            temp=1-t(rowsum(t(sampling.mcmc[[i]][[l]]), group = sub("\\..*", "", colnames(sampling.mcmc[[i]][[l]]))))
            colnames(temp)=rep(1:reps,each=1)
    
            #Combine the estimated and derived probabilities
            sampling.mcmc[[i]][[l]]=cbind(sampling.mcmc[[i]][[l]],temp)
            #Reorder the columns and provide column names
            sampling.mcmc[[i]][[l]]=sampling.mcmc[[i]][[l]][,order(colnames(sampling.mcmc[[i]][[l]]))]
            colnames(sampling.mcmc[[i]][[l]])=paste(rep(c("p_crep","p_day","p_night"), times = reps), rep(1:(reps),each=3), sep = "_")
            sampling.mcmc[[i]][[l]]=coda::as.mcmc(sampling.mcmc[[i]][[l]])
          }#End for loop of chains
      }else{ppc.list[[i]]=c(NA,NA,NA)}#End error check ifelse   
    }else{ #Ifelse indicator
      sampling.mcmc[[i]]="Not Fit"
      names(sampling.mcmc[[i]])=hyp.set[i]
      ppc.list[[i]]=c(NA,NA,NA)
    } #End indicator
  } #End Model Loop

  #remove models not fit
  #ppc.list=ppc.list[indicator %in% "1" == FALSE]  
  #only do this if not simulating frrm prior
  if(sum(y)>0){
  ppc=data.frame(names(ppc.list),matrix(unlist(ppc.list),ncol=3,byrow = TRUE))
  colnames(ppc)<-c("Model","X2_obs","X2_pred","ppp")
  }else{ppc=NULL}
#output results as list  
  list(sampling.mcmc=sampling.mcmc,
       ppc=ppc,
       ppc.list=ppc.list,
       gelm.diag=gelm.diag)
    
} #End function
