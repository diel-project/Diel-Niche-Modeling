diel.post=function(y,idx.mod,diel.setup,reps,
                   n.mcmc,burnin,n.cpu,
                   indicator,prints){

#the names of the models to use  
  hyp.set=names(idx.mod)

#Empty Storage lists  
  ppc.list=vector("list",length(idx.mod)); names(ppc.list)=hyp.set
  sampling.mcmc=ppc.list
  
  #Loop through models/hyps that need to be fit
  for(i in 1:length(idx.mod)){
    
    #Get A matrix and b vector
    A=diel.setup[[idx.mod[i]]][[2]]
    b=diel.setup[[idx.mod[i]]][[3]]
    
    #Need to repeat A matrix the number of reps  
    A=do.call("cbind", rep(list(A), reps))
    
    if(indicator[i]==0){
      #sample posterior distributions
      
      sampling.mcmc[[i]]= try(
#        retry::retry(
          multinomineq::sampling_multinom(k=y,options = rep(3,reps),
                                          A=A, 
                                          b=b,
                                          M=n.mcmc,cpu=n.cpu,burnin=burnin,progress = FALSE)
 #         ,silent=TRUE,max_tries=3, until = ~ nrow(.) > 0)
        ,silent=TRUE)
      
      
      #can not be empty or contain an error
      if(!is.null(sampling.mcmc[[i]]) & 
         isFALSE(grepl("Error", sampling.mcmc[[i]][[1]])) &
         length(which(is.na( sampling.mcmc[[i]])))<100
      ){
        #Calculate posterior predictive check 
        ppc.list[[i]]=multinomineq::ppp_multinom(sampling.mcmc[[i]],k=y,options=rep(3,reps))
        
        #First, find the paired reps and assign them the same column name
        colnames(sampling.mcmc[[i]])=rep(1:100, each=2)[1:(reps*2)]
        
        #Sum each paired columns and subtract the sum from one to get the third probability for each pair
        temp=1-t(rowsum(t(sampling.mcmc[[i]]), group = sub("\\..*", "", colnames(sampling.mcmc[[i]]))))
        colnames(temp)=rep(1:reps,each=1)
        
        #Combine the estimated and derived probabilities
        sampling.mcmc[[i]]=cbind(sampling.mcmc[[i]],temp)
        #Reorder the columns and provide column names
        sampling.mcmc[[i]]=sampling.mcmc[[i]][,order(colnames(sampling.mcmc[[i]]))]
        colnames(sampling.mcmc[[i]])=paste(rep(c("p_crep","p_day","p_night"), times = reps), rep(1:(reps),each=3), sep = "_")
        
        
      }else{ppc.list[[i]]=c(NA,NA,NA)}#End else   
    }else{
      sampling.mcmc[[i]]="Not Fit"
      names(sampling.mcmc[[i]])=hyp.set[i]
      ppc.list[[i]]=c(NA,NA,NA)
    } #End indicator

  } #End for loop

  #remove models not fit
  #ppc.list=ppc.list[indicator %in% "1" == FALSE]  
  ppc=data.frame(names(ppc.list),matrix(unlist(ppc.list),ncol=3,byrow = TRUE))
  colnames(ppc)<-c("Model","X2_obs","X2_pred","ppp")
  
  
  list(sampling.mcmc=sampling.mcmc,
       ppc=ppc,
       ppc.list=ppc.list)
    
}