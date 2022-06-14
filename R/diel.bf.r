diel.bf=function(y,
                 idx.mod,
                 diel.setup,
                 reps,
                 prior,
                 n.mcmc,
                 burnin,
                 n.cpu,
                 alt.optim){
  

  hyp.set=names(idx.mod)

  #Empty Storage lists
  indicator=rep(0,length(idx.mod))
  bf=vector("list",length(idx.mod)); names(bf)=hyp.set
  
  #Loop through models/hyps that need to be fit
  for(i in 1:length(idx.mod)){
    
    #if there is a 2 do this function
    if(grepl("2", diel.setup[[idx.mod[i]]]$Name, fixed = TRUE)){
    #Get A matrix and b vector
    A=diel.setup[[idx.mod[i]]][[2]]
    b=diel.setup[[idx.mod[i]]][[3]]
    C=diel.setup[[idx.mod[i]]][[4]]
    d=diel.setup[[idx.mod[i]]][[5]]
    #Need to repeat A matrix the number of reps  
    A=do.call("cbind", rep(list(A), reps))
    C=do.call("cbind", rep(list(C), reps))

    
    bf[[i]]= try(multinomineq::bf_equality(k=y,
                              options=rep(3,reps),
                              A=A,
                              b=b,
                              C=C,
                              d=d,
                              prior = rep(1,length(y)),
                              M1 = n.mcmc,
                              M2 = n.mcmc,
                              burnin=burnin,
                              delta = 0.5^(1:8))  
      ,silent=TRUE)
    }
    
    #Get A matrix and b vector
    A=diel.setup[[idx.mod[i]]][[2]]
    b=diel.setup[[idx.mod[i]]][[3]]
    
    #Need to repeat A matrix the number of reps  
    A=do.call("cbind", rep(list(A), reps))
    
    #Calculate bayes factor
    if(isFALSE(alt.optim)){
      bf[[i]]= try(
        #retry::retry(
        multinomineq::bf_multinom(k=y,options =  rep(3,reps),
                                  A=A, 
                                  b=b,
                                  M=n.mcmc,cpu=n.cpu,burnin=burnin,
                                  prior = rep(1,length(y)),progress = FALSE)
        #,silent=TRUE,max_tries=3, until = ~ nrow(.) > 1)
        ,silent=TRUE)
      
      
    #IF two values in bf are na then automatically do the alterntaive model fitting  
    if(length(which(is.infinite(bf[[i]][,1])))>1){
            warning("Trying alternative model fitting process...please be patient")

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
    
    if(grepl("Error", bf[[i]][[1]])| all(is.na(bf[[i]][,1]))){indicator[i]=1}
    
  } #End model for loop
  
  ##############################
  #Create input for bayes factor calculation using text string
  
  #Remove models from bf that did not fit
    hyp.set2=hyp.set[indicator!=1]
    model.inputs=apply(as.matrix(hyp.set2),1,FUN=function(x){paste0("bf$",x,",")})
    model.inputs=paste(model.inputs,collapse=" ")
  
  #Create text string for prior- assuming equal weight
  if(is.null(prior)){
    prior.num=as.character(MASS::fractions(rep(1/length(hyp.set2),length(hyp.set2))))
    temp=data.frame(hyp.set2,prior.num)
    
    prior.inputs=apply(temp,1,FUN=function(x){paste0(x[1]," = ",x[2])})
    prior.inputs=paste(prior.inputs,collapse=", ")
  }else{
    #User provided prior
    prior.num=as.character(MASS::fractions(prior[indicator!=1]))
    temp=data.frame(hyp.set2,prior.num)
    prior.inputs=apply(temp,1,FUN=function(x){paste0(x[1]," = ",x[2])})
    
    prior.inputs=paste(prior.inputs,collapse=", ")
  }    
  
  #additional text strings
  text1="multinomineq::postprob("
  text3=c("prior=c(")
  text5=c(",include_unconstr = FALSE)")
  
  #put it all together
  form.in=paste(c(text1,model.inputs,text3,prior.inputs,text5,")"),collapse="")
  
  #evaluate bayes factor
  prior.postbf.hyp=eval(str2lang(form.in))
  prior.postbf.hyp=prior.postbf.hyp[-nrow(prior.postbf.hyp),]
  prior.postbf.hyp=matrix(prior.postbf.hyp,ncol=2)
  rownames(prior.postbf.hyp)=hyp.set2
  colnames(prior.postbf.hyp)=c("Prior","Posterior")
  
  #NOTE- there will be an error if there is no maximum, such as all zeros and and NA
  if(isFALSE(any(is.na(matrix(prior.postbf.hyp,ncol=2)[,2])))){ #check for NA's in bf table
    if(length(hyp.set)>1){
      #Identify posteriors of most supported model
      idx.high.bf.model=row.names(prior.postbf.hyp)[which.max(prior.postbf.hyp[,2])]
    }
  }else{
    idx.high.bf.model=NA
  }
  

  #Put missing models back in
  if(sum(indicator)>0){
    temp.models=  matrix(NA,nrow=length(hyp.set[indicator==1]),ncol=2)
    colnames(temp.models)  =c("Prior","Posterior")
    rownames(temp.models)  =hyp.set[indicator==1]
    
    prior.postbf.hyp=rbind(prior.postbf.hyp,temp.models)
    reorder=match(rownames(prior.postbf.hyp),hyp.set)
    prior.postbf.hyp=prior.postbf.hyp[reorder,]
  }
  
  
list(prior.postbf.hyp=prior.postbf.hyp,bf=bf,
     idx.high.bf.model=idx.high.bf.model,
     indicator=indicator)  

} #End function loop

