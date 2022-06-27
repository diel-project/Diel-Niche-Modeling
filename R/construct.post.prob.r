construct.post.prob=function(bf,hyp.set,prior,indicator){
  
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
    
    
    list(prior.postbf.hyp=prior.postbf.hyp,
         idx.high.bf.model=idx.high.bf.model)
} #End function
