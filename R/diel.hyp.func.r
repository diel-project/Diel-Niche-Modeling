#' Diel Modeling 
#'
#' Diel model hypotheses evaluation and parameter estimation.
#' This is essentially a wrapper function for functions provided by the package ‘multinomineq’.
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'.
#' @param y Vector of frequencies in order of twilight, day, night.
#' @param hyp.set Vector of diel hypotheses names representing hypotheses set or individual hypotheses.
#' @param n.mcmc Number of mcmc iterations.
#' @param n.cpu Number of threads for each model fit, which is parallelized by core.
#' @param burnin Burn-in number of mcmc iterations.
#' @return A list of outputs, including bayes factors for a model set, model bayes factor inputs, posterior samples, warning indicator, and posterior predictive checks.

#' @return A list of outputs
#' \item{bf.final}{Bayes factor for hyopthesis set}
#' \item{bf}{A list of ordered individual model bayes factor inputs}  
#' \item{posteriors}{A list of ordered matrices for model posterior distributions}  
#' \item{most.supported.model}{Character name of the most supported model detrerminded by the maximum probability of support from the bayes factors}  
#' \item{indicator.highest.bf.model}{An indicator of which ordered model is the most supported model}  
#' \item{indicator}{A vector indicating which models had errors and results were not returned. A 1 indicates a model was not fit and 0 indicates no errors.}  
#' \item{ppc}{A list of ordered model posterior predictive check output}
#' \item{ppc.ms}{Posterior predictive check output from the most supported model}    
#' \item{model.ms}{Posterior distributions of the most supported model}    
#' @examples 
#' diel.hypotheses.func(diel.setup=diel.setup,y=y,hyp.set=hyp.set,
#'                     n.mcmc=2000,n.cpu=1,burnin=500);
#'                     
#' Required libraries:   multinomineq, retry, MASS
#' @importFrom MASS fractions
#' @importFrom retry retry
#' @import multinomineq
#' @export

diel.hypotheses.func=function(diel.setup,y,hyp.set,
                              n.mcmc,n.cpu,burnin,cath.avail){


#These are the list items of A and b that need to be fit based on hyp.set
  index.models=match(hyp.set,names(diel.setup))

#Empty Storage lists  
  ppc.list=vector("list",length(index.models))
  names(ppc.list)=hyp.set
  sampling.mcmc=bf=ppc.list
  indicator=rep(0,length(index.models))

#Loop through models/hyps that need to be fit
for(i in 1:length(index.models)){

  #Calculate bayes factor
      bf[[i]]= try(retry::retry(
                        multinomineq::bf_multinom(k=y,options = length(y),
                            A=diel.setup[[index.models[i]]][[2]], 
                            b=diel.setup[[index.models[i]]][[3]],
                            M=n.mcmc,cpu=n.cpu,burnin=burnin,
                            prior = rep(1,length(y)),progress = FALSE)
              ,silent=TRUE,max_tries=10, until = ~ nrow(.) > 1)
              ,silent=TRUE)
  
  if(grepl("Error", bf[[i]][[1]])| all(is.na(bf[[i]][,1]))){indicator[i]=1}
  
  
  
  if(indicator[i]==0){
  #sample posterior distributions
 sampling.mcmc[[i]]= try(
                     retry::retry(
                              multinomineq::sampling_multinom(k=y,options = length(y),
                                    A=diel.setup[[index.models[i]]][[2]], 
                                    b=diel.setup[[index.models[i]]][[3]],
                                    M=n.mcmc,cpu=n.cpu,burnin=burnin,progress = FALSE)
                        ,silent=TRUE,max_tries=10, until = ~ nrow(.) > 1)
                    ,silent=TRUE)

 
 #can not be empty or contain an error
 if(!is.null(sampling.mcmc[[i]]) & 
    isFALSE(grepl("Error", sampling.mcmc[[i]][[1]])) &
    length(which(is.na( sampling.mcmc[[i]])))<100
    ){
  #Calculate posterior predictive check 
    ppc.list[[i]]=ppp_multinom(sampling.mcmc[[i]],k=y,options=length(y))
  
  #Estimate last probability by subtraction   
    sampling.mcmc[[i]]=cbind(sampling.mcmc[[i]],1-apply(sampling.mcmc[[i]],1,sum))
    colnames(sampling.mcmc[[i]])=c("p_crep","p_day","p_night")
    }else{
      ppc.list[[i]]=c(NA,NA,NA)
 }#End else   
} #End indicator
} #End for loop

##############################
#Create input for bayes factor calculation using text string
  
#Which of bf list to include- need to maybe modify 1:4
#model.inputs=apply(as.matrix(1:4),1,FUN=function(x){paste0("bf[[",x,"]],")})
#model.inputs=paste(model.inputs,collapse=" ")

#Remove models from bf that did not fit
  hyp.set2=hyp.set[indicator!=1]
  model.inputs=apply(as.matrix(hyp.set2),1,FUN=function(x){paste0("bf$",x,",")})
  model.inputs=paste(model.inputs,collapse=" ")

#Create text string for prior- assuming equal weight
  prior.num=as.character(MASS::fractions(rep(1/length(hyp.set2),length(hyp.set2))))
  temp=data.frame(hyp.set2,prior.num)

  prior.inputs=apply(temp,1,FUN=function(x){paste0(x[1]," = ",x[2])})
  prior.inputs=paste(prior.inputs,collapse=", ")

#additional text strings
text1="postprob("
text3=c("prior=c(")
text5=c(",include_unconstr = FALSE)")

#put it all together
form.in=paste(c(text1,model.inputs,text3,prior.inputs,text5,")"),collapse="")

#evaluate bayes factor
prior.postbf.hyp=eval(str2lang(form.in))
prior.postbf.hyp=prior.postbf.hyp[-nrow(prior.postbf.hyp),]

#show warning if there is an issue with model fitting
#if(sum(indicator)>0){
#  warning=indicator
#  print(data.frame(hyp.set,warning))
#}

if(length(hyp.set)>1){
#Identify posteriors of most supported model
  indicator.highest.bf.model=which.max(prior.postbf.hyp[,2])
  most.supported.model=sampling.mcmc[[indicator.highest.bf.model]]
}else{
 indicator.highest.bf.model=1 
 most.supported.model=sampling.mcmc[[indicator.highest.bf.model]]
}
  #remove models not fit
  ppc.list=ppc.list[indicator %in% "1" == FALSE]  
    
ppc=data.frame(names(ppc.list),matrix(unlist(ppc.list),ncol=3,byrow = TRUE))
colnames(ppc)<-c("Model","X2_obs","X2_pred","ppp")

ppc.ms=ppc[indicator.highest.bf.model,]

model.ms=hyp.set2[indicator.highest.bf.model]

cat("The most supported model was: \n", 
          model.ms,"\n")

if(sum(indicator)>0){
warning("Model(s) not fit and were removed: \n",
        hyp.set[indicator==1],call.=FALSE)
}

#Output list
list(bf.final=prior.postbf.hyp,bf=bf,posteriors=sampling.mcmc,
     most.supported.model=most.supported.model,indicator.highest.bf.model=indicator.highest.bf.model,
     indicator=indicator,ppc=ppc,ppc.ms=ppc.ms,model.ms=model.ms)

}#end function
