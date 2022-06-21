#' Setup plotting hypotheses (Internal)
#'
#' Prepares data to plot hypotheses
#' @param diel.setup provided by user or used default as diel.setup=diel.ineq()
#' @param index.models vector of indices indicating which hypotheses to use from diel.setup
#' @param ... Other parameters
#' @return Internal list
#' @export
#' @keywords internal

setup.hyp.plot.params=function(diel.setup,index.models,...){

#Loop through hypotheses and get many sample points  
plot.points=vector("list",length(index.models))
 for(i in 1:length(index.models)){

if(diel.setup[[index.models[i]]]$func=="bf_multinom"){
  A=diel.setup[[index.models[i]]][[2]]
  b=diel.setup[[index.models[i]]][[3]]
}else{
  A=diel.setup[[index.models[i]]][[2]]
  b=diel.setup[[index.models[i]]][[3]]
  C=diel.setup[[index.models[i]]][[4]]
  d=diel.setup[[index.models[i]]][[5]]
  A=rbind(A,C)
  b=c(b,d)
}
  
 #Find all A %*% theta combinations
 p.ineq= apply(p.options2,2,FUN=function(x){A%*%x})  
 #find if that is <= b
 p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  

 index=which(p.ineq.logical)

 #These are the combinations of p's that match the constraints
  p.plot=t(p.options2[,index])
  
  #now add back in the third prob
  p.plot=cbind(p.plot,1-apply(p.plot,1,sum))
  colnames(p.plot)=c("p.crep","p.day","p.night")
  #head(p.plot)
  
  #make sure there are not mistakes
  index.remove=which(p.plot[,3]<0 | p.plot[,3]>1)
  if(length(index.remove)>0){p.plot=p.plot[-index.remove,]}
  
  plot.points[[i]]=p.plot
  
 }
  plot.points
  
}