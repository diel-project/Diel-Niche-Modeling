#' Plot Diel Hypothesis or Hypothesis Set Along with Posterior Samples
#'
#' Plots the diel niche space and posterior disribution of a fitted model.
#' @import plotly
#' @import coda
#' @param fit a list object output from the function 'diel.fit'. 
#' @param hyp a vector of hypothesis code names (characters)
#' @param diel.setup If NULL uses the default diel.ineq function. If a fit object is provided it will come from this object. Otherwise, a list of multinomial inequalities (Matrix A and vector b) representing diel hypotheses and the function needed for model fitting.
#' @param posteriors Posterior samples output from the function 'diel.fit'.
#' @param more.points To use more points for hyps in plotting. Default is FALSE. 
#' @param x.scene 3d graphical parameter
#' @param y.scene 3d graphical parameter
#' @param z.scene 3d graphical parameter
#' @param axis.size 3d graphical parameter
#' @param axis.lab.size 3d graphical parameter
#' @param legend.lab.size 3d graphical parameter
#' @return A plotly 3d plot
#' @examples 
#' out=diel.fit(y=cbind(11,87,2),hyp="D",post.fit=TRUE)
#' diel.plot(out)
#' @export

plot.diel=function(fit=NULL,
                   hyp=NULL, 
                   diel.setup=NULL, 
                   posteriors=NULL,
                   more.points=FALSE,
                   x.scene=2.5,
                   y.scene=1,
                   z.scene=0.3,
                   axis.size=16,
                   axis.lab.size=18,
                   legend.lab.size=15){
  
#x.scene=2.5; y.scene=1; z.scene=0.3; axis.size=16; axis.lab.size=18; legend.lab.size=15

if(is.null(fit) & is.null(hyp)){
  stop("Please include objects for either argument 'fit' or 'hyp")
}
  
if(!is.null(fit) & is.null(fit$post.samp.ms.model) & is.null(fit$post.samp) ){
  stop("No plot. Make sure post.fit=TRUE in your diel.fit objective")
}    

  
if(!is.null(fit) & !is.null(hyp)){
  warning("The argument hyp is being ignored because a fit object from diel.fit has been entered and has the hypotheses used to fit the data")
}    

if(!is.null(fit) & !is.null(posteriors)){
  warning("The argument 'posteriors' is being used for plotting, not post.samp.ms.model from the fit object")
}    
  

  
      
if(!is.null(fit)){hyp=fit$hyp.set; diel.setup=out$diel.setup
if(!is.null(fit$post.samp.ms.model)){post=coda::as.mcmc(fit$post.samp.ms.model)}else{
  
  if(length(fit$post.samp[[1]])>1){temp=do.call('rbind',fit$post.samp[[1]])}else{temp=fit$post.samp[[1]]}
  post=coda::as.mcmc(temp)
}

}
  

  
   
  
  
#Setup diel.setup and posterior samples    
  if(is.null(diel.setup) & is.null(fit)){diel.setup=diel.ineq()}
  if(!is.null(posteriors)){
    
    if(!is.list(posteriors)){post=coda::as.mcmc(posteriors)}else{
      post=coda::as.mcmc(do.call("rbind",lapply(out$post.samp,FUN=function(x){x[[1]]})))
    }
    
    
    }

#find models  and get points
  index.models=match(hyp,names(diel.setup))
  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points)))
  plot.points$hyp=as.factor(plot.points$hyp)
  
#colors to use  
  col.hyp=data.frame(D="#EFC000FF", N = "#0073C2FF", CR="#A73030FF",C="#868686FF", C2="#868686FF", D.CR="#CD534CFF",
                     D.N = "#79AF97FF", CR.N="#8F7700FF",D.max="#EFC000FF", N.max="#0073C2FF", CR.max="#A73030FF",
                     D.th="#EFC000FF", N.th="#0073C2FF",CR.th="#A73030FF", C.th="#868686FF", EC.th="black",
                     D.var="#EFC000FF",N.var="#0073C2FF",CR.var="#A73030FF",C.var="#868686FF",AV.var="black",
                     Uncon="lightgray",C.max="#868686FF", EC="#868686FF",AV.EQ="black")
  
  col.hyp.match=match(hyp,names(col.hyp))
  temp=col.hyp[col.hyp.match[!is.na(col.hyp.match)]]
  
  if(length(which(is.na(col.hyp.match)))>0){
  extra.col=colours()[length(which(is.na(col.hyp.match)))]
  temp=data.frame(temp,extra.col)
  }
  
  names(temp)[is.na(col.hyp.match)]=hyp[which(is.na(col.hyp.match))]
  
  color.set.use=as.character(temp[order(hyp)])
               
  color.set = c("#868686FF", "#A73030FF", "#EFC000FF", "#0073C2FF","#8F7700FF","#CD534CFF", "#79AF97FF", "#999999")
  #color.set.use=color.set[1:length(index.models)]
  
#Include posterior points if not null  
  if(!is.null(posteriors)| !is.null(fit)){
      post=data.frame(post,rep("posteriors",nrow(post)))
      colnames(post)=colnames(plot.points)
      plot.points2=rbind(plot.points,data.frame(post))
      plot.points2$col=as.factor(plot.points2$hyp)
      color.set.use=c(color.set.use,"#000000")
      
  }else{
    plot.points2=plot.points
    plot.points2$col=as.factor(plot.points2$hyp)
  }

  
#Define Plot  
  fig <- plotly::plot_ly(plot.points2, x = ~p.crep, y = ~p.day, z = ~p.night,
                  color=~col, colors=color.set.use,
                  marker = list(symbol = 'circle', sizemode = 'diameter', size = 3))

  fig <- fig %>% add_markers()

#margins
# m <- list(
#   l = 0,
#   r = 0,
#   b = 1,
#   t = 0,
#   pad = 0
# )

#fig <- fig %>% layout(autosize = F, margin = m)


#Setup layout
  xlim=range(as.numeric(plot.points2[,1]))
  ylim=range(as.numeric(plot.points2[,2]))
  zlim=range(as.numeric(plot.points2[,3]))

fig <- fig %>% layout(scene = list(
          camera = list(eye = list(x=x.scene, y=y.scene, z = z.scene)),
                     xaxis = list(title = 'x = P(Twilight)',range = xlim,gridwidth = 3,
                                  titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size)),
                     yaxis = list(title = 'y = P(Daytime)',range = ylim,gridwidth = 3,
                                  titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size)),
                     zaxis = list(titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size),
                       title = 'z = P(Nighttime)',range = zlim),gridwidth = 3
                                )
                )
fig <- fig %>% layout(showlegend = TRUE, legend = list(font = list(size = legend.lab.size),
                                                       itemsizing='constant',
                                                       orientation = "h",  xanchor = "center", x = 0.5))

#plot fig
fig

}
