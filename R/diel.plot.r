#' Plot Diel Hypothesis or Hypothesis Set Along with Posterior Samples
#'
#' Plots the diel niche space and posterior disribution of a fitted model.
#' @import plotly
#' @import coda
#' @param hyp a vector of hypothesis code names (characters)
#' @param diel.setup If NULL uses the default diel.ineq function. Otherwise, a list of multinomial inequalities (Matrix A and vector b) representing diel hypotheses and the function needed for model fitting.
#' @param posteriors A single models MCMC output from the function 'diel.fit'.
#' @param more.points To use more points for hyps in plotting. Default is FALSE. 
#' @param x.scene 3d graphical parameter
#' @param y.scene 3d graphical parameter
#' @param z.scene 3d graphical parameter
#' @param axis.size 3d graphical parameter
#' @param axis.lab.size 3d graphical parameter
#' @param legend.lab.size 3d graphical parameter
#' @return A plotly 3d plot
#' @examples 
#' out=diel.fit(y=t(matrix(c(11,87,2))),hyp="D",post.fit=TRUE)
#' diel.plot(hyp="D",posteriors=out$post.samp.ms.model)
#' @export

diel.plot=function(hyp, 
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

#Setup diel.setup and posterior samples    
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  if(!is.null(posteriors)){post=coda::as.mcmc(posteriors)}

#find models  and get points
  index.models=match(hyp,names(diel.setup))
  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points)))
  plot.points$hyp=as.factor(plot.points$hyp)
  
#colors to use  

  color.set = c("#868686FF", "#A73030FF", "#EFC000FF", "#0073C2FF","#8F7700FF","#CD534CFF", "#79AF97FF", "#999999")
  color.set.use=color.set[1:length(index.models)]
  
#Include posterior points if not null  
  if(!is.null(posteriors)){
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
