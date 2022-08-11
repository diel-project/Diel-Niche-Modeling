#' Plot Diel Hypothesis or Hypothesis Set
#'
#' Plots the diel niche space and posterior disribution of a fitted model.
#' @import plotly
#' @import coda
#' @param hyp a vector of hypotheses code names
#' @param diel.setup Defaults to using diel.ineq function. A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'.
#' @param posteriors A single models MCMC output from the function 'diel.hypotheses.func'.
#' @param more.points Default is FALSE. To use more points for hyps in plotting.
#' @param x.scene
#' @param y.scene
#' @param z.scene
#' @param axis.size
#' @param axis.lab.size
#' @param legend.lab.size
#' @return A plotly 3d plot
#' @examples 
#' out=diel.fit(y=t(matrix(c(11,87,2))),hyp.set="D.max",n.mcmc=1000,burnin=200)
#' diel.plot(hyp="D.max",posteriors=out$post.samp[[1]])
#' @export

diel.plot=function(hyp, 
                   diel.setup=NULL, 
                   posteriors=NULL,
                   more.points=FALSE,
                   x.scene=2,
                   y.scene=2,
                   z.scene=0.2,
                   axis.size=16,
                   axis.lab.size=18,
                   legend.lab.size=15){
  
#x.scene=0.8; y.scene=0.8; z.scene=0.2; axis.size=16; axis.lab.size=18; legend.lab.size=15

#Setup diel.setup and posterior samples    
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  if(!is.null(posteriors)){post=coda::as.mcmc(posteriors)}

#find models  and get points
  index.models=match(hyp,names(diel.setup))
  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points)))
  plot.points$hyp=as.factor(plot.points$hyp)
  
  color.set = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#F0E442")
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

fig <- plotly::plot_ly(plot.points2, x = ~p.crep, y = ~p.day, z = ~p.night,
           #     width=800,height=800,
                color=~col, colors=color.set.use,
                marker = list(symbol = 'circle', sizemode = 'diameter', size = 3))

#Add
fig <- fig %>% add_markers()


# m <- list(
#   l = 0,
#   r = 0,
#   b = 1,
#   t = 0,
#   pad = 0
# )

#fig <- fig %>% layout(autosize = F, margin = m)


#Setup layout
  xlim=range(plot.points2[,1])
  ylim=range(plot.points2[,2])
  zlim=range(plot.points2[,3])

fig <- fig %>% layout(scene = list(
          camera = list(eye = list(x=x.scene, y=y.scene, z = z.scene)),
                     xaxis = list(title = 'Crepuscular',range = xlim,gridwidth = 3,
                                  titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size)),
                     yaxis = list(title = 'Daytime',range = ylim,gridwidth = 3,
                                  titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size)),
                     zaxis = list(titlefont = list(size = axis.lab.size),tickfont = list(size=axis.size),
                       title = 'Nighttime',range = zlim),gridwidth = 3
                                )
                )
fig <- fig %>% layout(showlegend = TRUE, legend = list(font = list(size = legend.lab.size),
                                                       itemsizing='constant',
                                                       orientation = "h",  xanchor = "center", x = 0.5))


fig

}
