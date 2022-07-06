#' Plot Diel Hypotheis
#'
#' Plots the diel niche space and posterior disribution of a fitted model.
#' @import plotly
#' @import coda
#' @param hyp hypothesis code name to use
#' @param diel.setup Defaults to using diel.ineq function. A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'.
#' @param posteriors A single models MCMC output from the function 'diel.hypotheses.func'.
#' @param more.points Default is FALSE. To use more points for hyps in plotting.
#' @return A plotly 3d plot
#' @examples 
#' out=diel.fit(y=t(matrix(c(11,87,2))),hyp.set="D.max",n.mcmc=1000,burnin=200)
#' diel.plot(hyp="D.max",posteriors=out$post.samp[[1]])
#' @export

diel.plot=function(hyp, 
                   diel.setup=NULL, 
                   posteriors=NULL,
                   more.points=FALSE){
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  if(is.null(posteriors)){post=t(matrix(c(0,0,0)))}else{post=coda::as.mcmc(posteriors)}
  
  
  #source("plot.setup.hyp.params.r")
  index.models=match(hyp,names(diel.setup))
  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points)))
  plot.points$hyp=as.factor(plot.points$hyp)
  
  xlim=range(plot.points[,1])
  ylim=range(plot.points[,2])
  zlim=range(plot.points[,3])
  post=data.frame(post,rep("posteriors",nrow(post)))
  colnames(post)=colnames(plot.points)
  plot.points2=rbind(plot.points,data.frame(post))
  #plot.points2=data.frame(plot.points2,as.factor(c(rep("Niche",nrow(plot.points)),
  #                                     rep("Posteriors",nrow(post)))))
##  colnames(plot.points2)[4]="type"
  col2=c("#000000","#D8BFD8")
  #col2=colors(length(unique(plot.points2$hyp)))
  
#  plot.points2$size=(c(rep("2",nrow(plot.points)),
#                                       rep("3",nrow(post))))

if(length(unique(plot.points2$hyp))>2){
fig <- plotly::plot_ly(plot.points2, x = ~p.crep, y = ~p.day, z = ~p.night,
           #     width=800,height=800,
               color = ~hyp,# colors=col2,
                marker = list(symbol = 'circle', sizemode = 'diameter', size = 3))
}else{

    plot.points2$col=as.factor((c(rep(hyp,nrow(plot.points)),
                                       rep("posteriors",nrow(post)))))
  
fig <- plotly::plot_ly(plot.points2, x = ~p.crep, y = ~p.day, z = ~p.night,
           #     width=800,height=800,
                color=~col, colors=col2,
                marker = list(symbol = 'circle', sizemode = 'diameter', size = 3))
  
}

fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(
  #camera = list(eye = list(x=1, y=2, z = 0.25)),
                     xaxis = list(title = 'Crepuscular',range = xlim,gridwidth = 3,
                                  titlefont = list(size = 18),tickfont = list(size=10)),
                     yaxis = list(title = 'Daytime',range = ylim,gridwidth = 3,
                                  titlefont = list(size = 18),tickfont = list(size=10)),
                     zaxis = list(titlefont = list(size = 18),tickfont = list(size=10),
                       title = 'Nighttime',range = zlim),gridwidth = 3
                                )
                )
fig <- fig %>% layout(showlegend = TRUE, legend = list(font = list(size = 20),
                                                       itemsizing='constant'))

fig

}
