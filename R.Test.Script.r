#Script for Kadambari and Mason
#Add your auth key and install the package

 devtools::install_github("diel-project/Diel-Niche-Modeling"
                          ,ref="main"
                          ,auth_token = ""
 )

#load library 
library(Diel.Niche)

#Package Dependencies
# MASS, coda, multinomineq, plotly,retry,stats

#General information about hypotheses
  what.hyp()

#Hypotheses names
  what.hyp("?")

#Description for a specific hypothesis
  what.hyp("D.max")

#Pick a hypothesis to simulate data from  
  hyp=c("N.max")

#Setup inequalities using default values  
  diel.setup=diel.ineq()

#simulate a 1 dataset of 100 samples at reps sites/projects
  set.seed(45451)
  reps=3
  sim.dat=sim.diel.alt(n.sim=1,
                   reps=reps,
                   n.sample=100,
                   hyp,diel.setup)

  y=sim.dat$y
  y

#Names of hypotheses sets
  hyp.sets()
  
#Choose hypotheses sets  
  hyp.set=hyp.sets("hyp.max")
  
#Specify mcmc inputs
 n.mcmc=2000 #number of MCMC iterations
 n.cpu=1     #Number of processors to use or number of chains
 burnin=1000 #burn-in period to discard from n.mcmc

#Fit models to hypothesis set
  out=diel.hypotheses.func.alt(diel.setup=diel.setup,y=y,reps=reps,
                           hyp.set=hyp.set,
                           n.mcmc=n.mcmc,
                           n.cpu=n.cpu,
                           burnin=burnin)

out$bf.final
  #attributes output
attributes(out)

#can be understood here
?diel.hypotheses.func

#Plot posteriors

#Basic plot of posterior distributions and the true values
library(bayesplot)
library(ggplot2)

posteriors=coda::as.mcmc(out$most.supported.model)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posteriors, prob = 0.8) + plot_title+ 
  geom_vline(xintercept=sim.dat$p, linetype="dashed", 
                color = c("red","green","purple"), size=1)


#Open 3d plot of theoretical niche space for the hypothesis
#and the postesterior samples
diel.plot(hyp=out$model.ms,
          diel.setup=diel.setup,
          posteriors=out$most.supported.model)

#FYI, if you are using RStudio and no figure us opening
  #Tools-->Global Options-->Advanced-->Rengering Engine
  #Choose "Desktop OpenGL{}
  #restart RStudio completely