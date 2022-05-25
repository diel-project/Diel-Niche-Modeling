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
  what.hyp("D.th")

#Pick a hypothesis to simulate data from  
  hyp=c("D.th")

#Setup inequalities using default values  
  diel.setup=diel.ineq()

#simulate a single dataset of 100
  set.seed(45454)
  sim.dat=sim.diel(n.sim=1,n.sample=100,
                   hyp,diel.setup)

  y=sim.dat$y
  y

#Names of hypotheses sets
  hyp.sets()
  
#Choose hypotheses sets  
  hyp.set=hyp.sets("hyp.th")
  
#Specify mcmc inputs
 n.mcmc=2000 #number of MCMC iterations
 n.cpu=1     #Number of processors to use or number of chains
 burnin=1000 #burn-in period to discard from n.mcmc

#Fit models to hypothesis set
  out=diel.hypotheses.func(diel.setup=diel.setup,y=y,
                           hyp.set=hyp.set,
                           n.mcmc=n.mcmc,
                           n.cpu=n.cpu,
                           burnin=burnin)
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
  geom_vline(xintercept=prob.select, linetype="dashed", 
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