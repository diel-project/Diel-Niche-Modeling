# Diel.Niche

An R package to evaluate hypotheses of diel phenotypes based on empirical
data and estimate the probabilitiy of activity during the crepuscular,
daytime, and nighttime periods. The main idea of evaluating diel phenotypes as hypotheses is that they can be expressed as inequality
statements among the these three probabilities of activity.


## Install

First, install and load the library

``` r
# Install package from GitHub via devtools.
# devtools::install_github("diel-project/Diel-Niche-Modeling",ref="main")

# Load the pacakge
  library(Diel.Niche)
```

## Hypotheses

There are three fundamental hypothesis sets: *Maximizing*,
*Traditional*, and *General*. Additional hypotheses available are
described in the ‘Diel-Niche-Vignette.Rmd’

### **Maximizing**

This hypothesis set includes three hypotheses (Crepuscular Max, Diurnal
Max, and Nocturnal Max) with the objective to evaluate which time period
is used most. As such, there is no hypothesis about activity across
multiple time periods (i.e., cathemeral).

``` r
plot.diel(hyp=hyp.sets("Maximizing"))
```

<img src="README_files/figure-gfm/maximizing.png" width="5in" style="display: block; margin: auto;" />

### **Traditional**

This hypothesis set includes four hypotheses (Crepuscular, Diurnal,
Nocturnal, Traditional Cathemeral) that aim to capture the general
interpretation of these hypotheses from the literature. Crepuscular,
Diurnal, and Nocturnal are defined based on having at least 0.80
probability (threshold probability, $\xi_{1} = 0.80)$ in their
respective diel periods (twilight, daytime, nighttime). If an animal is
not mostly active in one period than it is defined as Traditional
Cathemeral; this occurs when either two or three time periods are used
more than $1-\xi_{1}$.

``` r
plot.diel(hyp=hyp.sets("Traditional"))
```

<img src="README_files/figure-gfm/traditional.png" width="5in" style="display: block; margin: auto;" />

### **General**

This hypothesis set includes seven hypotheses. The Diurnal, Crepuscular,
and Nocturnal hypotheses are defined the same as in Traditional. The
main difference is the separation of the probability space of
Traditional Cathemeral into four more specific hypotheses: General
Cathemeral, Crepuscular-Nocturnal, Diurnal-Nocturnal, and
Diurnal-Crepuscular. The General Cathemeral hypothesis—which represents
a subset of the parameter space taken by the previously mentioned
Traditional Cathemeral—aims to define when an animal uses all three diel
periods (twilight, daytime, nighttime) at equal to or more than a
minimum amount (i.e.,
$p_{\text{tw}}, p_{\text{d}}, p_{\text{n}} \leq \xi_{1}$ and
$p_{\text{tw}}, p_{\text{d}}, p_{\text{n}} \geq \xi_{2}$). We defined
the lower threshold probability as $\xi_{2} = 0.10$, such that we
consider it important to differentiate animal activity when a diel
period is used at least this much. However, if only two diel periods are
used above $\xi_{2}$ then we classify this activity using one of the
binomial hypotheses (Crepuscular-Nocturnal, Diurnal-Nocturnal, and
Diurnal-Crepuscular).

``` r
plot.diel(hyp=hyp.sets("General"))
```

<img src="README_files/figure-gfm/general.png" width="5in" style="display: block; margin: auto;" />

### Hypothesis Codes

Data simulation, model fitting, and plotting are done based on a
hypothesis or set of hypotheses. Users don’t need to remember hypothesis
codes, but can call them as,

``` r
  hyp.sets("Maximizing")
```

    ## [1] "D.max"  "N.max"  "CR.max"

``` r
  hyp.sets("Traditional")
```

    ## [1] "D"  "N"  "CR" "C"

``` r
  hyp.sets("General")
```

    ## [1] "D"    "N"    "CR"   "C2"   "D.CR" "D.N"  "CR.N"

# Simulating Data

Pick a hypothesis to simulate data from and how many samples,

``` r
  set.seed(45451)
  hyp=c("CR")
  sim=sim.diel(hyp=hyp,n.sample=100)
  
  #The probability value used to simulate data
  p=sim$p
  p
```

    ##       [,1]  [,2] [,3]
    ## [1,] 0.935 0.025 0.04

``` r
  #The simulated data
  y=sim$y
  y
```

    ##      y_crep y_day y_night
    ## [1,]     96     1       3

# Model Comparison

Models are compared using Bayes factors, which are used to derive a
posterior model probability

``` r
  hyp.set=hyp.sets("Traditional")
  out = diel.fit(y,hyp.set)
```

    ## Data checks Complete.

    ## Calculating Bayes Factors...

    ## The most supported model is: 
    ##  Crepuscular (Traditional)

``` r
#Call the model probabilities for each hypothesis in the set
  out$bf.table
```

    ##    Prior Posterior
    ## D   0.25         0
    ## N   0.25         0
    ## CR  0.25         1
    ## C   0.25         0

The function ‘diel.fit’ defaults to providing the model probabilities
for each hypothesis set, but not the posterior samples of the parameters
for each hypothesis. We can change this as well as include additional
MCMC chains,

``` r
  out = diel.fit(y,hyp.set,n.chains = 2,post.fit = TRUE)
```

    ## Data checks Complete.

    ## Calculating Bayes Factors...

    ## Posterior Sampling...

    ## The most supported model is: 
    ##  Crepuscular (Traditional)

We can look at a convergence criteria for the most supported model.

``` r
  out$ms.gelm.diag
```

    ## Potential scale reduction factors:
    ## 
    ##      Point est. Upper C.I.
    ## p1_1          1       1.01
    ## p1_2          1       1.01

We can plot the posterior samples from the most supported model to check
convergence/mixing as,

``` r
 plot(coda::as.mcmc(out$post.samp.ms.model))
```

![](README_files/figure-gfm/postcheck-1.png)<!-- -->

The posterior samples for all hypotheses are available in a list.

``` r
 names(out$post.samp)
```

    ## [1] "D"  "N"  "CR" "C"

``` r
#For each of these list is a list of chains
  length(out$post.samp[[1]])
```

    ## [1] 2

``` r
#Here are the means of the posterior samples of all hypotheses for chain 1
  lapply(out$post.samp,FUN=function(x){colMeans(x[[1]])})
```

    ## $D
    ##    p_crep_1     p_day_1   p_night_1 
    ## 0.190235520 0.801957493 0.007806987 
    ## 
    ## $N
    ##  p_crep_1   p_day_1 p_night_1 
    ## 0.0000000 0.1254649 0.8745351 
    ## 
    ## $CR
    ##   p_crep_1    p_day_1  p_night_1 
    ## 0.94286797 0.01899454 0.03813750 
    ## 
    ## $C
    ##   p_crep_1    p_day_1  p_night_1 
    ## 0.78993613 0.06946899 0.14059487

# Plotting

Using the packages bayesplot and ggplot2, we can examine our posterior
distributions along with the true probabilities values,

``` r
library(ggplot2)
library(bayesplot)
```

    ## This is bayesplot version 1.10.0

    ## - Online documentation and vignettes at mc-stan.org/bayesplot

    ## - bayesplot theme set to bayesplot::theme_default()

    ##    * Does _not_ affect other ggplot2 plots

    ##    * See ?bayesplot_theme_set for details on theme setting

``` r
posteriors=coda::as.mcmc(out$post.samp.ms.model)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posteriors, prob = 0.8) + plot_title+ 
  geom_vline(xintercept=p[1], linetype="dashed",color = c("red"), size=1)+
  geom_vline(xintercept=p[2], linetype="dashed",color = c("purple"), size=1)+
  geom_vline(xintercept=p[3], linetype="dashed",color = c("green"), size=1)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

![](README_files/figure-gfm/plot-1.png)<!-- -->

We can also plot the posteriors in 3D using ploty.

``` r
plot(out)
```

<img src="README_files/figure-gfm/3dplot 2-1.png" width="6.5in" style="display: block; margin: auto;" />

# Plotting Issues in RStudio

Plotting is done using the package plotly. Plotly can have issues with
RStudio. If you are using RStudio and no figures are opening then:
Tools–\>Global Options–\>Advanced–\>Rendering Engine Choose “Desktop
OpenGL{} and then restart RStudio.

# Badges

[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/licenses/)

# Acknowledgements

- The many data contributors that supported the evaluation of this framework.

# Authors

- [Brian Gerber](https://github.com/bgerber123)
- [Kadambari Devarajan](https://github.com/kadambarid)
- [Mason Fidino](https://masonfidino.com/)
- [Zach Farris](https://hes.appstate.edu/faculty-staff/zachary-farris)
