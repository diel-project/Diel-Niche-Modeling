# Single species with multiple data analysis units

#### Author: Mason Fidino

#### Date: 2023-05-25

Our framework has outlined a process for making inference on the diel
niche of species for a given sampling and spatial period. A next natural
research question is how to link hypotheses about how the diel niche of
a species may change over time or over space. Simply, how to link
spatial or temporal covariates to findings of diel niche classification.
Ideally, a fully hierarchical model that synthetically connects the diel
niche classification to covariates would be specified, such that
parameter estimation is done jointly and all parametric uncertainties
are fully recognized. However, the constrained optimization of the
multinomial model makes this challenging at this time.

We offer a two-staged modeling process that accomplishes the goal.
First, estimate the diel niche for a species at each spatial or temporal
period of interest to identify the most supported hypothesis. Second,
use the hypotheses as data in a subsequent categorical regression model
that links the hypotheses to covariates of interest. It should be
understood that if there is uncertainty in the most supported model then
this does not get acknowledged in the regression model. To remediate
this concern, researchers may want to only consider using hypotheses
with a given high level of probability of support (e.g., model
probability $> 0.80$).

As one example, Virginia opossum are a predominately nocturnal species
that, with their bare feet, ears, and tail, become more active during
the day during the winter in temperate environments (Gallo et al. 2022).
As a result, we might expect this species to switch between diel
phenotypes over the course of a year. As the previous Chicago data is
from a long-term biodiversity monitoring survey, we compiled 27
different analysis units, each of which represented roughly 28 days of
sampling across 131 sites in Chicago, Illinois. To capture seasonal
variation cameras were deployed in January, April, July, and October,
and data comes from between 2013 and 2019 (all of which is available in
the `diel.data.` object).

As a first step, we therefore need to 1) load libraries, 2) load the
data, 3) subset that data the opossum analysis units, and 4) classify
the most likely diel phenotype for each analysis unit with
`Diel.Niche::diel.fit()`. In addition to this we are also going to
calculate the ordinal day an analysis unit started to use as a covariate
in our secondary analysis.

``` r
# Load packages
  library(Diel.Niche)
  library(lubridate)
  library(bbplot)
  library(nimble)
  library(parallel)
  library(MCMCvis)

# URL to bbplot
# browseURL("https://github.com/dapperstats/bbplot")

# Load data
  data("diel.data")
  
# Subset to opossum
  opossum <- diel.data[
    diel.data$scientificName == "Didelphis virginiana",
  ]

# convert opossum$min_date to date object
  opossum$min_date <- lubridate::mdy(
    opossum$min_date
  )

# and calculate the ordinal day
  opossum$ord_day <- lubridate::yday(
    opossum$min_date
  )

# A list to store the results
  diel_results <- vector(
    "list",
    length = nrow(
      opossum
    )
  )

# Classify most likely diel phenotype in a loop
  for(i in 1:length(diel_results)){
    # need to convert the row element of opossum to a 
    #  matrix for Diel.Niche::diel.fit().
      tmp <- Diel.Niche::diel.fit(
        y = as.matrix(
          opossum[i,c("twilight", "day","night")]
        ),
        hyp.set = Diel.Niche::hyp.sets(
          "Traditional"
        ),
        n.chains = 2,
        burnin = 5000,
        n.mcmc = 10000,
        prints = FALSE
      )
    # compile results into a data.frame
      diel_results[[i]] <- data.frame(
        choice = tmp$ms.model,
        prob = max(
          tmp$bf.table[,2]
        ),
        ord_day = opossum$ord_day[i],
        season = opossum$season[i]
      )
  }

# bind all of the results together into a data.frame
  diel_results <- do.call(
    "rbind",
    diel_results
  )
```

After this, we are only going to retain analysis units where there is at
least an $0.80$ probability of a diel phenotype classification. This
leaves us with 23 analysis units, with most occurring in either Summer
or Autumn. Furthermore, almost every analysis unit was classified as
nocturnal (`N`), while two were classified as cathemeral (`C`) during
the winter.

``` r
  diel_results <- diel_results[
    diel_results$prob > 0.8,
  ]
  table(diel_results$choice, diel_results$season)
#>    
#>     Autumn Spring Summer Winter
#>   C      0      0      0      2
#>   N      9      3      8      1
```

# The secondary model

The `nimble` code to fit the secondary model, as described in the paper,
is:

``` r
model_single_species <- nimble::nimbleCode(
  {
    for(i in 1:ndata){
      # cathemeral, reference category 1
        mu[i,1] <- 1
      # diurnal, category 2
        mu[i,2] <- exp( 
          inprod(
            diur_unit_beta[1:ncov_unit],
            unit_dm[i, 1:ncov_unit]
          )
        )
      # nocturnal, category 3
        mu[i,3] <- exp(
          inprod(
            noct_unit_beta[1:ncov_unit],
            unit_dm[i, 1:ncov_unit]
          )
        )
      # rescale to make into a vector of probabilities
      #  that sum to 1 (i.e., complete the softmax
      #  function).
        mu_prob[i,1:3] <- mu[i,1:3] / sum(mu[i,1:3])
      # Using the categorical distribution here
        y[i] ~ dcat(
          mu_prob[i,1:3]
        )
    }
  # Priors for the different parameters.
    for(k in 1:ncov_unit){
      diur_unit_beta[k] ~ dnorm(
        0,
        sd = 2
      )
      noct_unit_beta[k] ~ dnorm(
        0,
        sd = 2
      )
    }
  }
)
```

This model requires us to get a few things together in order to fit the
model to the data.

1.  `unit_dm`: A unit-level design matrix.
2.  `ndata`: The number of analysis units.
3.  `ncov_unit`: The number of columns in `unit_dm`.
4.  `y`: A vector of integers that represents which diel phenotype was
    selected. In this case 1 = Cathemeral, 2 = Diurnal, and 3 =
    Nocturnal.

``` r

# Step 1. Make the unit-level design matrix. The column
#  of 1's is present for the model intercepts.
  unit_dm <- cbind(
    1,
    diel_results$ord_day
  )

# scale the data to unit variance
  unit_dm[,2] <- scale(
    unit_dm[,2]
  )
# also include ordinal_day^2
  unit_dm <- cbind(
    unit_dm,
    unit_dm[,2]^2
  )

# Step 2. Calculate number of analysis units.
  ndata <- nrow(unit_dm)

# Step 3. Calculate the number of parameters for the
#   non-reference categories.
  ncov_unit <- ncol(
    unit_dm
  )
  
# Step 4. Get vector of diel classifications.
  y <- as.numeric(
    factor(
      diel_results$choice,
      levels = c(
        "C","D","N"
      )
    )
  )

# put these objects in lists for nimble. Data and constants.
  data_list <- list(
    y = y,
    unit_dm = unit_dm
  )
  constant_list <- list(
    ncov_unit = ncov_unit,
    ndata = ndata
  )
```

To fit this model to the data on multiple cores simultaneously, we need
to bundle together all of the `nimble` code into data into a function.
Here is how we would do that.

``` r
run_MCMC_allcode <- function(
    seed, # random seed for each core.
    data, # data_list
    cons, # constant_list
    nimble_model # the nimble model from above
) {
  # Load nimble on each core
    library(nimble)
  # Generate initial values for each chain.
    my_inits <- function(){
      list(
        diur_unit_beta = rnorm(
              cons$ncov_unit
          ),
        noct_unit_beta = rnorm(
              cons$ncov_unit
          )
      )
    }
  # Set seed
    set.seed(seed = seed)
  
  # Create the model
    myModel <- nimble::nimbleModel(
      code = nimble_model,
      data = data,
      constants = cons,
      inits = my_inits()
    )
  # Compile model
    CmyModel <- nimble::compileNimble(
      myModel
    )
  # Set up MCMC, only using slice samplers
    mconf <- nimble::configureMCMC(
      myModel,
      onlySlice = TRUE
    )
  # Track parameters
    h <- my_inits()
    mconf$setMonitors(
      names(h)
    )
  # Build MCMC
    myMCMC <- nimble::buildMCMC(
      mconf
    )
  # Compile MCMC in C
    CmyMCMC <- nimble::compileNimble(
      myMCMC,
      project = myModel
    )
  # Fit model to data
    results <- nimble::runMCMC(
      CmyMCMC, 
      niter = 30000,
      nburnin = 10000,
      thin = 10,
      setSeed = seed
    )
  
  return(results)
}
```

And then we fit the model to the data and summarise it. This takes about
one minute to run.

``` r

this_cluster <- parallel::makeCluster(4)

chain_output <- parallel::parLapply(
  cl = this_cluster, 
  X = 1:4, 
  fun = run_MCMC_allcode, 
  data = data_list,
  cons = constant_list,
  nimble_model = model_single_species
)

parallel::stopCluster(this_cluster)

model_summary <- MCMCvis::MCMCsummary(
  chain_output,
  round = 2
)

model_summary
#>
#>                     mean   sd  2.5%   50% 97.5% Rhat n.eff
#>  diur_unit_beta[1] -1.40 1.63 -4.69 -1.36  1.66    1  7853
#>  diur_unit_beta[2] -0.06 1.57 -3.18 -0.07  3.07    1  8287
#>  diur_unit_beta[3] -1.89 1.34 -4.75 -1.76  0.37    1  8206
#>  noct_unit_beta[1]  4.26 1.15  2.22  4.19  6.67    1  8000
#>  noct_unit_beta[2] -0.58 0.71 -2.02 -0.57  0.80    1  8000
#>  noct_unit_beta[3] -1.38 0.59 -2.61 -1.36 -0.27    1  7841
```

Looking at the model summary we can see that there is strong evidence
that `noct_unit_beta[3]`, which is the squared ordinal term, is
negative. Let’s generate the predictions for this across a full year.

``` r

# sequence of days for predictions
  days <- 1:365

# scale days as we did to fit the model
  days_scaled <- (days - mean(diel_results$ord_day)) / 
    sd(diel_results$ord_day)

# prediction data.frame
  pred_dm <- rbind(
    1,
    days_scaled,
    days_scaled^2
  )

# make model output into a matrix
  my_mcmc <- do.call(
    "rbind",
    chain_output
  )
  
# And convert that into a named list, with a set of parameters
#  for each non-reference category.
  my_mcmc <- list(
    diur_unit_beta = my_mcmc[,
      grep(
        "diur_unit_beta",
        colnames(my_mcmc)
      )
    ],
    noct_unit_beta = my_mcmc[,
      grep(
        "noct_unit_beta",
        colnames(my_mcmc)
      )
    ]
  )

# make predictions for each category and exponentiate. The
#  prediction for the reference category is exp(0) = 1.
  diur_pred <- exp(
    my_mcmc$diur_unit_beta %*% pred_dm
  )

  noct_pred <- exp(
    my_mcmc$noct_unit_beta %*% pred_dm
  )

# The denominator for softmax to scale to probabilities.
  denom <- 1 + diur_pred + noct_pred

# and get the probability for nocturnality.
  noct_prob <- noct_pred / denom

# calculate median and 95% CI
  noct_prob <- apply(
    noct_prob,
    2,
    quantile,
    probs = c(
      0.025,0.5,0.975
    )
  )
```

With these predictions we can plot out the probability of nocturnality
as a function of ordinal day. You can see here that the probability of
nocturnality greatly decreases at the start and end of the year.
However, there is substantial uncertainty around this given that we had
less data during the spring and winter.

``` r
{
  # Set margins
    par(
      mar = c(4,4,1,1)
    )
  # Create a blank canvas
    bbplot::blank(
      xlim = c(0,366),
      ylim = c(0,1),
      bty = "l",
      xaxs = "i",
      yaxs = "i"
    )
  # Add y axis
    bbplot::axis_blank(
      side = 2,
      lwd = 2
    )
  
  # Add x axis
    bbplot::axis_blank(
      side = 1,
      minor = FALSE,
      lwd = 2,
      at = c(1,seq(0,365,73)[-1])
    )
  # Add text to y axis
    bbplot::axis_text(
      side = 2,
      las = 1,
      line = 0.75
    )
  # Add text to x axis
    bbplot::axis_text(
      text = c(1,seq(0,365,73)[-1]),
      side = 1,
      line = 0.55,
      at = c(1,seq(0,365,73)[-1])
      )
  # Add y axis label
    bbplot::axis_text(
      "Pr(Nocturnal)",
      side = 2,
      line = 2.25,
      cex = 1.5
    )
  # Add x axis label
    bbplot::axis_text(
      "Ordinal day",
      side = 1,
      line = 2.25,
      cex = 1.5
    )

  # Add shaded ribbon
    bbplot::ribbon(
      x = days,
      y = t(noct_prob[-2,]),
      col = "gray40",
      alpha = 0.8
    )
  # Add median line
    lines(
      x = days,
      y = noct_prob[2,],
      lwd = 4
    )
  # Add thicker 'L' to x and y axis.
  box(
    bty = "l",
    lwd = 2
  )
}
```

<div class="figure">

<img src="img/opossum_nocturnal_time.png" alt=" Figure 3.1. Opossum are predominately nocturnal during the middle of the year, but this precipitously drops off at the start and end of a year. The solid black line represents the median probability of nocturnality while the shaded ribbon is the 95% credible interval." width="70%" />
<p class="caption">
Figure 3.1. Opossum are predominately nocturnal during the middle of the
year, but this precipitously drops off at the start and end of a year.
The solid black line represents the median probability of nocturnality
while the shaded ribbon is the 95% credible interval.
</p>

</div>

# References

Gallo, T., Fidino, M., Gerber, B., Ahlers, A. A., Angstmann, J. L.,
Amaya, M., Concilio, A. L., Drake, D., Gray, D., Lehrer, E. W., Murray,
M. H., Ryan, T. J., Cassady St. Clair, C., Salsbury, C. M., Sander, H.
A., Stankowich, T., Williamson, J., Belaire, J. A., Simon, K., and
Magle, S. B. (2022). Mammals adjust diel activity across gradients of
urbanization. eLife. <https://elifesciences.org/articles/74756>
