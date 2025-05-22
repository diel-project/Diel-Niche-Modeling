# Diel.Niche 0.1.3

## 2025-05-15

- Added a suite of new functions to make it easier than every
  to process your raw data and prepare it for analysis. 
  The script below showcases a full analysis of the 
  diel phenotype of Virginia opossum (*Didelphis virginiana*)
  throughout Chicago, Illinois during Fall 2012 and are 
  available in the `Diel.Niche` package. Data came from
  this study [here](https://doi.org/10.1111/1365-2656.12967).
  
  ```R
  
# load packages
library(Diel.Niche)
library(dplyr)
library(tidyr)

# load in the example data, see
#  help file for metadata (i.e., ?camera.data)
data(camera.data)

# thin the data by removing images of the same species
#  at the same site that are within 15 minutes of 
#  one another. This is done with the new
#  function Diel.Niche::trim.time().
thinned_data <- Diel.Niche::trim.time(
  data = camera.data,
  datetime.column = "datetime",
  site.column = "surveyID",
  species.column = "species",
  minutes.between = 15,
  progress.bar = TRUE
)

# This new function defines the diel
#  bins (e.g., twilight, daym and night)
#  to use for the analysis based
#  on solar transitions that can be
#  queried via suncalc::getSunlightTimes()
bin_list <- Diel.Niche::make.diel.bin.list()

# Determine the diel bin for each
#  camera trap photo with the new
#  fucntion Diel.Niche::bin.diel.times().
#  This adds a new column 'dielBin' to
#  the data.frame that categorizes the
#  diel period (i.e., bin) associated
#  to a date.time record.
thinned_data <- Diel.Niche::bin.diel.times(
  data = thinned_data,
  datetime.column = "datetime",
  lat.column = "lat",
  lon.column = "lon",
  bin.type.list = bin_list,
  na_vals = "remove"
)
# For this example, we are going to analyze
#  the diel phenotype of opossum in the fall.
#  The surveyID column has seasonal information
#  and so we can determine fall data with that.

fall <- thinned_data[
  grep("FA12", thinned_data$surveyID),
]

# generate counts of the three different dielBins.
y <- fall %>%
  dplyr::count(
    species, dielBin
  ) %>%  
  tidyr::pivot_wider(
    names_from = dielBin,
    values_from = n,
    values_fill = 0  # fill missing dielBin values with 0
  ) %>% 
  data.frame

# convert the count columns to their own matrix,
#  which is required by Diel.Niche::diel.fit().
for_diel.fit <- as.matrix(
  y[,c("twilight", "day","night")]
)

# fit the model. Note that the order
#  must be twilight, day, night!
output <- Diel.Niche::diel.fit(
  y = for_diel.fit,
  hyp.set = hyp.sets("Traditional"),
  post.fit = TRUE
)

# The most supported model is: Nocturnal (Traditional).
#  Now can visualize the posterior on a ternary graph.
Diel.Niche::triplot(
  output
)
  ```

# Diel.Niche 0.1.2

## 2024-12-10

- Added a [new vignette](./vignettes/Diel.Hierarchical.Overlap.md) on connecting hierarchical models to the Diel.Niche package and estimating diel overlap.

# Diel.Niche 0.1.1

## 2024-01-19

- Updated `prob.overlap()` to evaluate outputs from the R package `activity`.

```r
# Sample data
y <- c(6.26, 0.56, 0.18, 0.97, 5.64, 6.23, 0.28, 5.92, 0.92, 5.71, 0.44)

# Fit model
mod1 <- activity::fitact(y)

# Get probabilities and define using Traditional hypothesis set
probs <- prob.overlap(densityplot = mod1, dawn = c(6, 7), dusk = c(17, 18))
posthoc.niche(probs, hyp.sets("Traditional"))