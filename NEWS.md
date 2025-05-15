# Diel.Niche 0.1.3

## 2025-05-15

- Added some new functionality.

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