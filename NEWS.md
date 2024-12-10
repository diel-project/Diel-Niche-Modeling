# Diel.Niche News and Updates

## Updates

- 12/10/2024: a [new vignette](./vignettes/Diel.Hierarchical.Overlap.md) on connecting hierarchical models to the Diel.Niche package and estiamting diel overlap.

## Updates

- 01/19/24: The function prob.overlap has been updated to evaluate outputs from the R package 'activity'. For example,

``` r
#Sample data
  y=c(6.25753965, 0.56420439, 0.17951958, 0.97453486, 5.64204395, 6.23189400, 
      0.28210220, 5.92414615, 0.92324356, 5.71898091, 0.43597612)

#fit model
  mod1<-activity::fitact(y) 

#Get probabilities and defin using Traditional hypotheis set
  probs=prob.overlap(densityplot=mod1,dawn=c(6,7),dusk=c(17,18))
  posthoc.niche(probs,hyp.sets("Traditional"))
``` 

- 10/01/23: A [new vignette](./GitHub_vignettes/Gomez-Diel-Niche-comparison.md) was created that implements diel phenotypes from [Gomez et al. 2005](https://doi.org/10.1080/01650520500129638), demonstrating how to create your own phenotypes in Diel.Niche. These phenotypes are implemented using data from [Haysom et al. 2023](https://doi.org/10.1111/btp.13248)

## News 

- 01/17/24: The manuscript associated to Diel.Niche R Package was published online open access at [Journal of Animal Ecology](https://doi.org/10.1111/1365-2656.14035)

- 06/24/23: The preprint describing the R package Diel.Niche was published online at [bioRxiv](https://www.biorxiv.org/content/10.1101/2023.06.21.545898v1)




