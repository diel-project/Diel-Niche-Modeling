# Example 4: Circular Kernel Analysis with Diel Hypotheses

#### Author: Brian D. Gerber

#### Date: 2023-05-10

We will consider how the Diel.Niche package can support other diel
activity analysis. One of the most common analyses of camera trap data
to estimate animal diel activity is that of the circular kernel density
estimators (Ridout and Linkie, 2009). These functions are implemented in
the R pakcage ‘overlap’. Our objective is to use overlap to visualize
activity though the 24-hr period and then estimate the diel niche using
the kernel output.

# The Setup

``` r
# Load packages
  library(Diel.Niche)
  library(overlap)
```

We will consider a set of observed data on a species of interest from
camera traps. For each observation, the time of the event is recorded
and then converted into radians.

``` r

# Observed detections in radians
y.radians=c(2.4315927, 2.1048671, 2.4190263, 1.1938052, 3.1792918, 2.1111503, 3.2861059, 3.9269908, 5.1836279, 4.1531855, 4.2223005, 5.3721234, 3.9835395, 3.6191147, 3.3363714, 5.2464597, 2.9719467, 3.4494687, 5.3092916, 3.9332740,  6.0004420, 3.8327430, 2.2179644, 4.1783182, 2.8148670, 2.4755750, 3.9144244, 2.0420352, 0.2513274, 1.5393804, 4.0212386, 3.8201767, 3.8264599, 4.2662828, 3.6316811, 3.5876988, 1.9540706, 3.8453094, 2.9593803, 2.7017697)
```

# The Kernel Density Plot

To estimate the smoothed kernel density estimate, we provide the
function ‘densityPlot’ with the times of detections in radians.

``` r

# Create a plot and add the smoothed fitted non-parametric kernel density estimate
  densityPlot(y.radians, extend=NULL, lwd=4,main="",col="white",xaxt="none",
            ylab="Probabilty Density",ylim=c(0,0.14))

  axis(1, at=c(0,6,12,18,24),lab=c("0:00","Sunrise","12:00","Sunset","24:00"))
  x <- c(5,7,7,5)
  y <- c(0, 0, 1, 1) # The y-coordinate of the vertices
  polygon(x, y, col="#80809052", border = "#80809052")
  
  x <- c(17,19,19,17)
  y <- c(0, 0, 1, 1) # The y-coordinate of the vertices
  polygon(x, y, col="#80809052", border = "#80809052")
  
  text(11,0.11,"Daylight",cex=2)
  text(2,0.11,"Night",cex=2)
  text(22,0.11,"Night",cex=2)

# Note that n.grid is specified to be larger than the default. A large number of density values are needed below when using the 'prob.Overlap' function.  
  kernel.out=densityPlot(y.radians,extend = NULL,main="",lwd=3,rug=TRUE,add=TRUE,n.grid=1000)
```

![](C:\Users\bgerber\GOOGLE~1\GITHUB\DIEL~1.NIC\GITHUB~1\EXAMPL~2/figure-gfm/kernel-1.png)<!-- -->

From the plot we can see that the species is generally activity
throughout the daytime with a peak in activity during the later part of
the day. There is also non-significant activity during the early part of
the night.

# Classifying the diel niche

We can use the Diel.Niche package to help classify the hypothesis that
is most supported from this analysis. To do so, we need to integrate
under the curve at the intervals along the x-axis that correspond to the
diel periods (twilight, daytime, nighttime). First, notice that when
using the ‘densityPlot’ function the attribute ‘n.grid’ is set to 1000,
so we have many evaluations of the probability density along the curve;
the higher this number, the more accurate the integration will be.
Second, to separate the diel periods, we need to know the time periods
of dawn and dusk. Below, we assume a one hour period for dawn between
6-7am and dusk between 17-18pm.

``` r
# Using the object kernel.out, we integrate under the curve to estimate the three probabilities
  prob.integrated=prob.Overlap(kernel.out,
                             dawn=c(6,7),
                             dusk=c(17,18))

# We then pass prob.integrated to the 'posthoc.niche' function to classify the hypothesis that is most supported under the Traditional hypothesis set
  posthoc.niche(prob.integrated,hyp.sets("Traditional"))
#>        p.twi     p.day   p.night             Hypothesis
#> 1 0.05542409 0.7480305 0.1936513 Cathemeral Traditional
```

Our result indicates that the Cathemeral Traditional hypothesis is most
supported. This may have been unintuitive simply by looking at the
kernel density plot. While there is a large amount of activity during
the daytime, the probability of nighttime use is also quite high at
0.19, thus supporting the Cathemeral designation.

If we use the Maximizing hypothesis set, we find out that the Diurnal
Max hypothesis is most supported.

``` r
posthoc.niche(prob.integrated,hyp.sets("Maximizing"))
#>        p.twi     p.day   p.night  Hypothesis
#> 1 0.05542409 0.7480305 0.1936513 Diurnal Max
```

Lastly, using the General hypothesis set, we find that the most
supported hypothesis is more specifically the ‘Diurnal-Nocturnal’
hypothesis.

``` r
posthoc.niche(prob.integrated,hyp.sets("General"))
#>        p.twi     p.day   p.night        Hypothesis
#> 1 0.05542409 0.7480305 0.1936513 Diurnal-Nocturnal
```

# Conclusion

By defining the diel hypotheses a priori and explicitly, we can more
accurately make inference based on a given objective. Visually, the
kernel density plot shows this animal is most active during the daytime,
which is confirmed with the Diurnal Max hypothesis being most supported
in the Maximizing hypothesis set. However, considering more traditional
definitions of diel activity, we should call the animal cathemeral,
given it’s support under the Cathemeral Traditional hypothesis. More
specifically though, if we want to make a clear delineation between
activity at two diel periods versus three diel periods, the General
hypothesis set makes it evident the species is mostly active during only
two diel periods, as supported by the Diurnal-Nocturnal hypothesis.

# References

Ridout, Martin S., and Matthew Linkie. “Estimating overlap of daily
activity patterns from camera trap data.” Journal of Agricultural,
Biological, and Environmental Statistics 14 (2009): 322-337.
