# Diel Activity from State-Dependent Animal Movement

#### Author: Brian D. Gerber

#### Date: 2023-09-12

In this example we consider how to use an animal movement behavior
analysis to provide inference on diel activity using the
$\texttt{Diel.Niche}$ R package. This is by no means the only way to
think about animal movement and diel activity. Specifically, we use the
R package $\texttt{momentuHMM}$ to fit a hidden Markov model that
assumes animal location data are a bivariate time series to estimate
step lengths and turning angle’s at each time point. We will consider
state-dependent movement, where we estimate the behavioral state (e.g.,
foraging or traveling) of each location based on step lengths and
turning angles. We also consider diel cycles of temperature as a
covariate when modeling movement parameters.

# The Setup

``` r
# Load packages
  library(Diel.Niche)
  library(lubridate)
  library(suncalc)
  library(momentuHMM)
  library(sf)
  library(ggplot2)
  library(ggpubr)
  library(conicfit)
```

We will use African Elephant (*Loxodonta africana*) data available from
Wall et al. 2014 and published online at
[MoveBank](http://dx.doi.org/10.5441/001/1.f321pf80).

The animal movement script that fits the hidden Markov model is adapted
from the example outlined in $\texttt{momentuHMM}$ and available on
GitHub
<https://github.com/bmcclintock/momentuHMM/blob/master/vignettes/examples/elephantExample.R>.

# Movement Data and Iniital Setup

First, we need to load the data and setup the data structure for animal
movement modeling.

``` r
# We will load data via RData file, but you may also wish to download the data and read it in this way
# CODE NOT RUN
  rawData<- read.csv(
    "Wall et al. 2014 African Elephant Dataset (Source-Save the Elephants).csv"
  )
```

``` r

# Select and rename relevant columns
  rawData <- rawData[,
    c(
      "individual.local.identifier",
      "timestamp",
      "location.long",
      "location.lat",
      "external.temperature"
    )
  ]
  colnames(rawData) <- c("ID","time","lon","lat","temp")

# There are two individual's tracks. Only keep the  first track - named, "Salif Keita"
  rawData <- subset(
    rawData,
    ID==unique(ID)[1]
  )

# Convert times from factors to POSIX
  rawData$time <- as.POSIXct(
    rawData$time,
    tz="GMT"
  )

  
# Project to UTM coordinates using sf
  llcoord <- sf::st_as_sf(
    rawData[,3:4],
    coords = c("lon","lat"),
    crs="+proj=longlat +datum=WGS84"
  )
  utmcoord <- sf::st_transform(
    llcoord,
    crs="+proj=utm +zone=30 ellps=WGS84"
  )

# Add UTM locations to data frame
  rawData$x <-  sf::st_coordinates(utmcoord)[,1]
  rawData$y <-  sf::st_coordinates(utmcoord)[,2]
```

Now with the data all setup, we can standardize the location intervals
at hour intervals by using the function `crawlWrap` which implements a
continuous-time correlated random walk model.

``` r
# Model and Predict locations using crawl - accomadates missing data at one hour intervals
  crwOut <- momentuHMM::crawlWrap(
    rawData,
    timeStep="hour",
    theta=c(6.855, -0.007),
    fixPar=c(NA,NA)
  )
```

Using the output, we can plot the observed and predicted standardized
locations.

``` r
  plot(
    crwOut,
    ask = FALSE
  )
```

![](Example6_files/figure-gfm/movement%20plot1-1.png)<!-- -->

# Fitting hidden Markov Models

Now that we have standardized fixed-location intervals, we can estimate
state-dependent turning angles and step lengths. First, we will setup
our data for model fitting in `momentuHMM`. Overall, we are going to fit
three models to these data. The first is a null model (`m1`), the second
incorporates some covariates on transition probabilities (`m2`), and the
third incorporates covariates onto multiple parameters (`m3`; i.e.,
transition probabilities, variation in step length, and turning angle
concentration).

``` r
# Create momentuHMMData object from crwData object
  elephantData <- momentuHMM::prepData(
    data=crwOut,
    covNames="temp"
  )

# Add cosinor covariate based on hour of day
  elephantData$hour <- as.integer(
    strftime(
      elephantData$time,
      format = "%H",
      tz="GMT"
      )
  )

# Label states
  stateNames <- c("encamped","exploratory")

# Distributions for observation processes
  dist = list(step = "gamma", angle = "wrpcauchy")

# Initial parameters
  Par0_m1 <- list(
    step=c(100,500,100,200),
    angle=c(0.3,0.7)
  )
```

The code to fit the null model is…

``` r
# Fit hidden markov model with 2 states
  m1 <- momentuHMM::fitHMM(
    data = elephantData,
    nbStates = 2,
    dist = dist,
    Par0 = Par0_m1, 
    estAngleMean = list(angle=FALSE),
    stateNames = stateNames
  )
```

Next, let’s consider a model (`m2`) where the transition probabilities
between states vary by temperature and the interaction with a cyclical
function based on the hour of the day. See
[https://cran.r-project.org/web/packages/momentuHMM/vignettes/momentuHMM.pdf](momentuHMM%20vignette)
for more details.

``` r
# Formula for transition probabilities
  formula <- ~ temp * cosinor(hour, period = 24)

# Initial parameters (obtained from nested model m1)
  Par0_m2 <- momentuHMM::getPar0(
    model=m1,
    formula=formula
  )
  
  # And fit the model
  m2 <- momentuHMM::fitHMM(
    data = elephantData,
    nbStates = 2,
    dist = dist,
    Par0 = Par0_m2$Par, 
    beta0=Par0_m2$beta,
    stateNames = stateNames,
    formula=formula
  )
```

Lastly, we will consider a more complex model (m3) where the transition
probabilities and mean and standard deviation of step lengths vary by
the cyclicality of temperature, and the concentration of the turning
angle varies by temperature. See
[https://cran.r-project.org/web/packages/momentuHMM/vignettes/momentuHMM.pdf](momentuHMM%20vignette)
for more details on using cyclical or spline functions to model complex
non-linear variation.

``` r
# Formulas for parameters of state-dependent observation distributions
  DM <- list(
    step = list(
      mean = ~ temp * cosinor(hour, period = 24),
      sd = ~ temp * cosinor(hour, period = 24)
    ),
    angle = list(
      concentration = ~ temp
    )
  )

# Initial parameters (obtained from nested model m2)
  Par0_m3 <- momentuHMM::getPar0(
    model=m2,
    formula=formula, DM=DM
  )

# And fit the model
  m3 <- momentuHMM::fitHMM(
    data = elephantData,
    nbStates = 2,
    dist = dist,
    Par0 = Par0_m3$Par, 
    beta0 = Par0_m3$beta,
    DM = DM,
    stateNames = stateNames,
    formula = formula
  )
```

We can compare the three models using Akaike’s Information Criterion
(AIC).

``` r
# Calculate and compare AIC
  aic_table <- AIC(m1,m2,m3)
# calculate delta AIC
  aic_table$deltaAIC <- round(aic_table$AIC - min(aic_table$AIC),2)
  aic_table[order(aic_table$deltaAIC),]
#>   Model      AIC deltaAIC
#> 1    m3 381322.3     0.00
#> 2    m2 382079.3   757.00
#> 3    m1 382589.9  1267.62
```

Using AIC as an estimate of the Expected Kullback-Leibler discrepancy,
we see that the most complex model, model 3, is the most supported. This
is because it is has the lowest AIC value, which means it is the model
with the minimum difference from the theoretical generating model,
relative to the other two models in the model set.

``` r
# Show model parameters, but clean them up a bit so it's easier to read

m3_results <- list(
  step_coefs = data.frame(
    parameter = colnames(m3$mle$step),
    estimate = round(as.numeric(m3$mle$step),2)
  ),
  step_params = data.frame(
    state = c("encamped", "exploratory"),
    mean = m3$CIreal$step$est["mean",],
    sd = m3$CIreal$step$est["sd",]
  ),
  angle_coefs = data.frame(
    parameter = colnames(m3$mle$angle),
    estimate = round(as.numeric(m3$mle$angle),2)
  ),
  angle_params = data.frame(
    state = c("encamped", "exploratory"),
    concentration = as.numeric(unname(m3$CIreal$angle$est))
  ),
  transition_coefs =  round(m3$mle$beta,2),
  transition_matrix = round(m3$CIreal$gamma$est,2),
  initial_distribution = round(m3$mle$delta,2)
)

m3_results
#> $step_coefs
#>                                    parameter estimate
#> 1                         mean_1:(Intercept)     5.47
#> 2                                mean_1:temp     0.00
#> 3       mean_1:cosinorCos(hour, period = 24)    -0.72
#> 4       mean_1:cosinorSin(hour, period = 24)    -0.48
#> 5  mean_1:temp:cosinorCos(hour, period = 24)     0.03
#> 6  mean_1:temp:cosinorSin(hour, period = 24)     0.01
#> 7                         mean_2:(Intercept)     6.84
#> 8                                mean_2:temp     0.01
#> 9       mean_2:cosinorCos(hour, period = 24)     0.04
#> 10      mean_2:cosinorSin(hour, period = 24)     0.10
#> 11 mean_2:temp:cosinorCos(hour, period = 24)     0.01
#> 12 mean_2:temp:cosinorSin(hour, period = 24)    -0.01
#> 13                          sd_1:(Intercept)     5.33
#> 14                                 sd_1:temp     0.01
#> 15        sd_1:cosinorCos(hour, period = 24)    -0.25
#> 16        sd_1:cosinorSin(hour, period = 24)    -0.65
#> 17   sd_1:temp:cosinorCos(hour, period = 24)     0.02
#> 18   sd_1:temp:cosinorSin(hour, period = 24)     0.02
#> 19                          sd_2:(Intercept)     6.61
#> 20                                 sd_2:temp     0.00
#> 21        sd_2:cosinorCos(hour, period = 24)    -0.07
#> 22        sd_2:cosinorSin(hour, period = 24)     0.09
#> 23   sd_2:temp:cosinorCos(hour, period = 24)     0.01
#> 24   sd_2:temp:cosinorSin(hour, period = 24)     0.00
#> 
#> $step_params
#>                   state     mean       sd
#> encamped       encamped 185.3877 172.8976
#> exploratory exploratory 865.6211 620.8493
#> 
#> $angle_coefs
#>                     parameter estimate
#> 1 concentration_1:(Intercept)    -0.32
#> 2        concentration_1:temp    -0.02
#> 3 concentration_2:(Intercept)     0.88
#> 4        concentration_2:temp    -0.01
#> 
#> $angle_params
#>         state concentration
#> 1    encamped     0.3115387
#> 2 exploratory     0.6611815
#> 
#> $transition_coefs
#>                                    1 -> 2 2 -> 1
#> (Intercept)                         -1.83  -0.81
#> temp                                -0.02  -0.02
#> cosinorCos(hour, period = 24)       -1.85   0.50
#> cosinorSin(hour, period = 24)        0.51   0.25
#> temp:cosinorCos(hour, period = 24)   0.05  -0.04
#> temp:cosinorSin(hour, period = 24)  -0.02   0.00
#> 
#> $transition_matrix
#>             encamped exploratory
#> encamped        0.88        0.12
#> exploratory     0.30        0.70
#> 
#> $initial_distribution
#>                encamped exploratory
#> ID:Salif Keita     0.53        0.47
```

There are a lot of results to consider from this model. We encourage
those interested to follow up with the
[https://cran.r-project.org/web/packages/momentuHMM/vignettes/momentuHMM.pdf](momentuHMM%20vignette).
However, notice that the mean step-lengths are much different between
the `encamped` (185 m) and `exploratory` (866 m) movement states.
Further, looking at the transition probabilities between states tells us
that when this individual is moving according to either of the two
behavioral states, they are more likely to stay in the same state
between 1 hour location intervals (at mean covariate values).

Now lets plot the covariate effects and movement track along with the
estimated movement states.

``` r
  plot(m3, plotCI = TRUE, covs = data.frame(hour=12))
#> Decoding state sequence... DONE
```

![](Example6_files/figure-gfm/plot%20HMM-1.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-2.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-3.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-4.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-5.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-6.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-7.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-8.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-9.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-10.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-11.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-12.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-13.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-14.png)<!-- -->![](Example6_files/figure-gfm/plot%20HMM-15.png)<!-- -->

Some interesting take-aways,

- The mean and standard deviation of step lengths decrease in the
  `encamped` state as temperature increases
- The mean and standard deviation of step lengths at the mean
  temperature vary cyclically over the diel period
- The mean and standard deviation of step lengths decrease very little
  or not at all in the `exploratory` state as temperature increases
- Turning angle concentratoin decreases as temperatures increase during
  the `encamped` state and less so in the `exploratory` state.
- The probability of staying in the `encamped` state increases as
  temperatures increase.
- There appears to be moderate cyclicality over the diel cycle in
  transitioning between states

Before leaving the hidden Markov model, we need to derive the states
associated with each location.

``` r
# Decode most likely state sequence
  states <- momentuHMM::viterbi(m3)
  states[which(states==1)] <- "encamped"
  states[which(states==2)] <- "exploratory"

# Derive percentage of time spent in each state
  table(states)/nrow(elephantData)
#> states
#>    encamped exploratory 
#>   0.7379145   0.2620855
```

# Diel Activity

Let’s turn to the $\texttt{Diel.Niche}$ R package and estimate the diel
phenotype of this individual by behavioral state. Since the individual
was tracked continuously from March 2008 to September 2010, lets
consider their diel phenotype on a rolling monthly basis. To do so, we
need to classify each location time into the appropriate twilight,
daytime, and nocturnal bins.

``` r
#Get time period cutoffs for each date/time  using the package suncalc 
  tod <- suncalc::getSunlightTimes(
    date = date(date(elephantData$time)),
    lat = mean(elephantData$lat,na.rm=TRUE),
    lon =  mean(elephantData$lon,na.rm=TRUE),
    keep = c("dusk", "night", "dawn","nightEnd"),
    tz = "GMT"
  )  

# Take these and extract the hours, minutes, and seconds    
  time.subset <- data.frame(
    apply(
      tod[,4:7],
      2,
      strftime,format="%H:%M:%S"
    )
  )

# Get Elephant times in hours, minutes, seconds
  elephant.times <- strftime(
    elephantData$time,
    format="%H:%M:%S"
  )

# These should be the same length  
  length(elephant.times)==nrow(time.subset)
#> [1] TRUE
  
# Create a vector to store how each observation is defined- twilight, daytime, and nighttime.
  time.cat <- rep(
    "nighttime",
    nrow(time.subset)
  )

# Find times that are b/w dawn and dusk and define that as daytime  
  index.day <- which(
    elephant.times > time.subset$dawn & elephant.times < time.subset$dusk
  )

# Find times that are before dawn (begining of daytime) and after the nightEnds
  index.dawn <- which(
    elephant.times < time.subset$dawn & elephant.times > time.subset$nightEnd
  )
  
# Find times that are after dusk and before the begining of night
  index.dusk <- which(
    elephant.times>time.subset$dusk & elephant.times<time.subset$night
  )

# Change vector elements correspond to daytime and twilight  
  time.cat[index.day] <- "daytime"  
  time.cat[c(index.dawn,index.dusk)] <- "twilight"  

# For the entire smapling period this is the frequency of classifications, disregarding particular sampling periods and behavioral state
  table(time.cat)
#> time.cat
#>   daytime nighttime  twilight 
#>     11956      8551      1627
  
# Create a new data.frame of dates, states, and time category and then split data into a list of elements
# Month and Year of sampling
  Month.Year <- paste(
    lubridate::month(elephantData$time),
    "/",
    lubridate::year(elephantData$time),
    sep=""
  )

  time.cat.dates <- data.frame(
    Month.Year,
    states,
    time.cat
  )

  dat.list <- split(
    time.cat.dates$time.cat ,
    f = as.factor(
      paste(
        time.cat.dates$Month.Year,
        time.cat.dates$states,
        sep="-"
      )
    )
  )
  
# Here is the sample size per month and behavioral state of sampling period  
  n.obs <-  as.vector(
    unlist(
      lapply(
        dat.list,
        FUN=length
      )
    )
  )
  n.obs
#>  [1] 501 243 555 189 347 397 434 310 508 212 484 236 607 137 637 107 520 152 483 189 212  11 635 109 692  52
#> [27] 600 120 601 119 701  19 620 124 490 254 631 113 409 311 520 200 558 162 650  94 663  81 701  43 531 213
#> [53] 402 342 629 115 337 383 363 357 312 407

# The General hypothesis set requires a fairly reasonable sample size. Let's drop sampling periods with < 50 observations.
  dat.list <- dat.list[-which(n.obs<50)]
  
  
# Create a frequency table  and arrange columns in the order: Twilight, Daytime, and Nighttime 
  dat.table <- lapply(
    dat.list,
    FUN=function(x)table(x)[c(3,1,2)]
  )
  
  n.units <- length(dat.table)
```

We have the data that we need in the object `dat.table`. Each list
elements includes the frequencies for twilight, daytime, and nighttime
for a specific movement behavioral state and month of the year.

Next, we need to use the `diel.fit` function to estimate the most
probable hypothesis within the General hypothesis set. Using the
`lapply` function we can fit each dataset quickly.

``` r
# For each list element, fit the General hypothesis set and extract the most probable hypothesis
  elephant.hyps <- lapply(
    dat.table,
    FUN=function(x){
      fit = diel.fit(
        t(matrix(x)),
        hyp.set = hyp.sets("General"),
        post.fit=FALSE,
        prints = FALSE
      )
    fit$ms.model
    }
  )
  

#  Grab the state names
  states.by.hyp <- sub(
    '.*-',
    '',
    as.character(names(elephant.hyps))
  )
  
# Frequency table of Supported Hypotheses by Behavioral States where each sampling period is 1 month from March 2008 to September 2010. 
  table(
    states.by.hyp,
    unlist(elephant.hyps)
  )
#>              
#> states.by.hyp D.N
#>   encamped     31
#>   exploratory  28

# This individual is always following a Diurnal-Nocturnal strategy. 
```

Now that we have the most supported model, we can use the `diel.fit`
function to estimate the posterior distributions of activity for each
sampling period and behavioral state. To change it up, lets use a for
loop to loop through each data set, saving the posterior median and
quantiles that will then use for plotting.

``` r

  # For each list element, get the posterior distributions from the most probable hypothesis. probable hypothesis

  crep <- day <- night <- matrix(
    NA, 
    ncol = 3,
    nrow = length(elephant.hyps)
  )
  for(i in 1:length(elephant.hyps)){
    out <- diel.fit(
      t(matrix(dat.table[[i]])),
      hyp.set = unlist(elephant.hyps)[i],
      post.fit=TRUE,
      bf.fit = FALSE,
      prints = FALSE
    )

    temp <- t(
      apply(
        out$post.samp[[1]][[1]],
        2,
        quantile,
        probs=c(0.025,0.5,0.975)
      )
    )
    crep[i,] <- temp[1,]
    day[i,] <- temp[2,]
    night[i,] <- temp[3,]
  }
  

# get year and month order for plotting
   month <-  as.integer(
     sub(
       "/.*",
       "",
       names(elephant.hyps)
     )
   )
   month <- sprintf(
     "%02d",
     month
   )
   year <- sub(
     '.*/',
     '',
     as.character(names(elephant.hyps))
   )
   year <- as.integer(
     sub(
       "-.*",
       "",
       year
     )
   )
   
   Sampling <- as.Date(
     paste(
       rep(
         "01",
         length(month)
        ),
       month,
       year,
       sep= "/"
     ),
     format = "%d/%m/%Y"
    )

# Arrange the data for plotting
  crep.plot <- data.frame(
    crep,
    states.by.hyp,
    Sampling
  )
  colnames(crep.plot) <- c("LCL","median", "UCL", "State", "Sampling") 
  crep.plot <- crep.plot[order(Sampling),]
  crep.plot$Sampling <- factor(crep.plot$Sampling, ordered = TRUE)
  
  day.plot <- data.frame(
    day,
    states.by.hyp,
    Sampling
  )
  colnames(day.plot) <- c("LCL","median", "UCL", "State", "Sampling") 
  day.plot <- day.plot[order(Sampling),]
  day.plot$Sampling <- factor(day.plot$Sampling, ordered = TRUE)

  night.plot <- data.frame(
    night,
    states.by.hyp,
    Sampling
  )
  colnames(night.plot) <- c("LCL","median", "UCL", "State", "Sampling") 
  night.plot <- night.plot[order(Sampling),]
  night.plot$Sampling <- factor(night.plot$Sampling, ordered = TRUE)
```

Lastly, lets plot the posterior distributions for each of the periods of
crepuscular, daytime, and nighttime by behavioral state and sampling
period.

``` r
col2 <- c("darkgrey","purple")

p1 <- ggplot(crep.plot, aes(x=Sampling, y=median,group=State,fill=State)) +
            theme_bw()+ 
            geom_line(linetype = "solid", linewidth=2) + 
            ylim(0,1)+
            geom_ribbon(aes(x=Sampling,ymin=LCL, ymax=UCL,group=State),alpha=0.4)+
            scale_fill_manual(values=col2)+
            labs(y = "P(Twilight)")+ 
            labs(x = "Sampling Month")+ 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(day.plot, aes(x=Sampling, y=median,group=State,fill=State)) +
            theme_bw()+ 
            geom_line(linetype = "solid", linewidth=2) + 
            ylim(0,1)+
            geom_ribbon(aes(x=Sampling,ymin=LCL, ymax=UCL,group=State),alpha=0.4)+
            scale_fill_manual(values=col2)+
            labs(y = "P(Daytime)")+ 
            labs(x = "Sampling Month")+ 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3 <- ggplot(night.plot, aes(x=Sampling, y=median,group=State,fill=State)) +
            theme_bw()+ 
            geom_line(linetype = "solid", linewidth=2) + 
            ylim(0,1)+
            geom_ribbon(aes(x=Sampling,ymin=LCL, ymax=UCL,group=State),alpha=0.4)+
            scale_fill_manual(values=col2)+
            labs(y = "P(Nightime)")+ 
            labs(x = "Sampling Month")+ 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(p1, p2, p3, ncol=1, nrow=3, widths = c(1,1,1))
```

![](Example6_files/figure-gfm/diel%20activitiy%20ploting-1.png)<!-- -->

# Conclusion

We found that the most supported animal movement model for this
individual African Elephant was one that allowed two movement states
where the transition probabilities, step lengths, and turning angle
parameters varied by the diel cycle and temperature. In regard to diel
activity, we found that over more than two years of continuous sampling
that this individual exclusively followed a Diurnal-Nocturnal diel
phenotype. In one sampling month (3/2008) there was high nocturnal
activity when in the `exploratory` movement state (~0.78 probability),
but this was not enough to be classified as nocturnal. We also only
found fairly smaller differences in diel activity between the two
movement states (`encamped` and `exploratory`) across the entire
sampling period. Twilight activity was also similar between the two
movement states and had the least variability across monthly sampling
periods.

# References

Wall J, Wittemyer G, LeMay V, Douglas-Hamilton I, Klinkenberg B. 2014.
Elliptical time-density model to estimate wildlife utilization
distributions. Methods Ecology and Evolution, 5:780-790.
<https://doi.org/10.1111/2041-210X.12218>
