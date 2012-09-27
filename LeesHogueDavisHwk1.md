Resampling Homework 
========================================================

Eitan Lees, Susannah Hogue, Tyler Davis


```
## Loading required package: PASWR
```

```
## Loading required package: e1071
```

```
## Loading required package: class
```

```
## Loading required package: MASS
```

```
## Loading required package: lattice
```

```
## [1] "2012-09-06 13:07:25 EDT"
```


Prob 1. Suppose you conduct an experiment by injecting a drug into mice.  Their times for running a maze are 8, 10, and 15 seconds; the times for two control mice are 5 and 9 seconds.

 * Compute the difference in mean times between the treatment group and the control group.


```r
treat <- c(8, 10, 15)
control <- c(5, 9)
OBS <- mean(treat) - mean(control)
OBS
```

```
## [1] 4
```


 * Write out all possible permutations of these times to the two groups and calculate the difference in means.


```r
DATA <- c(treat, control)
ANS <- t(Combinations(5, 2))
nn <- dim(ANS)[1]


means <- numeric(nn)

for (i in 1:nn) {
    controlVals <- DATA[ANS[i, ]]
    treatmentVals <- DATA[-ANS[i, ]]
    
    cat("Control: ", controlVals, "\tTreatment:", treatmentVals)
    
    means[i] <- mean(controlVals) - mean(treatmentVals)
    
    cat("\tDiff in Means: ", means[i], "\n")
}
```

```
## Control:  8 10 	Treatment: 15 5 9	Diff in Means:  -0.6667 
## Control:  8 15 	Treatment: 10 5 9	Diff in Means:  3.5 
## Control:  10 15 	Treatment: 8 5 9	Diff in Means:  5.167 
## Control:  8 5 	Treatment: 10 15 9	Diff in Means:  -4.833 
## Control:  10 5 	Treatment: 8 15 9	Diff in Means:  -3.167 
## Control:  15 5 	Treatment: 8 10 9	Diff in Means:  1 
## Control:  8 9 	Treatment: 10 15 5	Diff in Means:  -1.5 
## Control:  10 9 	Treatment: 8 15 5	Diff in Means:  0.1667 
## Control:  15 9 	Treatment: 8 10 5	Diff in Means:  4.333 
## Control:  5 9 	Treatment: 8 10 15	Diff in Means:  -4
```


 * What proportion of the differences are as large or larger than the observed difference in mean times?


```r
p <- sum(means >= OBS)/length(means)
p
```

```
## [1] 0.2
```


 * For each permutation, calculate the mean of the treatment group only.  What proportion of these means are as large or larger that the observed mean of the treatment group?


```r
mT <- numeric(nn)
for (i in 1:nn) {
    mT[i] <- mean(DATA[-ANS[i, ]])
}

tp <- sum(mT >= mean(treat))/length(mT)
tp
```

```
## [1] 0.2
```


Prob 2. Flight Delays --- If you have ever traveled by air, you probably have experienced the frustration of flight delay.  The Bureau of Transportation Statistics maintains data on all aspects of air travel, including flight delays at departure and arrival at [http://www.btw.gov/xml/ontimesummarystatistics/src/index.xml](http://www.btw.gov/xml/ontimesummarystatistics/src/index.xml).  LaGuardia Airport (LGA) is one of three major airports that serves the New York City metropolitan area.  In 2008, over 23 million passengers and over 375,000 planes flew in or out of LGA.  United Airlines and American Airlines are two major airlines that schedule services at LGA.  The data set **FlightDelays** contains information on all 4029 departures of these two airlines from LGA during May and June 2009.

  * Conduct a two-sided permutation test to see if there is evidence that the mean delay times are different for the two airlines.


```r
site <- "http://www1.appstate.edu/~arnholta/Data/FlightDelays.csv"
FlightDelays <- read.csv(file = url(site))
head(FlightDelays)
```

```
  ID Carrier FlightNo Destination DepartTime Day Month FlightLength Delay
1  1      UA      403         DEN      4-8am Fri   May          281    -1
2  2      UA      405         DEN     8-Noon Fri   May          277   102
3  3      UA      409         DEN      4-8pm Fri   May          279     4
4  4      UA      511         ORD     8-Noon Fri   May          158    -2
5  5      UA      667         ORD      4-8am Fri   May          143    -3
6  6      UA      669         ORD      4-8am Fri   May          150     0
  Delayed30
1        No
2       Yes
3        No
4        No
5        No
6        No
```

```r
ANS <- with(data = FlightDelays, tapply(Delay, Carrier, mean))
ANS
```

```
   AA    UA 
10.10 15.98 
```

```r
obsMeanDiffCarrier <- ANS[2] - ANS[1]
obsMeanDiffCarrier
```

```
   UA 
5.886 
```

```r
with(data = FlightDelays, table(Carrier))
```

```
Carrier
  AA   UA 
2906 1123 
```

```r
#
N <- 10^4 - 1
MeanDiffCarrier <- numeric(N)
for (i in 1:N) {
    # sample of size 1123 # of UA flights from the 4029 total
    index <- sample(4029, size = 1123, replace = FALSE)
    MeanDiffCarrier[i] <- mean(FlightDelays$Delay[index]) - mean(FlightDelays$Delay[-index])
}
hist(MeanDiffCarrier, col = "blue", breaks = "Scott", xlab = "", main = "Mean Delay Difference Between Carriers")
abline(v = obsMeanDiffCarrier, col = "red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
pvalue <- ((min(sum(MeanDiffCarrier >= obsMeanDiffCarrier), sum(MeanDiffCarrier <= 
    obsMeanDiffCarrier)) + 1)/(N + 1)) * 2
pvalue
```

```
[1] 4e-04
```


The _p_-value for testing to see if there is evidence the mean delay times are different for the two airlines ($H_0: \mu_{DelayAmerican} = \mu_{DelayUnited}$ versus $H_a:\mu_{DelayAmerican} \neq \mu_{DelayUnited}$) is 4e-04.  Such a small _p_-value suggests the mean delay times for the two airlines are not the same.

  * The flight delays occured in May and June of 2009.  Conduct a two-sided permutation test to see if the difference in mean delay times between the 2 months is statistically significant.


```r
ANS <- with(data = FlightDelays, tapply(Delay, Month, mean))
ANS
```

```
##   June    May 
## 14.548  8.884
```

```r
obsMeanDiffMonth <- ANS[2] - ANS[1]
obsMeanDiffMonth
```

```
##    May 
## -5.663
```

```r
with(data = FlightDelays, table(Month))
```

```
## Month
## June  May 
## 2030 1999
```

```r
N <- 10^4 - 1
MeanDiffMonth <- numeric(N)
for (i in 1:N) {
    # sample of size 1999 # of UA flights from the 4029 total
    index <- sample(4029, size = 1999, replace = FALSE)
    MeanDiffMonth[i] <- mean(FlightDelays$Delay[index]) - mean(FlightDelays$Delay[-index])
}
hist(MeanDiffMonth, col = "blue", breaks = "Scott", xlab = "", main = "Mean Delay Difference Between May and June")
abline(v = obsMeanDiffMonth, col = "red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
pvalue <- ((min(sum(MeanDiffMonth >= obsMeanDiffMonth), sum(MeanDiffMonth <= 
    obsMeanDiffMonth)) + 1)/(N + 1)) * 2
pvalue
```

```
## [1] 2e-04
```

  
  * Compute the proportion of times that each carrier's flight was delayed more than 20 minutes.  Conduct a two-sided test to see if the difference in these proportions is statistically significant.


```r
site <- "http://www1.appstate.edu/~arnholta/Data/FlightDelays.csv"
FlightDelays <- read.csv(file = url(site))
head(FlightDelays)
```

```
##   ID Carrier FlightNo Destination DepartTime Day Month FlightLength Delay
## 1  1      UA      403         DEN      4-8am Fri   May          281    -1
## 2  2      UA      405         DEN     8-Noon Fri   May          277   102
## 3  3      UA      409         DEN      4-8pm Fri   May          279     4
## 4  4      UA      511         ORD     8-Noon Fri   May          158    -2
## 5  5      UA      667         ORD      4-8am Fri   May          143    -3
## 6  6      UA      669         ORD      4-8am Fri   May          150     0
##   Delayed30
## 1        No
## 2       Yes
## 3        No
## 4        No
## 5        No
## 6        No
```

```r
ANS <- with(data = FlightDelays, tapply(Delay > 20, Carrier, mean))
ANS
```

```
##     AA     UA 
## 0.1693 0.2128
```

```r
obsMeanDiffCarrier <- ANS[2] - ANS[1]
obsMeanDiffCarrier
```

```
##      UA 
## 0.04352
```

```r

N <- 10^4 - 1
MeanDiffCarrier <- numeric(N)

with(data = FlightDelays, tapply(Delay > 20, Carrier, sum))
```

```
##  AA  UA 
## 492 239
```

```r

for (i in 1:N) {
    index <- sample(731, size = 239, replace = FALSE)
    MeanDiffCarrier[i] <- mean(FlightDelays$Delay[index]) - mean(FlightDelays$Delay[-index])
}
hist(MeanDiffCarrier, col = "blue", breaks = "Scott", xlab = "", main = "Mean Delay > 20min Difference Between Carriers")
abline(v = obsMeanDiffCarrier, col = "red")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
pvalue <- ((min(sum(MeanDiffCarrier >= obsMeanDiffCarrier), sum(MeanDiffCarrier <= 
    obsMeanDiffCarrier)) + 1)/(N + 1)) * 2
pvalue
```

```
## [1] 0.949
```


  * Compute the variance in the flight delay lengths for each carrier.  Conduct a test to see if the variance of United Airline is greater than that of American Airlines.


```r
ANS <- with(data = FlightDelays, tapply(Delay, Carrier, var))
ANS
```

```
##   AA   UA 
## 1606 2038
```

```r
names(ANS)[which.max(ANS)]
```

```
## [1] "UA"
```

  
  * Find the 25% trimmed mean of delay times for United Airlines and American Airlines.  Conduct a test to see if the difference in trimmed means is statistically significant.


```r
ANS <- with(data = FlightDelays, tapply(Delay, Carrier, mean, trim = 0.25))
ANS
```

```
##      AA      UA 
## -2.5702 -0.7957
```

```r
obsMeanDiffCarrier <- ANS[2] - ANS[1]
obsMeanDiffCarrier
```

```
##    UA 
## 1.774
```

```r
with(data = FlightDelays, table(Carrier))
```

```
## Carrier
##   AA   UA 
## 2906 1123
```

```r
#
N <- 10^4 - 1
MeanDiffCarrier <- numeric(N)
for (i in 1:N) {
    # sample of size 1123 # of UA flights from the 4029 total
    index <- sample(4029, size = 1123, replace = FALSE)
    MeanDiffCarrier[i] <- mean(FlightDelays$Delay[index], trim = 0.25) - mean(FlightDelays$Delay[-index], 
        trim = 0.25)
}
hist(MeanDiffCarrier, col = "blue", breaks = "Scott", xlab = "", xlim = c(-1, 
    2), main = "Mean Delay Difference Between Carriers")
abline(v = obsMeanDiffCarrier, col = "red")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
pvalue <- ((min(sum(MeanDiffCarrier >= obsMeanDiffCarrier), sum(MeanDiffCarrier <= 
    obsMeanDiffCarrier)) + 1)/(N + 1)) * 2
pvalue
```

```
## [1] 2e-04
```

```r

```

  
  * Compute the proportions of times the flights in May and in June were delayed more than 20 minutes, and conduct a two-sided test of whether the difference between months is statistically significant.


```r
ANS <- with(data = FlightDelays, tapply(Delay > 20, Month, mean))
ANS
```

```
##   June    May 
## 0.1961 0.1666
```

```r
obsMeanDiffMonth <- ANS[2] - ANS[1]
obsMeanDiffMonth
```

```
##      May 
## -0.02948
```

```r
with(data = FlightDelays, table(Month))
```

```
## Month
## June  May 
## 2030 1999
```

```r
N <- 10^4 - 1

with(data = FlightDelays, tapply(Delay > 20, Carrier, sum))
```

```
##  AA  UA 
## 492 239
```

```r

MeanDiffMonth <- numeric(N)
for (i in 1:N) {
    # sample of size 1999 # of UA flights from the 4029 total
    index <- sample(731, size = 239, replace = FALSE)
    MeanDiffMonth[i] <- mean(FlightDelays$Delay[index]) - mean(FlightDelays$Delay[-index])
}
hist(MeanDiffMonth, col = "blue", breaks = "Scott", xlab = "", main = "Mean Delay Difference Between May and June")
abline(v = obsMeanDiffMonth, col = "red")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
pvalue <- ((min(sum(MeanDiffMonth >= obsMeanDiffMonth), sum(MeanDiffMonth <= 
    obsMeanDiffMonth)) + 1)/(N + 1)) * 2
pvalue
```

```
## [1] 0.9178
```

  
  * Compute the variances of the flight delay times in May and June, and then conduct a two-sided test of whether the ratio of variances is statistically different from 1.


```r
ANS <- with(data = FlightDelays, tapply(Delay, Month, var))
ANS
```

```
## June  May 
## 2070 1376
```

```r
obsMeanDiffMonth <- ANS[1]/ANS[2]
obsMeanDiffMonth
```

```
##  June 
## 1.505
```

```r

with(data = FlightDelays, table(Month))
```

```
## Month
## June  May 
## 2030 1999
```

```r
N <- 10^4 - 1
MeanDiffMonth <- numeric(N)
for (i in 1:N) {
    # sample of size 1999 # of UA flights from the 4029 total
    index <- sample(4029, size = 1999, replace = FALSE)
    MeanDiffMonth[i] <- var(FlightDelays$Delay[index])/var(FlightDelays$Delay[-index])
}
hist(MeanDiffMonth, col = "blue", breaks = "Scott", xlab = "", main = "Mean Delay Difference Between May and June")
abline(v = obsMeanDiffMonth, col = "red")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

```r
pvalue <- ((min(sum(MeanDiffMonth >= obsMeanDiffMonth), sum(MeanDiffMonth <= 
    obsMeanDiffMonth)) + 1)/(N + 1)) * 2
pvalue
```

```
## [1] 0.0342
```

  
Prob 3. Black Spruce --- Black spruce (_Picea_ _marina_) is a species of slow-growing coniferous tree found across the northern part of North America.  It is commonly found on wet organic soils.  In a study conducted in the 1990s, a biologist interested in factors affecting the growth of the black spruce planted seedlings on sites located in boreal peatlands in northern Manitoba, Canada (Camill et al. (2010)).  The data set **Spruce** contains a part of the data from the study.  Seventy-two black spruce seedlings were planted in four plots under varying conditions (fertilizer - no fertilizer, competition - no competition) and their heights and diameters were measured over the course of 5 years.   
  
  * Conduct a test to see if the difference of the means in how much the seedlings grew over the course of the study under competition and without competition is statistically significant.


```r
site <- "http://www1.appstate.edu/~arnholta/Data/Spruce.csv"
Spruce <- read.csv(file = url(site))
head(Spruce)
```

```
  Tree Competition Fertilizer Height0 Height5 Diameter0 Diameter5
1    1          NC          F    15.0    60.0     1.984       7.4
2    2          NC          F     9.0    45.2     1.191       5.2
3    3          NC          F    12.0    42.0     1.786       5.7
4    4          NC          F    13.7    49.5     1.587       6.4
5    5          NC          F    12.0    47.3     1.587       6.2
6    6          NC          F    12.0    56.4     1.587       7.4
  Ht.change Di.change
1      45.0     5.416
2      36.2     4.009
3      30.0     3.914
4      35.8     4.812
5      35.3     4.613
6      44.4     5.812
```

```r

result <- with(data = Spruce, tapply(Ht.change, Competition, mean))
result
```

```
    C    NC 
25.70 36.16 
```

```r

DATA <- (Spruce$Ht.change)
obsMeanDiff <- result[2] - result[1]
obsMeanDiff
```

```
   NC 
10.46 
```

```r

with(data = Spruce, table(Competition))
```

```
Competition
 C NC 
36 36 
```

```r

N <- 10^4 - 1
MeanDiff <- numeric(N)
for (i in 1:N) {
    index <- sample(72, size = 36, replace = FALSE)
    MeanDiff[i] <- mean(DATA[index]) - mean(DATA[-index])
}

hist(MeanDiff, col = "blue", breaks = "Scott", xlab = "", xlim = c(-20, 20), 
    main = "Mean Hight Difference Between Trees with Competition and without")

abline(v = obsMeanDiff, col = "red")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

```r

pvalue <- ((min(sum(MeanDiff >= obsMeanDiff), sum(MeanDiff <= obsMeanDiff)) + 
    1)/(N + 1)) * 2
pvalue
```

```
[1] 2e-04
```

  
Prob 4. The file **Phillies2009** contains data from the 2009 season for the baseball team of the Philadelphia Phillies.
  * Find the mean number of strikeouts per game for the games played at home and the games played away from home.


```r
site <- "http://www1.appstate.edu/~arnholta/Data/Phillies2009.csv"
Phillies2009 <- read.csv(file = url(site))
head(Phillies2009)
```

```
    Date Location Outcome Hits Doubles HomeRuns StrikeOuts
1  5-Apr     Home    Lose    4       2        0          6
2  7-Apr     Home    Lose    6       1        0          3
3  8-Apr     Home     Win   11       3        1          6
4 10-Apr     Away    Lose    7       2        1          3
5 11-Apr     Away     Win   15       3        1          6
6 12-Apr     Away     Win   13       3        2          4
```

```r

result <- tapply(Phillies2009$StrikeOuts, Phillies2009$Location, mean)
result
```

```
 Away  Home 
7.309 6.951 
```

```r
obsMeanDiff <- result[2] - result[1]
obsMeanDiff
```

```
  Home 
-0.358 
```


  * Perform a permutation test to see if the difference in the means is statistically significant.


```r
DATA <- Phillies2009$StrikeOuts

with(data = Phillies2009, table(Location))
```

```
## Location
## Away Home 
##   81   81
```

```r

N <- 10^4 - 1
MeanDiff <- numeric(N)
for (i in 1:N) {
    index <- sample(162, size = 81, replace = FALSE)
    MeanDiff[i] <- mean(DATA[index]) - mean(DATA[-index])
}

hist(MeanDiff, col = "blue", breaks = "Scott", xlab = "", main = "Mean Difference Between Home and Away Games")

abline(v = obsMeanDiff, col = "red")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r

pvalue <- ((min(sum(MeanDiff >= obsMeanDiff), sum(MeanDiff <= obsMeanDiff)) + 
    1)/(N + 1)) * 2
pvalue
```

```
## [1] 0.4284
```



```
## R version 2.15.1 (2012-06-22)
## Platform: x86_64-pc-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] PASWR_1.1      lattice_0.20-6 MASS_7.3-18    e1071_1.6     
## [5] class_7.3-3    knitr_0.8     
## 
## loaded via a namespace (and not attached):
## [1] digest_0.5.2   evaluate_0.4.2 formatR_0.6    grid_2.15.1   
## [5] plyr_1.7.1     stringr_0.6.1  tools_2.15.1
```

