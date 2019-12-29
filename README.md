# Linear Mixed Model in R
The project used the written functions to compute curvature measures of the data set `mousetrap::KH2017_raw`; then, fit linear mixed models exploring how each curvature measures differs by condition. Note that `dyplr` was used intentionally for data manipulations.

## Overview
The repository directories contains:  

* [funcs](): A script of a function that accepts a normalized trajectory and computes four curvature measures including the total Euclidean distance traveled, the maximum absolute deviation, the average absolute deviation, and the area under curve (AUC).  
* [lmm]():  A script that examining how each curvature measure differs by condition by fitting linear mixed models and showing the final result.

## Installation
The packages can be installed from R as follows:

``` r
install.packages("tidverse")
install.packages("mousetrap")
insstall.packages("lme4")
```

## Linear Mixed Models
Linear mixed model allows both fixed and random effects, and is particularly used when there is non independence in the data, such as arises from a hierarchical structure.

* Fixed Effects: an unknown constant that we try to estimate from the data
* Random Effects: When data share the common feature of correlation of observations within the same group and the assumption of independence of the observations is inappropriate, the use of random effects is a common and convenient way to model such grouping structure.
* A random effects approach to modeling effects is more ambitious in the sense that it attempts to say something about the wider population beyond the particular sample.

In the project, condition (typical vs atypical) is the sole covariate for fixed effect, and subject and exemplar are random intercepts to account for the repeated nature of the data. 

# Remark
This project is a revisied work from the course Stats506 taught by Dr. Henderson.
