
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SelectionBias

<!-- badges: start -->
<!-- badges: end -->

The goal of SelectionBias is to calculate two different bounds for the
selection bias for binary variables in the extended M structure and a
bound for observed binary data. For the M structure, Smith and
VanderWeeles and the assumption free bound, are calculated based on user
input parameters. For the data, the assumption free bound is calculated
with the data as input.

## Installation

You can install the development version of SelectionBias from Github
(<https://github.com/stizet/SelectionBias>) with:

``` r
# install.packages("devtools") 
# devtools::install_github("stizet/SelectionBias")
```

## Examples

Selections of the study population can be the source of bias. Here, two
types of bounds for the selection bias is calculated; the bound by Smith
and VanderWeele (2019), which depends on untestable assumptions, and an
assumption free bound. The SV bound can be calculated for the extended M
structure. Below is a small example of how this is done:

``` r
library(SelectionBias)
pV = 0.1
pU = 0.1
pT = c(0,1)
pY = c(0,0,1)
pS = matrix(c(1,0,0,0,1,0,0,0),nrow=2,byrow=TRUE)
SVboundM(pV,pU,pT,pY,pS,"RR_tot",TRUE)
#>      [,1]                [,2] 
#> [1,] "SV bound"          1    
#> [2,] "Reverse treatment" FALSE
```

The output is the SV bound and an indicator if the treatment was recoded
if the bias in the original coding was negative.

The bias bound parameters may also be of interest. These can be
extracted with the code:

``` r
library(SelectionBias)
pV = 0.1
pU = 0.1
pT = c(0,1)
pY = c(0,0,1)
pS = matrix(c(1,0,0,0,1,0,0,0),nrow=2,byrow=TRUE)
SVboundparametersM(pV,pU,pT,pY,pS,"RR_tot",TRUE)
#>      [,1]                [,2]    
#> [1,] "BF_1"              1       
#> [2,] "RR_SU|T=1"         1       
#> [3,] "RR_UY|T=1"         1.682689
#> [4,] "BF_0"              1       
#> [5,] "RR_SU|T=0"         1       
#> [6,] "RR_UY|T=0"         1.682689
#> [7,] "Reverse treatment" FALSE
```

The output is the bias bound parameters and an indicator if the
treatment was recoded if the bias in the original coding was negative.

The AF bound can also be calculated for the extended M structure:

``` r
library(SelectionBias)
pV = 0.1
pU = 0.1
pT = c(0,1)
pY = c(0,0,1)
pS = matrix(c(1,0,0,0,1,0,0,0),nrow=2,byrow=TRUE)
AFboundM(pV,pU,pT,pY,pS,"RR_tot",TRUE)
#>      [,1]                [,2]   
#> [1,] "AF bound"          4.95166
#> [2,] "Reverse treatment" FALSE
```

The output is the AF bound and an indicator if the treatment was recoded
if the bias in the original coding was negative.

The AF bound can be calculated for the observed data as well. That can
be done in two ways, either if the selection indicator variable is
observed or if the selection probability is known. That is done as:

``` r
library(SelectionBias)
y = c(0,0,0,0,1,1,1,1)
tr = c(0,0,1,1,0,0,1,1)
sel = c(0,1,0,1,0,1,0,1)
AFbounddata(y,tr,sel,"RR_tot")
#> [1] 8

y = c(0,0,0,0,1,1,1,1)
tr = c(0,0,1,1,0,0,1,1)
selProb = 0.5
AFbounddata(y,tr,selProb,"RR_tot")
#> [1] 8
```

The output is the AF bound.

## Data example

For the purpose of illustration of the bounds we construct the simulated
dataset . The zika example is based on studies of a zika outbreak in
Brazil. See references in corresponding article and the data
documentation.

The zika data can be reached through

``` r
# library(SelectionBias)
# zika.learner
```

and used to calculate the AF bound as

``` r
library(SelectionBias)
AFbounddata(zika.learner$MC,1-zika.learner$zika,zika.learner$selIndicator,"RR_s")
#> [1] 3.002098
```

Note that in this example the treatment is reversed as discussed in the
corresponding article.
