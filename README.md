
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SelectionBias

<!-- badges: start -->
<!-- badges: end -->

The goal of SelectionBias is to calculate two different bounds for the
selection bias for binary outcome and treatment variables. The SV (Smith
and VanderWeele) bound is calculated for the generalized M-structure
based on user input parameters. The AF (assumption-free) bound is
calculated for a dataset.

## Installation

You can install the development version of SelectionBias from Github
(<https://github.com/stizet/SelectionBias>) with:

``` r
# install.packages("devtools") 
# devtools::install_github("stizet/SelectionBias")
```

## Bound examples

Selections of the study population can be the source of bias. Here, two
types of bounds for the selection bias is calculated; the bound by Smith
and VanderWeele (2019), which depends on untestable assumptions, and an
assumption free bound.

The sensitivity parameters in the SV bound is calculated in
`sensitivityparametersM()` for user defined parameters in the extended
M-structure, where causal dependencies are modeled with either probit or
logit models. Below is an example with binary unobserved variables and
two selection variables (zika example in corresponding article):

``` r
library(SelectionBias)
V = matrix(c(1, 0, 0.85, 0.15), nrow = 2)
U = matrix(c(1, 0, 0.5, 0.5), nrow = 2)
Tr = c(-6.2, 1.75)
Y = c(-5.2, 5, -1)
S = matrix(c(1.2, 2.2, 0, 0.5, 2, -2.75, -4, 0), ncol = 4)

sensitivityparametersM(whichEst = "RR_sub", whichBound = "SV", Vval = V, Uval = U, Tcoef = Tr,
                   Ycoef = Y, Scoef = S, Mmodel = "L", pY1_T1_S1 = 0.286,
                   pY1_T0_S1 = 0.004)
#>      [,1]                [,2]  
#> [1,] "BF_U"              1.5625
#> [2,] "RR_UY|S=1"         2.7089
#> [3,] "RR_TU|S=1"         2.3293
#> [4,] "Reverse treatment" TRUE
```

The output is the sensitivity parameters and an indicator that states if
the treatment was recoded, i.e. that the bias in the original coding was
negative. (See original article for details.)

The SV bound is calculated in `SVbound()` where the user input is the
sensitivity parameters. These can either be found for the extended
M-structure in `SVboundparametersM()`, or from another source. Below is
an example where the input from is the output from
`SVboundparametersM()` above:

``` r
library(SelectionBias)
SVbound(whichEst = "RR_sub", pY1_T1_S1 = 0.004, pY1_T0_S1 = 0.286,
        RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)
#>      [,1]       [,2]
#> [1,] "SV bound" 0.01
```

The output is the SV bound. Note that the eventual recoding of the
treatment has to be done manually, as discussed in the corresponding
article.

The AF bound is calculated for the observed data. That can be done in
two ways, either if the selection indicator variable is observed or if
the selection probability is known. The input is the outcome vector,
treatment vector, the selection vector or probability, and which causal
estimand is of interest. Below is an example illustrating both the
selection vector and probability:

``` r
library(SelectionBias)
set.seed(1)
n = 1000
tr = rbinom(n, 1, 0.5)
y = rbinom(n, 1, 0.2 + 0.05 * tr)
sel = rbinom(n, 1, 0.4 + 0.1 * tr + 0.3 * y)
selprob = mean(sel)

AFbound(whichEst = "RR_tot", outcome = y, treatment = tr, selection = sel)
#>      [,1]       [,2] 
#> [1,] "AF bound" 11.02
AFbound(whichEst = "RR_tot", outcome = y[sel==1], treatment = tr[sel==1],
        selection = selprob)
#>      [,1]       [,2] 
#> [1,] "AF bound" 11.02
```

The output is the AF bound. Note that the eventual recoding of the
treatment has to be done manually, as discussed in the corresponding
article.

## Data example

The simulated dataset `zika_learner` is included for the purpose of
illustration of the bounds. The simulated dataset is created to emulate
real data as well as a previous example. See references in corresponding
article and the data documentation.

The zika data can be reached through

``` r
# library(SelectionBias)
data(zika_learner)
```

and used to calculate the AF bound as

``` r
# library(SelectionBias)
attach(zika_learner)
AFbound(whichEst = "RR_sub", outcome = mic_ceph, treatment = 1-zika,
        selection = sel_ind)
#>      [,1]       [,2]
#> [1,] "AF bound" 3.5
```

Note that in this example the treatment is reversed as discussed in the
corresponding article.

## Sharp example

The sharpness of the SV bound in the subpopulation can be assessed in
the function `SVboundsharp()`. The input is $BF_U$, $P(Y=1|T=0,I_S=1)$
and the SV and AF bounds. Below is an example where the input is the
output from previous functions:

``` r
# library(SelectionBias)
SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.27)
#> [1] "SV bound is sharp."
```

The output states if the bound is sharp, or if it is inconclusive. There
are two optional arguments as well, SVbound and AFbound. These must be
entered if one wish to know if the bound is *not* sharp.

``` r
# library(SelectionBias)
SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.27, SVbound = 1.56, AFbound = 3.5)
#> [1] "SV bound is sharp."
```

The output states if the bound is sharp, inconclusive or not sharp. Note
that the eventual recoding of the treatment has to be done manually, as
discussed in the corresponding article.
