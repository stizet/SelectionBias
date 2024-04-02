
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SelectionBias

<!-- badges: start -->
<!-- badges: end -->

The goal of SelectionBias is to calculate different bounds for the
selection bias for binary outcome and treatment variables. The
sensitivity parameters for the SV (Smith and VanderWeele) bound and the
GAF (generalized assumption-free) bound are calculated for the
generalized M-structure based on user input parameters. The SV and GAF
bounds can then be calculated either based on these or other sensitivity
parameter given by the. The CAF (counterfactual assumption-free) bound
is calculated based on sensitivity parameters and a dataset/probabilites
from data given by the user. The AF (assumption-free) bound is
calculated for a dataset/probabilities from data.

## Installation

You can install the development version of SelectionBias from Github
(<https://github.com/stizet/SelectionBias>) with:

``` r
# install.packages("devtools") 
# devtools::install_github("stizet/SelectionBias")
```

## Bound examples

Selections of the study population can be the source of bias. Here,
different types of bounds for the selection bias is calculated; the
bound by Smith and VanderWeele (2019) and the GAF bounds, which depends
on untestable assumptions, and the CAF and AF bounds.

The simulated dataset `zika_learner` is included for the purpose of
illustration of the bounds. The simulated dataset is created to emulate
real data as well as a previous example. See references in corresponding
article and the data documentation.

The zika data can be reached through

``` r
library(SelectionBias)
data(zika_learner)
attach(zika_learner)
```

The sensitivity parameters in the SV and GAF bounds are calculated in
`sensitivityparametersM()` for user defined parameters in the extended
M-structure, where causal dependencies are modeled with either probit or
logit models. Which sensitivity parameters are indicated by the argument
`whichBound`. Below is an example with binary unobserved variables and
two selection variables (zika example in corresponding article):

``` r
# library(SelectionBias)
V = matrix(c(1, 0, 0.85, 0.15), nrow = 2)
U = matrix(c(1, 0, 0.5, 0.5), nrow = 2)
Tr = c(-6.2, 1.75)
Y = c(-5.2, 5, -1)
S = matrix(c(1.2, 2.2, 0, 0.5, 2, -2.75, -4, 0), ncol = 4)

# SV bound.
sensitivityparametersM(whichEst = "RR_sub", whichBound = "SV", Vval = V,
                       Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "L",
                       pY1_T1_S1 = 0.286, pY1_T0_S1 = 0.004)
#>      [,1]                [,2]  
#> [1,] "BF_U"              1.5625
#> [2,] "RR_UY|S=1"         2.7089
#> [3,] "RR_TU|S=1"         2.3293
#> [4,] "Reverse treatment" TRUE

# GAF bound.
sensitivityparametersM(whichEst = "RR_sub", whichBound = "GAF", Vval = V,
                       Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "L",
                       pY1_T1_S1 = 0.286, pY1_T0_S1 = 0.004)
#>      [,1]  [,2]  
#> [1,] "m_S" 0.002 
#> [2,] "M_S" 0.4502
```

The output for the SV bound is the sensitivity parameters and an
indicator that states if the treatment was recoded, i.e. that the bias
in the original coding was negative. (See original article for details.)
The output for the GAF bound is the sensitivity parameters. No recoding
of the treatment is necessary since both an upper and a lower GAF bound
is defined.

The SV bound is calculated in `SVbound()` where the user input is the
sensitivity parameters. These can either be found for the extended
M-structure in `sensitivityparametersM()`, or from another source. Below
is an example where the input is the output from
`sensitivityparametersM()` above:

``` r
# library(SelectionBias)
SVbound(whichEst = "RR_sub", pY1_T1_S1 = 0.004, pY1_T0_S1 = 0.286,
        RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)
#>      [,1]       [,2]
#> [1,] "SV bound" 0.01
```

The output is the SV bound. Here, the treatment has been recoded as
indicated by `sensitivityparametersM()`. Note that the eventual recoding
of the treatment has to be done manually, as discussed in the
corresponding article.

The GAF bounds are calculated in `GAFbound()` where the user input is
the sensitivity parameters and either a dataset or probabilities from
the data. The sensitivity parameters can either be found for the
extended M-structure in `sensitivityparametersM()`, or from another
source. If interest lies in the total population, the selection variable
or selection probability must be given. If interest lies in the
subpopulation, only data on the selected individuals must be given.
Below is an example where the input is the output from
`sensitivityparametersM()` above and the `zika_learner` dataset:

``` r
# library(SelectionBias)
GAFbound(whichEst = "RR_sub", M = 0.4502, m = 0.002, 
         outcome = mic_ceph[sel_ind == 1], treatment = zika[sel_ind == 1])
#>      [,1]              [,2]  
#> [1,] "GAF lower bound" 0.55  
#> [2,] "GAF upper bound" 117.45
```

The output is the GAF lower and upper bounds. Note that the treatment
does not need to be recoded since there is both a lower and upper bound.
Please have this in mind when comparing the bounds to the SV bound.

The CAF bounds are calculated in `CAFbound()` where the user input is
the sensitivity parameters and either a dataset or probabilities from
the data. If interest lies in the total population, the selection
variable or selection probability must be given. If interest lies in the
subpopulation, only data on the selected individuals must be given.
Below is an example where the input is sensitivity parameters given by
the user and the conditional outcome and treatment probabilities (see
the documentation for more details) from the `zika_learner` dataset:

``` r
# library(SelectionBias)
CAFbound(whichEst = "RR_sub", M = 0.5, m = 0.001, outcome = c(0.286, 0.004),
          treatment = c(0.002, 0.998))
#>      [,1]              [,2]  
#> [1,] "CAF lower bound" 0.31  
#> [2,] "CAF upper bound" 125.08
```

The output is the CAF lower and upper bounds. Note that the treatment
does not need to be recoded since there is both a lower and upper bound.
Please have this in mind when comparing the bounds to the SV bound.

The AF bound is calculated in `AFbound()` where the user input is either
a dataset or probabilities from the data. If interest lies in the total
population, the selection variable or selection probability must be
given. If interest lies in the subpopulation, only data on the selected
individuals must be given.

``` r
# library(SelectionBias)
AFbound(whichEst = "RR_sub", outcome = mic_ceph[sel_ind == 1],
        treatment = zika[sel_ind == 1])
#>      [,1]             [,2]
#> [1,] "AF lower bound" 0.11
#> [2,] "AF upper bound" 261
AFbound(whichEst = "RR_sub", outcome = c(0.286, 0.004),
        treatment = c(0.002, 0.998))
#>      [,1]             [,2]  
#> [1,] "AF lower bound" 0.1   
#> [2,] "AF upper bound" 250.14
```

The output is the AF lower and upper bounds. The difference in these two
examples comes from rounding errors. Note that the treatment does not
need to be recoded since there is both a lower and upper bound. Please
have this in mind when comparing the bounds to the SV bound.

## Sharp example

The sharpness of the SV bound in the subpopulation can be assessed in
the function `SVboundsharp()`. The input is $BF_U$ and
$P(Y=1|T=0,I_S=1)$. Below is an example where the input is the output
from previous functions:

``` r
# library(SelectionBias)
SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.27)
#> [1] "SV bound is sharp."
```

The output states if the bound is sharp, or if it is inconclusive. Note
that the eventual recoding of the treatment has to be done manually, as
discussed in the corresponding article.
