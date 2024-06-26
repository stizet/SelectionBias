% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitivityparametersM.R
\name{sensitivityparametersM}
\alias{sensitivityparametersM}
\title{Sensitivity parameters for the Smith and VanderWeele bound and the GAF bound}
\usage{
sensitivityparametersM(
  whichEst,
  whichBound,
  Vval,
  Uval,
  Tcoef,
  Ycoef,
  Scoef,
  Mmodel,
  pY1_T1_S1,
  pY1_T0_S1
)
}
\arguments{
\item{whichEst}{Input string. Defining the causal estimand of interest.
Available options are as follows. (1) Relative risk in the total
population: "RR_tot", (2) Risk difference in the total population:
"RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
difference in the subpopulation: "RD_sub".}

\item{whichBound}{Input string. Defining the bound of interest.
Available options are as follows. (1) SV bound: "SV", (2) GAF bound: "GAF".}

\item{Vval}{Input matrix. The first column is the values of the categories of
V. The second column is the probabilities of the categories of V. If V is
continuous, use a fine grid of values and probabilities.}

\item{Uval}{Input matrix. The first column is the values of the categories of
U. The second column is the probabilities of the categories of U. If U is
continuous, use a fine grid of values and probabilities.}

\item{Tcoef}{Input vector. Two numerical elements. The first element is the
intercept in the model for the treatment. The second element is the slope
in the model for the treatment.}

\item{Ycoef}{Input vector. Three numerical elements. The first element is the
intercept in the model for the outcome. The second element is the slope for
T in the model for the outcome. The third element is the slope for U in the
model for the outcome.}

\item{Scoef}{Input matrix. Numerical matrix of size K by 4, where K is the
number of selection variables. Each row is the coefficients for one
selection variable. The first column is the intercepts in the models for
the selection variables. The second column is the slopes for V in the
models for the selection variables. The third column is the slopes for U in
the models for the selection variables. The fourth column is the slopes for
T in the models for the selection variables.}

\item{Mmodel}{Input string. Defining the models for the variables in the M
structure. If "P", the probit model is used. If "L", the logit model is}

\item{pY1_T1_S1}{Input scalar. The observed probability P(Y=1|T=1,I_S=1).}

\item{pY1_T0_S1}{Input scalar. The observed probability P(Y=1|T=0,I_S=1).
used.}
}
\value{
A list containing the sensitivity parameters and, for the SV bound,
an indicator if the treatment has been reversed.
}
\description{
\code{sensitivityparametersM()} returns a list with the sensitivity parameters
and an indicator if bias is negative and the treatment coding is reversed
for an assumed model.
}
\examples{

# Examples with no selection bias.
V = matrix(c(1, 0, 0.1, 0.9), ncol = 2)
U = matrix(c(1, 0, 0.1, 0.9), ncol = 2)
Tr = c(0, 1)
Y = c(0, 0, 1)
S = matrix(c(1, 0, 0, 0, 1, 0, 0, 0), nrow = 2, byrow = TRUE)
probT1 = 0.534
probT0 = 0.534
sensitivityparametersM(whichEst = "RR_tot", whichBound = "SV", Vval = V,
 Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "P",
 pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)

sensitivityparametersM(whichEst = "RR_tot", whichBound = "GAF", Vval = V,
 Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "P",
 pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)


# Examples with selection bias. DGP from the zika example.
V = matrix(c(1, 0, 0.85, 0.15), ncol = 2)
U = matrix(c(1, 0, 0.5, 0.5), ncol = 2)
Tr = c(-6.2, 1.75)
Y = c(-5.2, 5.0, -1.0)
S = matrix(c(1.2, 2.2, 0.0, 0.5, 2.0, -2.75, -4.0, 0.0), ncol = 4)
probT1 = 0.286
probT0 = 0.004
sensitivityparametersM(whichEst = "RR_sub", whichBound = "SV", Vval = V,
 Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "L",
 pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)

sensitivityparametersM(whichEst = "RR_sub", whichBound = "GAF", Vval = V,
 Uval = U, Tcoef = Tr, Ycoef = Y, Scoef = S, Mmodel = "L",
 pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)


}
\references{
Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.

Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and multiple
inclusion criteria in observational studies" Epidemiologic Methods 11, no.
1 (2022): 20220108.

Zetterstrom, Stina. "Bounds for selection bias using outcome probabilities"
Epidemiologic Methods 13, no. 1 (2024): 20230033
}
