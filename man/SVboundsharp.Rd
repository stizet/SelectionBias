% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SVboundsharp.R
\name{SVboundsharp}
\alias{SVboundsharp}
\title{Check if the Smith and VanderWeele bound in the subpopulation is sharp}
\usage{
SVboundsharp(BF_U, pY1_T0_S1)
}
\arguments{
\item{BF_U}{Input scalar. The bounding factor for the SV bounds in the
subpopulation. Must be equal to or above 1. Can be inserted directly or as
output from \code{sensitivityparametersM()}.}

\item{pY1_T0_S1}{Input scalar. The probability P(Y=1|T=0,I_S=1).}
}
\value{
A string stating if the SV bound is sharp or inconclusive.
}
\description{
\code{SVboundsharp()} returns a string that indicates if the SV bound is sharp or
if it's inconclusive. If the bias is negative, the recoding of the treatment
has to be done manually.
}
\examples{

# Example where the SV bound is sharp.
SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.33)

# Example where the SV bound is inconclusive.
SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.8)

}
\references{
Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.

Zetterstrom, Stina, and Ingeborg Waernbaum. "SelectionBias: An R Package
for Bounding Selection Bias." arXiv preprint arXiv:2302.06518 (2023).
}
