#' Check if the Smith and VanderWeele bound in the subpopulation is sharp
#'
#' `SVboundsharp()` returns a string that indicates if the SV bound is sharp or
#' if it's inconclusive. If the bias is negative, the recoding of the treatment
#' has to be done manually.
#'
#' @param BF_U Input scalar. The bounding factor for the SV bounds in the
#'   subpopulation. Must be equal to or above 1. Can be inserted directly or as
#'   output from `sensitivityparametersM()`.
#' @param pY1_T0_S1 Input scalar. The probability P(Y=1|T=0,I_S=1).
#'
#' @return A string stating if the SV bound is sharp or inconclusive.
#' @export
#'
#' @examples
#'
#' # Example where the SV bound is sharp.
#' SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.33)
#'
#' # Example where the SV bound is inconclusive.
#' SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.8)
#'
#' @references  Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
#'   to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina, and Ingeborg Waernbaum. "SelectionBias: An R Package
#'   for Bounding Selection Bias." arXiv preprint arXiv:2302.06518 (2023).
#'
#'
SVboundsharp <- function(BF_U, pY1_T0_S1)
{
  # A function that tests if the SV bound is sharp.

  # Check if 0 < P(Y = 1|T = 0, I_S = 1) < 1 and BF_U >= 1. If not, throw an error.
  if(any(pY1_T0_S1 < 0 | pY1_T0_S1 > 1)) stop('P(Y=1|T=0,I_S=1) not between 0 and 1.')
  if(BF_U < 1) stop('BF_U must be greater than or equal to 1.')

  # Calculate the sharp limit.
  sharpLim = 1 / pY1_T0_S1

  # Test if the SV bound is sharp. If it is smaller than the limit, return
  # the message that it is sharp, and if it is larger return the message
  # that it is inconclusive.
  if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{returnMat = "Inconclusive."}

  return(returnMat)
}
