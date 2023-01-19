#' Check if the Smith and VanderWeele bound in the subpopulation is sharp
#'
#' `SVboundsharp()` returns a string that indicates if the SV bound is sharp or
#' if its inconclusive.
#'
#' @param BF_U Input scalar. The bounding factor for the SV bounds in the
#'   subpopulation. Must be equal to or above 1.
#' @param prob Input scalar. The probability P(Y=1|T=0,I_S=1). The eventual
#'   recoding has to be done manually.
#'
#' @return A string stating if the SV bound is sharp, or if it is inconclusive.
#' @export
#'
#' @examples
#' BF = 2
#' success = 0.4
#' SVboundsharp(BF_U = BF, prob = success)
#'
#'
SVboundsharp <- function(BF_U, prob)
{
  # A function that tests if the SV bound is sharp.

  # Check if 0<P(Y=1|T=0,I_S=1)<1 and BF_U>=1. If not, throw an error.
  if(any(prob < 0 | prob > 1)) stop('P(Y=1|T=0,I_S=1) not between 0 and 1.')
  if(BF_U < 1) stop('BF_U must be greater than or equal to 1.')

  # Calculate the sharp limit.
  sharpLim = 1 / prob

  # Test if the SV bound is sharp. If it is smaller than the limit, return
  # the message that it is sharp, and if it larger return the message
  # that it is inconclusive.
  if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{
    returnMat = "Inconclusive."}

  return(returnMat)
}
