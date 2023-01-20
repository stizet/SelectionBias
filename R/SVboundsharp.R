#' Check if the Smith and VanderWeele bound in the subpopulation is sharp
#'
#' `SVboundsharp()` returns a string that indicates if the SV bound is sharp, if
#' it's inconclusive or if it's not sharp.
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
#' #' @references  Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due to
#'   selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina and Waernbaum, Ingeborg. MANUSCRIPT XXX
#'
#'
SVboundsharp <- function(BF_U, prob, SVbound=NULL, AFbound=NULL)
{
  # A function that tests if the SV bound is sharp.

  # Check if 0<P(Y=1|T=0,I_S=1)<1 and BF_U>=1. If not, throw an error.
  if(any(prob < 0 | prob > 1)) stop('P(Y=1|T=0,I_S=1) not between 0 and 1.')
  if(BF_U < 1) stop('BF_U must be greater than or equal to 1.')

  # If SVbound and AFbound are not provided, just calculate the sharp limit.
  if(is.null(SVbound) & is.null(AFbound))
  {
    # Calculate the sharp limit.
    sharpLim = 1 / prob

    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, and if it larger return the message
    # that it is inconclusive.
    if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{
      returnMat = "Inconclusive."}
  }else if((is.null(SVbound) & !is.null(AFbound)) | (!is.null(SVbound) & is.null(AFbound)) )
  {
    # If SVbound or AFbound are not provided, calculate the sharp limit and
    # give a warning message that both SVbound and AFbound are needed to check
    # if the SV bound is not sharp.

    warning("To check if the SV bound is not sharp, both the SV and AF bounds must be provided.")

    # Calculate the sharp limit.
    sharpLim = 1 / prob

    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, and if it larger return the message
    # that it is inconclusive.
    if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{
      returnMat = "Inconclusive."}
  }else{
    # Calculate the sharp limit.
    sharpLim = 1 / prob

    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, and if it larger return the message
    # that it is inconclusive.
    if(BF_U <= sharpLim & SVbound <= AFbound)
    {
      returnMat = "SV bound is sharp."
    }else if(SVbound > AFbound)
    {
      returnMat = "SV bound is not sharp."}
    else{returnMat = "Inconclusive."}
  }
  return(returnMat)
}
