#' Check if the Smith and VanderWeele bound in the subpopulation is sharp
#'
#' `SVboundsharp()` returns a string that indicates if the SV bound is sharp, if
#' it's inconclusive or if it's not sharp. If the bias is negative, the recoding
#' of the treatment has to be done manually.
#'
#' @param BF_U Input scalar. The bounding factor for the SV bounds in the
#'   subpopulation. Must be equal to or above 1. Can be inserted directly or as
#'   output from `SVboundparametersM()`.
#' @param pY1_T0_S1 Input scalar. The probability P(Y=1|T=0,I_S=1).
#' @param SVbound Optional input scalar. The SV bound, can be inserted directly
#'   or as output from `SVbound()`. Only necessary if one wants to know if the
#'   SV bound is not sharp.
#' @param AFbound Optional input scalar. The AF bound, can be inserted directly
#'   or as output from `AFbound()`. Only necessary if one wants to know if the
#'   SV bound is not sharp.
#'
#' @return A string stating if the SV bound is sharp, inconclusive or not sharp.
#' @export
#'
#' @examples
#'
#' # Example where the SV bound is sharp.
#' SVboundsharp(BF_U = 1.56, pY1_T0_S1 = 0.33, SVbound = 1.56, AFbound = 3.0)
#'
#' # Example where the SV bound is not sharp.
#' SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9, SVbound = 2, AFbound = 1.8)
#'
#' # Example where the SV bound is inconclusive.
#' SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.8, SVbound = 2, AFbound = 3)
#'
#' @references  Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
#'   to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina and Waernbaum, Ingeborg. MANUSCRIPT XXX
#'
#'
SVboundsharp <- function(BF_U, pY1_T0_S1, SVbound=NULL, AFbound=NULL)
{
  # A function that tests if the SV bound is sharp.

  # Check if 0 < P(Y = 1|T = 0, I_S = 1) < 1 and BF_U >= 1. If not, throw an error.
  if(any(pY1_T0_S1 < 0 | pY1_T0_S1 > 1)) stop('P(Y=1|T=0,I_S=1) not between 0 and 1.')
  if(BF_U < 1) stop('BF_U must be greater than or equal to 1.')

  # Calculate the sharp limit.
  sharpLim = 1 / pY1_T0_S1

  # If SVbound and AFbound are not provided, just calculate the sharp limit.
  if(is.null(SVbound) & is.null(AFbound))
  {
    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, and if it is larger return the message
    # that it is inconclusive.
    if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{returnMat = "Inconclusive."}
  }else if((is.null(SVbound) & !is.null(AFbound)) | (!is.null(SVbound) & is.null(AFbound)))
  {
    # If one of SVbound and AFbound is provided and the other is not,
    # calculate the sharp limit and give a warning message that both SVbound
    # and AFbound are needed to check if the SV bound is not sharp.
    warning("Both the SV and AF bounds must be provided to check if the SV bound is not sharp.")

    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, and if it is larger return the message
    # that it is inconclusive.
    if(BF_U <= sharpLim){returnMat = "SV bound is sharp."}else{returnMat = "Inconclusive."}
  }else{
    # Test if the SV bound is sharp. If it is smaller than the limit, return
    # the message that it is sharp, if it is larger return the message
    # that it is inconclusive and if the AF is smaller than SV return
    # the message that it is not sharp.
    if(BF_U <= sharpLim & SVbound <= AFbound)
    {returnMat = "SV bound is sharp."}else if(SVbound > AFbound)
      {returnMat = "SV bound is not sharp."}else{returnMat = "Inconclusive."}
  }
  return(returnMat)
}
