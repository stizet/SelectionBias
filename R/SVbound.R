#' Smith and VanderWeele bound
#'
#' `SVbound()` returns a list with the SV bound. All sensitivity parameters for
#' the population of interest must be set to numbers, and the rest can be left
#' as `NULL`. The sensitivity parameters can be inserted directly or as output
#' from `sensitivityparametersM()`. If the causal estimand is expected to be
#' larger than the observational estimand, the recoding of the treatment has to
#' be done manually.
#'
#' @param whichEst Input string. Defining the causal estimand of interest.
#'   Available options are as follows. (1) Relative risk in the total
#'   population: "RR_tot", (2) Risk difference in the total population:
#'   "RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
#'   difference in the subpopulation: "RD_sub".
#' @param pY1_T1_S1 Input value. The probability P(Y=1|T=1,I_S=1). Must be
#'   between 0 and 1.
#' @param pY1_T0_S1 Input value. The probability P(Y=1|T=0,I_S=1). Must be
#'   between 0 and 1.
#' @param RR_UY_T1 Input value. The sensitivity parameter RR_UY|T=1. Must be
#'   greater than or equal to 1. Used in the bounds for the total population.
#' @param RR_UY_T0 Input value. The sensitivity parameter RR_UY|T=0. Must be
#'   greater than or equal to 1. Used in the bounds for the total population.
#' @param RR_SU_T1 Input value. The sensitivity parameter RR_SU|T=1. Must be
#'   greater than or equal to 1. Used in the bounds for the total population.
#' @param RR_SU_T0 Input value. The sensitivity parameter RR_SU|T=0. Must be
#'   greater than or equal to 1. Used in the bounds for the total population.
#' @param RR_UY_S1 Input value. The sensitivity parameter RR_UY|S=1. Must be
#'   greater than or equal to 1. Used in the bounds for the subpopulation.
#' @param RR_TU_S1 Input value. The sensitivity parameter RR_TU|S=1. Must be
#'   greater than or equal to 1. Used in the bounds for the subpopulation.
#'
#' @return A list containing the Smith and VanderWeele bound.
#' @export
#'
#' @examples
#' # Example for relative risk in the total population.
#' SVbound(whichEst = "RR_tot", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
#'  RR_UY_T1 = 2, RR_UY_T0 = 2, RR_SU_T1 = 1.7, RR_SU_T0 = 1.5)
#'
#' # Example for risk difference in the total population.
#' SVbound(whichEst = "RD_tot", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
#'  RR_UY_T1 = 2, RR_UY_T0 = 2, RR_SU_T1 = 1.7, RR_SU_T0 = 1.5)
#'
#' # Example for relative risk in the subpopulation.
#' SVbound(whichEst = "RR_sub", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
#'  RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)
#'
#' # Example for risk difference in the subpopulation.
#' SVbound(whichEst = "RD_sub", pY1_T1_S1 = 0.05, pY1_T0_S1 = 0.01,
#'  RR_UY_S1 = 2.71, RR_TU_S1 = 2.33)
#'
#' @references  Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
#'   to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and multiple
#'   inclusion criteria in observational studies" Epidemiologic Methods 11, no.
#'   1 (2022): 20220108.
#'
SVbound <- function(whichEst, pY1_T1_S1, pY1_T0_S1,
                    RR_UY_T1 = NULL, RR_UY_T0 = NULL, RR_SU_T1 = NULL,
                    RR_SU_T0 = NULL, RR_UY_S1 = NULL, RR_TU_S1 = NULL)
{
  # Check if the estimand is one of the four "RR_tot", "RD_tot", "RR_sub", "RD_sub".
  if(whichEst != "RR_tot" & whichEst != "RD_tot" & whichEst != "RR_sub" & whichEst != "RD_sub")
    stop('The estimand must be "RR_tot", "RD_tot", "RR_sub" or "RD_sub".')

  # Check if the probabilities are valid.
  if((pY1_T1_S1 < 0 | pY1_T1_S1 > 1 | pY1_T0_S1 < 0 | pY1_T0_S1 > 1))
    stop("P(Y=1|T=1,I_S=1) and P(Y=1|T=0,I_S=1) cannot be smaller than 0 or larger than 1.")

  # The observational estimands.
  RRobs = pY1_T1_S1 / pY1_T0_S1
  RDobs = pY1_T1_S1 - pY1_T0_S1

  # Calculations for the total population.
  if(whichEst == "RR_tot" | whichEst == "RD_tot")
  {
    # Check if the correct sensitivity parameters are specified.
    if(is.null(RR_UY_T1) | is.null(RR_UY_T0) | is.null(RR_SU_T1) | is.null(RR_SU_T0))
      stop("When the total population is of interest, RR_UY_T1, RR_UY_T0,
           RR_SU_T1 and RR_SU_T0 cannot be equal to NULL." )

    # Check if the sensitivity parameters are valid.
    if(RR_UY_T1 < 1 | RR_UY_T0 < 1 | RR_SU_T1 < 1 | RR_SU_T0 < 1)
      stop("All sensitivity parameters must be greater than or equal to 1.")

    # Calculate BF_1 and BF_0. Used in both "RR_tot" and "RD_tot".
    BF1 = (RR_UY_T1 * RR_SU_T1) / (RR_UY_T1 + RR_SU_T1 - 1)
    BF0 = (RR_UY_T0 * RR_SU_T0) / (RR_UY_T0 + RR_SU_T0 - 1)

    # SV bound for RR in tot pop.
    bound = BF1 * BF0
    boundRRtot = round((RRobs / bound), 2)


    # SV bound for RD in tot pop.
    bound = BF1 - pY1_T1_S1 / BF1 + pY1_T0_S1 * BF0
    boundRDtot = round((RDobs - bound), 2)


  }else
  {
    # Check if the correct sensitivity parameters are specified.
    if(is.null(RR_UY_S1) | is.null(RR_TU_S1))
      stop("When the subpopulation is of interest, RR_UY_S1 and RR_TU_S1 cannot be equal to NULL." )

    # Check if the sensitivity parameters are valid.
    if(RR_UY_S1 < 1 | RR_TU_S1 < 1)
      stop("All sensitivity parameters must be greater than or equal to 1.")

    # Calculate BF_U. Used in both "RR_sub" and "RD_sub".
    BFU = (RR_UY_S1 * RR_TU_S1) / (RR_UY_S1 + RR_TU_S1 - 1)

    # SV bound for RR in subpop.
    boundRRs = round(RRobs / BFU, 2)

    # SV bound for RD in subpop.
    bound = max((pY1_T0_S1 * (BFU - 1)), (pY1_T1_S1 * (1 - 1 / BFU)))
    boundRDs = round((RDobs - bound), 2)

  }

  # The return list.
  heading = c("SV bound")

  if(whichEst == "RR_tot"){
    values = list(boundRRtot)
  }else if(whichEst == "RD_tot"){
    values = list(boundRDtot)
  }else if(whichEst == "RR_sub"){
    values = list(boundRRs)
  }else{
    values = list(boundRDs)
  }

  returnDat = matrix(cbind(heading, values), ncol = 2)
  return(returnDat)


}
