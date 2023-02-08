#' Assumption free bound for a data set
#'
#' `AFbound()` returns the assumption free bound for a dataset that consists of
#' an outcome, a treatment and a selection variable. If the bias is negative,
#' the recoding of the treatment has to be done manually.
#'
#' @param whichEst Input string. Defining the population parameter of interest.
#'   Available options are as follows. (1) Relative risk in the total
#'   population: "RR_tot", (2) Risk difference in the total population:
#'   "RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
#'   difference in the subpopulation: "RD_sub".
#' @param outcome Input vector. A binary outcome variable.
#' @param treatment Input vector. A binary treatment variable.
#' @param selection Input vector or input scalar. A binary selection variable or
#'   a selection probability.
#'
#' @return A list with the assumption free bound.
#' @export
#'
#' @examples
#' # Example with selection indicator variable.
#' y = c(0, 0, 0, 0, 1, 1, 1, 1)
#' tr = c(0, 0, 1, 1, 0, 0, 1, 1)
#' sel = c(0, 1, 0, 1, 0, 1, 0, 1)
#' AFbound(whichEst = "RR_tot", outcome = y, treatment = tr, selection = sel)
#'
#' # Example with selection probability.
#' selprob = mean(sel)
#' AFbound(whichEst = "RR_tot", outcome = y[sel==1], treatment = tr[sel==1],
#'  selection = selprob)
#'
#' # Example with simulated data.
#' n = 1000
#' tr = rbinom(n, 1, 0.5)
#' y = rbinom(n, 1, 0.2 + 0.05 * tr)
#' sel = rbinom(n, 1, 0.4 + 0.1 * tr + 0.3 * y)
#' AFbound(whichEst = "RD_tot", outcome = y, treatment = tr, selection = sel)
#'
#' @references Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and
#'   multiple inclusion criteria in observational studies" Epidemiologic
#'   Methods 11, no. 1 (2022): 20220108.
#'
AFbound <- function(whichEst, outcome, treatment, selection)
{
  # A function that calculates the assumption free bound for the bias due to
  # selection, for multiple selection variables. The input is the data and
  # which causal estimand the calculations are performed for.

  # Check if the estimand is one of the four "RR_tot", "RD_tot", "RR_sub", "RD_sub".
  if(whichEst != "RR_tot" & whichEst != "RD_tot" & whichEst != "RR_sub" & whichEst != "RD_sub")
    stop('The estimand must be "RR_tot", "RD_tot", "RR_sub" or "RD_sub".')

  y = outcome
  tr = treatment
  Is = selection

  # If the selection indicator variable is included.
  if(length(Is) > 1)
  {
    # P(I_s = 1) and P(I_s = 0).
    pIs1 = length(Is[Is == 1]) / length(Is)
    pIs0 = 1 - pIs1

    # P(T = 1|I_s = 1) and P(T = 0|I_s = 1).
    pT1_Is1 = length(tr[tr == 1 & Is == 1]) / length(Is[Is == 1])
    pT0_Is1 = length(tr[tr == 0 & Is == 1]) / length(Is[Is == 1])

    # P(Y = 1|T = 1, I_s = 1) and P(Y = 1|T = 0, I_s = 1).
    pY1_T1_Is1 = length(y[y == 1 & tr == 1 & Is == 1]) / length(tr[tr == 1 & Is == 1])
    pY1_T0_Is1 = length(y[y == 1 & tr == 0 & Is == 1]) / length(tr[tr == 0 & Is == 1])
  }else{
    # If the selection probability is included.

    # Check if the selection probability is valid, else stop.
    if(any(Is < 0 | Is > 1)) stop('P(I_s=1) not between 0 and 1.')

    # P(I_s = 1) and P(I_s = 0).
    pIs1 = Is
    pIs0 = 1 - pIs1

    # P(T = 1|I_s = 1) and P(T = 0|I_s = 1).
    pT1_Is1 = length(tr[tr == 1]) / length(tr)
    pT0_Is1 = length(tr[tr == 0]) / length(tr)

    # P(Y = 1|T = 1, I_s = 1) and P(Y = 1|T = 0, I_s = 1).
    pY1_T1_Is1 = length(y[y == 1 & tr == 1]) / length(tr[tr == 1])
    pY1_T0_Is1 = length(y[y == 1 & tr == 0]) / length(tr[tr == 0])
  }

  if(is.nan(pY1_T1_Is1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pY1_T0_Is1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pT1_Is1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pT0_Is1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')

  # Calculate the assumption free bound for the relevant parameter.
  if(whichEst == "RR_tot"){
    AFbound = round(min((pT1_Is1 * pIs1 + 2*pIs0 + pY1_T0_Is1 * pT0_Is1 * pIs1), 1) /
                      (pY1_T0_Is1 * pT1_Is1 * pIs1), 2)
  }else if(whichEst == "RD_tot"){
    AFbound = round(min((pT1_Is1 * pIs1 + 2 * pIs0 + pY1_T0_Is1 * pT0_Is1 * pIs1), 1) +
                      pY1_T1_Is1 * (1 - pT1_Is1 * pIs1) - pY1_T0_Is1, 2)
  }else if(whichEst == "RR_sub"){
    AFbound = round(min((pT1_Is1 + pY1_T0_Is1 * pT0_Is1), 1) / (pY1_T0_Is1 * pT1_Is1), 2)
  }else{
    AFbound = round(min((pT1_Is1 + pY1_T0_Is1 * pT0_Is1), 1) +
                      pY1_T1_Is1 * (1 - pT1_Is1) - pY1_T0_Is1, 2)
  }

  heading = "AF bound"
  values = list(AFbound)
  returnDat = matrix(cbind(heading, values), ncol = 2)

  return(returnDat)
}
