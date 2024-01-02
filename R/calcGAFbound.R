calcGAFbound <- function(estimand, M, m, y, tr, sel, bound)
{
  # A function that calculates the GAF bound. The input is the conditional
  # outcome probabilities necessary for the calculations and the causal
  # estimand of interest. The output is the bound.

  # Check if the estimand is one of the four "RR_tot", "RD_tot", "RR_sub", "RD_sub".
  if(estimand != "RR_tot" & estimand != "RD_tot" & estimand != "RR_sub" & estimand != "RD_sub")
    stop('The estimand must be "RR_tot", "RD_tot", "RR_sub" or "RD_sub".')

  if(length(y) < 2 | length(tr) < 2)
    stop('The length of the arguments for the outcome and treatment must be at least 2.
         See documentation for more details.')

  # Assumes all input is probabilities. Will overwrite if not.
  pS1 = sel[1]
  pT1_S1 = tr[1]
  pT0_S1 = tr[2]
  pY1_T1_S1 = y[1]
  pY1_T0_S1 = y[2]

  if(length(sel) > 1) # If the selection indicator variable is included.
    {
    pS1 = length(sel[sel == 1]) / length(sel)
    if(length(tr) > 2){pT1_S1 = length(tr[tr == 1 & sel == 1]) / length(sel[sel == 1])
                      pT0_S1 = length(tr[tr == 0 & sel == 1]) / length(sel[sel == 1])}
    if(length(y) > 2){pY1_T1_S1 = length(y[y == 1 & tr == 1 & sel == 1]) / length(tr[tr == 1 & sel == 1])
                      pY1_T0_S1 = length(y[y == 1 & tr == 0 & sel == 1]) / length(tr[tr == 0 & sel == 1])}
  }else if(length(sel) == 1) # If the selection probability is included.
    {
    if(length(tr) > 2){pT1_S1 = length(tr[tr == 1]) / length(tr)
                        pT0_S1 = length(tr[tr == 0]) / length(tr)}
    if(length(y) > 2){pY1_T1_S1 = length(y[y == 1 & tr == 1]) / length(tr[tr == 1])
                      pY1_T0_S1 = length(y[y == 1 & tr == 0]) / length(tr[tr == 0])}
  }

  if(is.nan(pY1_T1_S1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pY1_T0_S1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pT1_S1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if(is.nan(pT0_S1)) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')

  if((pY1_T1_S1 < 0 | pY1_T1_S1 > 1 | pY1_T0_S1 < 0 | pY1_T0_S1 > 1))
    stop("P(Y=1|T=1,I_S=1) and P(Y=1|T=1,I_S=1) cannot be smaller than 0 or larger than 1.")
  if((pT1_S1 < 0 | pT1_S1 > 1 | pT0_S1 < 0 | pT0_S1 > 1))
    stop("P(T=1|I_S=1) and P(T=0|I_S=1) cannot be smaller than 0 or larger than 1.")
  if((pS1 < 0 | pS1 > 1))
    stop("P(I_S=1) cannot be smaller than 0 or larger than 1.")

  if(bound == "GAF")
  {
    if(m > min(pY1_T0_S1, pY1_T1_S1) | M < max(pY1_T0_S1, pY1_T1_S1))
      stop('"M" must be larger than max_t(P(Y=1|T=t,I_s=1)) and "m" must be smaller than min_t(P(Y=1|T=t,I_s=1)).')
  }


  if(estimand == "RR_sub") # RR_S.
  {
    LB = (pY1_T1_S1 * pT1_S1 + pT0_S1 * m) / (pY1_T0_S1 * pT0_S1 + pT1_S1 * M)
    UB = (pY1_T1_S1 * pT1_S1 + pT0_S1 * M) / (pY1_T0_S1 * pT0_S1 + pT1_S1 * m)
  }else if(estimand == "RD_sub") # RD_S.
  {
    LB = (pY1_T1_S1 * pT1_S1 + pT0_S1 * m) - (pY1_T0_S1 * pT0_S1 + pT1_S1 * M)
    UB = (pY1_T1_S1 * pT1_S1 + pT0_S1 * M) - (pY1_T0_S1 * pT0_S1 + pT1_S1 * m)
  }else if(estimand == "RR_tot") # RR_T.
  {
    LB = (pY1_T1_S1 * pT1_S1 * pS1 + (1 - pT1_S1 * pS1) * m) /
      (pY1_T0_S1 * pT0_S1 * pS1 + (1 - pT0_S1 * pS1) * M)
    UB = (pY1_T1_S1 * pT1_S1 * pS1 + (1 - pT1_S1 * pS1) * M) /
      (pY1_T0_S1 * pT0_S1 * pS1 + (1 - pT0_S1 * pS1) * m)
  }else # RD_T.
  {
    LB = (pY1_T1_S1 * pT1_S1 * pS1 + (1 - pT1_S1 * pS1) * m) -
      (pY1_T0_S1 * pT0_S1 * pS1 + (1 - pT0_S1 * pS1) * M)
    UB = (pY1_T1_S1 * pT1_S1 * pS1 + (1 - pT1_S1 * pS1) * M) -
      (pY1_T0_S1 * pT0_S1 * pS1 + (1 - pT0_S1 * pS1) * m)
  }

  returnVec = c(LB, UB)
  return(returnVec)
}
