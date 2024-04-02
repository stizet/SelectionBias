calcGAFparameters <- function(Y1, Y0, estimand)
{
  # A function that calculates the GAF sensitivity parameters. The input
  # is the conditional outcome probabilities necessary for the calculations and
  # the causal estimand of interest. The output is the sensitivity parameters.

  # Calculate the sensitivity parameters.

  # m_S = min_t[min_u(P(Y = 1|T = t, I_S = 1, U = u))].
  # m_T = min_t[min_u(P(Y = 1|T = t, U = u))].
  m_S = m_T = min(min(Y1[1, ]), min(Y0[1, ]))

  # M_S = max_t[max_u(P(Y = 1|T = t, I_S = 1, U = u))].
  # M_T = max_t[max_u(P(Y = 1|T = t, U = u))].
  M_S = M_T = max(max(Y1[1, ]), max(Y0[1, ]))

  if(estimand == "RD_sub" | estimand == "RR_sub") # The subpopulation.
  {
    returnVec = round(c(m_S, M_S), 4)
  }else{ #The total population.
    returnVec = round(c(m_T, M_T), 4)
  }
  return(returnVec)
}
