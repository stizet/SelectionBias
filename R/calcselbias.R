calcselbias <- function(Y1, Y0, Tr, S, U, V, estimand, obsProb)
{
  # A function that calculates the selection bias. The input is the conditional
  # probabilities necessary for the calculations and the causal estimand of
  # interest. The output is the selection bias and P(Y = 1|T = t, I_s = 1), t = 0,1.

  # Create the matrices for the calculations.
  # The number of categories in U and V, used to define the matrices.
  Ulen = length(U)
  Vlen = length(V)

  # The input vectors (matrices) are repeated in matrices in order to
  # perform matrix multiplication instead of loops.
  Umat = matrix(U, nrow = Ulen, ncol = Vlen, byrow = FALSE) #P(U = u).
  Vmat = matrix(V, nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(V = v).
  T1mat = matrix(Tr[1, ], nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(T = 1|V = v).
  T0mat = matrix(Tr[2, ], nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(T = 0|V = v).
  Y0mat = matrix(Y0[1, ], nrow = Ulen, ncol = Vlen, byrow = FALSE) #P(Y(0) = 1|U = u).
  Y1mat = matrix(Y1[1, ], nrow = Ulen, ncol = Vlen, byrow = FALSE) #P(Y(1) = 1|U = u).

  # The rowproduct of the selection variables.
  if(is.vector(S)){Svec = S
  }else{Svec = apply(S, 1, prod)}

  # P(I_S = 1|V = v, U = u, T = 1).
  ST1mat = matrix(Svec[1 : (length(Svec) / 4)], nrow = Ulen, ncol = Vlen, byrow = FALSE)
  # P(I_S = 1|V = v, U = u, T = 0).
  ST0mat = matrix(Svec[(length(Svec) / 4 + 1) : (length(Svec) / 2)], nrow = Ulen, ncol = Vlen, byrow = FALSE)

  # Calculate the observed estimands.
  # Extract P(Y = 1|T = 1, I_S = 1).
  pY1_T1S1 = obsProb[1]

  # Extract P(Y = 1|T = 0, I_S = 1).
  pY1_T0S1 = obsProb[2]

  # The observed relative risk and risk difference.
  obsRR = pY1_T1S1 / pY1_T0S1
  obsRD = pY1_T1S1 - pY1_T0S1

  # Calculate the causal estimands in the subpopulation.
  # Calculate P(Y(1) = 1|I_S = 1).
  Y1num = sum(Umat * Vmat * Y1mat * (ST1mat * T1mat + ST0mat * T0mat)) #The numerator.
  Y1denom = sum(Umat * Vmat * (ST1mat * T1mat + ST0mat * T0mat)) #The denominator.
  pY11_S1 = Y1num / Y1denom #P(Y(1) = 1|I_S = 1).

  # Calculate P(Y(0) = 1|I_S = 1).
  Y0num = sum(Umat * Vmat * Y0mat * (ST1mat * T1mat + ST0mat * T0mat)) #The numerator.
  Y0denom = sum(Umat * Vmat * (ST1mat * T1mat + ST0mat * T0mat)) #The denominator.
  pY01_S1 = Y0num / Y0denom #P(Y(0) = 1|I_S = 1).

  # The causal relative risk and risk difference in the subpopulation.
  subRR = pY11_S1 / pY01_S1
  subRD = pY11_S1 - pY01_S1

  # Calculate the causal estimands in the total population.
  # Calculate P(Y(1) = 1).
  pY11 = sum(Y1[1, ] * U)

  # Calculate P(Y(0) = 1).
  pY01 = sum(Y0[1, ] * U)

  # The causal relative risk and risk difference in the total population.
  totRR = pY11 / pY01
  totRD = pY11 - pY01

  # Calculate the selection bias, a difference for RD and a ratio for RR.
  if(estimand == "RD_sub"){selbias = obsRD - subRD
  }else if(estimand == "RR_sub"){selbias = obsRR / subRR
  }else if(estimand == "RD_tot"){selbias = obsRD - totRD
  }else{selbias = obsRR / totRR}

  selbias = round(selbias, digits = 14) #Round the selection bias.

  # Return the selection bias.
  returnVal = selbias

  return(returnVal)
}
