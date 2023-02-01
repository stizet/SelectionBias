calcSVbound <- function(Y1, Y0, Tr, S, U, V, estimand, obsProb)
{
  # A function that calculates the SV bound. The input is the conditional
  # probabilities necessary for the calculations and the causal estimand
  # of interest. The output is the SV bound and the sensitivity parameters.

  # The observed probabilites.
  pY_T1 = obsProb[1]
  pY_T0 = obsProb[2]

  # The number of categories in U and V, used to define the matrices.
  Ulen = length(U)
  Vlen = length(V)

  # The input vectors (matrices) are repeated in matrices in order to
  # perform matrix multiplication instead of loops.
  Umat = matrix(U, nrow = Ulen, ncol = Vlen, byrow = FALSE) #P(U = u).
  Vmat = matrix(V, nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(V = v).
  T1mat = matrix(Tr[1, ], nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(T = 1|V = v).
  T0mat = matrix(Tr[2, ], nrow = Ulen, ncol = Vlen, byrow = TRUE) #P(T = 0|V = v).

  # The rowproduct of the selection variables.
  if(is.vector(S)){Svec = S
  }else{Svec = apply(S, 1, prod)}

  # P(I_S = 1|V = v, U = u, T = 1).
  ST1mat = matrix(Svec[1 : (length(Svec) / 4)], nrow = Ulen, ncol = Vlen, byrow = FALSE)
  # P(I_S = 1|V = v, U = u, T = 0).
  ST0mat = matrix(Svec[(length(Svec) / 4 + 1) : (length(Svec) / 2)], nrow = Ulen, ncol = Vlen, byrow = FALSE)

  # Calculate the probabilities P(U = u|T = t, I_S = s).
  # Different parts of these are needed for both populations.

  #P(U = u|T = 1, I_S = 1).
  UT1S1num = Vmat * Umat * T1mat * ST1mat #The matrix of all combinations of U and V.
  UT1S1num = apply(UT1S1num, 1, sum) #The numerator.
  UT1S1denom = sum(UT1S1num) #The denominator.
  pUT1S1 = UT1S1num / UT1S1denom #P(U = u|T = 1, I_S = 1).

  #P(U = u|T = 0, I_S = 1).
  UT0S1num = Vmat * Umat * T0mat * ST0mat #The matrix of all combinations of U and V.
  UT0S1num = apply(UT0S1num, 1, sum) #The numerator.
  UT0S1denom = sum(UT0S1num) #The denominator.
  pUT0S1 = UT0S1num / UT0S1denom #P(U = u|T = 0, I_S = 1).

  #P(U = u|T = 1, I_S = 0).
  UT1S0num = Vmat * Umat * T1mat * (1 - ST1mat) #The matrix of all combinations of U and V.
  UT1S0num = apply(UT1S0num, 1, sum) #The numerator.
  UT1S0denom = sum(UT1S0num) #The denominator.
  pUT1S0 = UT1S0num / UT1S0denom #P(U = u|T = 1, I_S = 0).

  #P(U = u|T = 0, I_S = 0).
  UT0S0num = Vmat * Umat * T0mat * (1 - ST0mat) #The matrix of all combinations of U and V.
  UT0S0num = apply(UT0S0num, 1, sum) #The numerator.
  UT0S0denom = sum(UT0S0num) #The denominator.
  pUT0S0 = UT0S0num / UT0S0denom #P(U = u|T = 0, I_S = 0).


  # Calculate the sensitivity parameters and bounds for the different populations.
  if(estimand == "RD_sub" | estimand == "RR_sub") # The subpopulation.
  {
    # RR_(UY|S=1), max_t[max_u(P(Y = 1|T = t, I_S = 1, U = u))/min_u(P(Y = 1|T = t, I_S = 1, U = u))].
    RR_UY_S1 = max((max(Y1[1, ]) / min(Y1[1, ])), (max(Y0[1, ]) / min(Y0[1, ])))
    # RR_(TU|S=1), max_u(P(U = u|T = 1, I_S = 1)/P(U = u|T = 0, I_S = 1)).
    RR_TU_S1 = max(pUT1S1 / pUT0S1)

    # BF_U = (RR_(UY|S=1) * RR_(TU|S=1)) / (RR_(UY|S=1) + RR_(TU|S=1) - 1).
    BF_U = (RR_UY_S1 * RR_TU_S1) / (RR_UY_S1 + RR_TU_S1 - 1)

    # Bounds for the relative risk and risk difference.
    boundRR_s = round(BF_U, digits = 14)
    boundRD_s = round(max(pY_T0 * (BF_U - 1), pY_T1 * (1 - 1 / BF_U)), digits = 14)

    # The return vector.
    if(estimand == "RD_sub"){returnVec = round(c(boundRD_s, BF_U, RR_UY_S1, RR_TU_S1, pY_T1,pY_T0), 4)
    }else{returnVec = round(c(boundRR_s, BF_U, RR_UY_S1, RR_TU_S1), 4)}
  }else{ #The total population.
    # RR_(UY|T=1), max_u(P(Y = 1|T = 1, U = u)) / min_u(P(Y = 1|T = 1, U = u))
    RR_UY_T1 = max(Y1[1, ]) / min(Y1[1, ])
    # RR_(UY|T=0), max_u(P(Y = 1|T = 0, U = u)) / min_u(P(Y = 1|T = 0, U = u))
    RR_UY_T0 = max(Y0[1, ]) / min(Y0[1, ])

    # RR_(SU|T=1), max_u(P(U = u|T = 1, I_S = 1) / P(U = u|T = 1, I_S = 0)).
    RR_SU_T1 = max(pUT1S1 / pUT1S0)
    # RR_(SU|T=0), max_u(P(U = u|T = 0, I_S = 0) / P(U = u|T = 0, I_S = 1)).
    RR_SU_T0 = max(pUT0S0 / pUT0S1)

    # BF_1 and BF_0.
    BF1 = (RR_UY_T1 * RR_SU_T1) / (RR_UY_T1 + RR_SU_T1 - 1)
    BF0 = (RR_UY_T0 * RR_SU_T0) / (RR_UY_T0 + RR_SU_T0 - 1)

    # The bounds for the relative risk and risk difference.
    boundRR_tot = BF1 * BF0
    boundRD_tot = BF1 - pY_T1 / BF1 + pY_T0 * BF0

    # The return vector.
    if(estimand=="RR_tot"){returnVec = round(c(boundRR_tot, BF1, BF0, RR_UY_T1, RR_UY_T0, RR_SU_T1, RR_SU_T0), 4)
    }else{returnVec = round(c(boundRD_tot, BF1, BF0, RR_UY_T1, RR_UY_T0, RR_SU_T1, RR_SU_T0, pY_T1, pY_T0), 4)}
  }

  return(returnVec)
}
