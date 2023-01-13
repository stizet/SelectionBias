calcAFbound <- function(Y1,Y0,Tr,S,U,V,estimand,obsProb)
{
  # A function that calculates the AF bound. The input is the conditional
  # probabilities necessary for the calculations and the causal estimand
  # of interest. The output is the AF bound.

  # Create the matrices for the calculations.
  # The number of categories in U and V, used to define the matrices.
  Ulen = length(U)
  Vlen = length(V)

  # The input vectors (matrices) are repeated in matrices in order to
  # perform matrix multiplication instead of loops.
  Umat = matrix(U, nrow=Ulen, ncol=Vlen, byrow=FALSE) #P(U=u).
  Vmat = matrix(V, nrow=Ulen, ncol=Vlen, byrow=TRUE) #P(V=v).
  T1mat = matrix(Tr[1,], nrow=Ulen, ncol=Vlen, byrow=TRUE) #P(T=1|V=v).
  T0mat = matrix(Tr[2,], nrow=Ulen, ncol=Vlen, byrow=TRUE) #P(T=0|V=v).
  Y0mat = matrix(Y0[1,], nrow=Ulen, ncol=Vlen, byrow=FALSE) #P(Y(0)=1|U=u).
  Y1mat = matrix(Y1[1,], nrow=Ulen, ncol=Vlen, byrow=FALSE) #P(Y(1)=1|U=u).

  # The rowproduct of the selection variables.
  if(is.vector(S)){Svec=S
  }else{Svec = apply(S,1,prod)}

  # P(I_S=1|V=v,U=u,T=1).
  ST1mat = matrix(Svec[1:(length(Svec)/4)], nrow=Ulen, ncol=Vlen, byrow = FALSE)
  # P(I_S=1|V=v,U=u,T=0).
  ST0mat = matrix(Svec[(length(Svec)/4+1):(length(Svec)/2)], nrow=Ulen, ncol=Vlen, byrow = FALSE)

  # Calculate the observed estimands.
  # Extract P(Y=1|T=1,I_S=1) and P(Y=1|T=0,I_S=1).
  pY1_T1S1 = obsProb[1] #P(Y=1|T=1,I_S=1)
  pY1_T0S1 = obsProb[2] #P(Y=1|T=0,I_S=1)

  # The observed relative risk and risk difference.
  obsRR = pY1_T1S1/pY1_T0S1
  obsRD = pY1_T1S1-pY1_T0S1


  # Calculate the minimum estimands in the subpopulation.
  # Calculate min(P(Y(1)=1|I_S=1)).
  Y1minNum = sum(Umat*Vmat*Y1mat*T1mat*ST1mat)
  Y1minDenom = sum(Umat*Vmat*(T1mat*ST1mat + T0mat*ST0mat))
  pY11_S1_min = Y1minNum/Y1minDenom #min(P(Y(1)=1|I_S=1)).

  # Calculate max(P(Y(0)=1|I_S=1)).
  Y0maxNum = sum(Umat*Vmat*(T1mat*ST1mat + Y0mat*T0mat*ST0mat))
  Y0maxDenom = Y1minDenom
  pY01_S1_max = Y0maxNum/Y0maxDenom #max(P(Y(0)=1|I_S=1)).

  # Calculate the minimum relative risk and risk difference in the subpopulation.
  minRR_s = pY11_S1_min/min(pY01_S1_max,1)
  minRD_s = pY11_S1_min-min(pY01_S1_max,1)

  # Calculate the minimum estimands in the total population.
  # Calculate min(P(Y(1)=1)).
  pY11_min = sum(Umat*Vmat*Y1mat*T1mat*ST1mat)

  # Calculate max(P(Y(0)=1)).
  pY01_max = sum(Umat*Vmat*(2*(T1mat*(1-ST1mat) + T0mat*(1-ST0mat)) + T1mat*ST1mat + Y0mat*T0mat*ST0mat))

  # Calculate the minimum relative risk and risk difference in the total population.
  minRR_tot = pY11_min/min(pY01_max,1)
  minRD_tot = pY11_min-min(pY01_max,1)


  # Calculate the assumption free bound, a difference for RD and a ratio for RR.
  if(estimand=="RD_s"){AFbound = obsRD-minRD_s
  }else if(estimand=="RR_s"){AFbound = obsRR/minRR_s
  }else if(estimand=="RD_tot"){AFbound = obsRD-minRD_tot
  }else{AFbound = obsRR/minRR_tot}

  return(AFbound)
}
