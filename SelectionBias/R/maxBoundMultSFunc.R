maxBoundMultSFunc <- function(Y1,Y0,Tr,S,U,V,parameter)
{
  # Start with the incorrect estimator, P(Y=1|T=1,S=1) and P(Y=1|T=0,S=1).

  # Vectors for storing.
  Y1_numVec = c()
  Y1_denomVec = c()
  Y0_numVec = c()
  Y0_denomVec = c()

  counter11 = 1
  counter10 = 5

  # Loop through all combinations of U=u and V=v.
  for (iii in 1:length(V))
  {
    for (jjj in 1:length(U))
    {
      # The rowproduct of the selection variables, for T=1 and T=0, respectively.
      sProd1 = prod(S[counter11,])
      sProd0 = prod(S[counter10,])

      # The numerator for P(Y=1|T=1,S=1): P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v)*P(S=1|T=1,U=u,V=v).
      Y1num = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]*sProd1

      # The denominator for P(Y=1|T=1,S=1): P(U=u)*P(V=v)*P(T=1|V=v)*P(S=1|T=1,U=u,V=v).
      Y1denom = U[jjj]*V[iii]*Tr[1,iii]*sProd1

      # The numerator for P(Y=1|T=0,S=1): P(U=u)*P(V=v)*P(Y0=1|U=u)*P(T=0|V=v)*P(S=1|T=0,U=u,V=v).
      Y0num = U[jjj]*V[iii]*Y0[1,jjj]*Tr[2,iii]*sProd0

      # The denominator for P(Y=1|T=0,S=1): P(U=u)*P(V=v)*P(T=0|V=v)*P(S=1|T=0,U=u,V=v).
      Y0denom = U[jjj]*V[iii]*Tr[2,iii]*sProd0


      # Store the values of the numerator and denominator.
      Y1_numVec = c(Y1_numVec,Y1num)
      Y1_denomVec = c(Y1_denomVec,Y1denom)

      Y0_numVec = c(Y0_numVec,Y0num)
      Y0_denomVec = c(Y0_denomVec,Y0denom)

      counter11 = counter11 + 1
      counter10 = counter10 + 1
    }
  }

  # Calculate the biased estimators for the risk difference and the relative risk.
  biasEstRD = sum(Y1_numVec)/sum(Y1_denomVec) - sum(Y0_numVec)/sum(Y0_denomVec)
  biasEstRR = (sum(Y1_numVec)/sum(Y1_denomVec)) / (sum(Y0_numVec)/sum(Y0_denomVec))

  #----------------#

  # Now, do the calculations for the minimum of the causal parameter of interest, P(Y(1)=1|S=1) and P(Y(0)=1|S=1).

  # Storing vectors.
  Y1_minNumVec = c()
  Y0_maxNumVec = c()
  s_denomVec = c()

  counter21 = 1
  counter20 = 5

  # Loop through all combinations of U and V.
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      # The rowproduct of the selection variables, for T=1 and T=0, respectively.
      sProd1 = prod(S[counter21,])
      sProd0 = prod(S[counter20,])

      # The minimum value of P(Y1=1|S=1): sum_u sum_v P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v).
      Y1minNum = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]*sProd1

      # The maximum value P(Y0=1|S=1): sum_u sum_v P(U=u)*P(V=v)*P(Y0=1|U=u)*P(T=0|V=v).
      Y0maxNum = U[jjj]*V[iii]*(Tr[1,iii]*sProd1+Y0[1,jjj]*Tr[2,iii]*sProd0)

      # The probability P(I_s=1) = sum_u sum_v sum_t P(U=u)P(V=v)P(T=t|V=v)P(S=1|T=t,V=v,U=u).
      s_denom = U[jjj]*V[iii]*(Tr[1,iii]*sProd1+Tr[2,iii]*sProd0)

      # Store the values of the numerators and denominator.
      Y1_minNumVec = c(Y1_minNumVec,Y1minNum)
      Y0_maxNumVec = c(Y0_maxNumVec,Y0maxNum)
      s_denomVec = c(s_denomVec,s_denom)

      counter21 = counter21 + 1
      counter20 = counter20 + 1
    }
  }


  # Now, do the calculations for the minimum of the causal parameter of interest in the total population, E[Y1] and E[Y0].

  counter31 = 1
  counter30 = 5

  # Storing vectors.
  Y1_minTotVec = c()
  Y0_maxTotVec = c()

  # Loop through all combinations of U and V.
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      # The rowproduct of the selection variables, for T=1 and T=0, respectively.
      sProd1 = prod(S[counter31,])
      sProd0 = prod(S[counter30,])

      # The minimum for P(Y(1)=1): sum_u sum_v P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v)*P(S=1|T=1,U=u,V=v).
      Y1MinTot = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]*sProd1

      # The maximum for P(Y(0)=1): sum_u sum_v sum_t P(U=u)*P(V=v)*(2P(S=0|T=t,U=u,V=v)+P(T=1|V=v)*P(S=1|U=u,V=v)
      # +P(Y0=1|U=u)*P(T=0|V=v)*P(S=1|T=0,U=u,V=v)).
      Y0MaxTot = U[jjj]*V[iii]*(2*(Tr[1,iii]*(1-sProd1)+Tr[2,iii]*(1-sProd0))+ Tr[1,iii]*sProd1+Y0[1,jjj]*Tr[2,iii]*sProd0)

      # Store the values of the numerator and denominator.
      Y1_minTotVec = c(Y1_minTotVec,Y1MinTot)
      Y0_maxTotVec = c(Y0_maxTotVec,Y0MaxTot)

      counter31 = counter31 + 1
      counter30 = counter30 + 1
    }
  }

  # Calculate the minimum value of the unbiased estimators for the risk difference and the relative risk in the selected subpop.
  minRD_s = round(sum(Y1_minNumVec)/sum(s_denomVec) - min((sum(Y0_maxNumVec)/sum(s_denomVec)),1),digits = 14)
  minRR_s = round((sum(Y1_minNumVec)/sum(s_denomVec)) / min((sum(Y0_maxNumVec)/sum(s_denomVec)),1),digits = 14)

  # Calculate the minimum value of the unbiased estimators for the risk difference and the relative risk in the total pop.
  minRD_tot = round(sum(Y1_minTotVec) - min(sum(Y0_maxTotVec),1),digits = 14)
  minRR_tot = round(sum(Y1_minTotVec) / min(sum(Y0_maxTotVec),1),digits = 14)

  # Calculate the selection biases in the selected subpop.
  maxBoundRD_s =  round(biasEstRD - minRD_s,digits = 14)
  maxBoundRR_s = round(biasEstRR/minRR_s,digits = 14)

  # Calculate the selection biases in the total pop.
  maxBoundRD_tot = round(biasEstRD - minRD_tot,digits = 14)
  maxBoundRR_tot = round(biasEstRR/minRR_tot,digits = 14)

  # Which maximum bound to be returned from the function.
  if(parameter=="RD_s")
  {
    returnVal = maxBoundRD_s
  }else if(parameter=="RR_s")
  {
    returnVal = maxBoundRR_s
  }else if(parameter=="RD_tot")
  {
    returnVal = maxBoundRD_tot
  }else {returnVal = maxBoundRR_tot}

  return(returnVal)
}
