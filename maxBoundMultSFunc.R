maxBoundMultSFunc <- function(Y1,Y0,Tr,S,U,V,parameter)
{
  # Start with the incorrect estimator, E[Y|T=1,S=1] and E[Y|T=0,S=1].
  
  # Vectors for storing.
  Y1_numVec = c()
  Y1_denomVec = c()
  Y0_numVec = c()
  Y0_denomVec = c()
  
  counter1 = 1
  
  # Loop through all combinations of U=u and V=v.
  for (iii in 1:length(V))
  {
    for (jjj in 1:length(U))
    {
      # The rowproduct of the selection variables.
      sProd = prod(S[counter1,]) #apply(S[counter1,], 1, prod)
      
      # The numerator for E[Y|T=1,S=1]: P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v)*P(S=1|U=u,V=v).
      Y1num = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]*sProd
      
      # The denominator for E[Y|T=1,S=1]: P(U=u)*P(V=v)*P(T=1|V=v)*P(S=1|U=u,V=v).
      Y1denom = U[jjj]*V[iii]*Tr[1,iii]*sProd
      
      # The numerator for E[Y|T=0,S=1]: P(U=u)*P(V=v)*P(Y0=1|U=u)*P(T=0|V=v)*P(S=1|U=u,V=v).
      Y0num = U[jjj]*V[iii]*Y0[1,jjj]*Tr[2,iii]*sProd
      
      # The denominator for E[Y|T=0,S=1]: P(U=u)*P(V=v)*P(T=0|V=v)*P(S=1|U=u,V=v).
      Y0denom = U[jjj]*V[iii]*Tr[2,iii]*sProd  
      
      
      # Store the values of the numerator and denominator.
      Y1_numVec = c(Y1_numVec,Y1num)
      Y1_denomVec = c(Y1_denomVec,Y1denom)
      
      Y0_numVec = c(Y0_numVec,Y0num)
      Y0_denomVec = c(Y0_denomVec,Y0denom)
      
      counter1 = counter1 + 1
    }
  }
  
  # Calculate the biased estimators for the risk difference and the relative risk.
  biasEstRD = sum(Y1_numVec)/sum(Y1_denomVec) - sum(Y0_numVec)/sum(Y0_denomVec)
  biasEstRR = (sum(Y1_numVec)/sum(Y1_denomVec)) / (sum(Y0_numVec)/sum(Y0_denomVec))
  
  #----------------#
  
  # Now, do the calculations for the minimum of the causal parameter of interest, E[Y1|S=1] and E[Y0|S=1].
  
  # Storing vectors.
  Y1_minVec = c()
  Y0_maxVec = c()
  
  # Loop through all combinations of U and V.
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      # The minimum value of P(Y1=1|S=1): sum_u sum_v P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v).
      Y1min = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]
      
      # The maximum value P(Y0=1|S=1): sum_u sum_v P(U=u)*P(V=v)*P(Y0=1|U=u)*P(T=0|V=v).
      Y0max = U[jjj]*V[iii]*(Tr[1,iii]+Y0[1,jjj]*Tr[2,iii])
      
      # Store the values of the numerator and denominator.
      Y1_minVec = c(Y1_minVec,Y1min)
      Y0_maxVec = c(Y0_maxVec,Y0max)
      
    }
  }
  
  
  # Now, do the calculations for the minimum of the causal parameter of interest in the total population, E[Y1] and E[Y0].
  
  counter2 = 1
  
  # Storing vectors.
  Y1_minTotVec = c()
  Y0_maxTotVec = c()
  
  # Loop through all combinations of Uand V.
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      sProd = prod(S[counter2,]) #apply(S[counter2,], 1, prod)
      
      # The minimum for P(Y(1)=1): sum_u sum_v P(U=u)*P(V=v)*P(Y1=1|U=u)*P(T=1|V=v)*P(S=1|U=u,V=v).
      Y1MinTot = U[jjj]*V[iii]*Y1[1,jjj]*Tr[1,iii]*sProd
      
      # The maximum for P(Y(0)=1): sum_u sum_v P(U=u)*P(V=v)*(2P(S=0|U=u,V=v)+P(T=1|V=v)*P(S=1|U=u,V=v)
      # +P(Y0=1|U=u)*P(T=0|V=v)*P(S=1|U=u,V=v).
      Y0MaxTot = U[jjj]*V[iii]*(2*(1-sProd)+ Tr[1,iii]*sProd+Y0[1,jjj]*Tr[2,iii]*sProd)
      
      # Store the values of the numerator and denominator.
      Y1_minTotVec = c(Y1_minTotVec,Y1MinTot)
      Y0_maxTotVec = c(Y0_maxTotVec,Y0MaxTot)
      
      counter2 = counter2 + 1
    }
  }  
  
  # Calculate the minimum value of the unbiased estimators for the risk difference and the relative risk in the selected subpop.
  minRD_s = round(sum(Y1_minVec) - min(sum(Y0_maxVec),1),digits = 14)
  minRR_s = round(sum(Y1_minVec) / min(sum(Y0_maxVec),1),digits = 14)
  
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