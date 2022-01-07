selBiasMultSFunc <- function(Y1,Y0,Tr,S,U,V,parameter)
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
      sProd = prod(S[counter1,]) 
      
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
  
  # Now, do the calculations for the causal parameter of interest, E[Y1|S=1] and E[Y0|S=1].
  
  # Storing vectors.
  Y1_numVecUB = c()
  Y1_denomVecUB = c()
  Y0_numVecUB = c()
  Y0_denomVecUB = c()
  
  counter2 = 1
  
  # Loop through all combinations of U and V.
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      sProd = prod(S[counter2,]) #apply(S[counter2,], 1, prod)
      
      # The numerator for E[Y1|S=1]: P(U=u)*P(V=v)*P(Y1=1|U=u)*P(S=1|U=u,V=v).
      Y1numUB = U[jjj]*V[iii]*Y1[1,jjj]*sProd
      
      # The denominator for E[Y1|S=1]: P(U=u)*P(V=v)*P(S=1|U=u,V=v).
      Y1denomUB = U[jjj]*V[iii]*sProd
      
      # The numerator for E[Y0|S=1]: P(U=u)*P(V=v)*P(Y0=1|U=u)*P(S=1|U=u,V=v).
      Y0numUB = U[jjj]*V[iii]*Y0[1,jjj]*sProd
      
      # The denominator for E[Y0|S=1]: P(U=u)*P(V=v)*P(S=1|U=u,V=v).
      Y0denomUB = U[jjj]*V[iii]*sProd  
      
      
      # Store the values of the numerator and denominator.
      Y1_numVecUB = c(Y1_numVecUB,Y1numUB)
      Y1_denomVecUB = c(Y1_denomVecUB,Y1denomUB)
      
      Y0_numVecUB = c(Y0_numVecUB,Y0numUB)
      Y0_denomVecUB = c(Y0_denomVecUB,Y0denomUB)
      
      counter2 = counter2 + 1
    }
  }
  
  
  # Now, do the calculations for the causal parameter of interest in the total population, E[Y1] and E[Y0].
  
  # Storing vectors.
  Y1_vecUB = c()
  Y0_vecUB = c()
  
  # Loop through all combinations of U (V not needed).
  for (jjj in 1:2)
  {
    # The numerator for E[Y1|S=1]: P(U=u)*P(V=v)*P(Y1=1|U=u)*P(S=1|U=u,V=v).
    Y1UB = U[jjj]*Y1[1,jjj]
    
    # The numerator for E[Y0|S=1]: P(U=u)*P(V=v)*P(Y0=1|U=u)*P(S=1|U=u,V=v).
    Y0UB = U[jjj]*Y0[1,jjj]
    
    # Store the values of the numerator and denominator.
    Y1_vecUB = c(Y1_vecUB,Y1UB)
    Y0_vecUB = c(Y0_vecUB,Y0UB)
  }
  
  # Calculate the unbiased estimators for the risk difference and the relative risk in the selected subpop.
  unbiasEstRD_s = round(sum(Y1_numVecUB)/sum(Y1_denomVecUB) - sum(Y0_numVecUB)/sum(Y0_denomVecUB),digits = 14)
  unbiasEstRR_s = round((sum(Y1_numVecUB)/sum(Y1_denomVecUB)) / (sum(Y0_numVecUB)/sum(Y0_denomVecUB)),digits = 14)
  
  # Calculate the unbiased estimators for the risk difference and the relative risk in the total pop.
  unbiasEstRD_tot = round(sum(Y1_vecUB) - sum(Y0_vecUB),digits = 14)
  unbiasEstRR_tot = round(sum(Y1_vecUB) / sum(Y0_vecUB),digits = 14)
  
  # Calculate the selection biases in the selected subpop.
  selBiasRD_s =  round(biasEstRD - unbiasEstRD_s,digits = 14)
  selBiasRR_s = round(biasEstRR/unbiasEstRR_s,digits = 14)
  
  # Calculate the selection biases in the total pop.
  selBiasRD_tot = round(biasEstRD - unbiasEstRD_tot,digits = 14)
  selBiasRR_tot = round(biasEstRR/unbiasEstRR_tot,digits = 14)
  
  # Which bias and treatment effect to be returned from the function.
  if(parameter=="RD_s")
  {
    returnVal = c(selBiasRD_s,unbiasEstRD_s)
  }else if(parameter=="RR_s")
  {
    returnVal = c(selBiasRR_s,unbiasEstRR_s)
  }else if(parameter=="RD_tot")
  {
    returnVal = c(selBiasRD_tot,unbiasEstRD_tot)
  }else {returnVal = c(selBiasRR_tot,unbiasEstRR_tot)}
  
  return(returnVal)
}