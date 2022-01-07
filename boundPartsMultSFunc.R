boundPartsMultSFunc <- function(Y1,Y0,Tr,S,U,V,parameter)
{
  # Different calculations are needed for the two different populations. If the population is the subpop, do the first
  # calculations, if it's the total do the second part instead.
  
  # P(Y=1|T=t,S=1) is needed in both bounds.
  # Vectors for storing.
  U1_T1 = c()
  U0_T1 = c()
  T1_denom = c()
  
  U1_T0 = c()
  U0_T0 = c()
  T0_denom = c()
  
  # Loop through all values of V.
  for (iii in 1:2)
  {
    sProdU1 = prod(S[2*iii-1,]) #apply(S[2*iii-1,], 1, prod)
    sProdU0 = prod(S[2*iii,]) #apply(S[2*iii,], 1, prod)
    
    # P(U=1|T=1,S=1)=sum_v [P(V=v)*P(T=1|V=v)*P(S=1|V=v,U=1)*P(U=1)]
    U1_T1[iii] = V[iii]*U[1]*Tr[1,iii]*sProdU1
    # P(U=0|T=1,S=1)=sum_v [P(V=v)*P(T=1|V=v)*P(S=1|V=v,U=0)*P(U=0)]
    U0_T1[iii] = V[iii]*U[2]*Tr[1,iii]*sProdU0
    # The denominator is the sum of the two numerators.
    T1_denom[iii] = U1_T1[iii] + U0_T1[iii]
    
    # P(U=1|T=0,S=1)=sum_v [P(V=v)*P(T=0|V=v)*P(S=1|V=v,U=1)*P(U=1)]
    U1_T0[iii] = V[iii]*U[1]*Tr[2,iii]*sProdU1
    # P(U=0|T=0,S=1)=sum_v [P(V=v)*P(T=0|V=v)*P(S=1|V=v,U=0)*P(U=0)]
    U0_T0[iii] = V[iii]*U[2]*Tr[2,iii]*sProdU0
    # The denominator is the sum of the two numerators.
    T0_denom[iii] = U1_T0[iii] + U0_T0[iii] 
  }
  # P(Y=1|T=0,S=1).
  pY_T1 = Y1[1,1]*(sum(U1_T1)/sum(T1_denom))+Y1[1,2]*(sum(U0_T1)/sum(T1_denom))
  # P(Y=1|T=1,S=1).
  pY_T0 = Y0[1,1]*(sum(U1_T0)/sum(T0_denom))+Y0[1,2]*(sum(U0_T0)/sum(T0_denom))
  
  # If the bound is wanted for the selected subpop, do these calculations.
  if(parameter=="RD_s"|parameter=="RR_s")
  {
    # RR_UY|S=1, max_a[max_u(P(Y=1|T=t,S=1,U=u))/min_u(P(Y=1|T=t,S=1,U=u))].
    RR_UY_S1 = max((max(Y1[1,])/min(Y1[1,])),(max(Y0[1,])/min(Y0[1,])))
    
    # RR_TU|S=1 requires more calculations.
    # P(U=1|T=1,S=1)/P(U=1|T=0,S=1).
    pU1 = (sum(U1_T1)/sum(T1_denom))/(sum(U1_T0)/sum(T0_denom))
    
    # P(U=0|T=1,S=1)/P(U=0|T=0,S=1).
    pU0 = (sum(U0_T1)/sum(T1_denom))/(sum(U0_T0)/sum(T0_denom))
    
    # max_u[P(U=u|T=1,S=1)]
    #RR_TU_S1_num = max(c(sum(U1_T1)/sum(T1_denom),sum(U0_T1)/sum(T1_denom)))
    # min_u[P(U=u|T=0,S=1)]
    #RR_TU_S1_denom = min(c(sum(U1_T0)/sum(T0_denom),sum(U0_T0)/sum(T0_denom)))
    
    # RR_TU|S=1, max_u[P(U=u|T=1,S=1)/P(U=u|T=0,S=1)].
    #RR_TU_S1 = RR_TU_S1_num/RR_TU_S1_denom
    RR_TU_S1 = max(pU1,pU0)
    
    # BF_U = (RR_UY|S=1*RR_TU|S=1)/(RR_UY|S=1+RR_TU|S=1-1).
    BF_U = (RR_UY_S1*RR_TU_S1)/(RR_UY_S1+RR_TU_S1-1)

    parts = c(BF_U,RR_TU_S1,RR_UY_S1)
    
  }else{
    # P(Y=1|T=1,U=1)=P(Y(1)=1|U=1).
    Y1_T1_U1 = Y1[1,1]
    # P(Y=1|T=1,U=0)=P(Y(1)=1|U=0).
    Y1_T1_U0 = Y1[1,2]
    
    # P(Y=1|T=0,U=1)=P(Y(0)=1|U=1).
    Y1_T0_U1 = Y0[1,1]
    # P(Y=1|T=0,U=0)=P(Y(0)=1|U=0).
    Y1_T0_U0 = Y0[1,2]
    
    # RR_UY|T=1=max_u[P(Y=1|T=1,U=u)]/min_u[P(Y=1|T=1,U=u)].
    RR_UY_T1 = max(Y1_T1_U1,Y1_T1_U0)/min(Y1_T1_U1,Y1_T1_U0)
    # RR_UY|T=0=max_u[P(Y=1|T=0,U=u)]/min_u[P(Y=1|T=0,U=u)].
    RR_UY_T0 = max(Y1_T0_U1,Y1_T0_U0)/min(Y1_T0_U1,Y1_T0_U0)
    
    #For different combinations of U, T and S:
    # P(U=u|T=t,S=s)=sum_v[P(U=u)*P(V=v)*P(T=t|V=v)*P(S=s|U=u,V=v)]/sum_u[sum_v[P(U=u)*P(V=v)*P(T=t|V=v)*P(S=s|U=u,V=v)]].
    
    # Calculate the product of all S-variables, when I_s=1.
    sProdV1U1S1 = prod(S[1,]) #apply(S[1,], 1, prod)
    sProdV1U0S1 = prod(S[2,]) #apply(S[2,], 1, prod)
    sProdV0U1S1 = prod(S[3,]) #apply(S[3,], 1, prod)
    sProdV0U0S1 = prod(S[4,]) #apply(S[4,], 1, prod)
    
    U1_T1_S1_num = U[1]*(Tr[1,1]*sProdV1U1S1*V[1]+Tr[1,2]*sProdV0U1S1*V[2])
    U1_T1_S1_denom = U[1]*(Tr[1,1]*sProdV1U1S1*V[1]+Tr[1,2]*sProdV0U1S1*V[2])+U[2]*(Tr[1,1]*sProdV1U0S1*V[1]+Tr[1,2]*sProdV0U0S1*V[2])
    U1_T1_S1 = U1_T1_S1_num/U1_T1_S1_denom
    
    U0_T1_S1_num = (U[2]*(Tr[1,1]*sProdV1U0S1*V[1]+Tr[1,2]*sProdV0U0S1*V[2]))
    U0_T1_S1_denom = U1_T1_S1_denom
    U0_T1_S1 = U0_T1_S1_num/U0_T1_S1_denom
    
    U1_T0_S1_num = U[1]*(Tr[2,1]*sProdV1U1S1*V[1]+Tr[2,2]*sProdV0U1S1*V[2])
    U1_T0_S1_denom = U[1]*(Tr[2,1]*sProdV1U1S1*V[1]+Tr[2,2]*sProdV0U1S1*V[2])+U[2]*(Tr[2,1]*sProdV1U0S1*V[1]+Tr[2,2]*sProdV0U0S1*V[2])
    U1_T0_S1 = U1_T0_S1_num/U1_T0_S1_denom
    
    U0_T0_S1_num = U[2]*(Tr[2,1]*sProdV1U0S1*V[1]+Tr[2,2]*sProdV0U0S1*V[2])
    U0_T0_S1_denom = U1_T0_S1_denom
    U0_T0_S1 = U0_T0_S1_num/U0_T0_S1_denom
    
    
    # I_s = 0 happens for a lot of different cases, this takes care of all of them.
    n = length(S[1,]) # The number of selection variables.
    l = rep(list(0:1), n) # Repeat the number 0 and 1 n times.
    allPerm = t(expand.grid(l)) # Write ut all combinations of 0 and 1, when there are n selection variables.
    allPerm = as.data.frame(allPerm[,-length(allPerm[1,])]) # Remove the 1,1-combination.
    
    allPermRev = (-1)*(allPerm-1)
    
    sProdV1U1S0 = c()
    sProdV1U0S0 = c()
    sProdV0U1S0 = c()
    sProdV0U0S0 = c()
    
    # What is done here, is that the vectors a are the probability if the corresponding S=1, and 0 if the corresponding S=0.
    # The vector b are the probability if the corresponding S=0 and 0 if the corresponding S=1. The vectors c are the probabilities,
    # and the product if that vector is the probability that I_S=0 in that combination. 
    
    for (qqq in 1:ncol(allPerm))
    {
      a1 = S[1,]*allPerm[,qqq]
      b1 = S[5,]*allPermRev[,qqq]
      c1 = c(a1,b1)
      c1 = as.numeric(c1[c1!=0])
      
      a2 = S[2,]*allPerm[,qqq]
      b2 = S[6,]*allPermRev[,qqq]
      c2 = c(a2,b2)
      c2 = as.numeric(c2[c2!=0])
      
      a3 = S[3,]*allPerm[,qqq]
      b3 = S[7,]*allPermRev[,qqq]
      c3 = c(a3,b3)
      c3 = as.numeric(c3[c3!=0])
      
      a4 = S[4,]*allPerm[,qqq]
      b4 = S[8,]*allPermRev[,qqq]
      c4 = c(a4,b4)
      c4 = as.numeric(c4[c4!=0])
      
      sProdV1U1S0[qqq]=prod(c1)
      sProdV1U0S0[qqq]=prod(c2)
      sProdV0U1S0[qqq]=prod(c3)
      sProdV0U0S0[qqq]=prod(c4)
    }
    
    U1_T1_S0_num = U[1]*(Tr[1,1]*sum(sProdV1U1S0)*V[1]+Tr[1,2]*sum(sProdV0U1S0)*V[2])
    U1_T1_S0_denom = U[1]*(Tr[1,1]*sum(sProdV1U1S0)*V[1]+Tr[1,2]*sum(sProdV0U1S0)*V[2])+U[2]*(Tr[1,1]*sum(sProdV1U0S0)*V[1]+Tr[1,2]*sum(sProdV0U0S0)*V[2])
    U1_T1_S0 = U1_T1_S0_num/U1_T1_S0_denom
    
    U0_T1_S0_num = U[2]*(Tr[1,1]*sum(sProdV1U0S0)*V[1]+Tr[1,2]*sum(sProdV0U0S0)*V[2])
    U0_T1_S0_denom = U1_T1_S0_denom
    U0_T1_S0 = U0_T1_S0_num/U0_T1_S0_denom
    
    U1_T0_S0_num = U[1]*(Tr[2,1]*sum(sProdV1U1S0)*V[1]+Tr[2,2]*sum(sProdV0U1S0)*V[2])
    U1_T0_S0_denom = U[1]*(Tr[2,1]*sum(sProdV1U1S0)*V[1]+Tr[2,2]*sum(sProdV0U1S0)*V[2])+U[2]*(Tr[2,1]*sum(sProdV1U0S0)*V[1]+Tr[2,2]*sum(sProdV0U0S0)*V[2])
    U1_T0_S0 = U1_T0_S0_num/U1_T0_S0_denom
    
    U0_T0_S0_num = U[2]*(Tr[2,1]*sum(sProdV1U0S0)*V[1]+Tr[2,2]*sum(sProdV0U0S0)*V[2])
    U0_T0_S0_denom = U1_T0_S0_denom
    U0_T0_S0 = U0_T0_S0_num/U0_T0_S0_denom
    
    # RR_SU|T=1 = max_u[P(U=u|T=1,S=1)/P(U=u|T=1,S=0)].
    RR_SU_T1 = max(U1_T1_S1/U1_T1_S0,U0_T1_S1/U0_T1_S0)
    # RR_SU|T=0 = max_u[P(U=u|T=0,S=0)/P(U=u|T=0,S=1)].
    RR_SU_T0 = max(U1_T0_S0/U1_T0_S1,U0_T0_S0/U0_T0_S1)
    
    # BF_1 and BF_2 in the bounds.
    BF1 = (RR_UY_T1*RR_SU_T1)/(RR_UY_T1+RR_SU_T1-1)
    BF0 = (RR_UY_T0*RR_SU_T0)/(RR_UY_T0+RR_SU_T0-1)
    
    parts = c(BF1,RR_SU_T1,RR_UY_T1,BF0,RR_SU_T0,RR_UY_T0)
  }
  return(parts)
}