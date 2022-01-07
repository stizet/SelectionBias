corrSelBiasMultSFunc <- function(Y1,Y0,Tr,S,U,V)
{
  # Storing vectors.
  part1covSYvec = c()
  part2covSYvec = c()
  part3covSYvec = c()
  
  part1covSTvec = c()
  part2covSTvec = c()
  part3covSTvec = c()
  
  part1covSY1vec = c()
  part2covSY1vec = c()
  part3covSY1vec = c()
  
  part1covSY0vec = c()
  part2covSY0vec = c()
  part3covSY0vec = c()
  
  counter = 1
  
  for (iii in 1:2)
  {
    for (jjj in 1:2)
    {
      sProd = prod(S[counter,]) #apply(S[counter,], 1, prod)
      
      # E[SY]=sum_u sum_v[P(Y1=1|U=u)*P(S=1|U=u,V=v)*P(T=1|V=v)*P(U=u)*P(V=v) + P(Y0=1|U=u)*P(S=1|U=u,V=v)*P(T=0|V=v)*P(U=u)*P(V=v)]
      part1covSY = Y1[1,iii]*sProd*Tr[1,jjj]*U[iii]*V[jjj] + Y0[1,iii]*sProd*Tr[2,jjj]*U[iii]*V[jjj]
      # E[S]=sum_u sum_v[P(S=1|U=u,V=v)*P(U=u)*P(V=v)]
      part2covSY = U[iii]*V[jjj]*sProd
      # E[Y]=sum_u sum_v[P(Y1=1|U=u)*P(T=1|V=v)*P(U=u)*P(V=v) + P(Y0=1|U=u)*P(T=0|V=v)*P(U=u)*P(V=v)]
      part3covSY = Y1[1,iii]*U[iii]*Tr[1,jjj]*V[jjj] + Y0[1,iii]*U[iii]*Tr[2,jjj]*V[jjj]
      
      # E[ST]=sum_u sum_v[P(S=1|U=u,V=v)*P(T=1|V=v)*P(U=u)*P(V=v)]
      part1covST = U[iii]*V[jjj]*sProd*Tr[1,jjj]
      # E[S], stated above.
      part2covST = part2covSY
      # E[T]=sum_v[P(T=1|V=v)*P(V=v)]
      part3covST = V[jjj]*Tr[1,jjj]*(iii-1)
      
      # E[SY(1)]sum_u sum_v[P(Y1=1|U=u)*P(S=1|U=u,V=v)*P(T=1|V=v)*P(U=u)*P(V=v)]
      part1covSY1 = Y1[1,iii]*sProd*U[iii]*V[jjj]
      # E[S], stated above.
      part2covSY1 = part2covSY
      # E[Y(1)]=sum_u[P(Y1=1|U=u)*P(U=u)]
      part3covSY1 = U[iii]*Y1[1,iii]*(jjj-1)
      
      # E[SY(0)]=[P(Y0=1|U=u)*P(S=1|U=u,V=v)*P(T=0|V=v)*P(U=u)*P(V=v)]
      part1covSY0 = Y0[1,iii]*sProd*U[iii]*V[jjj]
      # E[S], stated above.
      part2covSY0 = part2covSY
      # E[Y(0)]=sum_u[P(Y0=1|U=u)*P(U=u)]
      part3covSY0 = U[iii]*Y0[1,iii]*(jjj-1)
      
      
      # Store the values of covariance parts and the variances.
      part1covSYvec = c(part1covSYvec,part1covSY)
      part2covSYvec = c(part2covSYvec,part2covSY)
      part3covSYvec = c(part3covSYvec,part3covSY)
      
      part1covSTvec = c(part1covSTvec,part1covST)
      part2covSTvec = c(part2covSTvec,part2covST)
      part3covSTvec = c(part3covSTvec,part3covST)
      
      part1covSY1vec = c(part1covSY1vec,part1covSY1)
      part2covSY1vec = c(part2covSY1vec,part2covSY1)
      part3covSY1vec = c(part3covSY1vec,part3covSY1)
      
      part1covSY0vec = c(part1covSY0vec,part1covSY0)
      part2covSY0vec = c(part2covSY0vec,part2covSY0)
      part3covSY0vec = c(part3covSY0vec,part3covSY0)
      
      counter=counter+1
      
    }
  }
  # Var(X)=E(X^2)-E(X)^2=E(X)-E(X)^2 when X is binary.
  varT = sum(part3covSTvec)-sum(part3covSTvec)^2
  varS = sum(part2covSTvec)-sum(part2covSTvec)^2
  varY = sum(part3covSYvec)-sum(part3covSYvec)^2
  varY1 = sum(part3covSY1vec)-sum(part3covSY1vec)^2
  varY0 = sum(part3covSY0vec)-sum(part3covSY0vec)^2
  
  # Cor(X,Y)=(E(XY)-E(X)E(Y))/(sqrt(Var(X))*sqrt(Var(Y)))
  corrST = (sum(part1covSTvec)-sum(part2covSTvec)*sum(part3covSTvec))/(sqrt(sum(varS))*sqrt(sum(varT)))
  corrSY = (sum(part1covSYvec)-sum(part2covSYvec)*sum(part3covSYvec))/(sqrt(sum(varS))*sqrt(sum(varY)))
  
  corrSY1 = (sum(part1covSY1vec)-sum(part2covSY1vec)*sum(part3covSY1vec))/(sqrt(sum(varS))*sqrt(sum(varY1)))
  corrSY0 = (sum(part1covSY0vec)-sum(part2covSY0vec)*sum(part3covSY0vec))/(sqrt(sum(varS))*sqrt(sum(varY0)))
  
  corrVec = round(c(corrST,corrSY,corrSY1,corrSY0),digits = 14)
  
}