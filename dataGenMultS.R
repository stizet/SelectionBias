dataGenMultS <- function(pVcoef,pUcoef,pTcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,probit)
{
  # Define probabilities for all variables in vectors which is then returned as a data frame.
  # This data frame is then turned into matrices in the main file.
  
  # The number of selection variables.
  numS = length(constS)
  # A matrix for storing the vectors for the different selection variables S.
  sMat = matrix(NA,nrow=8,ncol=numS)
  
  # --- pV --- #
  pV = c(pVcoef,1-pVcoef)
  # --- pV --- #
  
  # --- pU --- #
  pU = c(pUcoef,1-pUcoef)
  # --- pU --- #
  
  if(probit==T)
  {
    # --- pT --- #
    # pT[1,] are the probabilities for T=1, pT[,1] are the probabilities T=t, for V=1.
    pT = c(pnorm(pTcoef[1]+pTcoef[2]),1-pnorm(pTcoef[1]+pTcoef[2]),pnorm(pTcoef[1]),1-pnorm(pTcoef[1]))
    # --- pT --- #
    
    # --- pY0 --- #
    # pY0[1,] are the probabilities for Y0 = 1. pY0[,1] are the probabilities for Y0=y0 for U=1.
    pY0 = c(pnorm(pY0coef[1]+pY0coef[2]),1-pnorm(pY0coef[1]+pY0coef[2]),pnorm(pY0coef[1]),1-pnorm(pY0coef[1]))
    # --- pY0 --- #
    
    # pY1[1,] are the probabilities for Y1 = 1. pY1[,1] are the probabilities for Y1=y1 for U=1
    pY1 = c(pnorm(pY1coef[1]+pY1coef[2]),1-pnorm(pY1coef[1]+pY1coef[2]),pnorm(pY1coef[1]),1-pnorm(pY1coef[1]))
    # --- pY1 --- #
    
    
    # --- pS --- #
    # Layer 1 is S=1. Row 1 is U=1. Column 1 is V=1.
    for (sss in 1:numS)
    {
      # Layer 1 is S=1. Row 1 is U=1. Column 1 is V=1.
      pS = c(pnorm(constS[sss]+slopeSV[sss]+slopeSU[sss]),pnorm(constS[sss]+slopeSV[sss]),
             pnorm(constS[sss]+slopeSU[sss]),pnorm(constS[sss]),1-pnorm(constS[sss]+slopeSV[sss]+slopeSU[sss]),
             1-pnorm(constS[sss]+slopeSV[sss]),1-pnorm(constS[sss]+slopeSU[sss]),1-pnorm(constS[sss]))
      
      sMat[,sss] = pS
    }
    # --- pS --- #
    
  }else {
    
  # --- pT --- #
  # pT[1,] are the probabilities for T=1, pT[,1] are the probabilities T=t, for V=1.
  pT = c(invlogit(pTcoef[1]+pTcoef[2]),1-invlogit(pTcoef[1]+pTcoef[2]),invlogit(pTcoef[1]),1-invlogit(pTcoef[1]))
  # --- pT --- #
  
  # --- pY0 --- #
  # pY0[1,] are the probabilities for Y0 = 1. pY0[,1] are the probabilities for Y0=y0 for U=1.
  pY0 = c(invlogit(pY0coef[1]+pY0coef[2]),1-invlogit(pY0coef[1]+pY0coef[2]),invlogit(pY0coef[1]),1-invlogit(pY0coef[1]))
  # --- pY0 --- #
  
  # pY1[1,] are the probabilities for Y1 = 1. pY1[,1] are the probabilities for Y1=y1 for U=1
  pY1 = c(invlogit(pY1coef[1]+pY1coef[2]),1-invlogit(pY1coef[1]+pY1coef[2]),invlogit(pY1coef[1]),1-invlogit(pY1coef[1]))
  # --- pY1 --- #
  
  
  # --- pS --- #
  # Layer 1 is S=1. Row 1 is U=1. Column 1 is V=1.
  for (sss in 1:numS)
  {
    # Layer 1 is S=1. Row 1 is U=1. Column 1 is V=1.
    pS = c(invlogit(constS[sss]+slopeSV[sss]+slopeSU[sss]),invlogit(constS[sss]+slopeSV[sss]),
           invlogit(constS[sss]+slopeSU[sss]),invlogit(constS[sss]),1-invlogit(constS[sss]+slopeSV[sss]+slopeSU[sss]),
           1-invlogit(constS[sss]+slopeSV[sss]),1-invlogit(constS[sss]+slopeSU[sss]),1-invlogit(constS[sss]))

    sMat[,sss] = pS
  }
  # --- pS --- #
  }
  
  colnames(sMat) = colnames(sMat, do.NULL = FALSE, prefix = "pS")
  
  dfProb = as.data.frame(qpcR:::cbind.na(pV,pU,pT,pY0,pY1,sMat))
  
  return(dfProb)
}
