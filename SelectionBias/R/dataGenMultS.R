dataGenMultS <- function(pVcoef,pUcoef,pTcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,slopeST,probit)
{
  # Define probabilities for all variables in vectors which is then returned as a data frame.
  # This data frame is then turned into matrices in the main file.

  # The number of selection variables.
  numS = length(constS)
  # A matrix for storing the vectors for the different selection variables S.
  sMat = matrix(NA,nrow=16,ncol=numS)

  # --- pV --- #
  pV = c(pVcoef,1-pVcoef)
  # --- pV --- #

  # --- pU --- #
  pU = c(pUcoef,1-pUcoef)
  # --- pU --- #

  if(probit==T)
  {
    # --- pT --- #
    # The first is P(T=1|V=1), the second is P(T=0|V=1), the third is P(T=1|V=0), the fourth is P(T=0|V=0).
    pT = c(stats::pnorm(pTcoef[1]+pTcoef[2]),1-stats::pnorm(pTcoef[1]+pTcoef[2]),stats::pnorm(pTcoef[1]),1-stats::pnorm(pTcoef[1]))
    # --- pT --- #

    # --- pY0 --- #
    # The first is P(Y(0)=1|U=1), the second is P(Y(0)=0|U=1), the third is P(Y(0)=1|U=0), the fourth is P(Y(0)=0|U=0).
    pY0 = c(stats::pnorm(pY0coef[1]+pY0coef[2]),1-stats::pnorm(pY0coef[1]+pY0coef[2]),stats::pnorm(pY0coef[1]),1-stats::pnorm(pY0coef[1]))
    # --- pY0 --- #

    # The first is P(Y(1)=1|U=1), the second is P(Y(1)=0|U=1), the third is P(Y(1)=1|U=0), the fourth is P(Y(1)=0|U=0).
    pY1 = c(stats::pnorm(pY1coef[1]+pY1coef[2]),1-stats::pnorm(pY1coef[1]+pY1coef[2]),stats::pnorm(pY1coef[1]),1-stats::pnorm(pY1coef[1]))
    # --- pY1 --- #


    # --- pS --- #
    # First  half is S=1, second half is S=0. First half of halves is T=1, second half of the halves is T=0.
    # First two is V=1 and second two is V=0, etc. Every second is U=1 and every second is U=0.
    for (sss in 1:numS)
    {
      pS = c(stats::pnorm(constS[sss]+slopeST[sss]+slopeSV[sss]+slopeSU[sss]),stats::pnorm(constS[sss]+slopeST[sss]+slopeSV[sss]),
             stats::pnorm(constS[sss]+slopeST[sss]+slopeSU[sss]),stats::pnorm(constS[sss]+slopeST[sss]),
             stats::pnorm(constS[sss]+slopeSV[sss]+slopeSU[sss]),stats::pnorm(constS[sss]+slopeSV[sss]),
             stats::pnorm(constS[sss]+slopeSU[sss]),stats::pnorm(constS[sss]),1-stats::pnorm(constS[sss]+slopeST[sss]+slopeSV[sss]+slopeSU[sss]),
             1-stats::pnorm(constS[sss]+slopeST[sss]+slopeSV[sss]),1-stats::pnorm(constS[sss]+slopeST[sss]+slopeSU[sss]),
             1-stats::pnorm(constS[sss]+slopeST[sss]),1-stats::pnorm(constS[sss]+slopeSV[sss]+slopeSU[sss]),
             1-stats::pnorm(constS[sss]+slopeSV[sss]),1-stats::pnorm(constS[sss]+slopeSU[sss]),1-stats::pnorm(constS[sss]))

      sMat[,sss] = pS
    }
    # --- pS --- #

  }else {

    # --- pT --- #
    # The first is P(T=1|V=1), the second is P(T=0|V=1), the third is P(T=1|V=0), the fourth is P(T=0|V=0).
    pT = c(arm::invlogit(pTcoef[1]+pTcoef[2]),1-arm::invlogit(pTcoef[1]+pTcoef[2]),arm::invlogit(pTcoef[1]),1-arm::invlogit(pTcoef[1]))
    # --- pT --- #

    # --- pY0 --- #
    # The first is P(Y(0)=1|U=1), the second is P(Y(0)=0|U=1), the third is P(Y(0)=1|U=0), the fourth is P(Y(0)=0|U=0).
    pY0 = c(arm::invlogit(pY0coef[1]+pY0coef[2]),1-arm::invlogit(pY0coef[1]+pY0coef[2]),arm::invlogit(pY0coef[1]),1-arm::invlogit(pY0coef[1]))
    # --- pY0 --- #

    # The first is P(Y(1)=1|U=1), the second is P(Y(1)=0|U=1), the third is P(Y(1)=1|U=0), the fourth is P(Y(1)=0|U=0).
    pY1 = c(arm::invlogit(pY1coef[1]+pY1coef[2]),1-arm::invlogit(pY1coef[1]+pY1coef[2]),arm::invlogit(pY1coef[1]),1-arm::invlogit(pY1coef[1]))
    # --- pY1 --- #


    # --- pS --- #
    # First  half is S=1, second half is S=0. First half of halves is T=1, second half of the halves is T=0.
    # First two is V=1 and seconf two is V=0, etc. Every second is U=1 and every second is U=0.
    for (sss in 1:numS)
    {
      pS = c(arm::invlogit(constS[sss]+slopeST[sss]+slopeSV[sss]+slopeSU[sss]),arm::invlogit(constS[sss]+slopeST[sss]+slopeSV[sss]),
             arm::invlogit(constS[sss]+slopeST[sss]+slopeSU[sss]),arm::invlogit(constS[sss]+slopeST[sss]),
             arm::invlogit(constS[sss]+slopeSV[sss]+slopeSU[sss]),arm::invlogit(constS[sss]+slopeSV[sss]),
             arm::invlogit(constS[sss]+slopeSU[sss]),arm::invlogit(constS[sss]),
             1-arm::invlogit(constS[sss]+slopeST[sss]+slopeSV[sss]+slopeSU[sss]),1-arm::invlogit(constS[sss]+slopeST[sss]+slopeSV[sss]),
             1-arm::invlogit(constS[sss]+slopeST[sss]+slopeSU[sss]),1-arm::invlogit(constS[sss]+slopeST[sss]),
             1-arm::invlogit(constS[sss]+slopeSV[sss]+slopeSU[sss]),1-arm::invlogit(constS[sss]+slopeSV[sss]),
             1-arm::invlogit(constS[sss]+slopeSU[sss]),1-arm::invlogit(constS[sss]))

      sMat[,sss] = pS
    }
    # --- pS --- #
  }

  colnames(sMat) = colnames(sMat, do.NULL = FALSE, prefix = "pS")

  # Create vectors of the same length.
  length(pV) = length(pS)
  length(pU) = length(pS)
  length(pT) = length(pS)
  length(pY0) = length(pS)
  length(pY1) = length(pS)

  dfProb = as.data.frame(cbind(pV,pU,pT,pY0,pY1,sMat))

  return(dfProb)
}
