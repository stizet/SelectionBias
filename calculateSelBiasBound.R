calculateSelBiasBound <- function(pVcoef,pUcoef,pTcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,whichPar,probit)
{
  # A function that calculates a bound for the bias due to selection, for multiple
  # selection variables. The input is the hyper parameters used in the M structure and
  # which population parameter the calculations are performed for. 
  
  # Functions used in the code.
  source("dataGenMultS.R")
  source("selBiasMultSFunc.R")
  source("corrSelBiasMultSFunc.R")
  source("biasBoundMultSFunc.R")
  source("boundPartsMultSFunc.R")
  source("maxBoundMultSFunc.R")
  
  ### GETTING THE DATA ###
  
  # Using the data generating function to get the probabilities in the model.
  dataProb = dataGenMultS(pVcoef,pUcoef,pTcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,probit)
  # Extracting the vectors/matrices/array from the data frame.
  pV = na.omit(dataProb$pV)
  pU = na.omit(dataProb$pU)
  pT = matrix(na.omit(dataProb$pT),ncol=2)
  pY1 = matrix(na.omit(dataProb$pY1),ncol=2)
  pY0 = matrix(na.omit(dataProb$pY0),ncol=2)
  pSmat = as.data.frame(dataProb[,6:length(dataProb[1,])])

  ### END GETTING THE DATA ###
  
  ### CALCULATING THE BIAS AND THE TRUE TREATMENT EFFECT ###
  
  # Calculate the bias and treatment effect for the parameter of interest
  biasAndTE = selBiasMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)
  
  # To check if the bias is negative and re-coding of the treatment is needed.
  testSelBias = biasAndTE[1]
  
  # Store the original bias and treatment effect.
  selBias_ori = biasAndTE[1]
  trueTE_ori = biasAndTE[2]
  
  biasLimit = ifelse(whichPar=="RD_s"|whichPar=="RD_tot",0,1)
  
  # Check if the bias is negative, and if it is re-code treatment and calculate the new bias and treatment effect.
  if(testSelBias<biasLimit)
  {
    revTreat = TRUE
    pTnew = rbind(pT[2,],pT[1,])
    biasAndTEnew = selBiasMultSFunc(pY0,pY1,pTnew,pSmat,pU,pV,whichPar)
    selBias = biasAndTEnew[1]
    trueTE = biasAndTEnew[2]
  }else {selBias = biasAndTE[1]
  trueTE = biasAndTE[2]
  revTreat = FALSE}
  
  ### END CALCULATING THE BIAS AND THE TRUE TREATMENT EFFECT ###
  
  ### CALCULATING THE BOUNDS FOR THE BIAS ###
  
  if(testSelBias<biasLimit)
  {
    bound = biasBoundMultSFunc(pY0,pY1,pTnew,pSmat,pU,pV,whichPar)
  }else{bound = biasBoundMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)}
  
  ### END CALCULATING THE BOUNDS FOR THE BIAS ###
  
  ### CALCULATING THE CORRELATIONS ###
  
  # The correlations for the original coding.
  corrVec_ori = corrSelBiasMultSFunc(pY1,pY0,pT,pSmat,pU,pV)
  
  # Reverse the coding if the bias is negative.
  if(testSelBias<biasLimit)
  {
    corrVec = corrSelBiasMultSFunc(pY0,pY1,pTnew,pSmat,pU,pV)
  }else{corrVec = corrVec_ori}
  
  # Extrace the correlations from the vector.
  corrST = corrVec[1]
  corrSY = corrVec[2]
  corrSY1 = corrVec[3]
  corrSY0 = corrVec[4]
  
  corrST_ori = corrVec_ori[1]
  corrSY_ori = corrVec_ori[2]
  corrSY1_ori = corrVec_ori[3]
  corrSY0_ori = corrVec_ori[4]
  
  ### END CALCULATING THE CORRELATIONS ###
  
  ### CALCULATING THE PARTS OF THE BOUND ###
  
  if(testSelBias<biasLimit)
  {
    parts = boundPartsMultSFunc(pY0,pY1,pTnew,pSmat,pU,pV,whichPar)
  }else{parts = boundPartsMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)}
  
  ### END CALCULATING THE PARTS OF THE BOUND ###
  
  ### CALCULATE THE MAXIMUM LOGICAL BOUND ###
  
  if(testSelBias<biasLimit)
  {
    maxBound = maxBoundMultSFunc(pY0,pY1,pTnew,pSmat,pU,pV,whichPar)
  }else{maxBound = maxBoundMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)}
  
  ### END CALCULATE THE MAXIMUM LOGICAL BOUND ###
  
  heading = c("SV bound","AF bound","Reverse treatment")
  values = list(bound,maxBound,as.logical(revTreat))
  
  returnDat = matrix(cbind(heading,values),ncol=2)
  return(returnDat)

}
