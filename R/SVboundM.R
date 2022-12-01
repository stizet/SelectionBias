#' Calculate the Smith and VanderWeele bound for the M structure (Smith, L. H., & VanderWeele, T. J. (2019). Bounding bias due to selection.).
#'
#' @param pVprob A scalar that represents the probability that V=1.
#' @param pUprob A scalar that represents the probability that U=1.
#' @param pTcoef A numerical vector with two elements. The first element is the intercept in the model for the treatment (as the defined by the input "probit"). The second element is the slope in the model for the treatment (as defined by the input "probit").
#' @param pYcoef A numerical vector with three elements. The first element is the intercept in the model for the outcome (as the defined by the input "probit"). The second element is the slope for T in the model for the outcome (as defined by the input "probit"). The third element is the slope for U in the model for the outcome (as defined by the input "probit").
#' @param pScoef A matrix of size K by 4, where K is the number of selection variables. The first column is the intercepts in the models for the selection variables (as defined by the input "probit"). The second column is the slopes for V in the models for the selection variables (as defined by the input "probit"). The third column is the slopes for U in the models for the selection variables (as defined by the input "probit"). The fourth column is the slopes for T in the models for the selection variables (as defined by the input "probit").
#' @param whichPar A string defining the population parameter of interest. Available options are as follows. (1) Relative risk in the total population: "RR_tot", (2) Risk difference in the total population: "RD_tot", (3) Relative risk in the subpopulation: "RR_s", (4) Risk difference in the subpopulation: "RD_s".
#' @param probit A logical variable that is used to define the models for the variables in the M structure. If T, the probit model is used. If F, the logit model is used.
#'
#' @return A list containing the Smith and VanderWeele bound and an indicator if the treatment has been reversed.
#' @export
#'
#' @examples
#' pV = 0.1
#' pU = 0.1
#' pT = c(0,1)
#' pY = c(0,0,1)
#' pS = matrix(c(1,0,0,0,1,0,0,0),nrow=2,byrow=TRUE)
#' SVboundM(pV,pU,pT,pY,pS,"RR_tot",TRUE)
SVboundM <- function(pVprob,pUprob,pTcoef,pYcoef,pScoef,whichPar,probit)
{
  # A function that calculates the SV bound for the bias due to selection, for multiple
  # selection variables. The input is the hyper parameters used in the M structure and
  # which population parameter the calculations are performed for.

  # Functions used in the code.
  #source("dataGenMultS.R")
  #source("selBiasMultSFunc.R")
  #source("biasBoundMultSFunc.R")


  ### GETTING THE DATA ###

  # Check if 0<P(V=1)<1 and 0<P(U=1)<1. If not, throw an error.
  if( any(pVprob < 0 | pVprob > 1) ) stop('P(V=1) not between 0 and 1.')
  if( any(pUprob < 0 | pUprob > 1) ) stop('P(U=1) not between 0 and 1.')

  constS = pScoef[,1]
  slopeSV = pScoef[,2]
  slopeSU = pScoef[,3]
  slopeST = pScoef[,4]

  pY1coef = c(pYcoef[1]+pYcoef[2],pYcoef[3])
  pY0coef = c(pYcoef[1],pYcoef[3])

  # Using the data generating function to get the probabilities in the model.
  dataProb = dataGenMultS(pVprob,pUprob,pTcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,slopeST,probit)
  # Extracting the vectors/matrices/array from the data frame.
  pV = stats::na.omit(dataProb$pV)
  pU = stats::na.omit(dataProb$pU)
  pT = matrix(stats::na.omit(dataProb$pT),ncol=2)
  pY1 = matrix(stats::na.omit(dataProb$pY1),ncol=2)
  pY0 = matrix(stats::na.omit(dataProb$pY0),ncol=2)
  pSmat = as.data.frame(dataProb[,6:length(dataProb[1,])])

  ### END GETTING THE DATA ###

  ### CALCULATING THE BIAS AND THE TRUE TREATMENT EFFECT ###

  # Calculate the bias and treatment effect for the parameter of interest
  biasAndTE = selBiasMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)

  # To check if the bias is negative and re-coding of the treatment is needed.
  testSelBias = biasAndTE[1]

  # Check if the selection bias is a numerical value. If not, throw an error.
  if( is.nan(testSelBias) ) stop('Input parameters result in 0/0. This can for instance happen if P(T=t|V)=0 or P(I_S=1|U,V)=0.')

  # Store the original bias and treatment effect.
  selBias_ori = biasAndTE[1]
  trueTE_ori = biasAndTE[2]

  biasLimit = ifelse(whichPar=="RD_s"|whichPar=="RD_tot",0,1)

  # Check if the bias is negative, and if it is re-code treatment and calculate the new bias and treatment effect.
  if(testSelBias<biasLimit)
  {
    revTreat = TRUE
    pTnew = rbind(pT[2,],pT[1,])
    pSmatNew = pSmatNew = as.data.frame(matrix(rbind(as.matrix(pSmat[5:8,]),as.matrix(pSmat[1:4,]),as.matrix(pSmat[13:16,]),as.matrix(pSmat[9:12,])),nrow=16))
    biasAndTEnew = selBiasMultSFunc(pY0,pY1,pTnew,pSmatNew,pU,pV,whichPar)
    selBias = biasAndTEnew[1]
    trueTE = biasAndTEnew[2]
  }else {selBias = biasAndTE[1]
  trueTE = biasAndTE[2]
  revTreat = FALSE}

  ### END CALCULATING THE BIAS AND THE TRUE TREATMENT EFFECT ###

  ### CALCULATING THE SV BOUNDS FOR THE BIAS ###

  if(testSelBias<biasLimit)
  {
    bound = biasBoundMultSFunc(pY0,pY1,pTnew,pSmatNew,pU,pV,whichPar)
  }else{bound = biasBoundMultSFunc(pY1,pY0,pT,pSmat,pU,pV,whichPar)}

  ### END CALCULATING THE SV BOUNDS FOR THE BIAS ###

  heading = c("SV bound","Reverse treatment")
  values = list(bound,as.logical(revTreat))

  returnDat = matrix(cbind(heading,values),ncol=2)
  return(returnDat)

}
