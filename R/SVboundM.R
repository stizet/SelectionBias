#' Calculate the Smith and VanderWeele bound for the M-structure
#'
#' `SVboundM()` returns a list with the SV bound, the sensititivity parameters and an indicator if
#' the treatment coding is reversed for an assumed model (Smith, L. H., & VanderWeele, T. J. (2019). Bounding bias due to selection.).
#'
#' @param Vval Input matrix. The first column is the values of the categories of
#'   V. The second column is the probabilities of the categories of V. If V is
#'   continuous, use a fine grid of values and probabilities.
#' @param Uval Input matrix. The first column is the values of the categories of
#'   U. The second column is the probabilities of the categories of U. If U is
#'   continuous, use a fine grid of values and probabilities.
#' @param Tcoef Input vector. Two numerical elements. The first element is the
#'   intercept in the model for the treatment. The second element is the slope
#'   in the model for the treatment.
#' @param Ycoef Ycoef Input vector. Three numerical elements. The first element is the
#'   intercept in the model for the outcome. The second element is the slope for
#'   T in the model for the outcome. The third element is the slope for U in the
#'   model for the outcome.
#' @param Scoef Input matrix. Numerical matrix of size K by 4, where K is the
#'   number of selection variables. Each row is the coefficients for one
#'   selection variable. The first column is the intercepts in the models for
#'   the selection variables. The second column is the slopes for V in the
#'   models for the selection variables. The third column is the slopes for U in
#'   the models for the selection variables. The fourth column is the slopes for
#'   T in the models for the selection variables.
#' @param whichEst Input string. Defining the causal estimand of interest.
#'   Available options are as follows. (1) Relative risk in the total
#'   population: "RR_tot", (2) Risk difference in the total population:
#'   "RD_tot", (3) Relative risk in the subpopulation: "RR_s", (4) Risk
#'   difference in the subpopulation: "RD_s".
#' @param Mmodel Input string. Defining the models for the variables in the M
#'   structure. If "P", the probit model is used. If "L", the logit model is
#'   used.
#'
#' @return A list containing the Smith and VanderWeele bound and an indicator if the treatment has been reversed.
#' @export
#'
#' @examples
#' pV = matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE)
#' pU = matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE)
#' pT = c(0,1)
#' pY = c(0,0,1)
#' pS = matrix(c(1,0,0,0,1,0,0,0),nrow=2,byrow=TRUE)
#' SVboundM(pV,pU,pT,pY,pS,"RR_tot","P")
SVboundM <- function(Vval,Uval,Tcoef,Ycoef,Scoef,whichEst,Mmodel)
{
  # A function that calculates the SV bound for the bias due to selection, for
  # multiple selection variables. The input is the hyper parameters used in the
  # M-structure and which causal estimand the calculations are performed for.
  # The output is the SV bound, sensitivity parameters and an indicator for
  # coding of treatment.

  # Functions used in the code.
  #genprob()
  #calcselbias()
  #calcSVbound())


  ### RUN SOME CHECKS OF THE INPUT ###

  # Check if the estimand is one of the four "RR_tot", "RD_tot", "RR_s", "RD_s".
  if( any(whichEst != "RR_tot" & whichEst != "RD_tot" & whichEst != "RR_s" & whichEst != "RD_s") ) stop('The estimand must be "RR_tot", "RD_tot", "RR_s" or "RD_s".')

  # Check if the probabilities of V and U sum to 1. If not, throw an error.
  if( any((sum(Vval[,2]) > 1) | (sum(Vval[,2]) < 1))) stop('The probabilities of the categories of V do not sum to 1.')
  if( any((sum(Uval[,2]) > 1) | (sum(Uval[,2]) < 1))) stop('The probabilities of the categories of U do not sum to 1.')

  # Check if the probabilities of V and U are positive. If not, throw an error.
  if( any(Vval[,2] < 0) ) stop('At least one of the categories of V has a negative probability.')
  if( any(Uval[,2] < 0) ) stop('At least one of the categories of U has a negative probability.')

  ### END CHECKS OF THE INPUT ###


  ### GETTING THE DATA PROBABILITIES ###

  constS = Scoef[,1]
  slopeSV = Scoef[,2]
  slopeSU = Scoef[,3]
  slopeST = Scoef[,4]

  pY1coef = c(Ycoef[1]+Ycoef[2],Ycoef[3])
  pY0coef = c(Ycoef[1],Ycoef[3])

  # Using the data generating function to get the probabilities in the model.
  dataProb = genprob(Vval,Uval,Tcoef,pY1coef,pY0coef,constS,slopeSV,slopeSU,slopeST,Mmodel)
  # Extracting the vectors/matrices/array from the data frame.
  pV = stats::na.omit(dataProb$pV)
  pU = stats::na.omit(dataProb$pU)
  pT = matrix(stats::na.omit(dataProb$pT),nrow=2,byrow=TRUE)
  pY1 = matrix(stats::na.omit(dataProb$pY1),ncol=2,byrow=TRUE)
  pY0 = matrix(stats::na.omit(dataProb$pY0),ncol=2,byrow=TRUE)
  pSmat = as.data.frame(dataProb[,6:length(dataProb[1,])])

  ### END GETTING THE DATA PROBABILITIES ###

  ### CALCULATING THE BIAS AND THE OBSERVED PROBABILITIES ###

  # Calculate the bias and treatment effect for the parameter of interest
  biasAndObsProb = calcselbias(pY1,pY0,pT,pSmat,pU,pV,whichEst)

  # To check if the bias is negative and re-coding of the treatment is needed.
  testSelBias = biasAndObsProb[1]

  # Check if the selection bias is a numerical value. If not, throw an error.
  if( is.nan(testSelBias) ) stop('Input parameters result in 0/0. This can for instance happen if P(T=t|V)=0 or P(I_S=1|U,V)=0.')

  biasLimit = ifelse(whichEst=="RD_s"|whichEst=="RD_tot",0,1)

  # Check if the bias is negative, and if it is re-code treatment and calculate the new bias and treatment effect.
  if(testSelBias<biasLimit)
  {
    revTreat = TRUE
    pTnew = rbind(pT[2,],pT[1,])
    lenS = length(pSmat[,1])
    pSmatNew = as.data.frame(matrix(rbind(as.matrix(pSmat[(lenS/4+1):(lenS/2),]),as.matrix(pSmat[1:(lenS/4),]),as.matrix(pSmat[(3*lenS/4+1):lenS,]),as.matrix(pSmat[(lenS/2+1):(3*lenS/4),])),nrow=lenS))
    biasAndObsProbnew = calcselbias(pY0,pY1,pTnew,pSmatNew,pU,pV,whichEst)
    selBias = biasAndObsProbnew[1]
    obsProb = biasAndObsProbnew[2:3]
  }else {selBias = biasAndObsProb[1]
  obsProb = biasAndObsProb[2:3]
  revTreat = FALSE}

  ### END CALCULATING THE BIAS AND THE OBSERVED PROBABILITIES ###

  ### CALCULATING THE SV BOUND ###

  if(testSelBias<biasLimit)
  {
    SVbound = calcSVbound(pY0,pY1,pTnew,pSmatNew,pU,pV,whichEst,obsProb)
  }else{SVbound = calcSVbound(pY1,pY0,pT,pSmat,pU,pV,whichEst,obsProb)}

  ### END CALCULATING THE SV BOUND ###

  # The return list.
  if(whichEst=="RR_tot"){
    heading = c("SV bound","BF_1","BF_0","RR_SU|T=1","RR_SU|T=0","RR_UY|T=1","RR_UY|T=0","Reverse treatment")
    values = list(SVbound[1],SVbound[2],SVbound[3],SVbound[4],SVbound[5],SVbound[6],SVbound[7],as.logical(revTreat))
  }else if(whichEst=="RD_tot"){
    heading = c("SV bound","BF_1","BF_0","RR_SU|T=1","RR_SU|T=0","RR_UY|T=1","RR_UY|T=0","P(Y=1|T=1,I_S=1)","P(Y=1|T=0,I_S=1)","Reverse treatment")
    values = list(SVbound[1],SVbound[2],SVbound[3],SVbound[4],SVbound[5],SVbound[6],SVbound[7],SVbound[8],SVbound[9],as.logical(revTreat))
  }else if(whichEst=="RR_s"){
    heading = c("SV bound","BF_U","RR_TU|S=1","RR_UY|S=1","Reverse treatment")
    values = list(SVbound[1],SVbound[2],SVbound[3],SVbound[4],as.logical(revTreat))
  }else{
    heading = c("SV bound","BF_U","RR_TU|S=1","RR_UY|S=1","P(Y=1|T=1,I_S=1)","P(Y=1|T=0,I_S=1)","Reverse treatment")
    values = list(SVbound[1],SVbound[2],SVbound[3],SVbound[4],SVbound[5],SVbound[6],as.logical(revTreat))
  }

  returnDat = matrix(cbind(heading,values),ncol=2)
  return(returnDat)

}
