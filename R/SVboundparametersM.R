#' Sensitivity parameters for the Smith and VanderWeele bound
#'
#' `SVboundparametersM()` returns a list with the sensitivity parameters and an
#' indicator if bias is negative and the treatment coding is reversed for an
#' assumed model.
#'
#' @param whichEst Input string. Defining the causal estimand of interest.
#'   Available options are as follows. (1) Relative risk in the total
#'   population: "RR_tot", (2) Risk difference in the total population:
#'   "RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
#'   difference in the subpopulation: "RD_sub".
#' @param Vval Input matrix. The first column is the values of the categories of
#'   V. The second column is the probabilities of the categories of V. If V is
#'   continuous, use a fine grid of values and probabilities.
#' @param Uval Input matrix. The first column is the values of the categories of
#'   U. The second column is the probabilities of the categories of U. If U is
#'   continuous, use a fine grid of values and probabilities.
#' @param Tcoef Input vector. Two numerical elements. The first element is the
#'   intercept in the model for the treatment. The second element is the slope
#'   in the model for the treatment.
#' @param Ycoef Input vector. Three numerical elements. The first element is the
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
#' @param Mmodel Input string. Defining the models for the variables in the M
#'   structure. If "P", the probit model is used. If "L", the logit model is
#' @param pY1_T1_S1 Input scalar. The observed probability P(Y=1|T=1,I_S=1).
#' @param pY1_T0_S1 Input scalar. The observed probability P(Y=1|T=0,I_S=1).
#'   used.
#' @return A list containing the sensitivity parameters and an indicator if the
#'   treatment has been reversed.
#' @export
#'
#' @examples
#'
#' # Example with no selection bias.
#' V = matrix(c(1, 0, 0.1, 0.9), ncol = 2)
#' U = matrix(c(1, 0, 0.1, 0.9), ncol = 2)
#' Tr = c(0, 1)
#' Y = c(0, 0, 1)
#' S = matrix(c(1, 0, 0, 0, 1, 0, 0, 0), nrow = 2, byrow = TRUE)
#' probT1 = 0.534
#' probT0 = 0.534
#' SVboundparametersM(whichEst = "RR_tot", Vval = V, Uval = U, Tcoef = Tr,
#'   Ycoef = Y, Scoef = S, Mmodel = "P", pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)
#'
#' # Example with selection bias. DGP from the zika example.
#' V = matrix(c(1, 0, 0.85, 0.15), ncol = 2)
#' U = matrix(c(1, 0, 0.5, 0.5), ncol = 2)
#' Tr = c(-6.2, 1.75)
#' Y = c(-5.2, 5.0, -1.0)
#' S = matrix(c(1.2, 2.2, 0.0, 0.5, 2.0, -2.75, -4.0, 0.0), ncol = 4)
#' probT1 = 0.286
#' probT0 = 0.004
#' SVboundparametersM(whichEst = "RR_sub", Vval = V, Uval = U, Tcoef = Tr,
#'   Ycoef = Y, Scoef = S, Mmodel = "L", pY1_T1_S1 = probT1, pY1_T0_S1 = probT0)
#'
#'
#' @references  Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due
#'   to selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and multiple
#'   inclusion criteria in observational studies" Epidemiologic Methods 11, no.
#'   1 (2022): 20220108.
#'
SVboundparametersM <- function(whichEst, Vval, Uval, Tcoef, Ycoef, Scoef, Mmodel, pY1_T1_S1, pY1_T0_S1)
{
  # A function that calculates the sensitivity parameters for the SV bound
  # for multiple selection variables. The input is the hyper parameters used
  # in the M-structure and which causal estimand the calculations are performed
  # for. The output is the sensitivity parameters and an indicator for
  # coding of treatment.

  # Functions used in the code.
  #genprob()
  #calcselbias()
  #calcSVbound()

  ### RUN SOME CHECKS OF THE INPUT ###

  # Check if the estimand is one of the four "RR_tot", "RD_tot", "RR_sub", "RD_sub".
  if(whichEst != "RR_tot" & whichEst != "RD_tot" & whichEst != "RR_sub" & whichEst != "RD_sub")
    stop('The estimand must be "RR_tot", "RD_tot", "RR_sub" or "RD_sub".')

  # Check dimensions of the input arguments.
  if(length(Vval[1, ]) != 2) stop('The number of columns in Vval must be equal to 2.')
  if(length(Uval[1, ]) != 2) stop('The number of columns in Uval must be equal to 2.')
  if(length(Tcoef) != 2) stop('The number of parameters in Tcoef must be equal to 2.')
  if(length(Ycoef) != 3) stop('The number of parameters in Ycoef must be equal to 3.')
  if(length(Scoef[1,]) != 4) stop('The number of columns in Scoef must be equal to 4.')

  # Check if the probabilities of V and U sum to 1. If not, throw an error.
  if(any((sum(Vval[ , 2]) > 1) | (sum(Vval[ , 2]) < 1))) stop('The probabilities of the categories of V do not sum to 1.')
  if(any((sum(Uval[ , 2]) > 1) | (sum(Uval[ , 2]) < 1))) stop('The probabilities of the categories of U do not sum to 1.')

  # Check if the probabilities of V and U are positive. If not, throw an error.
  if(any(Vval[ , 2] < 0)) stop('At least one of the categories of V has a negative probability.')
  if(any(Uval[ , 2] < 0)) stop('At least one of the categories of U has a negative probability.')

  # Check if the probabilities of V and U are equal to 0. If they are, throw an error.
  if(any(Vval[ , 2] == 0)) stop('At least one of the categories of V has a probability
                                equal to 0. Remove that category, or change to a positive value.')
  if(any(Uval[ , 2] == 0)) stop('At least one of the categories of U has a probability
                                equal to 0. Remove that category, or change to a positive value.')

  if(any(c(pY1_T1_S1, pY1_T0_S1) < 0)) stop('The observed probabilities must be greater than 0 and smaller than 1.')
  if(any(c(pY1_T1_S1, pY1_T0_S1) > 1)) stop('The observed probabilities must be greater than 0 and smaller than 1.')

  ### END CHECKS OF THE INPUT ###


  ### GETTING THE DATA PROBABILITIES ###

  constS = Scoef[ , 1]
  slopeSV = Scoef[ , 2]
  slopeSU = Scoef[ , 3]
  slopeST = Scoef[ , 4]

  Y1coef = c(Ycoef[1] + Ycoef[2], Ycoef[3])
  Y0coef = c(Ycoef[1], Ycoef[3])

  obsProb = c(pY1_T1_S1, pY1_T0_S1)

  # Using the data generating function to get the probabilities in the model.
  dataProb = genprob(Vval, Uval, Tcoef, Y1coef, Y0coef, constS, slopeSV, slopeSU, slopeST, Mmodel)
  # Extracting the vectors/matrices/array from the data frame.
  pV = stats::na.omit(dataProb$pV)
  pU = stats::na.omit(dataProb$pU)
  pT = matrix(stats::na.omit(dataProb$pT), nrow = 2, byrow = TRUE)
  pY1 = matrix(stats::na.omit(dataProb$pY1), nrow = 2, byrow = TRUE)
  pY0 = matrix(stats::na.omit(dataProb$pY0), nrow = 2, byrow = TRUE)
  pSmat = as.data.frame(dataProb[ , 6 : length(dataProb[1, ])])

  ### END GETTING THE DATA PROBABILITIES ###

  ### CALCULATING THE BIAS AND THE OBSERVED PROBABILITIES ###

  # Calculate the bias and treatment effect for the parameter of interest
  biasAndObsProb = calcselbias(pY1, pY0, pT, pSmat, pU, pV, whichEst, obsProb)

  # To check if the bias is negative and re-coding of the treatment is needed.
  testSelBias = biasAndObsProb[1]

  # Check if the selection bias is a numerical value. If not, throw an error.
  if(is.nan(testSelBias)) stop('Input parameters result in 0/0. This can for
                               instance happen if P(T=t|V)=0 or P(I_S=1|U,V)=0.')

  biasLimit = ifelse(whichEst == "RD_sub" | whichEst == "RD_tot", 0, 1)

  # Check if the bias is negative, and if it is re-code treatment and calculate
  # the new bias and treatment effect.
  if(testSelBias < biasLimit)
  {
    revTreat = TRUE
    obsProb = c(pY1_T0_S1, pY1_T1_S1)
    pTnew = rbind(pT[2, ], pT[1, ])
    lenS = length(pSmat[ , 1])
    pSmatNew = as.data.frame(matrix(rbind(as.matrix(pSmat[(lenS / 4 + 1) : (lenS / 2), ]),
                                          as.matrix(pSmat[1 : (lenS / 4), ]),
                                          as.matrix(pSmat[(3 * lenS / 4 + 1) : lenS, ]),
                                          as.matrix(pSmat[(lenS / 2 + 1) : (3 * lenS / 4), ])), nrow = lenS))
    biasAndObsProbnew = calcselbias(pY0, pY1, pTnew, pSmatNew, pU, pV, whichEst, obsProb)
    selBias = biasAndObsProbnew[1]
  }else {selBias = biasAndObsProb[1]
  revTreat = FALSE}

  ### END CALCULATING THE BIAS AND THE OBSERVED PROBABILITIES ###

  ### CALCULATING THE SV BOUND ###

  if(testSelBias < biasLimit)
  {
    SVbound = calcSVbound(pY0, pY1, pTnew, pSmatNew, pU, pV, whichEst, obsProb)
  }else{SVbound = calcSVbound(pY1, pY0, pT, pSmat, pU, pV, whichEst, obsProb)}

  ### END CALCULATING THE SV BOUND ###

  # The return list.
  if(whichEst == "RR_tot"){
    heading = c("BF_1", "BF_0", "RR_UY|T=1", "RR_UY|T=0", "RR_SU|T=1", "RR_SU|T=0", "Reverse treatment")
    values = list(SVbound[2], SVbound[3], SVbound[4], SVbound[5], SVbound[6], SVbound[7], as.logical(revTreat))
  }else if(whichEst == "RD_tot"){
    heading = c("BF_1", "BF_0", "RR_UY|T=1", "RR_UY|T=0", "RR_SU|T=1", "RR_SU|T=0",
                "P(Y=1|T=1,I_S=1)", "P(Y=1|T=0,I_S=1)", "Reverse treatment")
    values = list(SVbound[2], SVbound[3], SVbound[4], SVbound[5], SVbound[6],
                  SVbound[7], SVbound[8], SVbound[9], as.logical(revTreat))
  }else if(whichEst == "RR_sub"){
    heading = c("BF_U", "RR_UY|S=1", "RR_TU|S=1", "Reverse treatment")
    values = list(SVbound[2], SVbound[3], SVbound[4], as.logical(revTreat))
  }else{
    heading = c("BF_U", "RR_UY|S=1", "RR_TU|S=1", "P(Y=1|T=1,I_S=1)", "P(Y=1|T=0,I_S=1)", "Reverse treatment")
    values = list(SVbound[2], SVbound[3], SVbound[4], SVbound[5], SVbound[6], as.logical(revTreat))
  }

  returnDat = matrix(cbind(heading, values), ncol = 2)
  return(returnDat)

}
