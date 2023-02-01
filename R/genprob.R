genprob <- function(Vcoef, Ucoef, Tcoef, Y1coef, Y0coef, constS, slopeSV, slopeSU, slopeST, model)
{
  # A function that takes the coefficients of the data generating process as
  # input, and then calculates the conditional probabilities of the DGP.
  # The output is a matrix with all the conditional probabilities.

  # Create vectors of the values and probabilities from the matrices of U and V.
  Vval = Vcoef[ , 1]
  pV = Vcoef[ , 2]

  Uval = Ucoef[ , 1]
  pU = Ucoef[ , 2]

  # Create the vectors that is used in the matrix multiplication further down.
  # Vvec is created in two steps since each entry in Vval is repeated x times.
  # Uvec is the vector Uval repeated x times.
  Vvec = matrix(Vval, nrow = length(Uval), ncol = length(Vval), byrow = TRUE)
  Vvec = matrix(Vvec, ncol = 1, byrow = FALSE)

  Uvec = matrix(Uval, nrow = length(Vval) * length(Uval), ncol = 1)

  # The number of selection variables.
  numS = length(constS)
  # An empty matrix for storing the selection probabilities in.
  sMat = matrix(NA, nrow = 4 * length(Uval) * length(Vval), ncol = numS)

  # Use different calculations if the models are probit or logit.
  if(model == "P")
  {
    # The probabilities P(T = t|V = v).
    pT1 = stats::pnorm(Tcoef[1] + Tcoef[2] * Vval)
    pT = c(pT1, 1 - pT1)

    # The probabilities P(Y(1) = y|U = u).
    pY11 = stats::pnorm(Y1coef[1] + Y1coef[2] * Uval)
    pY1 = c(pY11, 1 - pY11)

    # The probabilities P(Y(0) = y|U = u).
    pY01 = stats::pnorm(Y0coef[1] + Y0coef[2] * Uval)
    pY0 = c(pY01, 1 - pY01)

    # The probabilities P(S = s|V = v, U = u, T = t), where each iteration in
    # the loop is one selection variable.
    for (sss in 1 : numS)
    {
      pS1 = c(stats::pnorm(constS[sss] + slopeST[sss] + slopeSV[sss] * Vvec + slopeSU[sss] * Uvec),
              stats::pnorm(constS[sss] + slopeSV[sss] * Vvec + slopeSU[sss] * Uvec))
      pS = c(pS1, 1 - pS1)

      sMat[ , sss] = pS
    }
  }else if(model == "L"){
    # The probabilities P(T = t|V = v).
    pT1 = arm::invlogit(Tcoef[1] + Tcoef[2] * Vval)
    pT = c(pT1, 1 - pT1)

    # The probabilities P(Y(1) = y|U = u).
    pY11 = arm::invlogit(Y1coef[1] + Y1coef[2] * Uval)
    pY1 = c(pY11, 1 - pY11)

    # The probabilities P(Y(0) = y|U = u).
    pY01 = arm::invlogit(Y0coef[1] + Y0coef[2] * Uval)
    pY0 = c(pY01, 1 - pY01)

    # The probabilities P(S = s|V = v, U = u, T = t), where each iteration in
    # the loop is one selection variable.
    for (sss in 1 : numS)
    {
      pS1 = c(arm::invlogit(constS[sss] + slopeST[sss] + slopeSV[sss] * Vvec + slopeSU[sss] * Uvec),
              arm::invlogit(constS[sss] + slopeSV[sss] * Vvec + slopeSU[sss] * Uvec))
      pS = c(pS1, 1 - pS1)

      sMat[ , sss] = pS
    }
  }else{stop('Choose either "P" for probit or "L" for logit.')}

  # Create names for the columns of the matrix with selection probabilities.
  colnames(sMat) = colnames(sMat, do.NULL = FALSE, prefix = "pS")

  # Create vectors of the same length in order to merge all probabilities into
  # one data frame.
  length(pV) = length(pS)
  length(pU) = length(pS)
  length(pT) = length(pS)
  length(pY0) = length(pS)
  length(pY1) = length(pS)

  # Create the data frame that is then returned.
  dfProb = as.data.frame(cbind(pV, pU, pT, pY0, pY1, sMat))

  return(dfProb)
}
