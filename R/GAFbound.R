#' Generalized assumption-free bound
#'
#' `GAFbound()` returns a list with the GAF upper and lower bounds. The
#' sensitivity parameters can be inserted directly or as output from
#' `sensitivityparametersM()`.
#'
#' @param whichEst Input string. Defining the causal estimand of interest.
#'   Available options are as follows. (1) Relative risk in the total
#'   population: "RR_tot", (2) Risk difference in the total population:
#'   "RD_tot", (3) Relative risk in the subpopulation: "RR_sub", (4) Risk
#'   difference in the subpopulation: "RD_sub".
#' @param M Input value. Sensitivity parameter. Must be between 0 and 1, larger
#' than m and larger than max_t P(Y=1|T=t,I_s=1).
#' @param m Input value. Sensitivity parameter. Must be between 0 and 1, smaller
#' than M and smaller than min_t P(Y=1|T=t,I_s=1).
#' @param outcome Input vector. A binary outcome variable. Either the data
#' vector (length>=3) or two conditional outcome probabilities with
#' P(Y=1|T=1,I_s=1) and P(Y=1|T=0,I_s=1) as first and second element.
#' @param treatment Input vector. A binary treatment variable. Either the data
#' vector (length>=3) or two conditional treatment probabilities with
#' P(T=1|I_s=1) and P(T=0|I_s=1) as first and second element.
#' @param selection Input vector or input scalar. A binary selection variable or
#'   a selection probability. Can be omitted for subpopulation estimands.
#'
#' @return A list containing the upper and lower GAF bounds.
#' @export
#' @examples
#'
#' # Example with selection indicator variable.
#' y = c(0, 0, 0, 0, 1, 1, 1, 1)
#' tr = c(0, 0, 1, 1, 0, 0, 1, 1)
#' sel = c(0, 1, 0, 1, 0, 1, 0, 1)
#' Mt = 0.8
#' mt = 0.2
#' GAFbound(whichEst = "RR_tot", M = Mt, m = mt, outcome = y, treatment = tr,
#'  selection = sel)
#'
#' # Example with selection probability.
#' selprob = mean(sel)
#' GAFbound(whichEst = "RR_tot", M = Mt, m = mt, outcome = y[sel==1],
#'  treatment = tr[sel==1], selection = selprob)
#'
#' # Example with subpopulation and no selection variable or probability.
#' Ms = 0.7
#' ms = 0.1
#' GAFbound(whichEst = "RR_sub", M = Ms, m = ms, outcome = y, treatment = tr)
#'
#' # Example with simulated data.
#' n = 1000
#' tr = rbinom(n, 1, 0.5)
#' y = rbinom(n, 1, 0.2 + 0.05 * tr)
#' sel = rbinom(n, 1, 0.4 + 0.1 * tr + 0.3 * y)
#' Mt = 0.5
#' mt = 0.05
#' GAFbound(whichEst = "RD_tot", M = Mt, m = mt, outcome = y, treatment = tr,
#'  selection = sel)
#'
#' @references Zetterstrom, Stina. "Bounds for selection bias using outcome
#'  probabilities" Epidemiologic Methods 13, no. 1 (2024): 20230033
#'
GAFbound <- function(whichEst, M, m, outcome, treatment, selection = NULL)
{
  if(whichEst == "RR_tot" | whichEst == "RD_tot")
  {
    if(is.null(selection[1])){stop('The argument "selection" must be specified for total population estimands.')}
  }


  if(is.null(selection[1])){selection = 1} # Give arbitrary value in case of NULL.

  if((m < 0 | m > 1 | M < 0 | M > 1))
    stop("M and m cannot be smaller than 0 or larger than 1.")
  if(m >= M)
    stop("M must be larger than m.")

  # Calculate the GAF bound.
  bound = calcGAFbound(whichEst, M, m, outcome, treatment, selection, "GAF")
  bound = round(bound, 2)
  # Output.
  heading = c("GAF lower bound", "GAF upper bound")
  values = list(bound[1], bound[2])
  returnDat = matrix(cbind(heading, values), ncol = 2)
  return(returnDat)
}
