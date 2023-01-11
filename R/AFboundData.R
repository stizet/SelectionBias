#' Calculate the assumption free bound for a data set.
#'
#' @param outcome A vector with the binary outcome variable.
#' @param treatment A vector with the binary treatment variable.
#' @param selection A vector with the binary selection variable or a scalar representing the probability of being selected.
#' @param whichEst A string defining the population parameter of interest. Available options are as follows. (1) Relative risk in the total population: "RR_tot", (2) Risk difference in the total population: "RD_tot", (3) Relative risk in the subpopulation: "RR_s", (4) Risk difference in the subpopulation: "RD_s".
#'
#' @return A scalar with the assumption free bound.
#' @export
#'
#' @examples
#' y = c(0,0,0,0,1,1,1,1)
#' tr = c(0,0,1,1,0,0,1,1)
#' sel = c(0,1,0,1,0,1,0,1)
#' AFbounddata(y,tr,sel,"RR_tot")
#'
#' y = c(0,0,0,0,1,1,1,1)
#' tr = c(0,0,1,1,0,0,1,1)
#' selProb = 0.5
#' AFbounddata(y,tr,selProb,"RR_tot")
AFbounddata <- function(outcome,treatment,selection,whichEst)
{
  # A function that calculates the assumption free bound for the bias due to selection,
  # for multiple selection variables. The input is the data and which population
  # parameter the calculations are performed for.

  y = outcome
  tr = treatment
  Is = selection

  # If the selection indicator variable is included.
  if(length(Is)>1)
  {
    # P(I_s=1) and P(I_s=0).
    pIs1 = length(Is[Is==1])/length(Is)
    pIs0 = 1-pIs1

    # P(T=1|I_s=1) and P(T=0|I_s=1).
    pT1_Is1 = length(tr[tr==1&Is==1])/length(Is[Is==1])
    pT0_Is1 = length(tr[tr==0&Is==1])/length(Is[Is==1])

    # P(Y=1|T=1,I_s=1) and P(Y=1|T=0,I_s=1).
    pY1_T1_Is1 = length(y[y==1&tr==1&Is==1])/length(tr[tr==1&Is==1])
    pY1_T0_Is1 = length(y[y==1&tr==0&Is==1])/length(tr[tr==0&Is==1])
  }else{
    # If the selection probability is included.

    if( any(Is < 0 | Is > 1) ) stop('P(I_s=1) not between 0 and 1.')

    # P(I_s=1) and P(I_s=0).
    pIs1 = Is
    pIs0 = 1-pIs1

    # P(T=1|I_s=1) and P(T=0|I_s=1).
    pT1_Is1 = length(tr[tr==1])/length(tr)
    pT0_Is1 = length(tr[tr==0])/length(tr)

    # P(Y=1|T=1,I_s=1) and P(Y=1|T=0,I_s=1).
    pY1_T1_Is1 = length(y[y==1&tr==1])/length(tr[tr==1])
    pY1_T0_Is1 = length(y[y==1&tr==0])/length(tr[tr==0])
  }

  if( is.nan(pY1_T1_Is1) ) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if( is.nan(pY1_T0_Is1) ) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if( is.nan(pT1_Is1) ) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')
  if( is.nan(pT0_Is1) ) stop('Input data result in 0/0. This can for instance happen if P(T=t|I_s=1)=0 or P(Y=1|T=t,I_s=1)=0.')

  # Calculate the assumption free bound for the relevant parameter.
  if(whichEst=="RR_tot"){
    AFbound = min((pT1_Is1*pIs1+2*pIs0+pY1_T0_Is1*pT0_Is1*pIs1),1)/(pY1_T0_Is1*pT1_Is1*pIs1)
  }else if(whichEst=="RD_tot"){
    AFbound = min((pT1_Is1*pIs1+2*pIs0+pY1_T0_Is1*pT0_Is1*pIs1),1)+pY1_T1_Is1*(1-pT1_Is1*pIs1)-pY1_T0_Is1
  }else if(whichEst=="RR_s"){
    AFbound = min((pT1_Is1+pY1_T0_Is1*pT0_Is1),1)/(pY1_T0_Is1*pT1_Is1)
  }else{
    AFbound = min((pT1_Is1+pY1_T0_Is1*pT0_Is1),1)+pY1_T1_Is1*(1-pT1_Is1)-pY1_T0_Is1
  }
  return(AFbound)
}
