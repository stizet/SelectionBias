#' Simulated data set emulating a zika outbreak in Brazil
#'
#' The data set is simulated to mimic real data. For the data generating
#' process, see the vignette.
#'
#' @usage data(zika_learner)
#'
#' @format A data frame with 5,000 observations on the following 7 binary
#'   variables:
#' \describe{
#'   \item{mic_ceph}{Indication if the baby has microcephaly (1=microcephaly, 0=not microcephaly)}
#'   \item{zika}{Indication if the mother is infected by zika (1=infected, 0=not infected)}
#'   \item{urban}{Indication of the living area of the subject (1=urban, 0=rural)}
#'   \item{SES}{Indication of the socioeconomic status of the subject (1=high, 0=low)}
#'   \item{birth}{First selection variable. Indication if the baby is born (1=birth, 0=terminated birth)}
#'   \item{hospital}{Second selection variable. Indication if the delivery is in a public hospital (1=public, 0=private)}
#'   \item{sel_ind}{Selection indicator variable. Indication if the subject is included in the study (1=included, 0=not included)}
#' }
#'
#' @details The data set is created to use in examples of selection bias. A
#'   similar example has previously been used in articles that construct bounds
#'   for selection bias (Smith and VanderWeele, 2019; Zetterstrom and Waernbaum,
#'   2022).
#'
#' @references de Araújo, Thalia Velho Barreto, et al. "Association between
#'   microcephaly, Zika virus infection, and other risk factors in Brazil: final
#'   report of a case-control study." The Lancet infectious diseases 18.3
#'   (2018): 328-336.
#'
#'   de Oliveira, Wanderson Kleber, et al. "Infection-related microcephaly after
#'   the 2015 and 2016 Zika virus outbreaks in Brazil: a surveillance-based
#'   analysis." The Lancet 390.10097 (2017): 861-870.
#'
#'   Ali, Sofia, et al. "Environmental and social change drive the explosive
#'   emergence of Zika virus in the Americas." PLoS neglected tropical diseases
#'   11.2 (2017): e0005135.
#'
#'   Lebov, Jill F., et al. "International prospective observational cohort
#'   study of Zika in infants and pregnancy (ZIP study): study protocol." BMC
#'   Pregnancy and Childbirth 19.1 (2019): 1-10.
#'
#'   Malta, Monica, et al. "Abortion in Brazil: the case for women's rights,
#'   lives, and choices." The Lancet Public Health 4.11 (2019): e552.
#'
#'   Smith, Louisa H., and Tyler J. VanderWeele. "Bounding bias due to
#'   selection." Epidemiology (Cambridge, Mass.) 30.4 (2019): 509.
#'
#'   Zetterstrom, Stina and Waernbaum, Ingeborg. "Selection bias and multiple
#'   inclusion criteria in observational studies" Epidemiologic Methods 11, no.
#'   1 (2022): 20220108.
#'
#'
#'   https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS?locations=BR
#'
#'
#'   https://agenciabrasil.ebc.com.br/en/geral/noticia/2020-12/number-births-registered-brazil-down-2019
#'
#'
#'   https://www.angloinfo.com/how-to/brazil/healthcare/health-system
#'
"zika_learner"
