##########################################################################################################################################

test_that("Bounds are eqaul to 0 or 1 when there is no selection bias.", {

  # Bound for the relative risk in the total population should equal 1.
  expect_equal(as.numeric(SVbound(whichEst = "RR_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                                  RR_SU_T1 = 1, RR_SU_T0 = 1)[1, 2]), 1)

  # Bound for the risk difference in the total population should equal 1.
  expect_equal(as.numeric(SVbound(whichEst = "RD_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                                  RR_SU_T1 = 1, RR_SU_T0 = 1,
                                  pY1_T1_S1 = 0.5, pY1_T0_S1 = 0.5)[1, 2]), 1)


  # Bound for the relative risk in the subpopulation should equal 1.
  expect_equal(as.numeric(SVbound(whichEst = "RR_sub", RR_UY_S1 = 1, RR_TU_S1 = 1)[1, 2]), 1)

  # Bound for the risk difference in the subpopulation should equal 0.
  expect_equal(as.numeric(SVbound(whichEst = "RD_sub", RR_UY_S1 = 1, RR_TU_S1 = 1,
                                  pY1_T1_S1 = 0.5, pY1_T0_S1 = 0.5)[1, 2]), 0)
})

##########################################################################################################################################


test_that("SVbound throws an error if the estimand is not correctly specified.", {

  expect_error(SVbound(whichEst = "RR_t", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1), 'The estimand must be')

})

##########################################################################################################################################


test_that("SVbound throws an error if the wrong sensitivity parameter is equal to NULL.", {

  #Sensitivity parameters missing.
  expect_error(SVbound(whichEst = "RR_tot", RR_UY_T1 = NULL, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1),
               "When the total population is of interest, RR_UY_T1")
  expect_error(SVbound(whichEst = "RD_tot", RR_UY_T1 = 1, RR_UY_T0 = NULL,
                       RR_SU_T1 = 1, RR_SU_T0 = 1, pY1_T1_S1 = 0.5, pY1_T0_S1 = 0.5),
               "When the total population is of interest, RR_UY_T1")
  expect_error(SVbound(whichEst = "RR_sub", RR_UY_S1 = 1, RR_TU_S1 = NULL),
               "When the subpopulation is of interest, RR_UY_S1")
  expect_error(SVbound(whichEst = "RD_sub", RR_UY_S1 = NULL, RR_TU_S1 = 1,
                       pY1_T1_S1 = 0.5, pY1_T0_S1 = 0.5),
               "When the subpopulation is of interest, RR_UY_S1")

  # Probabilities missing.
  expect_error(SVbound(whichEst = "RD_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1, pY1_T1_S1 = 0.5, pY1_T0_S1 = NULL),
               "When the risk difference is of interest")

  expect_error(SVbound(whichEst = "RD_sub", RR_UY_S1 = 1, RR_TU_S1 = 1,
                       pY1_T1_S1 = NULL, pY1_T0_S1 = 0.5),
               "When the risk difference is of interest")

})

##########################################################################################################################################

test_that("SVbound throws an error if the input takes on incorrect values.", {

  # Sensitivity parameters smaller than 1.
  expect_error(SVbound(whichEst = "RR_tot", RR_UY_T1 = 0.5, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1),
               "All sensitivity parameters must be greater")
  expect_error(SVbound(whichEst = "RR_tot", RR_UY_T1 = 1, RR_UY_T0 = 0.5,
                       RR_SU_T1 = 1, RR_SU_T0 = 1),
               "All sensitivity parameters must be greater")
  expect_error(SVbound(whichEst = "RR_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 0.5, RR_SU_T0 = 1),
               "All sensitivity parameters must be greater")
  expect_error(SVbound(whichEst = "RR_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 0.5),
               "All sensitivity parameters must be greater")
  expect_error(SVbound(whichEst = "RR_sub", RR_UY_S1 = 0.5, RR_TU_S1 = 1),
               "All sensitivity parameters must be greater")
  expect_error(SVbound(whichEst = "RR_sub", RR_UY_S1 = 1, RR_TU_S1 = 0.5),
               "All sensitivity parameters must be greater")


  # Probabilities not valid.
  expect_error(SVbound(whichEst = "RD_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1,
                       pY1_T1_S1 = 0.5, pY1_T0_S1 = -1),
               "cannot be smaller than 0 or larger than 1")

  expect_error(SVbound(whichEst = "RD_tot", RR_UY_T1 = 1, RR_UY_T0 = 1,
                       RR_SU_T1 = 1, RR_SU_T0 = 1,
                       pY1_T1_S1 = 0.5, pY1_T0_S1 = 2),
               "cannot be smaller than 0 or larger than 1")

  expect_error(SVbound(whichEst = "RD_sub", RR_UY_S1 = 1, RR_TU_S1 = 1,
                       pY1_T1_S1 = -1, pY1_T0_S1 = 0.5),
               "cannot be smaller than 0 or larger than 1")

  expect_error(SVbound(whichEst = "RD_sub", RR_UY_S1 = 1, RR_TU_S1 = 1,
                       pY1_T1_S1 = 2, pY1_T0_S1 = 0.5),
               "cannot be smaller than 0 or larger than 1")

})

##########################################################################################################################################


