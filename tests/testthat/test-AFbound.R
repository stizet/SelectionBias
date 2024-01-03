##########################################################################################################################################

test_that("Bounds are equal to specific values.", {

  # Lower bound for the relative risk in the total population should equal 0.29.
  expect_equal(as.numeric(AFbound(whichEst = "RR_tot", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.9)[1, 2]), 0.29)

  # Upper bound for the relative risk in the total population should equal 3.44.
  expect_equal(as.numeric(AFbound(whichEst = "RR_tot", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.9)[2, 2]), 3.44)

  # Lower bound for the risk difference in the total population should equal -0.6.
  expect_equal(as.numeric(AFbound(whichEst = "RD_tot", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.8)[1, 2]), -0.6)

  # Upper bound for the risk difference in the total population should equal 0.6.
  expect_equal(as.numeric(AFbound(whichEst = "RD_tot", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.8)[2, 2]), 0.6)


  # Lower bound for the relative risk in the subpopulation should equal 0.33.
  expect_equal(as.numeric(AFbound(whichEst = "RR_sub", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[1, 2]), 0.33)

  # Upper bound for the relative risk in the subpopulation should equal 3.
  expect_equal(as.numeric(AFbound(whichEst = "RR_sub", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[2, 2]), 3)

  # Lower bound for the risk difference in the subpopulation should equal -0.5.
  expect_equal(as.numeric(AFbound(whichEst = "RD_sub", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[1, 2]), -0.5)

  # Upper bound for the risk difference in the subpopulation should equal 0.5.
  expect_equal(as.numeric(AFbound(whichEst = "RD_sub", outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[2, 2]), 0.5)

})

##########################################################################################################################################


test_that("AFbound throws an error if the estimand is not correctly specified.", {

  expect_error(AFbound(whichEst = "RR_t", outcome = c(0.5, 0.5),
                        treatment = c(0.5, 0.5), selection = 0.8), 'The estimand must be')

})

##########################################################################################################################################


test_that("AFbound throws an error if P(I_s=1) is equal to NULL for tot pop.", {

  expect_error(AFbound(whichEst = "RR_tot", outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               'The argument "selection"')
})


##########################################################################################################################################

test_that("AFbound throws an error the wrong dimension of the input is used.", {

  expect_error(AFbound(whichEst = "RR_sub", outcome = c(0.5, 0.5),
                       treatment = 0.5),
               'The length of the arguments')
})

##########################################################################################################################################

test_that("AFbound throws an error if the input takes on incorrect values.", {

  expect_error(AFbound(whichEst = "RR_tot", outcome = c(0.5, 0.5),
                       treatment = c(0.5, 0.5), selection = 1.2),
               "The probability")
  expect_error(AFbound(whichEst = "RR_sub", outcome = c(0.5, 0.5),
                       treatment = c(1.2, 0.5)),
               "The probabilities")
  expect_error(AFbound(whichEst = "RR_sub", outcome = c(0.5, 1.2),
                       treatment = c(0.5, 0.5)),
               'The probabilities')

})

##########################################################################################################################################


