##########################################################################################################################################

test_that("Bounds are equal to specific values.", {

  # Lower bound for the relative risk in the total population should equal 0.50.
  expect_equal(as.numeric(GAFbound(whichEst = "RR_tot", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                    treatment = c(0.5, 0.5), selection = 0.9)[1, 2]), 0.50)

  # Upper bound for the relative risk in the total population should equal 1.99.
  expect_equal(as.numeric(GAFbound(whichEst = "RR_tot", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.9)[2, 2]), 1.99)

  # Lower bound for the risk difference in the total population should equal -0.36.
  expect_equal(as.numeric(GAFbound(whichEst = "RD_tot", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.8)[1, 2]), -0.36)

  # Upper bound for the risk difference in the total population should equal 0.36.
  expect_equal(as.numeric(GAFbound(whichEst = "RD_tot", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5), selection = 0.8)[2, 2]), 0.36)


  # Lower bound for the relative risk in the subpopulation should equal 0.54.
  expect_equal(as.numeric(GAFbound(whichEst = "RR_sub", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[1, 2]), 0.54)

  # Upper bound for the relative risk in the subpopulation should equal 1.86.
  expect_equal(as.numeric(GAFbound(whichEst = "RR_sub", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[2, 2]), 1.86)

  # Lower bound for the risk difference in the subpopulation should equal -0.3.
  expect_equal(as.numeric(GAFbound(whichEst = "RD_sub", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[1, 2]), -0.3)

  # Upper bound for the risk difference in the subpopulation should equal 0.3.
  expect_equal(as.numeric(GAFbound(whichEst = "RD_sub", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                                   treatment = c(0.5, 0.5))[2, 2]), 0.3)

  })

##########################################################################################################################################


test_that("GAFbound throws an error if the estimand is not correctly specified.", {

  expect_error(GAFbound(whichEst = "RR_t", M = 0.8, m = 0.2, outcome = c(0.5, 0.5),
                        treatment = c(0.5, 0.5), selection = 0.8), 'The estimand must be')

})

##########################################################################################################################################


test_that("GAFbound throws an error if P(I_s=1) is equal to NULL for tot pop.", {

  #Sensitivity parameters missing.
  expect_error(GAFbound(whichEst = "RR_tot", M = 0.8, m = 0.2,
                        outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               'The argument "selection"')
})

##########################################################################################################################################

test_that("GAFbound throws an error if the input takes on incorrect values.", {

  # Sensitivity parameters smaller than 1.
  expect_error(GAFbound(whichEst = "RR_sub", M = 1.5, m = 0.2,
                        outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               "M and m cannot be smaller")
  expect_error(GAFbound(whichEst = "RR_sub", M = 0.8, m = -0.2,
                        outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               "M and m cannot be smaller")
  expect_error(GAFbound(whichEst = "RR_sub", M = 0.2, m = 0.8,
                        outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               "M must be larger than m")
  expect_error(GAFbound(whichEst = "RR_sub", M = 0.8, m = 0.6,
                        outcome = c(0.5, 0.5), treatment = c(0.5, 0.5)),
               '"M" must be larger than')

})

##########################################################################################################################################


