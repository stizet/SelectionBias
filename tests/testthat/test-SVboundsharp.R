
##########################################################################################################################################

test_that("SVsharp works for correct input.", {
  expect_equal(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.4), "SV bound is sharp.")
  expect_equal(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9), "Inconclusive.")
  expect_equal(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9, AFbound = 1.5, SVbound = 2),
               "SV bound is not sharp.")
})


##########################################################################################################################################

test_that("SVboundsharp throws an error for incorrect input.", {
  expect_error(SVboundsharp(BF_U = 0.5, pY1_T0_S1 = 0.8), "greater than or equal to 1")
  expect_error(SVboundsharp(BF_U = 2, pY1_T0_S1 = 2), "not between 0 and 1")
  expect_error(SVboundsharp(BF_U = 2, pY1_T0_S1 = -2), "not between 0 and 1")
})

##########################################################################################################################################

test_that("SVboundsharp throws a warning when missing one of AFbound and SVbound.", {
  expect_warning(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9, AFbound = 1.5),
                 "to check if the SV bound is not sharp")
  expect_warning(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9, SVbound = 1.5),
                 "to check if the SV bound is not sharp")
})

##########################################################################################################################################
