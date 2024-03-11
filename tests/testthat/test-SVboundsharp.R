
##########################################################################################################################################

test_that("SVsharp works for correct input.", {
  expect_equal(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.4), "SV bound is sharp.")
  expect_equal(SVboundsharp(BF_U = 2, pY1_T0_S1 = 0.9), "Inconclusive.")
})


##########################################################################################################################################

test_that("SVboundsharp throws an error for incorrect input.", {
  expect_error(SVboundsharp(BF_U = 0.5, pY1_T0_S1 = 0.8), "greater than or equal to 1")
  expect_error(SVboundsharp(BF_U = 2, pY1_T0_S1 = 2), "not between 0 and 1")
  expect_error(SVboundsharp(BF_U = 2, pY1_T0_S1 = -2), "not between 0 and 1")
})

##########################################################################################################################################

