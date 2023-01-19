
##########################################################################################################################################

test_that("SVsharp works for correct input.", {
  expect_equal(SVboundsharp(2,0.4), "SV bound is sharp.")
  expect_equal(SVboundsharp(2,0.4), "SV bound is sharp.")
})


##########################################################################################################################################

test_that("SVboundsharp throws an error for wrong input.", {
  expect_error(SVboundsharp(0.5,0.8), "greater than or equal to 1")
  expect_error(SVboundsharp(2,2), "not between 0 and 1")
  expect_error(SVboundsharp(2,-2), "not between 0 and 1")
})

##########################################################################################################################################
