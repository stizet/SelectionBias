
##########################################################################################################################################

test_that("genprob throws an error for incorrect model.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y1 = c(1, 0)
  Y0 = c(1, 0)
  Sconst = 1
  slopeSV = 1
  slopeSU = 1
  slopeST = 0

  expect_error(genprob(V, U, Tr, Y1, Y0, Sconst, slopeSV, slopeSU, slopeST, "T"), 'Choose either "P"')

})

##########################################################################################################################################
