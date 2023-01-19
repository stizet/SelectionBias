##########################################################################################################################################

test_that("BFs are equal to 1 when no collider is present.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y = c(1, 0, 0)
  S = matrix(c(1, 1, 1, 0), nrow=1, byrow=TRUE)

  # BF_1 should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_tot", "P")[1, 2]), 4), 1)

  # BF_0 should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_tot", "P")[2, 2]), 4), 1)

  # BF_U should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_s", "P")[1, 2]), 4), 1)
})

##########################################################################################################################################

test_that("BFs take specific values when setup as below.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow=1, byrow=TRUE)

  # BF_1 should equal 1.1363.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_tot", "P")[1, 2]), 4), 1.1363)

  # BF_0 should equal 1.0021.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_tot", "P")[2, 2]), 4), 1.0021)

  # BF_U should equal 1.0153.
  expect_equal(round(as.numeric(SVboundparametersM(V, U, Tr, Y, S, "RR_s", "P")[1, 2]), 4), 1.0153)
})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if the estimand is not correctly specified.", {

  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow=1, byrow=TRUE)

  expect_error(SVboundparametersM(V, U, Tr, Y, matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE), "RR_t", "P"), 'The estimand must be')

})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if inputs are incorrect.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow=1, byrow=TRUE)

  expect_error(SVboundparametersM(matrix(c(1, 0.1, 0, 0.9), nrow=1, byrow=TRUE), U, Tr, Y, S, "RR_tot", "P"), "The number of columns in Vval")
  expect_error(SVboundparametersM(V, matrix(c(1, 0.1, 0, 0.9), nrow=1, byrow=TRUE), Tr, Y, S, "RR_tot", "P"), "The number of columns in Uval")
  expect_error(SVboundparametersM(V, U, c(1, 1, 1), Y, S, "RR_tot", "P"), "The number of parameters in Tcoef")
  expect_error(SVboundparametersM(V, U, Tr, c(1, 1), S, "RR_tot", "P"), "The number of parameters in Ycoef")
  expect_error(SVboundparametersM(V, U, Tr, Y, matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE), "RR_tot", "P"), "The number of columns in Scoef")

})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if the probabilities in V and U are not correct.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow=2, byrow=TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow=1, byrow=TRUE)

  expect_error(SVboundparametersM(matrix(c(1, 0.2, 0, 0.9), nrow=2, byrow=TRUE), U, Tr, Y, S, "RR_tot", "P"), "The probabilities of the categories of V")
  expect_error(SVboundparametersM(matrix(c(1, 0.05, 0, 0.9), nrow=2, byrow=TRUE), U, Tr, Y, S, "RR_tot", "P"), "The probabilities of the categories of V")
  expect_error(SVboundparametersM(V, matrix(c(1, 0.2, 0, 0.9), nrow=2, byrow=TRUE), Tr, Y, S, "RR_tot", "P"), "The probabilities of the categories of U")
  expect_error(SVboundparametersM(V, matrix(c(1, 0.05, 0, 0.9), nrow=2, byrow=TRUE), Tr, Y, S, "RR_tot", "P"), "The probabilities of the categories of U")
  expect_error(SVboundparametersM(matrix(c(1, -0.05, 0, 1.05), nrow=2, byrow=TRUE), U, Tr, Y, S, "RR_tot", "P"), "At least one of the categories of V")
  expect_error(SVboundparametersM(V, matrix(c(1, -0.05, 0, 1.05), nrow=2, byrow=TRUE), Tr, Y, S, "RR_tot", "P"), "At least one of the categories of U")

})

##########################################################################################################################################



