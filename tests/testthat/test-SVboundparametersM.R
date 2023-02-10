##########################################################################################################################################

test_that("BFs are equal to 1 when no collider is present.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 0, 0)
  S = matrix(c(1, 1, 1, 0), nrow = 1, byrow = TRUE)

  # BF_1 should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.534, 0.534)[1, 2]), 4), 1)

  # BF_0 should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.534, 0.534)[2, 2]), 4), 1)

  # BF_U should equal 1.
  expect_equal(round(as.numeric(SVboundparametersM("RR_sub", V, U, Tr, Y, S, "P", 0.534, 0.534)[1, 2]), 4), 1)
})

##########################################################################################################################################

test_that("BFs take specific values with one selection variable and U and V are binary.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow = 1, byrow = TRUE)

  # BF_1 should equal 1.1363.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.979, 0.857)[1, 2]), 4), 1.1363)

  # BF_0 should equal 1.0021.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.979, 0.857)[2, 2]), 4), 1.0021)

  # BF_U should equal 1.0153.
  expect_equal(round(as.numeric(SVboundparametersM("RR_sub", V, U, Tr, Y, S, "P", 0.979, 0.857)[1, 2]), 4), 1.0153)
})

##########################################################################################################################################

test_that("BFs take specific values with two selection variables and U and V are categorical.", {
  V = matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 0.0078125, 0.0546875, 0.1640625,
               0.2734375, 0.2734375, 0.1640625, 0.0546875, 0.0078125), ncol = 2)
  U = matrix(c(0, 1, 2, 3, 4, 5, 0.16807, 0.36015, 0.30870, 0.13230, 0.02835, 0.00243), ncol = 2)
  Tr = c(-0.6, 0.3)
  Y = c(-1, 0.5, 0.1)
  S = matrix(c(0.1, 0.3, -0.2, 0.3, -0.2, 0.1, -0.3, 0.5), nrow = 2, byrow = TRUE)

  # BF_1 should equal 1.25.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.326, 0.171)[1, 2]), 2), 1.25)

  # BF_0 should equal 1.76.
  expect_equal(round(as.numeric(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.326, 0.171)[2, 2]), 2), 1.76)

  # BF_U should equal 1.29.
  expect_equal(round(as.numeric(SVboundparametersM("RR_sub", V, U, Tr, Y, S, "P", 0.326, 0.171)[1, 2]), 2), 1.29)
})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if the estimand is not correctly specified.", {

  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow = 1, byrow = TRUE)

  expect_error(SVboundparametersM("RR_t", V, U, Tr, Y, matrix(c(1, 0.1, 0, 0.9), nrow = 2,
                                                      byrow = TRUE), "P", 0.979, 0.857), 'The estimand must be')

})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if inputs are incorrect.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow = 1, byrow = TRUE)

  expect_error(SVboundparametersM("RR_tot", matrix(c(1, 0.1, 0, 0.9), nrow = 1, byrow = TRUE),
                                  U, Tr, Y, S, "P", 0.979, 0.857),
               "The number of columns in Vval")
  expect_error(SVboundparametersM("RR_tot", V, matrix(c(1, 0.1, 0, 0.9), nrow = 1, byrow = TRUE),
                                  Tr, Y, S, "P", 0.979, 0.857),
               "The number of columns in Uval")
  expect_error(SVboundparametersM("RR_tot", V, U, c(1, 1, 1), Y, S, "P", 0.979, 0.857),
               "The number of parameters in Tcoef")
  expect_error(SVboundparametersM("RR_tot", V, U, Tr, c(1, 1), S, "P", 0.979, 0.857),
               "The number of parameters in Ycoef")
  expect_error(SVboundparametersM("RR_tot", V, U, Tr, Y, matrix(c(1, 0.1, 0, 0.9), nrow = 2,
                                                      byrow = TRUE), "P", 0.979, 0.857),
               "The number of columns in Scoef")

})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if the probabilities in V and U are not correct.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow = 1, byrow = TRUE)

  expect_error(SVboundparametersM("RR_tot", matrix(c(1, 0.2, 0, 0.9), nrow = 2, byrow = TRUE),
                                  U, Tr, Y, S, "P", 0.979, 0.857),
               "The probabilities of the categories of V")
  expect_error(SVboundparametersM("RR_tot", matrix(c(1, 0.05, 0, 0.9), nrow = 2, byrow = TRUE),
                                  U, Tr, Y, S, "P", 0.979, 0.857),
               "The probabilities of the categories of V")
  expect_error(SVboundparametersM("RR_tot", V, matrix(c(1, 0.2, 0, 0.9), nrow = 2, byrow = TRUE),
                                  Tr, Y, S, "P", 0.979, 0.857),
               "The probabilities of the categories of U")
  expect_error(SVboundparametersM("RR_tot", V, matrix(c(1, 0.05, 0, 0.9), nrow = 2, byrow = TRUE),
                                  Tr, Y, S, "P", 0.979, 0.857),
               "The probabilities of the categories of U")
  expect_error(SVboundparametersM("RR_tot", matrix(c(1, -0.05, 0, 1.05), nrow = 2, byrow = TRUE),
                                  U, Tr, Y, S, "P", 0.979, 0.857),
               "At least one of the categories of V")
  expect_error(SVboundparametersM("RR_tot", V, matrix(c(1, -0.05, 0, 1.05), nrow = 2, byrow = TRUE),
                                  Tr, Y, S, "P", c(0.979, 0.857)),
               "At least one of the categories of U")
  expect_error(SVboundparametersM("RR_tot", matrix(c(1, 0, 0, 1.0), nrow = 2, byrow = TRUE),
                                  U, Tr, Y, S, "P", 0.979, 0.857),
               "At least one of the categories of V")
  expect_error(SVboundparametersM("RR_tot", V, matrix(c(1, 0, 0, 1.0), nrow = 2, byrow = TRUE),
                                  Tr, Y, S, "P", 0.979, 0.857),
               "At least one of the categories of U")

})

##########################################################################################################################################

test_that("SVboundparametersM throws an error if the observed probabilities are not correct.", {
  V = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  U = matrix(c(1, 0.1, 0, 0.9), nrow = 2, byrow = TRUE)
  Tr = c(1, 1)
  Y = c(1, 1, 1)
  S = matrix(c(1, 1, 1, 1), nrow = 1, byrow = TRUE)

  expect_error(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 1.2, 0.857),
               "The observed probabilities must")
  expect_error(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", -0.2, 0.857),
               "The observed probabilities must")
  expect_error(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.857, 1.2),
               "The observed probabilities must")
  expect_error(SVboundparametersM("RR_tot", V, U, Tr, Y, S, "P", 0.857, -0.2),
               "The observed probabilities must")
})

