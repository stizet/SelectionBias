
##########################################################################################################################################

test_that("No selection bias when no collider is present.", {
  V = c(0.1, 0.9)
  U = c(0.1, 0.9)
  Tr = matrix(c(0.97724987, 0.8413447, 0.02275013, 0.1586553), nrow = 2, byrow = TRUE)
  Y1 = matrix(c(0.8413447, 0.8413447, 0.1586553, 0.1586553), ncol = 2, byrow = TRUE)
  Y0 = matrix(c(0.8413447, 0.8413447, 0.1586553, 0.1586553), nco = 2, byrow = TRUE)
  S = matrix(c(0.998650102, 0.977249868, 0.977249868, 0.841344746, 0.998650102,
               0.977249868, 0.977249868, 0.841344746, 0.001349898, 0.022750132,
               0.022750132, 0.158655254, 0.001349898, 0.022750132, 0.022750132,
               0.158655254), ncol=1, byrow=TRUE)

  # The bias should be 1.
  expect_equal(calcselbias(Y1 = Y1, Y0 = Y0, Tr = Tr, S = S, U = U, V = V, "RR_tot")[1], 1)
  # The bias should be 0.
  expect_equal(calcselbias(Y1 = Y1, Y0 = Y0, Tr = Tr, S = S, U = U, V = V, "RD_tot")[1], 0)
  # The bias should be 1.
  expect_equal(calcselbias(Y1 = Y1, Y0 = Y0, Tr = Tr, S = S, U = U, V = V, "RR_s")[1], 1)
  # The bias should be 0.
  expect_equal(calcselbias(Y1 = Y1, Y0 = Y0, Tr = Tr, S = S, U = U, V = V, "RD_s")[1], 0)
})

##########################################################################################################################################