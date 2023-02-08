
##########################################################################################################################################

test_that("AFbound works for a small dataset for all estimands.", {

  # The small dataset.
  y = c(0, 0, 0, 0, 1, 1, 1, 1)
  tr = c(0, 0, 1, 1, 0, 0, 1, 1)
  sel = c(0, 1, 0, 1, 0, 1, 0, 1)
  selprob = 0.5

  # Should equal 8 for "RR_tot".
  expect_equal(as.numeric(AFbound("RR_tot", y, tr, sel)[1,2]), 8)
  expect_equal(as.numeric(AFbound("RR_tot", y, tr, selprob)[1,2]), 8)

  # Should equal 0.875 for "RD_tot".
  expect_equal(as.numeric(AFbound("RD_tot", y, tr, sel)[1,2]), 0.88)
  expect_equal(as.numeric(AFbound("RD_tot", y, tr, selprob)[1,2]), 0.88)

  # Should equal 3 for "RR_sub".
  expect_equal(as.numeric(AFbound("RR_sub", y, tr, sel)[1,2]), 3)
  expect_equal(as.numeric(AFbound("RR_sub", y, tr, selprob)[1,2]), 3)

  # Should equal 0.5 for "RD_sub".
  expect_equal(as.numeric(AFbound("RD_sub", y, tr, sel)[1,2]), 0.5)
  expect_equal(as.numeric(AFbound("RD_sub", y, tr, selprob)[1,2]), 0.5)

})

##########################################################################################################################################

test_that("AFbound throws an error if the selection probability is not between 0 and 1.", {

  # The small dataset.
  y = c(0, 0, 0, 0, 1, 1, 1, 1)
  tr = c(0, 0, 1, 1, 0, 0, 1, 1)

  expect_error(AFbound("RR_tot", y, tr, 2), 'not between 0 and 1.')
  expect_error(AFbound("RR_tot", y, tr, -2), 'not between 0 and 1.')

})

##########################################################################################################################################

test_that("AFbound throws an error if the estimand is not correctly specified.", {

  # The small dataset.
  y = c(0, 0, 0, 0, 1, 1, 1, 1)
  tr = c(0, 0, 1, 1, 0, 0, 1, 1)
  selprob = 0.5

  expect_error(AFbound("RR_t", y, tr, selprob), 'The estimand must be')

})

##########################################################################################################################################

