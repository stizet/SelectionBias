
##########################################################################################################################################

test_that("RR_tot AF bound equals 8 when all probabilities are equal to 0.5 and selection is given as a vector", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),c(0,1,0,1,0,1,0,1),"RR_tot"), 8)
})

##########################################################################################################################################

test_that("RR_tot AF bound equals 8 when all probabilities are equal to 0.5 and selection is given as a probability", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),0.5,"RR_tot"), 8)
})

##########################################################################################################################################

test_that("RD_tot AF bound equals 0.875 when all probabilities are equal to 0.5 and selection is given as a vector", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),c(0,1,0,1,0,1,0,1),"RD_tot"), 0.875)
})

##########################################################################################################################################

test_that("RD_tot AF bound equals 0.875 when all probabilities are equal to 0.5 and selection is given as a probability", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),0.5,"RD_tot"), 0.875)
})

##########################################################################################################################################

test_that("RR_s AF bound equals 3 when all probabilities are equal to 0.5 and selection is given as a vector", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),c(0,1,0,1,0,1,0,1),"RR_s"), 3)
})

##########################################################################################################################################

test_that("RR_s AF bound equals 3 when all probabilities are equal to 0.5 and selection is given as a probability", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),0.5,"RR_s"), 3)
})

##########################################################################################################################################

test_that("RD_s AF bound equals 0.5 when all probabilities are equal to 0.5 and selection is given as a vector", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),c(0,1,0,1,0,1,0,1),"RD_s"), 0.5)
})

##########################################################################################################################################

test_that("RD_s AF bound equals 0.5 when all probabilities are equal to 0.5 and selection is given as a probability", {
  expect_equal(AFbound(c(0,0,0,0,1,1,1,1),c(0,0,1,1,0,0,1,1),0.5,"RD_s"), 0.5)
})

##########################################################################################################################################
