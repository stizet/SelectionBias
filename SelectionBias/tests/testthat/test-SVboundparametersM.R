##########################################################################################################################################

test_that("BF_1 equals 1 when no collider", {
  # Let pY1coef = pY0coef = c(1,0)
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(1,0),c(1,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_tot",TRUE)[1,2]),4), 1)
})

test_that("BF_1 equals 1.1363 when setup as below", {
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(2,1),c(1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot",TRUE)[1,2]),4), 1.1363)
})

##########################################################################################################################################

test_that("BF_0 equals 1 when no collider", {
  # Let pY1coef = pY0coef = c(1,0)
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(1,0),c(1,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_tot",TRUE)[4,2]),4), 1)
})

test_that("BF_0 equals 1.0021 when setup as below", {
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(2,1),c(1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot",TRUE)[4,2]),4), 1.0021)
})


##########################################################################################################################################

test_that("BF_U equals 1 when no collider", {
  # Let pY1coef = pY0coef = c(1,0)
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(1,0),c(1,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_s",TRUE)[1,2]),4), 1)
})


test_that("BF_U equals 1.0153 when setup as below", {
  expect_equal(round(as.numeric(SVboundparametersM(0.1,0.1,c(1,1),c(2,1),c(1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_s",TRUE)[1,2]),4), 1.0153)
})


##########################################################################################################################################
