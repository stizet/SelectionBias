##########################################################################################################################################

test_that("BF_1 equals 1 when no collider", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,0,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_tot","P")[2,2]),4), 1)
})

test_that("BF_1 equals 1.1363 when setup as below", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot","P")[2,2]),4), 1.1363)
})

##########################################################################################################################################

test_that("BF_0 equals 1 when no collider", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,0,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_tot","P")[3,2]),4), 1)
})

test_that("BF_0 equals 1.0021 when setup as below", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot","P")[3,2]),4), 1.0021)
})


##########################################################################################################################################

test_that("BF_U equals 1 when no collider", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,0,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_s","P")[2,2]),4), 1)
})


test_that("BF_U equals 1.0153 when setup as below", {
  expect_equal(round(as.numeric(SVboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_s","P")[2,2]),4), 1.0153)
})


##########################################################################################################################################

