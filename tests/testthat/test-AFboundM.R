
##########################################################################################################################################

test_that("RR_tot AF bound equals 8.2138 when setup as below", {
  expect_equal(round(as.numeric(AFboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot","P")[1,2]),4), 8.2138)
})

##########################################################################################################################################

test_that("RD_tot AF bound equals 0.7709 when setup as below", {
  expect_equal(round(as.numeric(AFboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RD_tot","P")[1,2]),4), 0.7709)
})

##########################################################################################################################################

test_that("RR_s AF bound equals 7.7723 when setup as below", {
  expect_equal(round(as.numeric(AFboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_s","P")[1,2]),4), 7.7723)
})


##########################################################################################################################################

test_that("RD_s bound equals 0.7490 when setup as below", {
  expect_equal(round(as.numeric(AFboundM(matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),matrix(c(1,0.1,0,0.9),nrow=2,byrow=TRUE),c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RD_s","P")[1,2]),4), 0.7490)
})

##########################################################################################################################################
