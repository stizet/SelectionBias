
##########################################################################################################################################

### CHANGE WHEN DECIDED HOW THE FUNCTION WORKS!!! ###

test_that("RR_tot bound equals 1 when no collider", {
  # Let pY1coef = pY0coef = c(1,0)
  expect_equal(round(as.numeric(SVboundM(0.1,0.1,c(1,1),c(1,0,0),matrix(c(1,1,1,0),nrow=1,byrow=TRUE),"RR_tot",TRUE)[1,2]),4), 1)
})


test_that("RR_tot bound equals 1.1387 when setup as below", {
  expect_equal(round(as.numeric(SVboundM(0.1,0.1,c(1,1),c(1,1,1),matrix(c(1,1,1,1),nrow=1,byrow=TRUE),"RR_tot",TRUE)[1,2]),4), 1.1387)
})

##########################################################################################################################################
