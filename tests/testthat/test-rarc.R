library(data.table)

test_that("check output of tidy rarc function", {

 expect_equal( mean(tidy_rarc(add_miss_qtr(rarc_test), premium_type = "Premium" , col_key=c( "Cedant" , "Name" , "Coverage"))$RARC) , 1.651548 , tolerance = 0.1)

}

)


