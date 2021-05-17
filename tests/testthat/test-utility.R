test_that("check data types" ,{
  expect_s3_class(convert_data_type(rarc_test, num_cols = c("Premium" , "Limit", "Attach" , "LEV"), date_cols = c("Policy Inception Date") ,
                                    factor_cols = c("Coverage", "QY", "Year"))$Coverage, "factor")

  expect_false(is.na(convert_data_type( data.table::data.table( A= c(NA,1,2,3) ,
                                                               B = c("1/1/19" , "31/12/19" , "1/1/12" , "1/1/12") ,
                                                               C=c ("X" , "Y" , "Z" , "A")) ,
                                       num_cols = "A", date_cols = "B" , factor_cols = "C")[1,1]))


          } )

test_that("conform dates", {

  expect_true (lubridate::is.Date (conform_date(30050) ))
  expect_equal (conform_date("01/05/1983") , as.Date("1983-01-05"))


})
