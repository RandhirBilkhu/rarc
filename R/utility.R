#' Convert NA to Not Categorized for Risk Factor Levels
#'
#' @param x a character vector of Risk Factor Levels
#'
#' @return
#' @export

clean_RLCC <- function(x){
  data.table::fcase(
    x = "NA" ~ "Not Categorised",
    x = "" ~ "Not Categorised",
    x = NA ~ "Not Categorised"
  )
}

#' Quarter Year
#'
#' @param x month in digits (1-12)
#'
#' @return a character either Q1, Q2, Q3, or Q4
#' @export

quarter_year <- function(x){
  data.table::fcase(
    x<=3 , "Q1",
    x>3 & x<=6 ,"Q2",
    x>6 & x <=9 , "Q3",
    x>9 & x<=12 , "Q4"
  )
}


#' cluster names by similarity
#'
#' @param edit_threshold Numeric value, indicating the threshold at which a merge is performed, based on the sum of the edit values derived from param weight. Default value is 1. If this parameter is set to 0 or NA, then no approximate string matching will be done, and all merging will be based on strings that have identical ngram fingerprints.
#' @param numgram Numeric value, indicating the number of characters that will occupy each ngram token. Default value is 2.
#' @param ignore_strings Character vector, these strings will be ignored during the merging of values within vect. Default value is NULL.
#' @param dict Character vector, meant to act as a dictionary during the merging process. If any items within vect have a match in dict, then those items will always be edited to be identical to their match in dict. Default value is NULL.
#' @param x Character vector, items to be potentially clustered and merged.
#'
#' @return  standardised names
#' @examples
#' refine_string(c("Acme Pizza, Inc.", "AcMe PiZzA, Inc.", "ACME PIZZA COMPANY", "acme pizza LLC"))
#' @export

refine_string <- function (x, edit_threshold=1 , numgram=2, ignore_strings=NULL, dict =NULL) {
  x %>%
    refinr::key_collision_merge(ignore_strings=ignore_strings , dict = dict) %>%
    refinr::n_gram_merge(edit_threshold = edit_threshold , numgram = numgram, ignore_strings = ignore_strings)
}





#' Conform Date Fields
#'
#' @param x date vector (numeric or date type)
#'
#' @return a POSICXct date format vector
#' @examples
#'conform_date(30050)
#'conform_date("01/05/1983")
#'conform_date("05/28/1983")
#'
#' @export

conform_date <- function(x){

  x <- gsub(pattern =  "00:00:00" , replacement = "" , x)

  x <- gsub(pattern =  "12:0 AM"  , replacement = "", x )

  x <- gsub(pattern =  "1:0 AM"  , replacement = "", x )


  y1 <- suppressWarnings( openxlsx::convertToDate(x , origin="1900-01-01") )

  y2 <- suppressWarnings(as.Date(lubridate::parse_date_time(x ,orders=c("mdy" , "dmy" , "dmY" , "mdY" , "Ydm" , "Ymd" , "myd")) , "UTC"))

  y3 <- dplyr::if_else( is.na(y1) , y2 , y1 , missing=NULL)

  return(y3)

}




#' Half year Label
#'
#' @param x month in digits (1-12)
#'
#' @return a character either H1 or H2
#' @export

half_year <-function(x) {
  data.table::fcase(
    x<=6 , "H1",
    x>6 & x<=12 ,"H2"
  )
}


#' Quarter Year
#'
#' @param x month in digits (1-12)
#'
#' @return a character either Q1, Q2, Q3, or Q4
#' @export

quarter_year <- function(x){
  data.table::fcase(
    x<=3 , "Q1",
    x>3 & x<=6 ,"Q2",
    x>6 & x <=9 , "Q3",
    x>9 & x<=12 , "Q4"
  )
}


#'  Transform Wide View data into correct data types for R
#'
#' @param dt a data.table
#' @param num_cols character vector of list of identified numeric fields
#' @param date_cols character vector of list of identified date fields
#' @param factor_cols character vector of list of identified factor fields
#'
#' @return
#' @export

convert_data_type <- function(dt, num_cols, date_cols, factor_cols){

  is.na(dt) <- dt == "NULL"

  conform_date_names <- paste("conform " , date_cols)

  ## convert cols to numeric
  dt[ , (num_cols) := lapply(.SD , as.numeric), .SDcols = num_cols]

  ## convert cols to factor
  dt[ , (factor_cols) := lapply(.SD , as.factor), .SDcols = factor_cols]

  usethis::ui_done("numeric and factor fields converted")
  ## convert nas to 0
  dt[ , (num_cols) := lapply(.SD ,FUN= function(x) {ifelse(is.na(x) , 0 , x )} ), .SDcols = num_cols]

  ## Clean dt fields
  dt[ , (date_cols) := lapply(.SD ,gsub , pattern="0:00" , replacement = "" ), .SDcols = date_cols]
  dt[ , (conform_date_names) := lapply(.SD ,conform_date ), .SDcols = date_cols]

  usethis::ui_done("date fields conformed")

  data.table::data.table(dt)
}

#' Backfill missing categorical levels for Insured
#'
#' @param dt a data.table with Insured Name, and at least one risk factor
#' @param ins_name  insured name field
#' @param field a categorical risk factor
#'
#' @return a datatable populated with unique insured names and the most frequently populated level from the data
#' @export

enrich_levels <- function( dt, ins_name, field){
  prep_dt <- dt[, .("std_Insured_Name"= get(ins_name), "RiskFactor"= get(field)) ][order(std_Insured_Name, RiskFactor)]
  data.table::setkey(prep_dt, std_Insured_Name, RiskFactor)

  prep_dt[ , count_level := .N , by = .(std_Insured_Name, RiskFactor)]
  prep_dt <- suppressWarnings (unique(prep_dt[ , max_level := max(count_level[RiskFactor != "Not Categorised"]), by=.(std_Insured_Name) ]))
  prep_dt[ , most_frequent_level := max_level==count_level]
  prep_dt[, max_level:=NULL]
  prep_dt[, count_level:=NULL]

  prep_dt <-prep_dt[RiskFactor!= "Not Categorised"]

  most_freq <- prep_dt[most_frequent_level==TRUE]
  most_freq[ , most_frequent_level:= RiskFactor]
  most_freq[ , RiskFactor := NULL ]
  prep_dt[,  most_frequent_level:=NULL]

  final_dt <-most_freq[prep_dt, on = c("std_Insured_Name") ]
  data.table::setkey(final_dt, std_Insured_Name, RiskFactor)

  final_dt[ , chosen_level := select_level(RiskFactor , most_frequent_level) ]
  final_dt[ , RiskFactor:=NULL]
  final_dt[, most_frequent_level:=NULL]
  data.table::data.table (unique(final_dt))
}


#' Helper function to enrich levels to classify categorical levels
#'
#' @param l  a string. the level of a risk factor for an insured
#' @param mfl  a string, the mostly frequently used  risk factor level for that insured
#'
#' @return a string.
#' @export

select_level <- function(l,mfl){
  data.table::fcase(
    is.na(mfl) ==TRUE, l,
    l == mfl ,l,
    l == "Not Categorised" & mfl != "Not Categorised"  , l,
    l != "Not Categorised " & l != mfl , mfl

  )}



#' preprocess names
#'
#' @param x character string
#'
#' @return character string
#' @export

name_preprocess <- function(x , clean_patt){

  x<- tolower(x)
  x<- stringr::str_replace_all(x, pattern= clean_patt, replacement ="")
  x<- tm::removePunctuation(x)
  x<- stringr::str_trim(x, side = "both")


}


#' Compare Standardised name to original
#'
#' @param org original values
#' @param ref refined values
#'
#' @return datatable with 3 columns,original, edited and TRUE/FALSE to indicate a  match
#' @export

inspect_results <- function(org, ref) {
  data.table(original_values = org , edited_values = ref) %>%
    mutate(equal = original_values == edited_values)


}


#' Standardise names
#'
#' @param dt data.table
#' @param name_col vector of names to standardise
#' @param clean_patt string of patterns to remove before applying clustering algorithm
#'
#' @return data.table with an additional column called std_Insured_Name
#' @export

standardise_names <- function(dt,  name_col , clean_patt){
  ## tidy and create standardised Insured names
  dt[ , (name_col) := lapply(.SD , FUN = function(x) {stringr::str_replace_all( x,"[^[:graph:]]", " ")}) , .SDcols = name_col ]
  dt[ , (name_col) := lapply(.SD , FUN = function(x) {stringr::str_replace_all( x,"[^[:alnum:]]", " ")}) , .SDcols = name_col ]
  dt[ , (name_col) := lapply(.SD , FUN = function(x) { name_preprocess(x , clean_patt)}) , .SDcols = name_col ]
  dt[ , std_Insured_Name := lapply(.SD, FUN = function(x) {refine_string(x)}) , .SDcols = name_col]
  usethis::ui_done("name field preprocessed and standardised")

  data.table::data.table(dt)

}

