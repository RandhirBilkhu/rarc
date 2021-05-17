.datatable.aware = TRUE


#' Create a tidy RARC dataset
#'
#' @param description this function calculates RARC using a calculated LEV to adjust for layer changes from bordereau data. It returns a long formatted data set
#' segmented by the features described in col_key and by period in the format YYYYQX.
#' The outputs are RARC, LEVChange (relies on pre-calculation of LEV using appropriate exposure curves)
#'
#'
#' @param dt a data.table object
#' @param premium_type a character string of a premium term in the data
#' @param col_key a vector key categorical features e.g. name, cedant, coverage, industry
#'
#' @return a tidy data set with RARC, LEV Change, Premium Change and RARC Weights according to the OG methodology
#' @export

tidy_rarc <- function (dt,  premium_type , col_key ) {

  split_cols <- list( col_key)
  cols_param <- unlist(lapply( split_cols, paste , collapse=" + "))
  # cols_param_wide <- paste (cols_param ,  "~",  year_type )
  # cols_param_long <- paste (cols_param ,  "+ Policy_Year ~ component" )
  cols_param_wide_qtr <- paste ( cols_param ,  "~ QY" )
  cols_param_long_qtr  <- paste ( cols_param ,  "+ Policy_Period ~ component")


  Prem_wide <- data.table::dcast(dt,  cols_param_wide_qtr ,
                                 fun.agg = function(x) sum(x) ,
                                 value.var = c(premium_type))

  data.table::setkeyv(Prem_wide, col_key)

  LEV_wide <- data.table::dcast(dt, cols_param_wide_qtr ,
                                fun.agg = function(x) sum(x) ,
                                value.var = c("LEV"))

  data.table::setkeyv(LEV_wide, col_key)

  n <- lengths(split_cols)+1

  Prem_wide[, paste0("Prem_Chg " , colnames(Prem_wide)[ (n+4): dim(Prem_wide)[2]  ]) := lapply(n: (dim(Prem_wide)[2]-4), function(x) Prem_wide[[x+4]]/Prem_wide[[x]])]
  LEV_wide[, paste0("LEV_Chg " , colnames(LEV_wide)[ (n+4): dim(LEV_wide)[2]  ]) := lapply(n: (dim(LEV_wide)[2]-4), function(x) LEV_wide[[x+4]]/LEV_wide[[x]])]


  ## Calculate RARC

  Wide_RARC <- Prem_wide[,.SD , .SDcols = names(Prem_wide)[names(Prem_wide) %like% "Prem_Chg"] ]  / LEV_wide[,.SD , .SDcols = names(LEV_wide)[names(LEV_wide) %like% "LEV"] ]

  usethis::ui_done("calculated rarc by period")

  ## define maximum and minimum for unclean  max =999, min =-999

  # Wide_RARC <- replace(Wide_RARC, Wide_RARC > max_cap , 999 )
  # Wide_RARC <- replace(Wide_RARC, Wide_RARC > 0 & Wide_RARC < min_cap , -999  )
  #


  ## edit the col names slightly

  setnames(Wide_RARC , gsub("Prem" , "RARC" ,names(Wide_RARC), perl = TRUE ))

  ## create the combined table

  Wide_Combined <- cbind(Prem_wide, LEV_wide ,Wide_RARC )
  Wide_Combined <- Wide_Combined[ , .SD , .SDcols = unique(names(Wide_Combined))]

  setkeyv(Wide_Combined, col_key)
  ## reshape to long

  Long_Combined <- melt(Wide_Combined, measure = patterns("[[:digit:]]") , value.name = "amount" , variable.name = "component")

  setkeyv(Long_Combined, col_key)

  Long_Combined[ , amount := as.numeric(amount)]
  Long_Combined$amount[!is.finite(Long_Combined$amount)] <- 0

  #Long_Combined[ ,Policy_Year := as.numeric(gsub("\\D", "", component, perl =TRUE  )) ]

  Long_Combined[, Policy_Year :=  stringr::str_extract(component, "[0-9]{1,4}" )]
  Long_Combined[, Policy_qtr := stringr::str_sub(component,  -2,-1)      ]
  Long_Combined[ ,component := gsub("^(?!.[[:alpha:]])" ,"Premium" , component , perl =TRUE   ) ]
  Long_Combined[ , component := RARC_Component(component)]
  Long_Combined[ ,Policy_Period := paste0(Policy_Year, Policy_qtr)]
  #Long_Combined[ ,Policy_Year := NULL]
  Long_Combined[ ,Policy_qtr := NULL]



  ## reshape components like RARC PREMIUM Change to wide

  Final_Combined <- dcast(Long_Combined, cols_param_long_qtr  ,
                          fun.agg = function(x) sum(x) ,
                          value.var = "amount")

  Final_Combined <- Final_Combined[ Prem_Chg>0 | Premium>0  ]

  usethis::ui_done("reshaped output data into tidy format")

  # Final_Combined[RARC==999, Status := "High" ]
  # Final_Combined[RARC==-999, Status := "Low" ]
  # Final_Combined[RARC!=-999 & RARC!=999 , Status := "Clean"]
  #
  Final_Combined[, RARC_WT := Premium * RARC]
  Final_Combined[, Restated_Expiry := as.numeric(gsub("Inf|NaN", 0,  Premium / RARC))]
  Final_Combined[, Restated_Expiry_Wt := Restated_Expiry * (RARC -1)]

  usethis::ui_done("calculated weights for RARC")

  setkeyv(Final_Combined, col_key)

  return(data.table(Final_Combined))


}


#' Add missing quarter
#'
#' @param x a data.table with a column QY with data in format YYYYQX
#'
#' @return a vector pf data format YYYYQX with missing quarters filled in
#' @export

add_miss_qtr <- function (x) {

  yr <-c(2000:2050)
  qtr <-c("Q1" , "Q2" , "Q3" , "Q4")

  foo <-expand.grid(yr, qtr, stringsAsFactors = FALSE)
  all_qtr <-paste0(foo$Var1, foo$Var2)

  min_max_qtr <- sort(all_qtr[ all_qtr>=min( x$QY ) & all_qtr <= max(x$QY)])

  not_in_qtr <- data.frame(min_max_qtr[!(min_max_qtr %in% unique(x$QY))] , stringsAsFactors = FALSE)
  names(not_in_qtr) <- "QY"

  x$QY <-as.character(x$QY)

  x <-rbind(x, not_in_qtr, fill=TRUE)

  x

}

#' @examples
foo<-data.table::as.data.table(c("2020Q1" , "2020Q2" ,"2020Q4"))
names(foo) <-"QY"

add_miss_qtr(foo)
## QY
## 1: 2020Q1
## 2: 2020Q2
## 3: 2020Q4
## 4: 2020Q3


#' Clean up rate change components
#'
#' @param x a character vector of names
#'
#' @return a character vector
#' @export

RARC_Component <-function(x) {
  data.table::fcase(
    x %like% "Premium" , "Premium",
    x %like% "Prem_Chg" , "Prem_Chg" ,
    x %like% "LEV_Chg" , "LEV_Chg",
    x %like% "RARC_Chg" , "RARC"
  )
}

#' Create a feature for Policy period in quarters
#'
#' @param dt a data.table
#' @param year_col a field specif
#' @param incept_date a date field
#' @param add_miss optional. TRUE evaluates missing quarters see add_miss_qtr()
#'
#' @return a datatable with an additional field called QY with period in YYYYQX format
#' @export

add_qy <- function(dt, year_col, incept_date , add_miss)  {
  year_col=deparse(substitute(year_col))
  incept_date =deparse(substitute(incept_date))

  dt[ , QY := paste0(eval(as.name(year_col)) , quarter_year(data.table::month(eval(as.name(incept_date)))))    ]

  data.table::data.table( if (add_miss == TRUE) {add_miss_qtr(dt)} else {dt})

}

