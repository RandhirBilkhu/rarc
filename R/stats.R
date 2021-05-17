#'  Calculate Limited Expected Value
#'
#' @param dt a datatable
#' @param limit a field with limit values
#' @param att_pt a field with attachment values
#' @param k moment of lognormal distribution
#' @param mu mean of log transform
#' @param sigma  sd of log transform
#' @return adds a column to the datatable with LEV
#' @export

calc_lev<- function(dt, limit, att_pt , k=1 , mu= 15.2830643838331 , sigma = 1.74824394618441   ){

  limit=deparse(substitute(limit))
  att_pt =deparse(substitute(att_pt))

  dt[, LEV := lev_log_norm( eval(as.name(limit)) + eval(as.name(att_pt)), k, mu  , sigma ) - lev_log_norm( eval(as.name(att_pt)) ,k, mu  , sigma )]
  data.table::data.table(dt)

}

#' Limited Expected Value of Lognormal
#'
#' @param x limit point
#' @param k moment (e.g. k=2 produces second moment)
#' @param mu mean of log transform
#' @param sigma std of log transform
#'
#' @return kth moment of lognormal limited at x
#' @export

lev_log_norm <- function(x, k , mu , sigma) {

  dplyr::case_when(

    x == 0 ~ 0,
    x > 0 ~ exp( k * mu  + 0.5 * k * k * sigma * sigma) * pnorm( (log(x) - mu - k * sigma * sigma ) / abs(sigma) ) + (x ^ k) * ( 1-  pnorm( (log(x) - mu )/ abs(sigma) )),
    x < 0 ~ exp( k * mu  + 0.5 * k * k * sigma * sigma)
  )
  #' @examples
  #' lev_log_norm(1000, 1,  15.2830643838331, 1.74824394618441 )
  #'


}


#' Standard deviation of Limited Lognormal
#'
#' @param p
#' @param mu
#' @param sigma
#'
#' @return
#' @export

lev_log_norm_sdev <- function(p , mu , sigma){

  sqrt(lev_log_norm( p , 2  , mu , sigma) - lev_log_norm(p , 1 , mu , sigma)^2 )


}

