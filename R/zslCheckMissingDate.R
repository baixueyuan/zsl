#' Checking The Missing Date
#'
#' Checking the missing date of given data.frame, usually from database to check
#' the completeness of the zsl ratio. The data.frame given \bold{MUST} have a
#' "date" column.
#'
#' The param \code{code} can be given or missed, it depends on whether the given
#' data is filtered, so it is just a filtering element. Then "start" and "end"
#' are like it, if given the exchange date series is influenced.
#'
#' The mechanism is that, the function find the "range" of the "date" column,
#' and use this range to get the exchange trading date series. After comparing
#' two "Date" series, the difference is returned.
#'
#' @param data the data.frame is going to be checked, must have a "date" column
#' @param code the code, only length 1 character vector
#' @param start the "start" date of the exchange date series
#' @param end the "end" date of the exchange date series
#'
#' @return If there is no missing date, return FALSE, otherwise, return the
#'   missing date(s) series.
#' @export

zslCheckMissingDate <- function(data, code, start, end) {
  # 本函数检查给定数据框的日期列是否完整
  # 完整的标准是对比时间区间内的交易所交易日，是否有缺失
  # 参数data为要检查的数据框，必须含有“date”列
  # 参数code如果不指定则检查全部数据，如果指定，则检查code对应的日期
  # 参数start和end如果给定，则可以以其为开始和结束，否则待检查的区间与data一致

  if (!missing(code)) data <- dplyr::filter(data, code=code)
  d_seq <- data$date
  rng <- range(d_seq)
  if (!missing(start)) rng[1] <- as.Date(start)
  if (!missing(end)) rng[2] <- as.Date(end)
  ex_seq <- exdate[exdate >= rng[1] & exdate <= rng[2]]
  ind <- !(ex_seq %in% d_seq)
  if (!any(ind)) {
    return(FALSE)
  } else {
    return(ex_seq[ind])
  }
}
