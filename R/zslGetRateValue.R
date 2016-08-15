#' Get the zsl Value in Several Ways
#'
#' @param code character, the bond code in Wind sytle, which is six digits with
#'   ".SH" or ".SZ" to indicate the exchange
#' @param date Date, the effect date of the ratio to be obtained
#' @param ch the RODBC connection object
#'
#' @return
#' @name zslget
NULL

#' @rdname zslget
#' @export
zslGetSingle <- function(code, date, ch) {
  # 本函数返回指定债券代码在指定日期的折算率值
  # 日期应为交易日，如果不为交易日，则使用该日期之前最近的交易日的值
  # 如果该日期没有值，则提示错误
  # 给出的代码应为六位数字加上交易所标识“.SH”或“.SZ”

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  pattern <- '[0-9]{6}\\.S[HZ]'
  if (!stringr::str_detect(code, pattern)) {
    stop('Please give the "code" as Wind style.')
  }

  date <- as.Date(date)
  date <- exdate[which.min(exdate <= date) - 1]

  qry <- paste('SELECT ratio FROM zsl WHERE code="', code, '" AND date="',
               date, '"', sep='')
  value <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)

  if (nrow(value) > 0) {
    return(value[1, 1])
  } else {
    message('There is no value retrieved, please check the "code" and "date."')
  }
}

zslGetSeries <- function(code, start, end, ch) {
  # 本函数提取的是债券的折算率序列
  # 参数start和end为序列的起止日期，如果省略，则提取所有数据

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  # 对于开始日期start和截止日期end是否给出的四种情况分别设定查询
  ## 不给定起止日期则返回全部数据
  if (missing(start) && missing(end)) {
    qry <- paste('SELECT date,ratio FROM zsl WHERE code="', code, '"', sep='')
  }
  ## 只给定起始日则返回其后的所有数据
  if (!missing(start) && missing(end)) {
    qry <- paste('SELECT date,ratio FROM zsl WHERE code="', code, '" ',
                 'AND date>="', start, '"', sep='')
  }
  ## 只给定截止日则返回其之前的所有数据
  if (missing(start) && !missing(end)) {
    qry <- paste('SELECT date,ratio FROM zsl WHERE code="', code, '" ',
                 'AND date<="', end, '"', sep='')
  }
  ## 给定起止日则返回区间数据
  if (!missing(start) && !missing(end)) {
    start <- as.Date(start)
    end <- as.Date(end)
    if (start >= end) stop('Date "start" should be earlier than "end."')
    qry <- paste('SELECT date,ratio FROM zsl WHERE code="', code, '" ',
                 'AND date BETWEEN "', start, '" AND "', end, '"', sep='')
  }

  value <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)

  if (nrow(value) > 0) {
    return(xts::xts(value[,-1], value[,1]))
  } else {
    message('There is no value retrieved, please check the "code" and "date."')
  }
}
