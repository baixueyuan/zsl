#' Get the zsl Value in Several Ways
#'
#' Get the zsl value from the database in several ways, including get the sigle
#' code's single day's value, get single code's series, and so on.
#'
#' The functions use the code to identify the bond, not the name. And the code
#' must be in Wind style, or with regex \code{'[0-9]{6}\\.S[HZ]'}.
#'
#' For \code{zslGetSingle}, it uses params \code{code} and \code{date} to get
#' single value of the "code" and "date". The date should be exchange's trading
#' date, of course the function can also automatically find the closest trading
#' day before it if it is not. If there is no value returned, message will be
#' given, "code" and "date" should be re-checked carefully. Obviously, the
#' result is a single numeric value.
#'
#' For \code{zslGetSeries}, it gets the series values. Surely, the params
#' including "start" and "end", but it can be given or missed. If both are
#' given, the values of specific period are returned; if only "start" or "end"
#' is given and the other is missed, the values after "start" or before "end"
#' are returned; if both are missed, all values of the bond are returned. For
#' the convenience, the result is an \code{xts} object as default.
#'
#' If the exact period series is required, which means that the series dates
#' must contain all exchange trading days between "start" and "end", set param
#' \code{zero.fill} to \code{TRUE}. If there exists NA value, it will be
#' replaced by "0". This is useful when being used in \code{for} loop or
#' \code{*ply} functions, as the retured series all have the same length.
#'
#' For \code{zslGetBundle}, it returns the "bundled" series
#' of more than one bond. The params \code{start} and \code{end} must be given,
#' and the returned the times series is an \code{xts} object of this period.
#'
#' @param code character, the bond code in Wind sytle, which is six digits with
#'   ".SH" or ".SZ" to indicate the exchange
#' @param date Date, the effect date of the ratio to be obtained
#' @param ch the RODBC connection object
#' @param start the start of the period, Date object of like "YYYY-MM-DD"
#' @param end the end of the period, Date object of like "YYYY-MM-DD"
#' @param xts logical, default is TRUE, return the final series as \code{xts}
#'   object
#' @param zero.fill logical, default is FALSE, if TRUE the returned series will
#'   be filled by zero to replace those NA value while the length of the series
#'   is equal to the period
#'
#' @return The returned value(s) are different according to the use of
#'   functions, see the "details."
#' @name GetValue
NULL

#' @rdname GetValue
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

#' @rdname GetValue
#' @export
zslGetSeries <- function(code, start, end, ch, xts=TRUE, zero.fill=FALSE) {
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

  if (exists('qry'))
    value <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)

  ## 给定起止日则返回区间数据
  if (!missing(start) && !missing(end)) {
    start <- as.Date(start)
    end <- as.Date(end)
    if (start >= end) stop('Date "start" should be earlier than "end."')
    dates <- exdate[exdate >= start & exdate <= end]
    qry <- paste('SELECT date,ratio FROM zsl WHERE code="', code, '" ',
                 'AND date BETWEEN "', start, '" AND "', end, '"', sep='')
    value <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)
    ### 使用参数zero.fill，如果为TRUE则构造一个包括所有start和end之间的时间序列
    ### 其中没有数值的NA用0填补，用于批量提取数据时序列的长度是一致的
    if (zero.fill) {
      value <- merge(value, data.frame(date=dates), by='date', all=TRUE)
      value[is.na(value$ratio), 'ratio'] <- 0
    }
  }

  if (nrow(value) > 0) {
    # 设置xts逻辑值，为TRUE则返回xts对象，否则直接返回数据框
    if (xts) value <- xts::xts(as.numeric(value[,-1]), value[,1])
    return(value)
  } else {
    message('There is no value retrieved, please check the "code" and "date."')
  }
}

#' @rdname GetValue
#' @export
zslGetBundle <- function(codes, start, end, ch) {
  # 本函数批量提取折算率数据

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  # 代码应为长度大于1的字符串
  # if (!is.character(codes) && !length(codes) > 1) {
  #   stop('The param "code" should be "character" and the has more than one element.')
  # }

  # 用start和end构造一个时间序列
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- exdate[exdate >= start & exdate <= end]

  # 提取数据，返回折算率矩阵
  # 使用zero.fill参数将所有序列找齐，直接返回矩阵
  # 暂停使用sapply方式
  # ratios <- sapply(codes, zslGetSeries, start=start, end=end, ch=ch,
  #                  zero.fill=TRUE)

  # 使用构建query的方式提取数据，相比使用sapply，效率有所提高
  code_sql <- paste(codes, collapse='","')
  code_sql <- paste('("', code_sql, '")', sep='')
  qry <- paste('SELECT date,code,ratio FROM zsl WHERE date BETWEEN "',
               start, '" AND "', end, '" AND code IN ', code_sql, sep='')
  data <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)
  # 将code列作为factor类，并将levels设为所有代码，可以在后面使用spread函数时
  # 直接得到所有债券对应的列，包括全为0的列
  data$code <- factor(data$code, levels=codes)
  ratios <- tidyr::spread(data, code, ratio, fill=0, drop=FALSE)

  # 用矩阵和时间序列构建xts对象
  res <- xts::xts(ratios[, -1], dates)
  colnames(res) <- 'sbond'

  return(res)
}
