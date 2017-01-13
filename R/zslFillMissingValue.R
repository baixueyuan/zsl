#' @export

zslFillMissingValue <- function(code, ch) {
  # 本函数用于补充缺失数据，数据来源为从WindR提取
  # 工作机制是从数据库端提取给定代码的数据框，检查确实日期
  # 通过WindR提取缺失日期的折算率值，写入数据库
  # 其中，写入的数据中“公布日期”列，统一为T-2日

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  # 提取数据库数据
  qry <- paste('SELECT * FROM zsl WHERE code="', code, '"', sep='')
  data <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)

  # 提取WindR中的折算率数据
  miss <- zslCheckMissingDate(data)
  if (is.logical(miss)) {
    if (!miss) {
      cat('There is NO missing value of code', code, '!\n', sep=' ')
    }
  } else {
    if (!WindR::w.isconnected()) WindR::w.start(showmenu=FALSE)
    ratio <- sapply(miss, function(x) {
      opt <- paste('tradeDate=', format(x, format='%Y%m%d'), sep='')
      w <- w.wss(code, 'rateofstdbnd', opt)
      if (w$ErrorCode!=0) return(0) else return(w$Data[1, 'RATEOFSTDBND'])
    })
    # 处理数据框
    exchange <- stringr::str_extract(code, '[SHZ]{2}$')
    code_digit <- as.character(stringr::str_extract(code, '^[[:digit:]]{6}'))
    annce <- as.Date(
      sapply(miss, function(x) return(exdate[which(exdate==x) - 2])),
      origin='1970-01-01')

    serial <- paste('ZS', format(annce, format='%Y%m%d'), 'ST',
                    format(miss, format='%m%d'), exchange, code_digit, sep='')
    name <- w.wss(code, 'sec_name')$Data[1, 'SEC_NAME']

    data <- data.frame(serial=serial, exchange=exchange, code=code, name=name,
                       annce=annce, date=miss, ratio=ratio,
                       stringsAsFactors=FALSE)

    return(data)
  }
}
