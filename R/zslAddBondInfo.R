#' Adding Bond Information to Given data.frame
#'
#' The function adds the basic bond information to the given data.frame, using
#' data's "code" column. So, a "code" column of Wind style bond code vector is
#' necessary in the given data.
#'
#' The bond information added includes bond's maturity, bond rating, issuer
#' rating, rating change, bond type, industry, municipal or not. The function
#' will process the data retrieved from \code{WindR} Package, especially, most
#' information has no value for the risk-free bonds, e.g. treasury bond, local
#' government bond, and so on.
#'
#' @param data the given \code{data.frame}, including "code" column
#'
#' @return A \code{data.frame} which is \code{cbind}ed with original "data" and
#'   bond information columns.
#' @export

zslAddBondInfo <- function(data) {
  # 本函数用于对给定的数据框添加债券要素列，主要包括债券类型，评级，行业，城投等
  # 提取数据使用WindR，因此要求给定数据必须由code列

  # 提取WindR数据
  code <- data$code
  fields <- c('ptmyear', 'amount', 'latestissurercreditrating',
              'rate_lateissuerchng', 'windl1type', 'industry_sw',
              'municipalbond')
  opts <- 'industryType=1'
  tdate <- 'tradeDate='

  if (!WindR::w.isconnected()) WindR::w.start(showmenu=FALSE)
  dat <- w.wss(code, fields, opts, tdate)

  if (dat$ErrorCode!=0) {
    stop(paste('There is error occuring, the code is ', dat$ErrorCode, '.',
               sep=''))
  } else {
    dat <- dat$Data
  }

  colnames(dat) <- c('code', 'maturity', 'rating', 'issuer', 'rating_chg',
                     'type', 'industry', 'muni')

  # 整理数据
  dat$maturity <- round(dat$maturity, 2)

  dat$type[stringr::str_detect(data$name, 'ETF')] <- 'ETF'
  dat[dat$rating=='NaN', 'rating'] <- ''
  dat[dat$issuer=='NaN', 'issuer'] <- ''
  dat[dat$rating_chg=='NaN', 'rating_chg'] <- ''
  dat[dat$industry=='NaN', 'industry'] <- ''

  # 将“地方政府债”替换为“地方债”，将“可交换债”替换为“可交债”
  dat[dat$type=='\u5730\u65b9\u653f\u5e9c\u503a', 'type'] <- '\u5730\u65b9\u503a'
  dat[dat$type=='\u53ef\u4ea4\u6362\u503a', 'type'] <- '\u53ef\u4ea4\u503a'

  # 检查两组数据框的code列一致时，合并返回结果，否则显示错误信息
  if (identical(data$code, dat$code)) {
    return(cbind(data, dat[, -1]))
  } else {
    stop('The "code" column is different, cannot "cbind" data together!')
  }
}
