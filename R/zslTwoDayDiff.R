#' Compare Two Days' Difference of zsl Data
#'
#' Consecutive tow days' discount rate will be compared and returned the result.
#'
#' The param \code{date} is the later day of compared two days, if missed, then
#' the function will automatically find the latest two days. First, the function
#' will query the "last day" from the database, that is the day which appears
#' most in the lastest annoncement. Then, the function uses the trading day
#' series to find the previous day, which is used to make the comparison.
#'
#' For the comparison, the records only exist in the "old" will be marked as
#' "STOP", and the records only exist in the "new" will be marked as "New".
#' The records exist in two will be computed the change. The result data.frame
#' contains only the "NEW", "STOP" and which changes are not equal to 0.
#' Of course, "NEW" and "STOP" records can also be igored in the result with
#' the param.
#'
#' There are two params can be used to filter the result. If \code{scale} is set
#' to a number other than 0, the records change bigger than the "scale" will be
#' reserved, others are deleted, but this has no effect on "NEW" and "STOP"
#' records. Pay attention, according to the convention, here we use Xbp concept
#' which is prevailing in the bond market. For example, if scale is set to 5,
#' meaning all the records change bigger than 0.05% are reserved, or you can
#' see in the result data.frame. However, you see 0.05 or bigger change in the
#' data.frame, and, of course, -0.05 or smaller, that is to say, scale is an
#' absolute scale.
#'
#' The other filtering param is \code{mark.label}. Param \code{scale} just has effect
#' on the records other than "NEW" and "STOP", so you can filter them with
#' \code{mark.label}.
#'
#' @param date the later date of compared two days, the previous day can be
#'   detected automatically
#' @param ch RODBC connection object
#' @param changed.only logical, default is FALSE, if TRUE "NEW" and "STOP"
#'   records will be igored in the result
#' @param scale the scale of the change, default is 0, that's meaning it does
#'   not work, see the details
#' @param mark.label charater, can only be '', 'NEW' or 'STOP', used to filter the
#'   mark column, see the details
#'
#' @return The compared result data.frame.
#' @export
#'
zslTwoDayDiff <- function(date, ch, changed.only=FALSE, scale=0,
                          mark.label=NULL) {

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  # 确定要比较的前后两个日期
  if (missing(date)) {
    qry <- 'SELECT date FROM lastday'
    day2 <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)[1,1]
    day1 <- exdate[which(exdate==day2) - 1]
  } else {
    day2 <- as.Date(date)
    day1 <- exdate[which(exdate==day2) - 1]
  }

  # 提取新旧比较数据
  old <- RODBC::sqlQuery(ch,
                         paste('SELECT code,name,ratio FROM zsl ',
                               'WHERE date="', day1, '"', sep=''),
                         stringsAsFactors=FALSE)
  new <- RODBC::sqlQuery(ch,
                         paste('SELECT code,name,ratio FROM zsl ',
                               'WHERE date="', day2, '"', sep=''),
                         stringsAsFactors=FALSE)
  # 比较提取的两日的数据记录条数，如果差异过大则提示，此处的标准是记录相对少的
  # 一天不低于记录相对多的80%
  len1 <- nrow(old)
  len2 <- nrow(new)
  if (min(len1, len2) < max(len1, len2) * 0.8) {
    message('The records of two days are significantly different in amount.')
  }

  # 构建新的数据框计算两日的差异
  # 以代码“code”合并新旧两日数据，用NA查找缺失的数据，旧列缺失为新增“NEW”
  # 就列缺失说明是已停止“STOP”，将为NA的折算率赋值为0
  # 计算两日差异，去掉变动为0的行，新数据框按照变动幅度由低到高排列
  comp <- merge(old, new, by='code', suffixes=c('_old', '_new'), all=TRUE)
  comp$mark <- ''
  comp$mark[is.na(comp$ratio_new)] <- 'STOP'
  comp$mark[is.na(comp$ratio_old)] <- 'NEW'
  comp$ratio_new[is.na(comp$ratio_new)] <- 0
  comp$ratio_old[is.na(comp$ratio_old)] <- 0
  comp$name_old[is.na(comp$name_old)] <- comp$name_new[is.na(comp$name_old)]
  comp$name_new <- NULL
  colnames(comp)[2] <- 'name'
  comp$chg <- round(comp$ratio_new - comp$ratio_old, 2)
  res <- comp %>%
    dplyr::filter(chg != 0) %>%
    dplyr::arrange(mark, chg) %>%
    dplyr::select(code, name, ratio_old, ratio_new, chg, mark)
  # 可选择只保留变动了的行，去掉新增和停止的记录
  if (changed.only) {
    res <- res %>%
      dplyr::filter(mark=='') %>%
      dplyr::select(-mark)
  }

  # 对结果的筛选功能
  ## 参数scale限定某个变动范围以上的记录才被保留，但不影响新增或停止的记录
  if (as.numeric(scale) && scale > 0 && scale <= 30) {
    res <- dplyr::filter(res, abs(chg) >= scale/100 | mark !='')
  }
  ## 参数mark.label指定查看“NEW”、“STOP”或mark为空的记录，输入其他字符串无作用
  if (!changed.only && !is.null(mark.label)) {
    if (mark %in% c('', 'NEW', 'STOP')) {
      res <- dplyr::filter(res, mark == mark.label)
    } else {
      message('Param "mark" should be "", "New" or "STOP", otherwise it\'s useless.')
    }
  }

  # 显示对比日的信息
  cat('The differences of', format(day1, format='%Y/%m/%d'), 'and',
      format(day2, format='%Y/%m/%d'), 'have been computed.\n')

  return(res)
}
