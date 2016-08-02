#' Title
#'
#' @param date
#' @param ch
#'
#' @return
#' @export
#'
zslTwoDayDiff <- function(date, ch) {


  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    ch <- get('ch', envir=.GlobalEnv)
  } else {
    stop('The RODBC connection should be given, or named "ch" in global.')
  }

  # 确定要比较的前后两个日期
  if (missing(date)) {
    qry <- 'SELECT date FROM zsl GROUP BY date ORDER BY date DESC LIMIT 2'
    dates <- RODBC::sqlQuery(ch, qry)
    day1 <- dates[2, 1]
    day2 <- dates[1, 1]
  } else {
    date <- as.Date(date)
    qry <- paste('SELECT date FROM zsl WHERE date<="', date,
                 '" GROUP BY date ORDER BY date DESC LIMIT 2', sep='')
    dates <- RODBC::sqlQuery(ch, qry)
    if (nrow(dates)==2) {
      day1 <- dates[2, 1]
      day2 <- dates[1, 1]
    } else {
      stop('Please make sure that the date given is correct.')
    }
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

  comp <- merge(old, new, by='code', suffixes=c('_old', '_new'), all=TRUE)
  comp$mark <- ''
  comp$mark[is.na(comp$ratio_new)] <- 'STOP'
  comp$mark[is.na(comp$ratio_old)] <- 'NEW'
  comp$ratio_new[is.na(comp$ratio_new)] <- 0
  comp$ratio_old[is.na(comp$ratio_old)] <- 0
  comp$name_old[is.na(comp$name_old)] <- comp$name_new[is.na(comp$name_old)]
  comp$name_new <- NULL
  colnames(comp)[2] <- 'name'
  comp$chg <- comp$ratio_new - comp$ratio_old
  res <- comp %>%
    dplyr::filter(chg != 0) %>%
    dplyr::arrange(mark, chg) %>%
    dplyr::select(code, name, ratio_old, ratio_new, chg, mark)
  return(res)
}
