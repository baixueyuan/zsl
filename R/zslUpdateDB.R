#' Update zsl Data
#'
#' Update the bond discount rate data to MySQL database. The date of the lastest
#' records can be detected from the database, and search the new data from the
#' website, so the param \code{date} can be ignored.
#'
#' @param ch
#' @param date
#' @param folder
#' @param delete.file
#'
#' @return
#' @export
#'
zslUpdateDB <- function(ch, date, folder='', delete.file=TRUE) {
  # 本函数更新折算率数据，日期可以给定也可以自动从服务器端获取已有数据的最后日期

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    ch <- get('ch', envir=.GlobalEnv)
  } else {
    stop('The RODBC connection should be given, or named "ch" in global.')
  }

  # 获取日期
  if (missing(date)) {
    qry <- 'SELECT MAX(date) FROM zsl'
    date <- RODBC::sqlQuery(ch, qry, stringsAsFactors=FALSE)[1,1] + 1
  } else {
    date <- as.Date(date)
  }

  dl <- zslCombineList(date)
  if (nrow(dl) > 0) {
    dl <- zslDownFile(dl)
    date_list <- unique(dl$annce)
    date_list <- date_list[date_list <= date]
    if (length(date_list) > 0) {
      for (i in date_list) {
        i <- as.Date(i, origin='1970-01-01')
        qry <- paste('DELETE FROM zsl WHERE date="', i, '"', sep='')
        RODBC::sqlQuery(ch, qry)
        cat('\nThe records of', as.Date(i, origin='1970-01-01'),
            'has been deleted')
      }
    }
    for (i in dl$fn) {
      cat('\nWriting the file', i, '...')
      i %>%
        tidyData() %>%
        writeToDB(ch, 'zsl', quiet=FALSE)
      if (delete.file) suppressMessages(file.remove(i))
    }
  } else {
    message('There is no file can be downloaded now!')
  }
}