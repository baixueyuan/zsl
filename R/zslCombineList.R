#' Get the File List From Now to the Given Date
#'
#' This function get the full file list from the website from now to the given
#' date.
#'
#' The given date should not be earlier than May 18th, 2005, that is the start
#' date of the announcement of the discount rate. If the start date isn't given,
#' the first page of file list is returned.
#'
#' @param start the start date by which the function will get all files after
#'   this date
#'
#' @return The data.frame contains all files from now to the given date.
#' @export

zslCombineList <- function(start) {
  # 本函数用来给定开始日期以提取下载列表，即从某个日期开始的所有折算率文件
  # start，开始日期，函数会不断读取下载页面，直至找到这个日期

  if (missing(start)) {
    return(zslDownList())
  } else {
    start <- as.Date(start)
    if (start < as.Date('2005-05-18')) start <- as.Date('2005-05-18')

    # 设置基本参数
    base_url <- 'http://www.chinaclear.cn/zdjs/xbzzsl/center_flist_'
    num <- 2
    dl <- zslDownList()

    # 循环提取
    while (!any(dl$annce < start)) {
      dl <- num %>%
        paste(base_url, ., '.shtml', sep='') %>%
        zslDownList %>%
        rbind(dl, .)
      num <- num + 1
      if (min(dl$annce)==as.Date('2005-05-18')) break
    }

    # dl <- dl[dl$annce >= start, ]
    dl <- dl %>%
      dplyr::filter(annce >= start) %>%
      dplyr::arrange(desc(annce), exchange)

    return(dl)
  }
}
