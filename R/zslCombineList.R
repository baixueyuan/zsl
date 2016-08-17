#' Get the File List From Now to the Given Date
#'
#' This function get the full file list from the website from now to the given
#' date.
#'
#' The given date should not be earlier than May 18th, 2005, that is the start
#' date of the announcement of the discount rate. If the start date isn't given,
#' the first page of file list is returned.
#'
#' The working mechanism of the function is as the following, the function first
#' get the first page links, then check whether the given "start" date exists,
#' if not, the function "paste" the url with "base_url" and "num", "num" starts
#' from 2. The function get the download links with "num" continues as 2, 3, ...
#' When the "start" date is found, the loop stops.
#'
#' There might be some duplicated file names, the function will \code{paste} a
#' "p" before one of the duplicated file, or maybe more until there is no
#' duplicated file name. However the file name if important when tidying data,
#' adding prefix has no influence.
#'
#' @param start the start date by which the function will get all files after
#'   this date
#' @param num the start num which is used to "paste" the url, default is 2
#'
#' @return The data.frame contains all files from now to the given date.
#' @export

zslCombineList <- function(start, num) {
  # 本函数用来给定开始日期以提取下载列表，即从某个日期开始的所有折算率文件
  # start，开始日期，函数会不断读取下载页面，直至找到这个日期

  if (missing(start)) {
    return(zslDownList())
  } else {
    start <- as.Date(start)
    if (start < as.Date('2005-05-18')) start <- as.Date('2005-05-18')

    # 设置基本参数
    base_url <- 'http://www.chinaclear.cn/zdjs/xbzzsl/center_flist_'
    if (missing(num)) {
      num <- 2L
    } else {
      if (!as.integer(num) && length(num) > 1 && num < 2) {
        stop('Param "num" should be length 1 numeric and equal or greater than 2.')
      }
    }
    dl <- zslDownList()
    if (num > 2) dl <- dl[-c(1:nrow(dl)), ]

    # 循环提取
    while (!any(dl$annce < start)) {
      url <- paste(base_url, num, '.shtml', sep='')
      if (httr::GET(url)$status_code > 400) break else {
      dl <- url %>%
        zslDownList %>%
        rbind(dl, .)
      }
      num <- num + 1L #; cat(num, '\n')
    }

    # 截取大于等于参数start的数据
    dl <- dl %>%
      dplyr::filter(annce >= start) %>%
      dplyr::arrange(desc(annce), exchange)

    # 处理重复的文件名，将重复文件名的第一个用paste修改
    while (anyDuplicated(dl$fn)) {
      ind <- which(duplicated(dl$fn))
      dl$fn[ind] <- paste('p', dl$fn[ind], sep='')
    }

    return(dl)
  }
}
