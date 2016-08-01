#' Download the URL List of the zsl Data
#'
#' Check the website and scape the urls and other attibutes, then form the
#' result data.frame containing the infomation for the further use.
#'
#' This function mostly use the \code{xml2} package to get information from the
#' website. The final data.frame contains the announcement date, exchange,
#' file name and url of the file. There is only one param \code{url}, which is
#' used to be checked and get the corresponding info. The zsl list is in several
#' pages (now nearly 150 pages), the default \code{url} is the first page where
#' the newest files' urls can be get.
#'
#' The file name contained in the result data.frame is strictly organized. That
#' is "date + exchange + .xls". For the further use, this is the basic rule.
#'
#' @param url the url to be parsed and get the information
#'
#' @return A data.frame contains the announcement date, exchange,
#'   file name and url of the file.
#' @import xml2
#' @import stringr
#' @import lubridate
#' @export

zslDownList <- function(url) {
  # 本函数利用xml2包爬取中证登的折算率文件网站，返回下载文件列表
  # 返回的列表包括了交易所，文件日期，文件链接，下载文件名

  # 读取url，url可以给定，以用于未来循环读取多个页面
  if (missing(url))
    url <- 'http://www.chinaclear.cn/zdjs/xbzzsl/center_flist.shtml'
  xl <- xml2::read_html(url)

  # 设置文件名列表的XPath和日期的XPath并读取Node
  xpath <- "//div[@class='pageTabContent']/ul/li/a[contains(@title, '折算率的通知')]"
  xpathd <- "//div[@class='pageTabContent']/ul/li/span[@class='time']"
  li <- xml2::xml_find_all(xl, xpath)
  ind1 <- stringr::str_detect(xml2::xml_attr(li, 'href'), 'xls$')
  ind2 <- stringr::str_detect(xml2::xml_attr(li, 'href'), 'shtml$')
  li <- li[ind1 | ind2]
  title <- xml2::xml_text(li, trim=TRUE)

  # 获取日期
  annce <- lubridate::ymd(stringr::str_extract(title, '[[:digit:]]{8}'))

  # 获取文件链接，其中上交所的没有给出前缀，需要补充
  href <- as.character(sapply(xml2::xml_attr(li, 'href'), function(x) {
    if (!stringr::str_detect(x, '^http')) {
      return(paste('http://www.chinaclear.cn', x, sep=''))
    } else {
      return(x)
    }
  }))
  href <- stringr::str_replace(href, ' ', '%20')

  # 根据title，判断交易所
  exchange <- as.character(sapply(title, function(x) {
    if (stringr::str_detect(x, '\u6df1\u5733')) return('SZ')
    if (stringr::str_detect(x, '\u4e0a\u6d77')) return('SH')
  }))

  # 用日期和交易所组合成文件名
  fn <- paste(format(annce, format='%Y%m%d'), exchange, '.xls', sep='')

  # 生成并返回完整列表
  dl_list <- data.frame(annce, exchange, fn, href, stringsAsFactors=FALSE)

  return(dl_list)
}
