#' Tidy the Data Read From File Downloaded From the Website
#'
#' After downloading the file from the website, the function tidy the data read
#' from the file. Then organize the data to the form needed and ready to write
#' to the MySQL database.
#'
#' The input file name must follow the rule, that is "date + exchange + .xls".
#' The file name contains two necessary element for the result, the "file date"
#' and the exchange. These will be two columns.
#'
#' There is another important column called "serial" which will be the primary
#' key of database. It is organized as the following rule: "ZS" + file date +
#' "ST" + start date (month day) + exchange + code. Such rule will make sure
#' that there is no duplicated record by eliminating the duplicated rows.
#'
#' @param file character, the file name must follow the rule
#'
#' @return The well-organized data.frame, ready to be written to MySQL database.
#' @export

tidyData <- function(file) {

  # 提取文件日期
  file_s <- stringr::str_extract(file, '([[:alnum:]]{10})\\.xls')
  if (is.na(date <-
            as.numeric(stringr::str_extract(file_s, '^[[:digit:]]{8}')))) {
    stop('The file name given is NOT following the rule.')
  }

  if (date < 20050518) {
    stop('The date should be later than May 18th, 2005.')
  } else {
    date <- lubridate::ymd(date)
  }

  # 判断交易所是哪一个
  if (stringr::str_detect(file, 'SZ')) {
    exchange <- 'SZ'
  } else {
    if (stringr::str_detect(file, 'SH')) {
      exchange <- 'SH'
    } else {
      stop('Cannot detect "SZ" or "SH" from the file name.')
    }
  }

  # 读取Excel文件
  startRow <- switch(exchange, SZ=4, SH=3)
  data <- XLConnect::readWorksheetFromFile(file, sheet=1, startRow=startRow)

  # 设置需要保留列的正则表达式
  pattern <- paste(
    # '([代][码])',
    # '([名][称])',
    # '([折][算][率])',
    # '([开][始])',
    # '([结][束])',
    '([\u4ee3][\u7801])',
    '([\u540d][\u79f0])',
    '([\u6298][\u7b97][\u7387])',
    '([\u5f00][\u59cb])',
    '([\u7ed3][\u675f])',
    sep='|'
  )

  # 利用magrittr和dplyr整理数据
  data <- data %>%
    ## 取需要的几列
    dplyr::select(dplyr::matches(pattern)) %>%
    ## 修改列名称
    magrittr::set_colnames(c('code', 'name', 'ratio', 'start', 'end')) %>%
    # 以code列为准去掉NA行
    dplyr::filter(code!=is.na(code)) %>%
    ## 清理数据
    dplyr::mutate(
      start = stringr::str_replace(start, ' 00:00:00', '') %>% lubridate::ymd(),
      end = stringr::str_replace(end, ' 00:00:00', '') %>% lubridate::ymd(),
      serial = paste('ZS', format(date, format='%Y%m%d'), 'ST',
                     format(start, format='%m%d'),
                     exchange, code, sep=''),
      code = paste(code, exchange, sep='.'),
      name = name %>%
        stringr::str_trim(side="both") %>%
        stringr::str_replace(stringr::fixed('*failed to decode utf16*'), ''),
      ratio = sapply(ratio, function(x) if (is.na(x)) 0 else x) %>% as.numeric,
      exchange = exchange,
      date = date
    ) %>%
    ## 整理列
    dplyr::select(
      serial, exchange, date, code, name, ratio, start, end
    ) %>%
    ## 删除重复行
    dplyr::distinct()

  return(data)
}
