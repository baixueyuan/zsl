#' Tidy the Data Read From File Downloaded From the Website
#'
#' After downloading the file from the website, the function tidy the data read
#' from the file. Then organize the data to the form needed and ready to write
#' to the MySQL database.
#'
#' The input file name must follow the rule, that is "date + exchange + .xls".
#' (Of course, now it can also handle with "shtml" file.)
#' The file name contains two necessary element for the result, the "file date"
#' and the exchange. These will be two columns.
#'
#' There is another important column called "serial" which will be the primary
#' key of database. It is organized as the following rule: "ZS" + file date +
#' "ST" + effects date (month day) + exchange + code. Such rule will make sure
#' that there is no duplicated record by eliminating the duplicated rows.
#'
#' @param file character, the file name must follow the rule
#'
#' @return The well-organized data.frame, ready to be written to MySQL database.
#' @export

tidyData <- function(file) {

  # 提取文件名后缀，“xls”和“shtml”采取不同的方式处理数据
  ext <- stringr::str_extract(file, '[^.]{3,5}$')

  # 提取文件日期
  # file_s <- stringr::str_extract(file, '([[:alnum:]]{10})\\.xls')
  file_s <- stringr::str_extract(file, '([0-9]{8}[SHZ]{2})\\.(xls|shtml)')
  if (is.na(annce <-
            as.numeric(stringr::str_extract(file_s, '^[[:digit:]]{8}')))) {
    stop('The file name given is NOT following the rule.')
  }

  if (annce < 20050518) {
    stop('The annce date should be later than May 18th, 2005.')
  } else {
    annce <- lubridate::ymd(annce)
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

  # 读取文件
  if (ext=='xls') {
    startRow <- switch(exchange, SZ=4, SH=3)
    data <- XLConnect::readWorksheetFromFile(file, sheet=1, startRow=startRow)
    while (!any(stringr::str_detect(colnames(data), '\u503a\u5238.*\u4ee3\u7801'))) {
      # 查找债券列名称当中是否存在“债券代码”，没有就将“statRow”加1来尝试
      # 如果跳过20行仍没有找到正确标题行则提示错误
      data <- XLConnect::readWorksheetFromFile(file, sheet=1,
                                               startRow=startRow + 1)
      startRow <- startRow + 1
      if (startRow > 20) stop('statRow is more than 20, please check!')
    }
  }

  if (ext=='shtml') {
    data <- XML::readHTMLTable(file, header=TRUE, stringsAsFactors=FALSE)[[1]]
    if (exchange=='SH') {
      colnames(data) <- data[1, ]
      ind <- which(data[, 1]=='')
      data <- dplyr::slice(data, -c(1, ind))
    }
  }


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
    dplyr::filter(code!=is.na(code) & name!=is.na(name)) %>%
    ## 清理数据
    dplyr::mutate(
      start = stringr::str_replace(start, ' 00:00:00', '') %>% lubridate::ymd(),
      end = stringr::str_replace(end, ' 00:00:00', '') %>% lubridate::ymd(),
      code1 = as.character(code),
      code = paste(code, exchange, sep='.'),
      name = name %>%
        stringr::str_trim(side="both") %>%
        stringr::str_replace(stringr::fixed('*failed to decode utf16*'), ''),
      ratio = sapply(ratio, function(x) if (is.na(x)) 0 else x) %>% as.numeric,
      exchange = exchange,
      annce = annce
    )# %>%
    ## 整理列
    # dplyr::select(
    #   exchange, annce, code, name, ratio, start, end
    # )

  # 由于上交所债券ETF的折算率以100为基数给出，而不是1，统一至以1为基数
  data[stringr::str_detect(data$name, 'ETF') & data$exchange=='SH',
       'ratio'] <- data[stringr::str_detect(data$name, 'ETF') &
                          data$exchange=='SH', 'ratio'] / 100

  # 调整列：
  ## 如果开始可结束日期相同，则返回这个日期作为生效日
  ## 如果开始和结束日期不同，则以他们为开始和结束返回一个日期序列，以“,”分隔
  ## 用seperate_rows函数按照这个列进行拆分，每行都为一个单一日期
  ## 日期序列从交易所日期序列“exdate”中提取，因此所有日期比为交易日
  data$date <- apply(data, 1, function(x) {
    if (x[['start']]==x[['end']]) {
      return(x[['start']])
    } else {
      seq <- exdate[exdate >= min(as.Date(c(x[['start']], x[['end']]))) &
                    exdate <= max(as.Date(c(x[['start']], x[['end']])))]
      seq <- paste(seq, collapse = ',')
      return(seq)
    }
  })
  data <- tidyr::separate_rows(data, date, sep=',')# %>%
    # dplyr::select(exchange, code, name, annce, date, ratio)

  data$date <- as.Date(data$date)

  data$serial <- paste('ZS', format(data$annce, format='%Y%m%d'), 'ST',
                       format(data$date, format='%m%d'),
                       data$exchange, data$code1, sep='')

  data <-
    dplyr::select(data, serial, exchange, code, name, annce, date, ratio) %>%
    ## 删除重复行
    dplyr::distinct()

  return(data)
}
