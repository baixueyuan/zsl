# 编译文挡，从注释到rds文件
roxygen2::roxygenize()

# 每日更新折算率数据 ###########################################################
rm(list=ls())
library(zsl)
RODBC::odbcCloseAll(); ch <- RODBC::odbcConnect('research')
RODBC::odbcCloseAll(); ch <- RODBC::odbcConnect('local')

# 自动补齐未更新的数据
zslUpdateDB()
# zslUpdateDB(ch=local)

# 提取最新变动大于等于5bp的记录
ztd <- zslTwoDayDiff()
zslSaveToExcel(data=ztd, assign=TRUE)
zsp <- zslPeriodDiff('2016-07-12', '2016-08-12')
zslSaveToExcelP(data=zsp, wb=wb)
dl <- zslCombineList()[c(1,2),]
zslDownFile(dl)


file='20160808SH.xls'
system.time(tmp <- tidyData('files/20160729SZ.xls'))
system.time(tmp <- tidyData('files/20160729SH.xls'))
writeToDB(tmp, ch, 'zsl', quiet=FALSE, delete.first=TRUE)


# 将中文转为ASCII码
str <- c(
  '债券代码',
  '债券简称',
  '起始日',
  '起始折算率',
  '结束日',
  '结束折算率',
  '天数',
  '变动'
)
cat(stringi::stri_escape_unicode(str), sep='\n')
# 结果中的引号被转义，最好复制到Notepad++中稍作修改

# 调整数据库端 ### 已调整
zzz <- read.csv('zzz.txt', quote = '', fileEncoding = 'utf8',
                stringsAsFactors = F)
zzz$annce <- as.Date(zzz$annce)
zzz$start <- as.Date(zzz$start)
zzz$end <- as.Date(zzz$end)
zzz$date <- apply(zzz, 1, function(x) {
  if (x[['start']]<=x[['end']]){
    seq <- exdate[exdate >= as.Date(x[['start']]) &
                    exdate <= as.Date(x[['end']])]
  } else {
    seq <- exdate[exdate <= as.Date(x[['start']]) &
                    exdate >= as.Date(x[['end']])]
  }

  seq <- paste(seq, collapse = ',')
  return(seq)
})
# cnt <- stringr::str_count(zzz$date, ',')
zzz <- tidyr::separate_rows(zzz, date, sep=',') %>%
  dplyr::select(serial, exchange, code, name, annce, date, ratio) %>%
  ## 删除重复行
  dplyr::distinct()

zzz$date <- as.Date(zzz$date)
zzz$serial <- paste('ZS', format(zzz$annce, format='%Y%m%d'), 'ST',
               format(zzz$date, format='%m%d'),
               exchange, zzz$code, sep='')
zzz$serial <- substring(zzz$serial, 1, 24)
writeToDB(zzz, channel, 'zsl', F)

