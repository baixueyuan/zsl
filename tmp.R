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
zsp <- zslPeriodDiff(scale=5)
zslSaveToExcelP(data=zsp, wb=wb)

# 提取全部下载列表
dl <- zslCombineList('2005-05-01')
dl1 <- filter(dl, annce < as.Date('2014-01-01'))
dl2 <- zslDownFile(tail(dl1, 8), 'C:/DataWorks/zsl/files', overwrite=TRUE)
files <- dir('C:/DataWorks/zsl/files')
filelist <- paste('C:/DataWorks/zsl/files', files, sep='/')
for (i in filelist) {
  i %>%
    tidyData() %>%
    writeToSQL('all.sql', 'zsl')
  cat(i, ', ')
}
file='C:/DataWorks/zsl/files/20050608SH.shtml'
tidyData(file)

# 计算区间的折算率和持仓总和
codes <- c('122103.SH','122108.SH','122119.SH','122141.SH','122155.SH',
           '122201.SH','122242.SH','122280.SH','122348.SH','122353.SH')
amt <- c(2000000, 34948000, 70000000, 40000000, 90000000,
         30000000, 40015000, 78942000, 30000000, 30000000)
dat <- read.csv('dat.csv', header = TRUE, stringsAsFactors=FALSE)

start='2016-01-01'
end='2016-08-17'
ratios <- zslGetBundle(dat$code, '2016-08-01', '2016-08-17')
pos <- xts(ratios %*% dat$amt, index(ratios))


# 将中文转为ASCII码 ###
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

# 读取写入数据
exdate <- as.Date(read.csv('ex_date.csv', FALSE, stringsAsFactors=FALSE)[[1]])
save(exdate, file='data/exdate.RData')


# 调整数据库端, 已调整 ####
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


# 读取HTML表格
url <- 'http://www.chinaclear.cn/zdjs/xbzzsl/center_flist_147.shtml'
url <- 'http://www.chinaclear.cn/zdjs/zshanhai/201306/c71d13baf5ac4e2c914d67a75d3d1d50.shtml'
url <- 'http://www.chinaclear.cn/zdjs/zshenzhen/201306/15741b3cde6a465db6586738b86fa728.shtml'
htm <- htmlParse(url)
tbl <- readHTMLTable(htm, header=TRUE, stringsAsFactors=FALSE)[[1]]

url <- 'http://www.chinaclear.cn/zdjs/xbzzsl/center_flist_147.shtml'
xml_text(xml_find_all(read_html(url), '/html/head/title'))
