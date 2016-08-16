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


# 计算区间的折算率和持仓总和
codes <- c('122103.SH','122108.SH','122119.SH','122141.SH','122155.SH',
           '122201.SH','122242.SH','122280.SH','122348.SH','122353.SH')
amt <- c(2000000, 34948000, 70000000, 40000000, 90000000,
         30000000, 40015000, 78942000, 30000000, 30000000)
dat <- read.csv('dat.csv', header = TRUE, stringsAsFactors=FALSE)

qry <- "select code,date,ratio from zsl where date between '2016-01-01' and '2016-08-17' and code in ('020105.SH','020122.SH','020124.SH','020125.SH','020126.SH','020132.SH','113008.SH','120610.SH','122032.SH','122080.SH','122103.SH','122108.SH','122119.SH','122141.SH','122155.SH','122201.SH','122242.SH','122280.SH','122348.SH','122353.SH','122357.SH','122363.SH','122553.SH','122588.SH','122628.SH','122630.SH','122631.SH','122638.SH','122642.SH','122649.SH','122658.SH','122859.SH','122866.SH','122882.SH','122897.SH','122905.SH','122906.SH','122930.SH','122956.SH','124021.SH','124053.SH','124098.SH','124121.SH','124213.SH','124244.SH','124248.SH','124277.SH','124287.SH','124297.SH','124319.SH','124330.SH','124512.SH','132001.SH','136013.SH','136140.SH','136188.SH','136321.SH','111057.SZ','112112.SZ','112123.SZ','112124.SZ','112126.SZ','112153.SZ','112171.SZ','112311.SZ','112419.SZ','123001.SZ','128013.SZ');"
dat1 <- sqlQuery(ch, qry, stringsAsFactors=FALSE)
dat2 <- spread(dat1, code, ratio)
start='2016-01-01'
end='2016-08-17'
ratios <- zslGetBundle(dat$code, '2016-01-01', '2016-08-17')
pos <- xts::xts(ratios %*% dat$amt, index(ratios))


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




