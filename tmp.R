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
zslSaveToExcel(data=ztd)
zsp <- zslPeriodDiff('2016-07-12', '2016-08-12')

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
  '前折算率',
  '新折算率',
  '变动',
  '备注'
)
cat(stringi::stri_escape_unicode(str), sep='\n')
# 结果中的引号被转义，最好复制到Notepad++中稍作修改

df <- data.frame(
  lt = c('A', 'B', 'C', 'C', 'C'),
  num = c(1, 3, 4, 5, 4),
  stringsAsFactors = FALSE
)

hist(change$chg, breaks=30, main='Change of zsl From Start of 2016',
     xlab='Change of zsl')



# zslTwoDayDiff with dplyr ###########
system.time(comp <- dplyr::inner_join(old, new, by='code', suffix=c('_old', '_new')))
system.time(comp1 <- merge(old, new, by='code', suffixes=c('_old', '_new'), all=FALSE))

comp2 <- dplyr::mutate(comp, mark='')
comp2 <- dplyr::mutate(comp, mark='NEW')


dplyr::group_by(comp, is.na(name_old))

comp3 <- dplyr::left_join(old, new)

tmp <- ztd

exdate <- as.Date(read.csv('ex_date.csv')[[1]])
save(exdate, file='data/exdate.RData')
start <- as.Date('2016-08-05')
end <- as.Date('2016-08-08')
exdate[exdate >=start & exdate <= end]


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

system.time({
# 优化区间差
dat <- RODBC::sqlQuery(ch,
                       paste('SELECT date,code,ratio FROM zsl ',
                             'WHERE date BETWEEN "', day1, '" AND "',
                             day2, '"', sep=''),
                       stringsAsFactors=FALSE) %>%
  as_tibble %>%
  filter(ratio != 0)
})

oneline <- filter(count(group_by(dat, code)), n==1)$code

datt <- lapply(unique(dat$code), function(x) {
  filter(dat, code==x) %>%
    filter(date==max(date) | date==min(date)) %>%
    arrange(date)
}) %>%
  bind_rows() %>%
  filter(!code %in% oneline) %>%
  mutate(day=rep(c('day1', 'day2'), times=nrow(.)/2))

res <- bind_cols(
  filter(datt, day=='day1'),
  filter(datt, day=='day2') %>% select(date, ratio)
) %>%
  set_colnames(c('date_old', 'code', 'ratio_old', 'day', 'date_late', 'ratio_late')) %>%
  select(code, date_old, ratio_old, date_late, ratio_late) %>%
  mutate(period=as.numeric(date_late - date_old),
         change=round(as.numeric(ratio_late - ratio_old), 2)
         ) %>%
  filter(change != 0) %>%
  arrange(change)

