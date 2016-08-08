# 编译文挡，从注释到rds文件
roxygen2::roxygenize()

# 每日更新折算率数据 ###########################################################
rm(list=ls())
library(zsl)
ch <- RODBC::odbcConnect('research')

# 自动补齐未更新的数据
zslUpdateDB()

# 提取最新变动大于等于5bp的记录
ztd <- zslTwoDayDiff(changed.only = F)
zslSaveToExcel(data=ztd)





file='files/20160729SZ.xls'
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


