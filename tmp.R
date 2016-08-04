# 编译文挡，从注释到rds文件
roxygen2::roxygenize()

# 每日更新折算率数据 ###########################################################
rm(list=ls())
library(zsl)
ch <- RODBC::odbcConnect('research')

# 自动补齐未更新的数据
zslUpdateDB()

# 提取最新变动大于等于5bp的记录
ztd <- zslTwoDayDiff(scale=5)





file='files/20160729SZ.xls'
system.time(tmp <- tidyData('files/20160729SZ.xls'))
system.time(tmp <- tidyData('files/20160729SH.xls'))
writeToDB(tmp, ch, 'zsl', quiet=FALSE, delete.first=TRUE)


# 将中文转为ASCII码
str <- c(
  '([代][码])',
  '([名][称])',
  '([折][算][率])',
  '([开][始])',
  '([结][束])'
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
