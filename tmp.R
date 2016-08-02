# 编译文挡，从注释到rds文件
roxygen2::roxygenize()

a=2
while (a > 1) {
  print(a)
  a <- a + 1
  if (a==10) break
}

rm(list=ls())
ch <- RODBC::odbcConnect('research')
zslUpdateDB(date=Sys.Date())

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
