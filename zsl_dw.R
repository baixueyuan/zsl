# 每日更新折算率数据 ###########################################################
setwd('C:/DataWorks/zsl')
rm(list=ls())
suppressMessages(library(rJava))
suppressMessages(library(XLConnect))
library(zsl)
library(WindR)
ch <- RODBC::odbcConnect('research')

# 信息输出
cat('The work is starting ...\n')
cat('Clearing the workspace ...\n')
cat('Loading the packages needed ...\n')
if (exists('ch')) {
  if (class(ch)=='RODBC') cat('The MySQL DSN is connected!\n')
}

# 自动补齐未更新的数据
cat('Starting downloading files and updating the database ...\n')
zslUpdateDB()

# 指定文件名
file <- paste('C:/工作共享文档/国信固收事业部投研共享中/02_每日发行、待发、评级调整和标准券打折率汇总/0折算率数据_bxy/折算率', format(Sys.Date(), format='%Y%m%d'), '.xlsx', sep='')

# 提取最新变动
cat('Processing two days\' data ...\n')
ztd <- zslTwoDayDiff()
ztd <- zslAddBondInfo(ztd)
zslSaveToExcel(data=ztd, assign=TRUE)

cat('Processing 30 days\' data ...\n')
zsp <- zslPeriodDiff(scale=5)
zsp <- zslAddBondInfo(zsp)
zslSaveToExcelP(data=zsp, wb=wb, output.file=file)

if (file.exists(file)) {
  cat('The file is saved!\n')
} else {
  cat('The file is not saved, please check!\n')
}
RODBC::odbcCloseAll()
if (!exists('ch')) {
  cat('The MySQL DSN is closed!\n')
}
cat('The work is completed, and will quit in 30 seconds!\n'); Sys.sleep(30)
quit(save='no')
