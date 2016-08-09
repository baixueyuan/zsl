# 每日更新折算率数据 ###########################################################
setwd('C:/DataWorks/zsl')
rm(list=ls())
library(zsl)
ch <- RODBC::odbcConnect('research')

# 自动补齐未更新的数据
zslUpdateDB()

# 提取最新变动
ztd <- zslTwoDayDiff()
RODBC::odbcCloseAll()
file <- paste('C:/Dropbox/Documents/国信证券/04债券研究/00投资管理/07其他资料/折算率/折算率', format(Sys.Date(), format='%Y%m%d'), '.xlsx', sep='')
zslSaveToExcel(data=ztd, output.file=file)

quit(save='no')
