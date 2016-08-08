#' @export

zslSaveToExcel <- function(data, output.file='zsl.xlsx', assign=FALSE) {

  # 更改备注列，NEW改为“新增”，STOP改为“停止”
  data$mark <- stringr::str_replace(data$mark, 'NEW', '\u65b0\u589e')
  data$mark <- stringr::str_replace(data$mark, 'STOP', '\u505c\u6b62')

  # 将列名称改为中文列名称
  # “债券代码”、“债券简称”、“前折算率”、“新折算率”、“变动”、“备注”
  colnames(data) <- c('\u503a\u5238\u4ee3\u7801',
                     '\u503a\u5238\u7b80\u79f0',
                     '\u524d\u6298\u7b97\u7387',
                     '\u65b0\u6298\u7b97\u7387',
                     '\u53d8\u52a8',
                     '\u5907\u6ce8')

  # 读取模板文件
  tmpl <- system.file('template', 'template.xlsx', package='zsl')
  wb <- XLConnect::loadWorkbook(tmpl)
  # wb <- loadWorkbook('inst/template/template.xlsx')
  XLConnect::renameSheet(wb, 'Sheet1', 'zsl')

  # 设置文档的样式模式为命名前缀模式，并将前缀设为“zsl”
  # 相关的样式已经在模板文件中设置好了
  XLConnect::setStyleAction(wb, XLC$"STYLE_ACTION.NAME_PREFIX")
  XLConnect::setStyleNamePrefix(wb, 'zsl')

  # 写入数据
  XLConnect::createName(wb, 'data', 'zsl!$A$1', overwrite=TRUE)
  XLConnect::writeNamedRegion(wb, data, 'data')

  # 为变动较大的做标记，按照0.05、0.1和0.2分三段标记
  big <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Big')
  large <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Large')
  huge <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Huge')
  ind <- which(abs(data[, 5]) > 0.05 & abs(data[, 5]) <= 0.1 & data$mark=='')
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='zsl', row=i, col=5, cellstyle=big)
  }
  ind <- which(abs(data[, 5]) > 0.11 & abs(data[, 5]) <= 0.2 & data$mark=='')
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='zsl', row=i, col=5, cellstyle=large)
  }
  ind <- which(abs(data[, 5]) > 0.2 & data$mark=='')
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='zsl', row=i, col=5, cellstyle=huge)
  }

  # 设置首行冻结、筛选、自动列宽
  XLConnect::createFreezePane(wb, 'zsl', colSplit=1, rowSplit=2)
  XLConnect::setAutoFilter(wb, 'zsl', aref('A1', dim(data)))
  XLConnect::setColumnWidth(wb, sheet='zsl', column=c(1:length(data)), width=-1)

  # 是否将Workbook对象存放在Global环境
  if (assign) assign('wb', wb, envir=.GlobalEnv)

  # 保存文件
  XLConnect::saveWorkbook(wb, file=output.file)
}
