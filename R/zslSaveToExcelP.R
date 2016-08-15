#' @rdname savetoexcel
#' @export

zslSaveToExcelP <- function(data, wb, output.file='zsl.xlsx') {

  # 本函数将区间折算率变化保存至Excel文件

  # 将列名称改为中文列名称
  # “债券代码”、“债券简称”、“起始日”、“起始折算率”、“结束日”、
  # “结束折算率”、“天数”、“变动”
  colnames(data) <- c('\u503a\u5238\u4ee3\u7801',
                      '\u503a\u5238\u7b80\u79f0',
                      '\u8d77\u59cb\u65e5',
                      '\u8d77\u59cb\u6298\u7b97\u7387',
                      '\u7ed3\u675f\u65e5',
                      '\u7ed3\u675f\u6298\u7b97\u7387',
                      '\u5929\u6570',
                      '\u53d8\u52a8')

  # 如果参数wb给定，则使用wb对象，否则读取模板文件
  if (missing(wb)) {
    tmpl <- system.file('template', 'template.xlsx', package='zsl')
    wb <- XLConnect::loadWorkbook(tmpl)
  }

  XLConnect::createSheet(wb, 'period')

  # 设置文档的样式模式为命名前缀模式，并将前缀设为“zsl”
  # 相关的样式已经在模板文件中设置好了
  XLConnect::setStyleAction(wb, XLC$"STYLE_ACTION.NAME_PREFIX")
  XLConnect::setStyleNamePrefix(wb, 'zsl')

  # 写入数据
  XLConnect::createName(wb, 'period', 'period!$A$1', overwrite=TRUE)
  XLConnect::writeNamedRegion(wb, data, 'period')

  # 为变动较大的做标记，按照0.05、0.1和0.2分三段标记，负数用红色，正数用绿色
  small <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Small')
  less <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Less')
  least <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Least')
  big <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Big')
  bigger <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Bigger')
  biggest <- XLConnect::getCellStyle(wb, 'zsl.Numeric.Biggest')
  ind <- which(data[, 8] > 0.05 & data[, 8] <= 0.1)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=big)
  }
  ind <- which(data[, 8] > 0.1 & data[, 8] <= 0.2)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=bigger)
  }
  ind <- which(data[, 8] > 0.2)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=biggest)
  }
  ind <- which(data[, 8] < -0.05 & data[, 8] >= -0.1)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=small)
  }
  ind <- which(data[, 8] < -0.1 & data[, 8] >= -0.2)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=less)
  }
  ind <- which(data[, 8] < -0.2)
  if (length(ind) > 0) {
    for (i in ind)
      XLConnect::setCellStyle(wb, sheet='period',
                              row=i+1, col=8, cellstyle=least)
  }

  # 设置首行冻结、筛选、自动列宽
  XLConnect::createFreezePane(wb, 'period', colSplit=1, rowSplit=2)
  XLConnect::setAutoFilter(wb, 'period', aref('A1', dim(data)))
  XLConnect::setColumnWidth(wb, sheet='period', column=c(1:length(data)),
                            width=-1)
  if (XLConnect::existsSheet(wb, 'Sheet1'))
    XLConnect::removeSheet(wb, 'Sheet1')

  # 将Sheet名称改为“区间变化”
  XLConnect::renameSheet(wb, 'period', '\u533a\u95f4\u53d8\u5316')
  # 将Sheet名称改为“最新折算率”
  if (XLConnect::existsSheet(wb, 'zsl'))
    XLConnect::renameSheet(wb, 'zsl', '\u6700\u65b0\u6298\u7b97\u7387')

  # 保存文件
  XLConnect::saveWorkbook(wb, file=output.file)
}
