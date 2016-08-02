zslTwoDayDiff <- function(date, ch) {


  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    ch <- get('ch', envir=.GlobalEnv)
  } else {
    stop('The RODBC connection should be given, or named "ch" in global.')
  }

  #
}
