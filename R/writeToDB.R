#' Write The data.frame to MySQL Database
#'
#' Write the data.frame to MySQL database using the method that sent the query
#' to the database server.
#'
#' The function can delete the records before writing to the database, however
#' if it can be make sure that the there is no duplicated records, the deletion
#' can be skipped, saving time. The deletion work is based on the primary key,
#' so the column index has to be given, and the column name should the same as
#' that in the database.
#'
#' The mechanism of the function is to **paste** an INSERT query and send to
#' the database server. Sum method seems safe to be executed
#'
#' @param data the data.frame will be written to the database
#' @param channel an ODBC channel object
#' @param table the table name of database, which is used to organized query
#' @param quiet logical, default is TRUE, show the progress bar
#' @param sql.out logical, whether give the SQL message, just in case there are
#'   errors occur
#' @param delete.first logical, whether to delete the records first using the
#'   primary key column
#' @param primary integer, which column is the primary column, and just used
#'   when delete records
#'
#' @return Besides errors messages, no return.
#' @import RODBC
#' @export

writeToDB <- function(data, channel, table, quiet=TRUE, sql.out=FALSE,
                      delete.first=FALSE, primary=1L) {
  # 本函数直接将给定的数据框写入数据库
  # 参数data为给定的待写入的数据框
  # 参数channel为数据库连接对象
  # 参数table表示写入服务器端时对应的表名称，本函数采用的方式是使用
  # “INSERT INTO table ...”，因此后面直接跟VALUES(...)，即写入每行的完整数据
  # 如果数据框与服务器端不对应就会出现写入错误
  # 参数quiet表示是否输出写入进度，默认为TRUE，即不输出
  # 参数sql.out表示输出sql语句，便于查找错误，默认为FALSE
  # 本函数提供删除已有记录的机制，参数delete.first为TRUE时，首先删除服务器端数据
  # 参数primary认为给定一个写入数据的列作为主键，从而使用DELETE语句删除

  # 按照给定主键列删除重复记录
  if (delete.first) {
    if (!is.numeric(primary) || length(primary)>1)
      stop('Param "primary" should be "integer" and length 1.')
    cat('Deleting records ... \n')
    pb <- txtProgressBar(min=0, max=nrow(data), width=40, style=3)
    for (i in 1:nrow(data)) {
      qry <- paste('DELETE FROM ', table, ' WHERE ', colnames(data)[primary],
                   '="', data[i, primary], '"', sep='')
      sqlQuery(channel, qry)
      setTxtProgressBar(pb, i)
    }
  }

  # 将数据转为SQL语句
  for (i in 1:length(data)) {
    cls <- class(data[[i]])
    if(cls=='character' || cls=='Date')
      data[[i]] <- paste('\"', as.character(data[[i]]), '\"', sep='')
  }
  qry <- apply(data, 1, paste, collapse=',')
  qry <- gsub('\"NA\"', 'NULL', qry)
  qry <- gsub('NA,', 'NULL,', qry)
  qry <- paste('INSERT INTO ', table, ' VALUES(', qry, ');', sep='')

  # 写入数据库
  if (!quiet) cat('\nWriting ', nrow(data), ' records ...\n', sep='')
  if (!quiet) {
    pb <- txtProgressBar(min=0, max=nrow(data), width=40, style=3)
  }
  for (i in 1:nrow(data)) {
    if (sql.out) assign('query', qry, envir=.GlobalEnv)
    rpt <- sqlQuery(channel, qry[i])
    if (length(rpt)==0) {
      if (!quiet) setTxtProgressBar(pb, i)
    } else {
      print(rpt)
    }
  }
  if (!quiet) close(pb)
}
