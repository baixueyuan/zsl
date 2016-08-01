writeToSQL <- function(data, sql.file, table, append=TRUE) {
  # 本函数直接将给定的数据框写入SQL文件

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
  write.table(qry, file=sql.file, append=append, quote=FALSE,
              row.names=FALSE, col.names=FALSE)
}
