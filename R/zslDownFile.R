#' Download the zsl Files
#'
#' Download the zsl files by the file list given by \code{zslDownList} or
#' \code{zslCombineList}. A folder name can be given to save the files, if which
#' does not exist it will be created.
#'
#' @param dl the download file list, containing the file name and urls
#' @param folder the folder saving the files, can ben missed
#' @param overwrite logical, if TRUE any existing file will be overwritten
#'
#' @return No return, the file will be downloaded.
#' @export

zslDownFile <- function(dl, folder, overwrite=FALSE) {
  # 本函数对给定的下载列表进行下载，下载列表应由zslDownList函数生成
  # dl，下载列表数据框，应包含文件名和链接列，如果不给定也可以从Global环境取
  # folder，可以设置目录，函数会将其与dl文件名列连接
  #         函数会将“\\”改为“/”，并将首尾的“/”去掉
  #         同时检查是否存在目录，没有存在则创建，可以创建多级目录
  # overwrite，是否覆盖已有文件，默认为FALSE，已存在文件则不重新下载

  if (missing(dl)) get('dl', envir=.GlobalEnv)

  if (!missing(folder)) {
    folder <- stringr::str_replace(folder, '\\\\', '/')
    folder <- stringr::str_replace(folder, '^/(.*)/$', '\\1')
    if (!dir.exists(folder)) dir.create(folder, recursive=TRUE)
    dl$fn <- paste(folder, dl$fn, sep='/')
  }

  for (i in 1:nrow(dl)) {
    if (i==1) {
      cat('There are', nrow(dl), 'files to be downloaded. ')
      cat('Downloading in progress ...\n')
    }
    cat(paste('The file "', dl[i, 'fn'], '" is downloading ... ', sep=''))
    if (overwrite || !file.exists(dl[i, 'fn'])) {
      download.file(url=dl[i, 'href'], destfile=dl[i, 'fn'],
                    method='internal', mode='wb', quiet=TRUE)
    }
    cat('done. ')
    if (i==nrow(dl)) {
      cat('\nCompleted!\n')
    } else {
      cat('Still', nrow(dl) - i, 'file(s) left.\n')
    }
  }
}
