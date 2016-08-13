#' Compute the zsl Change of a Period
#'
#' The input are the days of the beginning and end of the period, then obtain
#' the change of zsl of this period, that is the difference between the first
#' day and last day's data. Also, the result provides the days of the period for
#' each code.
#'
#' First of all, the records with "0" ratio are deleted, because those are
#' going to be unlisted from the market, not worth consideration. Then, not all
#' the bonds have more than two days' records, therefore, those with one record
#' are thought to be unqualified for period computation, and deleted.
#'
#' For the result, only the bonds with change other than "0" will be reserved.
#'
#' @param day1 the start of the period, Date object of like "YYYY-MM-DD"
#' @param day2 the end of the period, Date object of like "YYYY-MM-DD"
#'
#' @return Tibble data.frame containing the change of the period.
#' @export

zslPeriodDiff <- function(day1, day2, ch) {
  # 本函数用来比较给定两个日期之间的折算率变化

  # 处理比较日期
  # day1='2016-04-01'
  # day2='2016-06-30'
  day1 <- as.Date(day1)
  day2 <- as.Date(day2)
  if (day1 >= day2) stop('Date "day1" should be earlier than "day2".')

  # 从全局环境中获取“ch”数据库连接对象
  if (missing(ch)) {
    if (exists('ch', envir=.GlobalEnv)) {
      ch <- get('ch', envir=.GlobalEnv)
    } else {
      stop('The RODBC connection should be given, or named "ch" in global.')
    }
  }

  # 提取新旧比较数据
  ## 提取数据，去掉ratio为0的，多数为到期摘牌的
  dat <- RODBC::sqlQuery(ch,
                         paste('SELECT date,code,name,ratio FROM zsl ',
                               'WHERE date BETWEEN "', day1, '" AND "',
                               day2, '"', sep=''),
                         stringsAsFactors=FALSE) %>%
    as_tibble %>%
    filter(ratio != 0)

  ## 找到只有一条记录的代码，需要剔除
  oneline <- filter(count(group_by(dat, code)), n==1)$code

  ## 提取区间内每个代买对应的首日和末日
  datt <- lapply(unique(dat$code), function(x) {
    filter(dat, code==x) %>%
      filter(date==max(date) | date==min(date)) %>%
      arrange(date)
  }) %>%
    bind_rows() %>%
    filter(!code %in% oneline) %>%
    mutate(day=rep(c('day1', 'day2'), times=nrow(.)/2))

  ## 组合新的数据框并计算区间天数（period）和折算率变化（change)
  res <- bind_cols(
    filter(datt, day=='day1'),
    filter(datt, day=='day2') %>% select(date, ratio)
  ) %>%
    set_colnames(c('date_old', 'code', 'name', 'ratio_old', 'day', 'date_late',
                   'ratio_late')) %>%
    select(code, name, date_old, ratio_old, date_late, ratio_late) %>%
    mutate(period=as.numeric(date_late - date_old),
           change=round(as.numeric(ratio_late - ratio_old), 2)
    ) %>%
    filter(change != 0) %>%
    arrange(change)

  return(res)
}
