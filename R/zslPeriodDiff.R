zslPeriodDiff <- function(day1, day2) {
  # 本函数用来比较给定两个日期之间的折算率变化
  # 处理比较日期
  day1='2016-01-01'
  day2='2016-06-30'
  day1 <- as.Date(day1)
  day2 <- as.Date(day2)
  if (day1 >= day2) stop('Date "day1" should be earlier than "day2".')
  # 提取新旧比较数据
  cn <- RODBC::sqlQuery(ch, 'SELECT * FROM codename', stringsAsFactors=FALSE)
  dat <- RODBC::sqlQuery(ch,
                         paste('SELECT date,code,ratio FROM zsl ',
                               'WHERE date BETWEEN "', day1, '" AND "',
                               day2, '"', sep=''),
                         stringsAsFactors=FALSE)
  dat <- dplyr::filter(dat, ratio!=0)
  res <- data.frame(code=unique(dat$code), stringsAsFactors=FALSE)
  res$day1 <- sapply(res$code, function(x) {
    range(dat[dat$code==x, 'date'])[1]
  })
  res$day2 <- sapply(res$code, function(x) {
    range(dat[dat$code==x, 'date'])[2]
  })
  res$day1 <- as.Date(res$day1, origin='1970-01-01')
  res$day2 <- as.Date(res$day2, origin='1970-01-01')

  res$ratio1 <- apply(res, 1, function(x) {
    dat[dat$code==x['code'] & dat$date==x['day1'], 'ratio'][1]
  })
  res$ratio2 <- apply(res, 1, function(x) {
    dat[dat$code==x['code'] & dat$date==x['day2'], 'ratio'][1]
  })
  res$chg <- round(res$ratio2 - res$ratio1, 2)
  res1 <- merge(res, cn, by='code')

  change <- res1[res1$chg != 0, ]
}


