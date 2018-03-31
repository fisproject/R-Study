# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- read.csv("data/baseball.csv")

countByTeam <- function(x, date) {
  if (!is.null(date)) {
    x <- x[x$last_game == date,]
    d <- c(NULL)
  }

  c <- c(NULL)
  t <- unique(x$team)

  for (i in 1:length(t)) {
    c[i] <- nrow(x[x$team == t[i],])
    if (!is.null(date)) d[i] <- date
  }

  if (!is.null(date)) {
    df <- data.frame(team = t, count = c, last_game = d)
  }else{
    df <- data.frame(team = t, count = c)
  }

  return(df)
}

d.team.count <- countByTeam(d, NULL)
#   team count
# 1 広島東洋カープ     4
# 2 千葉ロッテマリーンズ     3
# 3 福岡ソフトバンクホークス     1

df <- data.frame(NULL)
last <- unique(d$last_game)
for (i in 1:length(last)){
  y <- countByTeam(d, toString(last[i]))
  df <- rbind(df, y)
}
# write.csv(df, "data/count-by-team.csv", quote=FALSE, row.names=FALSE)
# team,count,last_game
# 広島東洋カープ,2,2015/7/1
# 千葉ロッテマリーンズ,2,2015/7/1
# 広島東洋カープ,1,2015/6/30
# 千葉ロッテマリーンズ,1,2015/6/30
# 広島東洋カープ,1,2015/6/25
# 福岡ソフトバンクホークス,1,2015/7/2

d.sorted <- d[order(as.Date(d$birthday, format = "%Y/%m/%d")),]
#   name                     team   birthday
# 5 黒田　博樹           広島東洋カープ  1975/2/10
# 7 荻野　貴司     千葉ロッテマリーンズ 1985/10/21
# 6 清田　育宏     千葉ロッテマリーンズ  1986/2/11
# 3 角中　勝也     千葉ロッテマリーンズ  1987/5/25
# 2 前田　健太           広島東洋カープ  1988/4/11
# 8 柳田　悠岐 福岡ソフトバンクホークス  1988/10/9
# 4   丸　佳浩           広島東洋カープ  1989/4/11
# 1 菊池　涼介           広島東洋カープ  1990/3/11
