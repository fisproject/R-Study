require(rstan)
require(ggplot2)

# change working directory
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
setwd(dirname(frame_files[[length(frame_files)]]))

d <- as.data.frame(read.csv(file="data/coal-mining-disasters.csv"))
head(d)
# year coal_mining_disasters
# 1 1851                     4
# 2 1852                     5
# 3 1853                     4
# 4 1854                     0
# 5 1855                     1
# 6 1856                     4

g <- ggplot(
  d,
  aes(
    x=as.Date(as.character(year), format='%Y'),
    y=coal_mining_disasters
  )
)
g <- g + geom_line() + labs(title="", x="year", y="coal-mining-disasters")
plot(g)

d.list <- list(
    T=length(d$coal_mining_disasters),
    d=d$coal_mining_disasters
)

d.fit <- stan(
    file='model/mining-disasters.stan',
    data=d.list,
    iter=1000,
    chains=4
)

ex <- extract(d.fit)
plot(table(ex$s),ylab="")
ex.point <-mean(ex$s)
ex.early <-mean(ex$e)
ex.late <-mean(ex$l)

changes.y <- 1:d.list$T
for(i in 1:d.list$T){
  changes.y[i] <- ifelse( i < ex.point, ex.early, ex.late)
}

changes <- data.frame(
    x=as.Date(as.character(d$year), format='%Y'),
    y=changes.y
  )

g <- ggplot(
  d,
  aes(
    x=as.Date(as.character(year), format='%Y'),
    y=coal_mining_disasters
  )
)
g <- g + geom_line() + labs(title="", x="year", y="coal-mining-disasters") +
  geom_line(data=changes, aes(x=x, y=y), colour="magenta")
plot(g)
