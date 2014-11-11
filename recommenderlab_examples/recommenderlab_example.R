# install.packages("recommenderlab")
library(recommenderlab)

data(Jester5k)
d.sample <- sample(Jester5k)

hist(rowCounts(d.sample), breaks=50)

d.norm <- normalize(d.sample)
hist(getRatings(d.norm), breaks=100)

# render by X11
# x11()
# image(d.norm[1:100,], main="normalized")

recommenderRegistry$get_entries(dataType="realRatingMatrix")
r.model <- Recommender(d.sample, method="UBCF")
r.result <- predict(r.model, Jester5k[1001:1005], n=5)
as(r.result, "list")

scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,goodRating=5)

# evaluate
eval.UBCF <- evaluate(scheme, method="UBCF", n=c(1,3,5,10,15,20))
eval.IBCF <- evaluate(scheme, method="IBCF", n=c(1,3,5,10,15,20))
eval.RANDOM <- evaluate(scheme, method="RANDOM", n=c(1,3,5,10,15,20))
eval.POPULAR <- evaluate(scheme, method="POPULAR", n=c(1,3,5,10,15,20))

avg(eval.results)

# ROC curve image
plot(eval.UBCF, annotate=TRUE, xlim=c(0,0.3), ylim=c(0,0.5), pch=1)
par(new=TRUE)
plot(eval.IBCF, annotate=TRUE, xlim=c(0,0.3), ylim=c(0,0.5), pch=2)
par(new=TRUE)
plot(eval.RANDOM, annotate=TRUE, xlim=c(0,0.3), ylim=c(0,0.5), pch=3)
par(new=TRUE)
plot(eval.POPULAR, annotate=TRUE, xlim=c(0,0.3), ylim=c(0,0.5), pch=4)
par(new=TRUE)
legend(0,0.5,c("UBCF","IBCF","RANDOM","POPULAR"),pch=1:4)

# precision/recall plots
plot(eval.UBCF,"prec/rec", annotate=TRUE, xlim=c(0,0.5), ylim=c(0,0.5), pch=1)
par(new=TRUE)
plot(eval.IBCF,"prec/rec", annotate=TRUE, xlim=c(0,0.5), ylim=c(0,0.5), pch=2)
par(new=TRUE)
plot(eval.RANDOM,"prec/rec", annotate=TRUE, xlim=c(0,0.5), ylim=c(0,0.5), pch=3)
par(new=TRUE)
plot(eval.POPULAR,"prec/rec", annotate=TRUE,xlim=c(0,0.5), ylim=c(0,0.5), pch=4)
par(new=TRUE)
legend(0.4,0.1,c("UBCF","IBCF","RANDOM","POPULAR"),pch=1:4)