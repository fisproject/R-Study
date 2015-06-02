require(kernlab)

set.seed(10)

y <- as.matrix(iris[51:150, 5])
datas <- data.frame(iris[51:150,3:4], y)

ir.ksvm <- ksvm(y ~.,
               data=datas,
               kernel="rbfdot",
               kpar=list(sigma=0.2), # kernel param sigma
               C=5, # margin param C
               cross=3
           )

print(ir.ksvm)

plot(ir.ksvm,data=datas[, 1:2])
table(datas$y,predict(ir.ksvm,datas[, 1:2]))
