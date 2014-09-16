library(kernlab)

set.seed(10)

# アヤメの品種 (データフレームを行列に変換)
y <- as.matrix(iris[51:150,5]) 
datas <- data.frame(iris[51:150,3:4],y)
ir.ksvm <- ksvm(y ~.,
               data = datas,
               kernel = "rbfdot",
               kpar = list(sigma = 0.2),
               C = 5,
               cross = 3
               )

ir.ksvm

# versicolorとvirginicaの分類をplot
plot(ir.ksvm,data=datas[,1:2])
table(datas$y,predict(ir.ksvm,datas[,1:2]))