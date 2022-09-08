library(e1071)

set.seed(101011)
# Creamos unos datos en dos dimensiones aleatoriamente, puede que tengamos algunos datos mal calsificados por que son aleatorios
x = matrix(rnorm(60), 30, 2)
# y va a ser nuestra variable de pertenencia
y = rep(c(-1, 1), c(15, 15))

# movemos la media de 0 a 1 en cada coordenada si y = 1
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "linear", #como podemos trazar una linea, usamos un kernel lineal
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo

# codigo para graficar el modelo de SVM
grange = apply(x, 2, range)
n = 75
x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
xgrid = expand.grid(X1 = x1, X2 = x2)

ygrid = predict(svmfit, xgrid)
# Multiplicamos los coeficientes por cada support vector
beta = drop(t(svmfit$coefs)%*%svmfit$SV)
beta0 = svmfit$rho

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

# hagamos algunas predicciones
set.seed(10)
test = matrix(rnorm(20, mean=1), 5, 2)
test_y = predict(svmfit, test)
points(test[test_y == -1,], col = "black", bg="red", pch=24)
points(test[test_y == 1,], col = "black", bg="blue",pch=24)