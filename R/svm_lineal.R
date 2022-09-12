library(e1071)

# Datos, creemos dos grupos claramente separables
set.seed(34)
x1 <- runif(30)
x2 <- 5 * x1 ^ 2 + rnorm(length(x1), sd = 2)
x <- matrix(c(x1, x2), nrow = 30, ncol= 2)
y = ifelse(x1 < 0.6, -1, 1)
plot(x, col=y+3, pch = 19)

dat = data.frame(x, y = as.factor(y))

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "linear", #como podemos trazar una linea, usamos un kernel lineal
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo

# codigo para graficar el modelo de SVM, podemos volverlo una función
# Multiplicamos los coeficientes por cada support vector
beta = drop(t(svmfit$coefs)%*%svmfit$SV)
beta0 = svmfit$rho

plot(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

# Cambiando el valor de c
svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "linear",
  cost = 5, # cost = c
  scale = FALSE)
print(svmfit)

beta = drop(t(svmfit$coefs)%*%svmfit$SV)
beta0 = svmfit$rho

plot(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "linear",
  cost = 30, # cost = c
  scale = FALSE)
print(svmfit)

beta = drop(t(svmfit$coefs)%*%svmfit$SV)
beta0 = svmfit$rho

plot(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

# Otro ejemplo lineal, pero con datos sobrelapados
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
# Multiplicamos los coeficientes por cada support vector
beta = drop(t(svmfit$coefs)%*%svmfit$SV)
beta0 = svmfit$rho

plot(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)

# hagamos algunas predicciones
set.seed(10)
test = matrix(rnorm(20, mean=1), 5, 2)
test_y = predict(svmfit, test)
points(test[test_y == -1,], col ="black", bg=-1 + 3, pch=24)
points(test[test_y == 1,], col ="black", bg= 1 + 3, pch=24)
