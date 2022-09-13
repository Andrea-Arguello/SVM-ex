library(e1071)
# Polinomial
# Código para generar el area o "grid" de cada clase
grids = function(x, n = 100) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

# Ejemplo 1
set.seed(34)
x1 <- runif(100)
x2 <- 5 * x1 ^ 2 + rnorm(length(x1), sd = 2)
x <- matrix(c(x1, x2), nrow = 50, ncol= 2)
y = as.numeric(c(x[,1] < 0.8 & x[,2] < 0.55))
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "linear", #Intentemos usar un kernel lineal
  cost = 25, # cost = c
  scale = FALSE)
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)



svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "polynomial", #comparando con un kernel polinomial grado 2
  degree = 2,
  cost = 25, # cost = c
  scale = FALSE)

xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", #comparando con un kernel polinomial grado 2
  cost = 25, # cost = c
  scale = FALSE)

xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", #comparando con un kernel polinomial grado 2
  cost = 5, # cost = c
  scale = FALSE)

xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", #comparando con un kernel polinomial grado 2
  cost = 50, # cost = c
  scale = FALSE)

xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)


# Ahora, algo más interesante
set.seed(123)
theta1 = runif(100, 0,2*pi)
x1 = matrix(c(cos(theta1) + rnorm(100, 0, 0.03),
            sin(theta1) + rnorm(100, 0, 0.03)),
            nrow = 100, ncol = 2)
set.seed(321)
x2 = matrix(runif(200, -0.4, 0.4), 100, 2)
x=rbind(x1,x2)
y = c(rep(1, dim(x1)[1]),
      rep(-1, dim(x2)[1]))
plot(x, col = y + 3, pch=20)
dat = data.frame(x, y=as.factor(y))

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "polynomial", #como no podemos trazar una linea, usamos un kernel polinomial
  degree = 2, # probemos con uno de grado 2 primero
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

# Gaussiano
svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", #como no podemos trazar una linea, usamos un kernel polinomial
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

# Ejemplo 2
set.seed(434)
x <- matrix(rnorm(400), 200, 2)
y <- as.numeric(c(xor(x[,1] > 0, x[,2] > 0.55)))
plot(x, col = y + 3, pch = 19)
dat = data.frame(x, y=as.factor(y))

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "polynomial", #como no podemos trazar una linea, usamos un kernel polinomial
  degree = 4, # probemos con uno de grado 4 primero
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) 
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)


svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "polynomial", 
  degree = 5,
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) 
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "polynomial", 
  degree = 6,
  cost = 10, # cost = c
  scale = FALSE)
print(svmfit) 
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)

# Gaussiano
svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", 
  cost = 10, # cost = c
  scale = FALSE)
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)

# Ahora, juguemos con gamma. por default, es 1/(tamaño de datos)
svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", 
  cost = 10, # cost = c
  scale = FALSE,
  gamma = 1,
  )
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)

svmfit = svm(
  y ~ .,
  data = dat,
  kernel = "radial", 
  cost = 10, # cost = c
  scale = FALSE,
  gamma = 50,
)
print(svmfit) # indica num. de support vectors utilizados, ademas del kernel y tipo
xgrid = grids(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("green","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
