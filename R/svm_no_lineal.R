# Nuestro working directory será donde esté el archivo, para tener acceso al .rda
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file = "ESL.mixture.rda")
names(ESL.mixture)
rm(x,y)
attach(ESL.mixture)

plot(x, col= y + 1)
# Notemos que hay mucho más overlap de datos