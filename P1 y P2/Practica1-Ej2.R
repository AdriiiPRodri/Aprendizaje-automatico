# Autor: Adrián Jesús Peña Rodríguez

set.seed(4)

# Ejercicio 2:

# APARTADO 1

# Función que calcula la simetria de cada ejemplo de nuestro dataset
fSimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

pasoARecta = function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[2]
  b = -w[3]/w[2]
  c(a,b)
}

# Funcion que devuelve unas predicciones en funcion de los pesos hallados anteriormente
h = function(w, x) {
  sign(t(w) %*% t(x))
}


# Preparacion de los datos
# Preparacion de los datos
# 1º. Lectura zip del train y del test y asignación a una variable

digit.train <- read.table("datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
digit.test <- read.table("datos/zip.test", quote="\"", comment.char="", stringsAsFactors=TRUE)

# Ahora nos quedamos solamente con los 1 y los 5
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]

digitos.train = digitos15.train[,1]    # vector de etiquetas(clases) del train, en este caso columna 1
ndigitos.train = nrow(digitos15.train)  # numero de muestras del train
digitos.test = digitos15.test[,1]    # vector de etiquetas(clases) del test, en este caso columna 1
ndigitos.test = nrow(digitos15.test)  # numero de muestras del test

# se retira la clase y se monta una matriz 3D: 599*16*16
grises.train = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos.train,16,16)) # Cada numero esta representado en una matriz 16x16
rm(digit.train)
rm(digitos15.train)
# Ahora con el test
grises.test = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos.test,16,16))
rm(digit.test)
rm(digitos15.test)

# 2º. Vamos a obtener la intensidad media de nuestro dataset:

intesidad.train = apply(X = grises.train, MARGIN = 1, FUN = mean) # Intensidad de cada uno de los numeros(599)
intesidad.test = apply(X = grises.test, MARGIN = 1, FUN = mean)

# 3º. Vamos a obtener la simetria respecto al eje vertical de nuestro dataset:

simetria.train = apply(X = grises.train, MARGIN = 1, FUN = fSimetria) # Simetria de cada uno de los numeros(599)
simetria.test = apply(X = grises.test, MARGIN = 1, FUN = fSimetria) # Simetria de cada uno de los numeros(599)

rm(grises.train) # Liberamos memoria pues ya no necesitamos a grises
rm(grises.test)

# 4º. Vamos a recodificar nuestras etiquetas, cambiando el 5 por -1

digitos.train[digitos.train == 5] = -1
digitos.test[digitos.test == 5] = -1

# 5º. Componemos datosTr, que es la unión por columnas de la intensidad con la simetria, columna 1 = intesidad, columna 2 = simetria

datosTr = as.matrix(cbind(intesidad.train, simetria.train))
datosTst = as.matrix(cbind(intesidad.test, simetria.test))

# Introducimos la variable independiente (1) en nuestros datos de intesidad y simetria
indep = vector("numeric", length = ndigitos.train)
indep_test = vector("numeric", length = ndigitos.test)
indep[indep == 0] = 1
indep_test[indep_test == 0] = 1
datosTr = cbind(datosTr, indep)
datosTst = cbind(datosTst, indep_test)

old.par = par(mfrow=c(1,2))
plot(intesidad.train, simetria.train, col = digitos.train + 3)
plot(intesidad.test, simetria.test, col = digitos.test + 3)

rm(indep, indep_test)



# REGRESION CON PSEUDO-INVERSA
# Regress_Lin, para la obtención de pesos del modelo lineal
Regress_Lin = function(datos, digitos){
  pseudo = svd(datos)
  pinversa = pseudo$v %*% diag(1/pseudo$d) %*% t(pseudo$u) # Formula para obtener la pseudo inversa
  return (pinversa %*% digitos)
}

pesos_2a_svd = Regress_Lin(datosTr, digitos.train)  # Calculo de los pesos mediante la pseudo-inversa

prediccion_train_2a_svd = h(pesos_2a_svd, datosTr) # Atraves de los pesos hallados con anterioridad damos una prediccion sobre el dataset de train

fallos_train_2a_svd = prediccion_train_2a_svd == digitos.train   # Vemos cuantos aciertos hemos tenido

Ein_2a_svd = length(fallos_train_2a_svd[fallos_train_2a_svd != 1]) / ndigitos.train  # Elegimos solo los fallos y los dividimos entre el tamaño total de la muestra

prediccion_test_2a_svd = h(pesos_2a_svd, datosTst) # Volvemos a repetir para el test y así poder obtener el Eout_2a_svd

fallos_test_2a_svd = prediccion_test_2a_svd == digitos.test

Eout_2a_svd = length(fallos_test_2a_svd[fallos_test_2a_svd != 1]) / ndigitos.test

# Imprimimos por pantalla las dos gráficas y la recta de regresión donde podemos ver que ocurre
old.par = par(mfrow=c(1,2))
plot(intesidad.train, simetria.train, col = digitos.train + 3, type = "p", xlim = c(min(datosTr[,1]),max(datosTr[,1])), ylim = c(min(datosTst[,2]), 0))
x = c(min(datosTr[,1]),max(datosTr[,1]))
y = c(pasoARecta(pesos_2a_svd)[1]*x[1] + pasoARecta(pesos_2a_svd)[2], pasoARecta(pesos_2a_svd)[1]*x[2] + pasoARecta(pesos_2a_svd)[2])
lines(x, y, type = "l", col = "green")

plot(datosTst, col = digitos.test + 3, type = "p", xlim = c(min(datosTr[,1]),max(datosTr[,1])), ylim = c(min(datosTst[,2]), 0))
x = c(min(datosTr[,1]),max(datosTr[,1]))
y = c(pasoARecta(pesos_2a_svd)[1]*x[1] + pasoARecta(pesos_2a_svd)[2], pasoARecta(pesos_2a_svd)[1]*x[2] + pasoARecta(pesos_2a_svd)[2])
lines(x, y, type = "l", col = "green")

rm(x,y)



# Gradiente descendiente estocástico

SGD = function(datos, resultados, wini = c(0, 0, 0), nitr = 100000, nu, tminibatches) {
  numero_de_iteraciones = 0
  sumatoria = c(1,1,1)
  N = as.integer(length(resultados) * tminibatches)
  
  for(i in 1:nitr) {
    for(i in 1:N) {
      aleatorio = sample(length(resultados),1)
      sumatoria = sumatoria - (as.vector(resultados[aleatorio] * datos[aleatorio,])) * (as.vector((exp(-resultados[aleatorio] * t(wini) %*% datos[aleatorio,]))) / as.vector((1 + exp(-resultados[aleatorio] * t(wini) %*% datos[aleatorio,]))))
    }
    
    sumatoria = sumatoria / N
    wini = wini - nu * sumatoria
    numero_de_iteraciones = numero_de_iteraciones + 1
  }
  
  wini
}


# REGRESION CON GRADIENTE DESCENDENTE ESTOCÁSTICO
pesos_2a_sgd = SGD(datosTr, digitos.train, c(0, 0, 0), 100, 0.1, 0.2)  # Calculo de los pesos mediante SGD

prediccion_train_2a_sgd = h(pesos_2a_sgd, datosTr) # Atraves de los pesos hallados con anterioridad damos una prediccion sobre el dataset de train

fallos_train_2a_sgd = prediccion_train_2a_sgd == digitos.train   # Vemos cuantos aciertos hemos tenido

Ein_2a_sgd = length(fallos_train_2a_sgd[fallos_train_2a_sgd != 1]) / ndigitos.train  # Elegimos solo los fallos y los dividimos entre el tamaño total de la muestra

prediccion_test_2a_sgd = h(pesos_2a_sgd, datosTst) # Volvemos a repetir para el test y así poder obtener el Eout_2a_svd

fallos_test_2a_sgd = prediccion_test_2a_sgd == digitos.test

Eout_2a_sgd = length(fallos_test_2a_sgd[fallos_test_2a_sgd != 1]) / ndigitos.test

# Imprimimos por pantalla las dos gráficas y la recta de regresión donde podemos ver que ocurre
old.par = par(mfrow=c(1,2))
plot(intesidad.train, simetria.train, col = digitos.train + 3, type = "p", xlim = c(min(datosTr[,1]),max(datosTr[,1])), ylim = c(min(datosTst[,2]), 0))
x = c(min(datosTr[,1]),max(datosTr[,1]))
y = c(pasoARecta(pesos_2a_sgd)[1]*x[1] + pasoARecta(pesos_2a_sgd)[2], pasoARecta(pesos_2a_sgd)[1]*x[2] + pasoARecta(pesos_2a_sgd)[2])
lines(x, y, type = "l", col = "green")

plot(datosTst, col = digitos.test + 3, type = "p", xlim = c(min(datosTr[,1]),max(datosTr[,1])), ylim = c(min(datosTst[,2]), 0))
x = c(min(datosTr[,1]),max(datosTr[,1]))
y = c(pasoARecta(pesos_2a_sgd)[1]*x[1] + pasoARecta(pesos_2a_sgd)[2], pasoARecta(pesos_2a_sgd)[1]*x[2] + pasoARecta(pesos_2a_sgd)[2])
lines(x, y, type = "l", col = "green")

rm(x,y)

# ^^ 

# APARTADO 2

# por defecto genera 2 puntos(N) entre [0,1](rango) de 2 dimensiones(dims)

simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]), nrow = N, ncol=dims, byrow=T)
  m
}

# 2.a
old.par = par(mfrow=c(1,1))
muestra_entrenamiento = simula_unif(1000, 2, c(-1,1))
plot(muestra_entrenamiento, col = "red")

# 2.b

f = function(x1, x2) {
  sign((x1 - 0.2)^2 + x2^2 - 0.6)
}

genera_etiquetas_y_ruido = function(muestra) {
  devolver = c()
  N = dim(muestra)[1]
  
  for(i in 1:N) {
    devolver[i] = f(muestra[i,1],muestra[i,2])
  }
  
  aleatorios_d = sample(1:N) # Vamos a introducir ruido cambiando aleatoriamente el signo de un 10% de las etiquetas
  
  for(i in 1:N * 0.1) {
    devolver[aleatorios_d[i]] = devolver[aleatorios_d[i]] * -1
  }
  
  devolver
}

etiquetas = genera_etiquetas_y_ruido(muestra_entrenamiento)

old.par = par(mfrow=c(1,1))
plot(muestra_entrenamiento[,1], muestra_entrenamiento[,2], col = etiquetas + 3)

rm(old.par)

# 2.c

indep = vector("numeric", length = length(etiquetas))
indep[indep == 0] = 1
muestra_entrenamiento = cbind(indep, muestra_entrenamiento)

pesos_2c_sgd = SGD(muestra_entrenamiento, etiquetas, c(0, 0, 0), 1000, 0.05, 0.2)  # Calculo de los pesos mediante SGD

prediccion_2c_sgd = h(pesos_2c_sgd, muestra_entrenamiento) # Atraves de los pesos hallados con anterioridad damos una prediccion sobre el dataset de train

fallos_2c_sgd = prediccion_2c_sgd == etiquetas   # Vemos cuantos aciertos hemos tenido

Ein_2c_sgd = length(fallos_2c_sgd[fallos_2c_sgd != 1]) / length(etiquetas)  # Elegimos solo los fallos y los dividimos entre el tamaño total de la muestra

print(sprintf("Ein = %s", Ein_2c_sgd))

# Imprimimos por pantalla las dos gráficas y la recta de regresión donde podemos ver que ocurre
plot(muestra_entrenamiento[,2], muestra_entrenamiento[,3], col = etiquetas + 3, type = "p", xlim = c(min(muestra_entrenamiento[,2]),max(muestra_entrenamiento[,2])), ylim = c(min(muestra_entrenamiento[,3]), max(muestra_entrenamiento[,3])))
x = c(min(muestra_entrenamiento[,2]),max(muestra_entrenamiento[,2]))
y = c(pasoARecta(pesos_2c_sgd)[1]*x[1] + pasoARecta(pesos_2c_sgd)[2], pasoARecta(pesos_2c_sgd)[1]*x[2] + pasoARecta(pesos_2c_sgd)[2])
lines(x, y, type = "l", col = "green")

# 2.d
# Generamos 1000 muestras de 1000 componentes cada una
muestra_entrenamiento_d = list()
indep = vector("numeric", length = length(etiquetas))
indep[indep == 0] = 1
Ein_2d_sgd = 0
Eout_2d_sgd = 0

for(i in 1:1000) {
  # Entrenamiento
  muestra_entrenamiento_d = simula_unif(1000, 2, c(-1,1))
  etiquetas_d = genera_etiquetas_y_ruido(muestra_entrenamiento_d)
  muestra_entrenamiento_d = cbind(indep, muestra_entrenamiento_d)
  pesos_2d_sgd = SGD(muestra_entrenamiento_d, etiquetas_d, c(0, 0, 0), 10, 0.05, 0.2)  # Calculo de los pesos mediante SGD
  prediccion_2d_sgd = h(pesos_2d_sgd, muestra_entrenamiento_d) # Atraves de los pesos hallados con anterioridad damos una prediccion sobre el dataset de train
  fallos_2d_sgd = prediccion_2d_sgd == etiquetas_d   # Vemos cuantos aciertos hemos tenido
  Ein_2d_sgd = Ein_2d_sgd + length(fallos_2d_sgd[fallos_2d_sgd != 1]) / length(etiquetas_d)  # Elegimos solo los fallos y los dividimos entre el tamaño total de la muestra
  
  # Test
  muestra_entrenamiento_d = simula_unif(1000, 2, c(-1,1))
  etiquetas_d = genera_etiquetas_y_ruido(muestra_entrenamiento_d)
  muestra_entrenamiento_d = cbind(indep, muestra_entrenamiento_d)
  prediccion_2d_sgd = h(pesos_2d_sgd, muestra_entrenamiento_d) # Atraves de los pesos hallados con anterioridad damos una prediccion sobre el dataset de train
  fallos_2d_sgd = prediccion_2d_sgd == etiquetas_d   # Vemos cuantos aciertos hemos tenido
  Eout_2d_sgd = Eout_2d_sgd + length(fallos_2d_sgd[fallos_2d_sgd != 1]) / length(etiquetas_d)  # Elegimos solo los fallos y los dividimos entre el tamaño total de la muestra
}

Ein_2d_sgd_media = Ein_2d_sgd / 1000
Eout_2d_sgd_media = Eout_2d_sgd / 1000
rm(Ein_2d_sgd, Eout_2d_sgd, i)

# Apartado e, explicar que no es un buen ajuste pues no conseguimos una buena tasa de acierto en el train ni en el test 60% fallos