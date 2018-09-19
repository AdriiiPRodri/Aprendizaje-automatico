set.seed(3)

simula_unif = function (N=2,dim=2, rango = c(0,1)){
  m = matrix(runif(N*dim, min=rango[1], max=rango[2]), nrow = N, ncol=dim, byrow=T)
  m
}

simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

simula_recta = function (intervalo = c(-1,1), visible=F){
  
  ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
  b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte
  
  if (visible) {  # pinta la recta y los 2 puntos
    if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    points(ptos,col=3)  #pinta en verde los puntos
    abline(b,a,col=3)   # y la recta
  }
  c(a,b) # devuelve el par pendiente y punto de corte
}

pintar_frontera = function(f,rango=c(-50,50)) {
  x=y=seq(rango[1],rango[2],length.out = 500)
  z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
    plot(1, type="n", xlim=rango, ylim=rango)
  contour(x,y,z, levels = 1:20, xlim =rango, ylim=rango, xlab = "x", ylab = "y", add=TRUE, col="green")
}

f = function(x,y,a,b){
  if (y - a * x - b >= 0)
    1
  else
    -1
}

f1 = function(x, y){
  (x-10)^2 + (y-20)^2 - 400
}

f2 = function(x,y){
  0.5 * (x+10)^2 + (y-20)^2 - 400
}

f3 = function(x,y){
  0.5 * (x-10)^2 - (y+20)^2 - 400
}

f4 = function(x,y){
  y - 20*x^2 - 5*x + 3
}

genera_etiquetas_y_ruido = function(muestra, porcentaje) {
  N = length(muestra)
  posiciones = sample(1:length(muestra),length(muestra),replace=F)
  positivas = 0
  negativas = 0
  tope = length(muestra) * porcentaje

  for(i in 1:N) {
    if(muestra[posiciones[i]] == 1 & positivas < tope) {
      muestra[posiciones[i]] = muestra[posiciones[i]] * -1
      positivas = positivas + 1
    }
    else if(muestra[posiciones[i]] == -1 & negativas < tope) {
      muestra[posiciones[i]] = muestra[posiciones[i]] * -1
      negativas = negativas + 1
    }
  }
  
  muestra
}

# 1.- EJERCICIOS SOBRE LA COMLEJIDAD DE H Y EL RUIDO

# Ejercicio 1
# a)
unif = simula_unif(N=50,dim=2,rango=c(-50,50))
unif = cbind(unif, 1)
plot(unif, col="green")
# b)
gaus = simula_gaus(N=50,dim=2,sigma=c(5,7))
plot(gaus,col="red")

# Ejercicio 2
signos = c()

recta = simula_recta(visible=F)

for(i in 1:dim(unif)[1]){
  signos[i] = f(unif[i,1], unif[i,2], recta[1], recta[2])
}

# a)
plot(unif, col = signos + 3)
abline(recta[2],recta[1],col=3)

# b)
signos_ruido = genera_etiquetas_y_ruido(signos, 0.1)

plot(unif, col = signos_ruido + 3)
abline(recta[2],recta[1],col=3)

# Ejercicio 3
old.par = par(mfrow=c(2,2))
intervalo = c(-100,100)

plot(unif, col = signos_ruido + 3)
pintar_frontera(f1, intervalo)

plot(unif, col = signos_ruido + 3)
pintar_frontera(f2, intervalo)

plot(unif, col = signos_ruido + 3)
pintar_frontera(f3, intervalo)

plot(unif, col = signos_ruido + 3)
pintar_frontera(f4, intervalo)

# 2.- MODELOS LINEALES
# Ejercicio 1.- Algoritmo Perceptron

pasoARecta= function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[2]
  b = -w[3]/w[2]
  c(a,b)
}

prediccion = function(w, x) {
  sign(t(w) %*% t(t(x)))
}

ajusta_PLA = function(datos, label, max_iter = 10000, vini = c(0,0,0)) {
  weight = vini
  iter = 0
  converge = F
  
  while(converge == F & iter < max_iter) {
    converge = T
    for(i in 1:dim(datos)[1]) {
      mult = prediccion(weight, datos[i, ])
      
      if(mult != label[i]) {
        weight = weight + label[i] * datos[i, ]
        converge = F
      }
    }
    iter = iter + 1
  }
  
  fallos = 0
  
  if(converge == F) {
    for(i in 1:dim(datos)[1]) {
      mult = prediccion(weight, datos[i, ])
      
      if(mult != label[i]) {
        fallos = fallos + 1
      }
    }
  }
  c(fallos, iter, weight)
}

old.par = par(mfrow=c(1,1))

# a)
vini = rep(0, dim(unif)[2])
max_iter = 10000
pla = ajusta_PLA(unif, signos, max_iter, vini)

plot(unif, col = signos + 3)
abline(pasoARecta(pla[3:5])[2], pasoARecta(pla[3:5])[1], col = "green")

print("PLA con pesos iniciados a 0:")
print(sprintf("Error = %s", pla[1] / dim(unif)[1]))
print(sprintf("Iteraciones hasta converger = %s", pla[2]))

##############

media = 0
media_f = 0
iteraciones = 10

for(i in 1:iteraciones) {
  pesos_iniciales = runif(n=dim(unif)[2], min=0, max=1)
  
  pla = ajusta_PLA(unif, signos, max_iter, pesos_iniciales)
  media = media + pla[2]
  media_f = media_f + pla[1]
}

media = media / iteraciones
media_f = media_f / iteraciones

print("PLA con pesos iniciados aleatoriamente, media de las 10 ejecuciones:")
print(sprintf("Error = %s", media_f/dim(unif)[1]))
print(sprintf("Media de iteraciones hasta converger = %s", media))

##############

# b)

pesos_iniciales = rep(0, dim(unif)[2])
pla = ajusta_PLA(unif, signos_ruido, max_iter, pesos_iniciales)

plot(unif, col = signos_ruido + 3)
abline(pasoARecta(pla[3:5])[2], pasoARecta(pla[3:5])[1], col = "green")

print("PLA con pesos a cero y ruido")
print(sprintf("Error = %s", pla[1]/dim(unif)[1]))
print(sprintf("Iteraciones hasta converger = %s", pla[2]))

##############

media = 0
media_f = 0

for(i in 1:iteraciones) {
  pesos_iniciales = runif(n=dim(unif)[2], min=0, max=1)
  
  pla = ajusta_PLA(unif, signos_ruido, max_iter, pesos_iniciales)
  media = media + pla[2]
  media_f = media_f + pla[1]
}

media = media / iteraciones
media_f = media_f / iteraciones

print("PLA con pesos iniciados aleatoriamente, media de las 10 ejecuciones:")
print(sprintf("Error = %s", media_f/dim(unif)[1]))
print(sprintf("Media de iteraciones hasta converger = %s", media))

##############

# Ejercicio 2.- Regresión logística

genera_recta = function (x, y){
  a = (x[2] - y[2]) / (x[1]-y[1]) # calculo de la pendiente
  b = x[2]-a*x[1]  # calculo del punto de corte
  c(a,b) # devuelve el par pendiente y punto de corte
}

fSGD = function(x,y,a,b){
  if (-y + a * x + b >= 0)
    1
  else
    -1
}

# Preparando datos
datosRL = simula_unif(N=100,dim=2,rango=c(0,2))
datosRL = cbind(datosRL, 1)
puntos = sample(1:dim(datosRL)[1], 2, replace = FALSE)
rectaE = genera_recta(datosRL[puntos[1],], datosRL[puntos[2],])
x = c(min(datosRL[,1]),max(datosRL[,1]))
y = c(rectaE[1]*x[1] + rectaE[2], rectaE[1]*x[2] + rectaE[2])

etiquetas = c()

for(i in 1:dim(datosRL)[1]) {
  etiquetas[i] = fSGD(datosRL[i, 1], datosRL[i, 2], rectaE[1], rectaE[2])
}

plot(datosRL, col = etiquetas + 3)
lines(x, y, type = "l", col = "green")
####################

# Gradiente descendiente estocástico

h = function(etiqueta, w, x) {   # Funcion sigmoide, esta es la h de la regresión logística
  #(exp(etiqueta*w*x) / (exp(etiqueta*w*x)+1))
  1 / (1 + exp(-(etiqueta*t(w)%*%x)))
}

SGD = function(datos, resultados, wini = c(0, 0, 0), nitr = 100000, nu = 0.01, tminibatches, tope = 0.01) {
  numero_de_iteraciones = 0
  sumatoria = c(0,0,0)
  N = as.integer(length(resultados) * tminibatches)
  wini_anterior = c(999,999,999)
  
  while(abs(sum(wini_anterior) - sum(wini)) >= tope) {
    sumatoria = c(0,0,0)
    wini_anterior = wini
    for(j in 1:N) {
      aleatorio = sample(length(resultados),1)
      sumatoria = sumatoria + (-as.vector(resultados[aleatorio] * datos[aleatorio,])) * as.vector(h(-resultados[aleatorio], wini, datos[aleatorio,]))
    }
    
    sumatoria = -sumatoria / N
    wini = wini + nu * sumatoria
    numero_de_iteraciones = numero_de_iteraciones + 1
  }

  fallos = 0
  for(i in 1:dim(datos)[1]) {
    mult = prediccion(wini, datos[i, ])
    
    if(mult != etiquetas[i]) {
      fallos = fallos + 1
    }
  }
  wini
}

pesosSGD = rep(0, dim(datosRL)[2])  # Pesos inicializados a 0
pesosSGD = SGD(datos = datosRL, resultados = etiquetas, nitr = 10000, tminibatches = 0.2, tope = 0.0000001)

x = c(min(datosRL[,1]),max(datosRL[,1]))
y = c(pasoARecta(pesosSGD)[1]*x[1] + pasoARecta(pesosSGD)[2], pasoARecta(pesosSGD)[1]*x[2] + pasoARecta(pesosSGD)[2])
plot(datosRL, col = etiquetas + 3)
abline(pasoARecta(pesosSGD)[2], pasoARecta(pesosSGD)[1], col = "green")

# b)
datosRL_out = simula_unif(N=1000,dim=2,rango=c(0,2))
datosRL_out = cbind(datosRL_out, 1)
x = c(min(datosRL_out[,1]),max(datosRL_out[,1]))
y = c(rectaE[1]*x[1] + rectaE[2], rectaE[1]*x[2] + rectaE[2])

etiquetas = c()

for(i in 1:dim(datosRL_out)[1]) {
  etiquetas[i] = fSGD(datosRL_out[i, 1], datosRL_out[i, 2], rectaE[1], rectaE[2])
}

plot(datosRL_out, col = etiquetas + 3)
lines(x, y, type = "l", col = "green")

fallos = 0
for(i in 1:dim(datosRL_out)[1]) {
  mult = prediccion(pesosSGD, datosRL_out[i, ])
  
  if(mult >= 0.5)
    mult = 1
  else
    mult = -1
  
  if(mult != etiquetas[i]) {
    fallos = fallos + 1
  }
}
print(sprintf("Error = %s",fallos/dim(datosRL_out)[1]))

plot(datosRL_out, col = etiquetas + 3)
abline(pasoARecta(pesosSGD)[2], pasoARecta(pesosSGD)[1], col = "green")





# Bonus:

fSimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

prediccion2 = function(w, x) {
  sign(t(w) %*% t(x))
}

digit.train <- read.table("datos/zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE)
digit.test <- read.table("datos/zip.test", quote="\"", comment.char="", stringsAsFactors=TRUE)

# Ahora nos quedamos solamente con los 1 y los 5
digitos48.train = digit.train[digit.train$V1==4 | digit.train$V1==8,]
digitos48.test = digit.test[digit.test$V1==4 | digit.test$V1==8,]

digitos.train = digitos48.train[,1]    # vector de etiquetas(clases) del train, en este caso columna 1
ndigitos.train = nrow(digitos48.train)  # numero de muestras del train
digitos.test = digitos48.test[,1]    # vector de etiquetas(clases) del test, en este caso columna 1
ndigitos.test = nrow(digitos48.test)  # numero de muestras del test

# se retira la clase y se monta una matriz 3D: 599*16*16
grises.train = array(unlist(subset(digitos48.train,select=-V1)),c(ndigitos.train,16,16)) # Cada numero esta representado en una matriz 16x16
rm(digit.train)
rm(digitos48.train)
# Ahora con el test
grises.test = array(unlist(subset(digitos48.test,select=-V1)),c(ndigitos.test,16,16))
rm(digit.test)
rm(digitos48.test)

# 2º. Vamos a obtener la intensidad media de nuestro dataset:

intesidad.train = apply(X = grises.train, MARGIN = 1, FUN = mean) # Intensidad de cada uno de los numeros(599)
intesidad.test = apply(X = grises.test, MARGIN = 1, FUN = mean)

# 3º. Vamos a obtener la simetria respecto al eje vertical de nuestro dataset:

simetria.train = apply(X = grises.train, MARGIN = 1, FUN = fSimetria) # Simetria de cada uno de los numeros(599)
simetria.test = apply(X = grises.test, MARGIN = 1, FUN = fSimetria) # Simetria de cada uno de los numeros(599)

rm(grises.train) # Liberamos memoria pues ya no necesitamos a grises
rm(grises.test)

# 4º. Vamos a recodificar nuestras etiquetas, cambiando el 5 por -1

digitos.train[digitos.train == 4] = -1
digitos.test[digitos.test == 4] = -1
digitos.train[digitos.train == 8] = 1
digitos.test[digitos.test == 8] = 1

# 5º. Componemos datosTr, que es la unión por columnas de la intensidad con la simetria, columna 1 = intesidad, columna 2 = simetria

datosTr = as.matrix(cbind(intesidad.train, simetria.train))
datosTst = as.matrix(cbind(intesidad.test, simetria.test))

# Introducimos la variable independiente (1) en nuestros datos de intesidad y simetria
datosTr = cbind(datosTr, 1)
datosTst = cbind(datosTst, 1)

old.par = par(mfrow=c(1,2))
plot(intesidad.train, simetria.train, col = digitos.train + 3)
plot(intesidad.test, simetria.test, col = digitos.test + 3)


# Gradiente descendiente estocástico

SGD_noRL = function(datos, resultados, wini = c(0, 0, 0), nitr = 100000, nu, tminibatches) {
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
max_iter = 100
pesos_2a_sgd = SGD_noRL(datosTr, digitos.train, c(0, 0, 0), max_iter, 0.01, 0.5)  # Calculo de los pesos mediante SGD
fallos_tr = 0
for(i in 1:dim(datosTr)[1]) {   # Medimos Ein con el vector de pesos actual
  mult = prediccion(pesos_2a_sgd, datosTr[i, ])
  
  if(mult != digitos.train[i]) {
    fallos_tr = fallos_tr + 1
  }
}

cuatros = length(digitos.train[digitos.train == -1])
ochos = length(digitos.train[digitos.train == 1])

# Imprimimos por pantalla las dos gráficas y la recta de regresión donde podemos ver que ocurre
old.par = par(mfrow=c(1,2))
plot(datosTr, col = digitos.train + 3, type = "p", ylim=c(-250,50))
abline(pasoARecta(pesos_2a_sgd)[2], pasoARecta(pesos_2a_sgd)[1], col = "green")
print(sprintf("Ein = %s", fallos_tr/dim(datosTr)[1]))
print(sprintf("Nº de cuatros = %s", cuatros))
print(sprintf("Nº de ochos = %s", ochos))

cuatros = length(digitos.test[digitos.test == -1])
ochos = length(digitos.test[digitos.test == 1])
fallos_tr = 0
for(i in 1:dim(datosTst)[1]) {   # Medimos Ein con el vector de pesos actual
  mult = prediccion(pesos_2a_sgd, datosTst[i, ])
  
  if(mult != digitos.test[i]) {
    fallos_tr = fallos_tr + 1
  }
}

plot(datosTst, col = digitos.test + 3, type = "p", ylim=c(-600,50))
abline(pasoARecta(pesos_2a_sgd)[2], pasoARecta(pesos_2a_sgd)[1], col = "green")
print(sprintf("Etest = %s", fallos_tr/dim(datosTst)[1]))
print(sprintf("Nº de cuatros = %s", cuatros))
print(sprintf("Nº de ochos = %s", ochos))

## Comprobacion funcionamiento
pesos_2a_sgd = SGD_noRL(unif, signos, c(0, 0, 0), 1000, 0.01, 0.5)  # Calculo de los pesos mediante SGD
plot(unif, col = signos + 3)
abline(pasoARecta(pesos_2a_sgd)[2], pasoARecta(pesos_2a_sgd)[1], col = "green")

pocket = function(datos, label, max_iter, vini) {
  pesos_pla = vini
  iter = 0
  fallos = 0
  fallos_anterior = length(datos)
  
  for(j in 1:max_iter) {
    for(i in 1:dim(datos)[1]) {
      mult = prediccion(pesos_pla, datos[i, ])  # Vemos si nuestro vector de pesos clasifica bien la muestra actual
        
      if(mult != label[i]) {  # Si no la clasifica bien ajustamos
        pesos_pla = pesos_pla + label[i] * datos[i, ]
        
        for(i in 1:dim(datos)[1]) {   # Medimos Ein con el vector de pesos actual
          mult = prediccion(pesos_pla, datos[i, ])
          
          if(mult != label[i]) {
            fallos = fallos + 1
          }
        }
        
        if(fallos < fallos_anterior) {
          weight = pesos_pla
        }
        fallos_anterior = fallos
        fallos = 0
      }
    }
  }
  
  weight
}

vini = rep(0, dim(datosTr)[2])
old.par = par(mfrow=c(1,2))
pock = pocket(datosTr, digitos.train, max_iter, vini)
plot(datosTr, col = signos + 3, ylim=c(-250,50))
abline(pasoARecta(pock)[2], pasoARecta(pock)[1], col = "green")

fallos_tr = 0
for(i in 1:dim(datosTr)[1]) {   # Medimos Ein con el vector de pesos actual
  mult = prediccion(pock, datosTr[i, ])
  
  if(mult != digitos.train[i]) {
    fallos_tr = fallos_tr + 1
  }
}

cuatros = length(digitos.train[digitos.train == -1])
ochos = length(digitos.train[digitos.train == 1])

print(sprintf("Ein = %s", fallos_tr/dim(datosTr)[1]))
print(sprintf("Nº de cuatros = %s", cuatros))
print(sprintf("Nº de ochos = %s", ochos))

plot(datosTst, col = digitos.test + 3, ylim=c(-600,50))
abline(pasoARecta(pock)[2], pasoARecta(pock)[1], col = "green")
print(sprintf("Iteraciones de ambos algortimos = %s", max_iter))

cuatros = length(digitos.test[digitos.test == -1])
ochos = length(digitos.test[digitos.test == 1])
fallos_tr = 0
for(i in 1:dim(datosTst)[1]) {   # Medimos Ein con el vector de pesos actual
  mult = prediccion(pock, datosTst[i, ])
  
  if(mult != digitos.test[i]) {
    fallos_tr = fallos_tr + 1
  }
}

print(sprintf("Etest = %s", fallos_tr/dim(datosTst)[1]))
print(sprintf("Nº de cuatros = %s", cuatros))
print(sprintf("Nº de ochos = %s", ochos))