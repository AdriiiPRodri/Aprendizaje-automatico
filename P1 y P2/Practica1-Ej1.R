# Autor: Adrián Jesús Peña Rodríguez

options(digits=10)

# Ejercicio 1:

# APARTADO A

# Algoritmo gradiente descendente para una variable, ejercicio 1.a

GD = function(f, fp, wini, nitr = 10000, nu, umbral_parada) {
  numero_de_iteraciones = 0
  
  while(numero_de_iteraciones < nitr & f(wini) > umbral_parada) {
    wini = wini - nu * fp(wini)
    
    numero_de_iteraciones = numero_de_iteraciones + 1
  }
  
  print(sprintf("Iteraciones = %s", numero_de_iteraciones))
  print(sprintf("Minimo en %s , %s", wini))
  print(sprintf("Mínimo encontrado = %s", f(wini)))
}


#################################################

# Ejercicio 1.2

# Implementamos el algoritmo de gradiente descendente para dos variables

GD2 = function(f, fdu, fdv, wini = c(0, 0), nitr = 100000, nu, umbral_parada) {
  numero_de_iteraciones = 0
  wini_x = c()
  wini_y = c()
  
  while(numero_de_iteraciones < nitr & umbral_parada < f(wini[1], wini[2])) {
    wini_ahora = wini
    
    wini[1] = wini_ahora[1] - nu * fdu(wini_ahora[1], wini_ahora[2])
    wini[2] = wini_ahora[2] - nu * fdv(wini_ahora[1], wini_ahora[2])
    
    wini_x[numero_de_iteraciones + 1] = wini[1]
    wini_y[numero_de_iteraciones + 1] = wini[2]
    
    numero_de_iteraciones = numero_de_iteraciones + 1
  }
  
  print(sprintf("Iteraciones = %s", numero_de_iteraciones))
  print(sprintf("Minimo en %s , %s", wini[1], wini[2]))
  print(sprintf("Mínimo encontrado = %s", f(wini[1], wini[2])))
  
  cbind(wini_x, wini_y)
}

# Declaramos la función y sus derivadas parciales

f = function(u, v) {
  (u^3 * exp(v-2) - 4 * v^3 * exp(-u))^2
}

fdu = function(u, v) {
  2*(u^3 * exp(v-2) - 4*v^3 * exp(-u)) * (3*exp(v-2) * u^2 + 4*v^3*exp(-u))
}

fdv = function(u, v) {
  2*(u^3 * exp(v-2) - 4*v^3 * exp(-u)) * (u^3*exp(v-2) - 12*exp(-u)*v^2)
}

x = c(-100:100)
y = c(-100:100)
plot(f(x,y),col="blue")

borrar = GD2(f, fdu, fdv, c(1, 1), 100000, 0.05, 10*10^(-14))

#################################################

# Ejercicio 1.3

# Declaramos la función y sus derivadas parciales

f = function(x, y) {
  (x - 2)^2 + 2*(y + 2)^2 + 2 * sin(2*pi*x)*sin(2*pi*y)
}

fdx = function(x, y) {
  4*pi*cos(2*pi*x)*sin(2*pi*y)+2*(x-2)
}

fdy = function(x, y) {
  4*pi*sin(2*pi*x)*cos(2*pi*y)+4*(y+2)
}

old.par = par(mfrow=c(1,2))
print("Punto de comienzo (1, 1)")
puntos = GD2(f, fdx, fdy, c(1, 1), 50, 0.01, -5)
plot(puntos[,1], puntos[,2], col = "red", sub="(1, 1)")
puntos_2 = GD2(f, fdx, fdy, c(1, 1), 50, 0.1, -5)
plot(puntos_2[,1], puntos_2[,2], col = "red", sub="(1, 1)")

old.par = par(mfrow=c(2,2))
puntos = GD2(f, fdx, fdy, c(2.1, -2.1), 50, 0.01, -5)
plot(f(puntos[,1], puntos[,2]), col = "red", type = "b", sub="(2.1, -2.1)")
puntos_2 = GD2(f, fdx, fdy, c(2.1, -2.1), 50, 0.1, -5)
plot(f(puntos_2[,1], puntos_2[,2]), col = "red", type = "b", sub="(2.1, -2.1)")

puntos = GD2(f, fdx, fdy, c(3, -3), 50, 0.01, -5)
plot(f(puntos[,1], puntos[,2]), col = "red", type = "b", sub="(3, -3)")
puntos_2 = GD2(f, fdx, fdy, c(3, -3), 50, 0.1, -5)
plot(f(puntos_2[,1], puntos_2[,2]), col = "red", type = "b", sub="(3, -3)")

puntos = GD2(f, fdx, fdy, c(1.5, -1.5), 50, 0.01, -5)
plot(f(puntos[,1], puntos[,2]), col = "red", type = "b", sub="(1.5, -1.5)")
puntos_2 = GD2(f, fdx, fdy, c(1.5, -1.5), 50, 0.1, -5)
plot(f(puntos_2[,1], puntos_2[,2]), col = "red", type = "b")

puntos = GD2(f, fdx, fdy, c(1, -1), 50, 0.01, -5)
plot(f(puntos[,1], puntos[,2]), col = "red", type = "b", sub="(1, -1)")
puntos_2 = GD2(f, fdx, fdy, c(1, -1), 50, 0.1, -5)
plot(f(puntos_2[,1], puntos_2[,2]), col = "red", type = "b", sub="(1, -1)")

rm(puntos, puntos_2, old.par)

#pintar_frontera(f) ???

#################################################

# Ejercicio 1.4 -> Responder en PDF

# ^^
