#PPED_Anexo_Proyecciones_Poblacion_Nacional2018_2070 Pre covid
#1
pob_pre= data.frame(PPED_PreCovid_Anexo_Proyecciones_Poblacion_Nacional2018_2070)

head(pob_pre)
dim(pob_pre)

#2
pob_pre[, 99]
pob_pre[5:109, 100]

#3
base_NacPre = data.frame(pob_pre[3:159, ])
head(base_NacPre)

#4
proy_NacPre = data.frame(base_NacPre$AÑO,base_NacPre$ÁREA.GEOGRÁFICA, base_NacPre$Total.Mujeres,
                                 base_NacPre$Total.Hombres,base_NacPre$Total.General)
head(proy_NacPre)
proy_NacPre2=NULL
proy_NacPre2[1]=sum(as.numeric(proy_NacPre[which((
  proy_NacPre$base_NacPre.AÑO == 2018)& (proy_NacPre$base_NacPre.ÁREA.GEOGRÁFICA=="TOTAL")),5]))

for (i in 1:52){
  proy_NacPre2[i]= sum(as.numeric(proy_NacPre[which((
    proy_NacPre$base_NacPre.AÑO == 2017+i)&(proy_NacPre$base_NacPre.ÁREA.GEOGRÁFICA=="TOTAL")),5]))
}
proy_NacPre2

#5
plot(2018:2069,proy_NacPre2, type="p",col="red",pch=20, ylim = c(40000000, max(proy_NacPre2)))

#6
#install.packages("npregfast")
library(npregfast)
#?frfast

#7

proy_NacPre2[1]

poblacionPre_NA = c(
  rep(NA, 51), proy_NacPre2[1],  # 2018
  rep(NA, 51), proy_NacPre2[2],  # 2019
  rep(NA, 52), proy_NacPre2[3],  # 2020
  rep(NA, 51), proy_NacPre2[4],  # 2021
  rep(NA, 51), proy_NacPre2[5],  # 2022
  rep(NA, 51), proy_NacPre2[6],  # 2023
  rep(NA, 51), proy_NacPre2[7]  # 2024
)
length(poblacionPre_NA)

#8
plot(poblacionPre_NA, pch = 20, ylim = c(0, max(na.omit(poblacionPre_NA))))


#9
datosFinales_pre = data.frame(año = (2018:2024), poblacion =
                             proy_NacPre2[1:7])
datosFinales_pre

#10
regresion_Pre = frfast(datosFinales_pre[, 2] ~ datosFinales_pre[, 1], model =
                        "np",
                      p = 3, smooth = "kernel", kbin = (365-52))
#11
imputacion_Pre = data.frame(regresion_Pre$p)

#12
length(regresion_Pre$x)

plot(regresion_Pre$x, poblacionPre_NA[52:364], pch = 20,
     ylim = c(7000000, max(na.omit(poblacionPre_NA))), col = "orchid")

lines(regresion_Pre$x, imputacion_Pre$X1, lwd = 2, col = "skyblue2")

#13
fechas = seq(as.Date("2018-12-30"), as.Date("2024-12-28"), by = "week")


#14
imputacion_Pre$X1[which(fechas == as.Date("2023-04-23"))]

#15
length(imputacion_Pre$X3)

plot(fechas, imputacion_Pre$X3, lwd = 2, type = "l")

abline(h = 0, lwd = 2, col = "red")
abline(v = as.Date("2023-08-06"), lwd = 2, col = "blue")


data.frame(fechas, imputacion_Pre$X3)
plot(fechas, imputacion_Pre$X2, lwd = 2, type = "l")

x <- regresion_Pre$x # Tiempo (ej. años como fracción)
y <- imputacion_Pre$X1 # Población imputada suavizada
plot(x, y, main = "Datos observados", xlab = "x", ylab = "y", pch = 19)

#16
A_init <- 6 # Asíntota superior (capacidad máxima esperada)
B_init <- 0.12 # Desplazamiento horizontal (ligado al punto deinflexión)
C_init <- 0.05 # Tasa de crecimiento

#17
x1 <- (x - 2018)[1:311] # Se traslada el eje temporal para que 2018 sea el origen
y1 <- na.omit(y / 10000000) # Se escala la población a millones para facilitar el ajuste numérico

#18

GompertzModel_Pre <- nls(
  y1 ~ A * exp(-B * exp(-C * x1)),
  start = list(A = 5, B = 1, C = 0.05), # valores iniciales razonables
  control = list(maxiter = 500)
)

GompertzModel_Pre

summary(GompertzModel_Pre)

plot(predict(GompertzModel_Pre))
lines(y1, lwd = 2, col = "red")


modelo_logistico1 <- nls(y1 ~ a / (1 + b * exp(-c * x1)), 
                        start = list(a = A_init, b = B_init, c = 0.2))

summary(modelo_logistico1)


# Ajuste de Richards
# Ecuación: y = a / (1 + b * exp(-c * x1))^(1/m)
modelo_richards1 <- nls(
  y1 ~ a / (1 + b * exp(-c * x1))^(1/m),
  start = list(a = A_init, b = B_init, c = 0.2, m = 0.5),
  algorithm = "port",
  lower = c(1, 0.001, 0.001, 0.01),
  upper = c(Inf, Inf, Inf, 6.5),
  control = list(maxiter = 1000, minFactor = 1/2048)
)

summary(modelo_richards1)



# 1. Definir los años de interés (cada 5 años)
# 1. Crear la secuencia base de 5 en 5 hasta 2068
años_base <- seq(2018, 2068, by = 5)

# 2. Agregar manualmente el año 2070 al final
años_proyeccion <- c(años_base, 2070)

# 2. Convertirlos a la escala x1 del modelo (donde 2018 = 0)
x1_futuro <- data.frame(x1 = años_proyeccion - 2018)

# 3. Generar las predicciones (recordando que están en escala de millones)
pre_gompertz <- predict(GompertzModel_Pre, newdata = x1_futuro)
pre_logistico <- predict(modelo_logistico1, newdata = x1_futuro)
pre_richards  <- predict(modelo_richards1, newdata = x1_futuro)

# 4. Crear la tabla comparativa (multiplicamos por 10 para volver a la escala real si es necesario)
tabla_proyecciones1 <- data.frame(
  Año = años_proyeccion,
  Gompertz = round(pre_gompertz, 5),
  Logistico = round(pre_logistico, 5),
  Richards = round(pre_richards, 5)
)

print(tabla_proyecciones1)



# Limpiar configuración previa
dev.off() 

# Configurar márgenes mínimos: c(abajo, izquierda, arriba, derecha)
par(mar = c(4, 4, 2, 1)) 

# Ahora ejecuta tu código de plot
plot(años_proyeccion, pre_logistico, type = "n", 
     main = "Proyección 2018 - 2070 Pre_Covid",
     xlab = "Año", ylab = "Población ",
     ylim = c(5.15, max(pre_gompertz)+0.2))

# Añadir las líneas y la leyenda
lines(años_proyeccion, pre_gompertz, col = "red", lwd = 2)
lines(años_proyeccion, pre_logistico, col = "blue", lwd = 2, lty = 2)
lines(años_proyeccion, pre_richards, col = "green", lwd = 2, lty = 3)
legend("topleft", legend = c("Gompertz", "Logístico", "Richards"),
       col = c("red", "blue", "green"), lty = 1:3, lwd = 3, cex = 0.5)

confint(GompertzModel_Pre)
