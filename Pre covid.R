#PPED_Anexo_Proyecciones_Poblacion_Nacional2018_2070 Pre covid
#1
pob_pos= data.frame(PPED_Anexo_Proyecciones_Poblacion_Nacional2018_2070)

head(pob_pos)
dim(pob_pos)

#2
pob_pos[, 99]
tab_pob1[5:109, 100]

#3
base_NacPos = data.frame(pob_pos[3:159, ])
head(base_NacPos)

#4
proy_NacPos = data.frame(base_NacPos$AÑO,base_NacPos$ÁREA.GEOGRÁFICA, base_NacPos$Total.Hombres,
                                 base_NacPos$Total.Mujeres,base_NacPos$Total.General)
head(proy_NacPos)
proyeccion_NacPos=NULL
proyeccion_NacPos[1]=sum(as.numeric(proy_NacPos[which((
  proy_NacPos$base_NacPos.AÑO == 2018)& (proy_NacPos$base_NacPos.ÁREA.GEOGRÁFICA=="TOTAL")),5]))

for (i in 1:52){
  proyeccion_NacPos[i]= sum(as.numeric(proy_NacPos[which((
    proy_NacPos$base_NacPos.AÑO == 2017+i)&(proy_NacPos$base_NacPos.ÁREA.GEOGRÁFICA=="TOTAL")),5]))
}
proyeccion_NacPos

#5
plot(2019:2070,proyeccion_NacPos, type="p",col="red",pch=20, ylim = c(0, max(proyeccion_NacPos)))

#6
install.packages("npregfast")
library(npregfast)
?frfast

#7

proyeccion_NacPos[1]

poblacion1_NA = c(
  rep(NA, 51), proyeccion_NacPos[1],  # 2018
  rep(NA, 51), proyeccion_NacPos[2],  # 2019
  rep(NA, 52), proyeccion_NacPos[3],  # 2020
  rep(NA, 51), proyeccion_NacPos[4],  # 2021
  rep(NA, 51), proyeccion_NacPos[5],  # 2022
  rep(NA, 51), proyeccion_NacPos[6],  # 2023
  rep(NA, 51), proyeccion_NacPos[7]  # 2024
)
length(poblacion1_NA)

#8
plot(poblacion1_NA, pch = 20, ylim = c(0, max(na.omit(poblacion1_NA))))


#9
datos1_finales = data.frame(año = (2018:2024), poblacion =
                             proyeccion_NacPos[1:7])
datos1_finales

#10
regresion_np1 = frfast(datos1_finales[, 2] ~ datos1_finales[, 1], model =
                        "np",
                      p = 3, smooth = "kernel", kbin = (365-52))
#11
imputacion_pob1 = data.frame(regresion_np1$p)

#12
length(regresion_np1$x)
plot(regresion_np1$x, poblacion1_NA[52:364], pch = 20,
     ylim = c(7000000, max(na.omit(poblacion1_NA))), col = "orchid")

lines(regresion_np1$x, imputacion_pob1$X1, lwd = 2, col = "skyblue2")

#13
fechas = seq(as.Date("2018-12-30"), as.Date("2024-12-28"), by = "week")


#14
imputacion_pob1$X1[which(fechas == as.Date("2023-04-23"))]

#15
length(imputacion_pob1$X3)
plot(fechas, imputacion_pob1$X3, lwd = 2, type = "l")

abline(h = 0, lwd = 2, col = "red")
abline(v = as.Date("2023-08-06"), lwd = 2, col = "blue")


data.frame(fechas, imputacion_pob1$X3)
plot(fechas, imputacion_pob1$X2, lwd = 2, type = "l")

x <- regresion_np1$x # Tiempo (ej. años como fracción)
y <- imputacion_pob1$X1 # Población imputada suavizada
plot(x, y, main = "Datos observados", xlab = "x", ylab = "y", pch = 19)

#16
A_init <- 6 # Asíntota superior (capacidad máxima esperada)
B_init <- 0.12 # Desplazamiento horizontal (ligado al punto deinflexión)
C_init <- 0.05 # Tasa de crecimiento

#17
x1 <- (x - 2018)[1:311] # Se traslada el eje temporal para que 2018 sea el origen
y1 <- na.omit(y / 10000000) # Se escala la población a millones para facilitar el ajuste numérico

#18

gompertz_model <- nls(
  y1 ~ A * exp(-B * exp(-C * x1)),
  start = list(A = 5, B = 1, C = 0.05), # valores iniciales razonables
  control = list(maxiter = 500)
)

summary(gompertz_model)
plot(predict(gompertz_model))
lines(y1, lwd = 2, col = "red")
gompertz_model
