#PPED_AreaSexoEdadNac_2018_2070_1 Post covid

#1
tab_pob2 = data.frame(PPED_AreaSexoEdadNac_2018_2070_1_)

head(tab_pob)
dim(tab_pob)

#2
tab_pob[, 99]
tab_pob[5:109, 100]

#3
base_Nacional = data.frame(tab_pob[2:119, ])
head(base_Nacional)

#4
proyeccion_Nacional = data.frame(base_Nacional$...2,base_Nacional$...3, base_Nacional$Hombres,
                                 base_Nacional$Mujeres,base_Nacional$Total)
head(proyeccion_Nacional)
proyeccion_Nacional2=NULL
proyeccion_Nacional2[1]=sum(as.numeric(proyeccion_Nacional[which((
  proyeccion_Nacional$base_Nacional....2 == 2018)& ( proyeccion_Nacional$base_Nacional....3=="Total")),5]))

for (i in 1:52){
  proyeccion_Nacional2[i]= sum(as.numeric(proyeccion_Nacional[which((
    proyeccion_Nacional$base_Nacional....2 == 2017+i)&( proyeccion_Nacional$base_Nacional....3=="Total")),5]))
}
proyeccion_Nacional2

#5
plot(2019:2070,proyeccion_Nacional2, type="p",col="blue",pch=20, ylim = c(0, max(proyeccion_Nacional2)))

#6
install.packages("npregfast")
library(npregfast)
?frfast

#7

proyeccion_Nacional2[1]

poblacion_NA = c(
  rep(NA, 51), proyeccion_Nacional2[1],  # 2018
  rep(NA, 51), proyeccion_Nacional2[2],  # 2019
  rep(NA, 51), proyeccion_Nacional2[3],  # 2020
  rep(NA, 51), proyeccion_Nacional2[4],  # 2021
  rep(NA, 51), proyeccion_Nacional2[5],  # 2022
  rep(NA, 51), proyeccion_Nacional2[6],  # 2023
  rep(NA, 51), proyeccion_Nacional2[7]  # 2024
)
length(poblacion_NA)

#8
plot(poblacion_NA, pch = 20, ylim = c(0, max(na.omit(poblacion_NA))))


#9
datos_finales = data.frame(año = (2018:2024), poblacion =
                             proyeccion_Nacional2[1:7])
datos_finales

#10
regresion_np = frfast(datos_finales[, 2] ~ datos_finales[, 1], model =
                        "np",
                      p = 3, smooth = "kernel", kbin = (364-51))
#11
imputacion_pob = data.frame(regresion_np$p)

#12
plot(regresion_np$x, poblacion_NA[52:364], pch = 20,
     ylim = c(7000000, max(na.omit(poblacion_NA))), col = "orchid")

lines(regresion_np$x, imputacion_pob$X1, lwd = 2, col = "skyblue2")

#13
fechas = seq(as.Date("2018-12-30"), as.Date("2024-12-28"), by = "week")


#14
imputacion_pob$X1[which(fechas == as.Date("2023-04-23"))]

#15
plot(fechas, imputacion_pob$X3, lwd = 2, type = "l")

abline(h = 0, lwd = 2, col = "red")
abline(v = as.Date("2023-08-06"), lwd = 2, col = "blue")


data.frame(fechas, imputacion_pob$X3)
plot(fechas, imputacion_pob$X2, lwd = 2, type = "l")

x <- regresion_np$x # Tiempo (ej. años como fracción)
y <- imputacion_pob$X1 # Población imputada suavizada
plot(x, y, main = "Datos observados", xlab = "x", ylab = "y", pch = 19)

#16
A_init <- max(na.omit(imputacion_pob$X1))/10000000 # Asíntota superior (capacidad máxima esperada)
B_init <- 0.11 # Desplazamiento horizontal (ligado al punto deinflexión)
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
