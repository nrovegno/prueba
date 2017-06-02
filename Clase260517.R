library(ncdf4)
library(fields)

nc = nc_open("physics.nc")

lon = ncvar_get(nc, "lon")
lat = ncvar_get(nc, "lat")
time = ncvar_get(nc, "time")
sst = ncvar_get(nc, "sst")
sss = ncvar_get(nc, "sss")

meses = rep(1:12, length=length(time))
meses2 = rep(month.abb, length=length(time))

apply(sst[,, meses==1], MARGIN=c(1,2), FUN = mean, na.rm = TRUE)

clim = array(dim=c(dim(sst)[1:2], 12))
dim(sst)
dim(clim)
clim

for(i in 1:12) {
clim[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), FUN = mean, na.rm = TRUE)
}

dev.off()



par(mfrow=c(3,4), oma=c(0,0,0,0))
for(j in 1:12) {
  image.plot(lon, lat, clim[,,j], main = j)
  }

 calculateClim = function(sst,time) {
   
   meses = rep(1:12, length=length(time))
   
   clim = array(dim=c(dim(sst)[1:2],12))
   
   for(i in 1:12) {
   clim[,,i] = apply(sst[,, meses==i], MARGIN=c(1,2), #crear meses
                     FUN = mean, na.rm = TRUE)
   }
    
   return(clim)
   
 }

clim2 = calculateClim(sst, time)
identical(clim, clim2)
clim[3]
grafClim = function(lon, lat, clim, ...) { #... pasa a FUN = image.plot
  
  if(dim(clim)[3] !=12) {
    stop("clim debe tener 12 pasos de tiempo.")
  }
  
  par(mfrow=c(3,4))
  
  for(j in 1:12) {
    image.plot(lon, lat, clim[,,j], ...)
  }
  
  return(invisible())
   
}

grafClim (lon, lat, clim)

dev.off()

climSSS = calculateClim(sss, time)
grafClim (lon, lat, climSSS)

grafClim (lon, lat, clim) #validación
grafClim (lon, lat, clim[,,1:8]) #error

grafClim (lon, lat, clim, axes = FALSE, xlab="LON", ylab = "LAT", las=1) #validación
sst

####

dev.off()

#15 min expo 5min de preguntas
#vender el paquete para uso práctico
#analisis de datos para tesis, etc.
#aprender nombres de funciones, que hace, como se llaman
#marketing, promoción, impacto paquete, posición crítica (??)
#intro al paquete y el contexto que trata p.e
#paquete genetica - intro genética = 5 mins
#criterio uso del tiempo - balance intro y paquete
#nota hacer preguntas y responder preguntas
#apoyo audiovisual - debe haber sinergia entre ppt y expo
#ppt no es apoyo para el expositor sino para el publico y que siga
#NO LEER
#preguntas que hacen puntos extrasssssssss