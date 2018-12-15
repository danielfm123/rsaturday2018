library(openxlsx)
library(tidyverse)

#Cargar datos
primera.raw = read.xlsx("votos 1ra.xlsx")
segunda.raw = read.xlsx("votos 2da.xlsx")
primera.votos.raw = read.xlsx("participacion_1ra_0-2156_.xlsx")
segunda.votos.raw = read.xlsx("participacion_2da_0-2156_.xlsx")

#Votos Totales
primera.votos = primera.votos.raw
primera.votos$votos_mesa = NULL

#Votos Emitidos
primera = primera.raw
primera[is.na(primera)] = 0

# Cleaning Votos Emitidos
colnames(primera)[-1:-7] = paste0(colnames(primera)[-1:-7],"_pv")
primera = transform(primera,mesa = gsub(" \\(Descuadrada\\)","",mesa))
primera = merge(primera, primera.votos)
primera = transform(primera, noVoto_pv = total_mesa - goic_pv-kast_pv-pinera_pv-guillier_pv-sanchez_pv-meo_pv-artes_pv-navarro_pv-nulos_pv-blanco_pv)
primera$total_mesa = NULL
head(primera)

#data cleaning Segunda vuelta
# los totales son identicos, por lo que no usamos segunda.votos.raw 
segunda = segunda.raw
segunda[is.na(segunda)] = 0
colnames(segunda)[-1:-7] = paste0(colnames(segunda)[-1:-7],"_sv")
segunda = transform(segunda,mesa = gsub(" \\(Descuadrada\\)","",mesa))
segunda = merge(segunda, primera.votos)
segunda = transform(segunda, noVoto_sv = total_mesa - pinera_sv - guillier_sv - nulos_sv - blancos_sv)
segunda$total_mesa = NULL
head(segunda)

# Creando dataset
dataset = merge(primera,segunda,by=colnames(primera)[1:7],all=T)
# write.xlsx(dataset,"dataset.xlsx")
any(is.na(dataset))
head(dataset)

saveRDS(dataset,"dataset.RDS")
