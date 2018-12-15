library(CVXR)
library(tidyverse)
library(future.apply)
library(ggthemes)
plan(multiprocess)

dataset = readRDS("dataset.RDS")

dataset = mutate(dataset, 
                 noVoto_pv = nulos_pv  + blanco_pv + noVoto_pv,
                 noVoto_sv = nulos_sv  + blancos_sv + noVoto_sv) %>% 
  select(-nulos_pv, -blanco_pv,-nulos_sv,-blancos_sv)

data_kmeans = dataset[,-1:-7]
data_kmeans[,1:9] = data_kmeans[,1:9]/rowSums(data_kmeans[,1:9])
data_kmeans[,10:12] = data_kmeans[,10:12]/rowSums(data_kmeans[,10:12])
data_kmeans = scale(data_kmeans)

set.seed(123)
fits_kmeans = future_lapply(1:50, function(x) kmeans(data_kmeans,x,iter.max = 200, nstart = 30))

ggplot(data.frame(
  clusters = 1:length(fits_kmeans), 
  btw_SS_div_totSS = map_dbl(fits_kmeans,~1-.$betweenss/.$totss)),aes(clusters,btw_SS_div_totSS)) +
  geom_line() + 
  geom_vline(xintercept = 20,color="red")
ggsave("codo.png",width = 5,height = 4,dpi = 300)
  
ggplot(data.frame(
  clusters = 1:length(fits_kmeans), 
  Qmin = map_dbl(fits_kmeans,~min(table(.$cluster)))),aes(clusters,Qmin)) +
  geom_line() + 
  scale_y_continuous(limits = c(0,1000))+
  geom_vline(xintercept = 20,color="red")
ggsave("peneno.png",width = 5,height = 4,dpi = 300)

# Modelo planteado como un problem de minimización cuadratico
get_model = function(dataset){
  votos_ant = as.matrix(dataset[, c("noVoto_pv","goic_pv","kast_pv","pinera_pv",
                                    "guillier_pv","sanchez_pv", "meo_pv",  "artes_pv",  "navarro_pv" )])
  
  #Crear variables de desición, que porcenaje se cede a cada dimencion
  delta_pinera <- Variable(ncol(votos_ant))
  delta_guillier <- Variable(ncol(votos_ant))
  delta_no <- Variable(ncol(votos_ant))
  
  #Función objetivo
  obj <- Minimize(sum(square(votos_ant %*% delta_pinera - dataset$pinera_sv), 
                      square(votos_ant %*% delta_guillier - dataset$guillier_sv),
                      square(votos_ant %*% delta_no - dataset$noVoto_sv)
  ) )
  # Restricciones
  constr <- list(delta_pinera >= 0,
                 delta_guillier >= 0,
                 delta_no >= 0,
                 delta_pinera + delta_guillier + delta_no == 1 #Los votos se conservan
  )
  
  
  #Resolver el modelo
  prob <- Problem(obj,constr)
  result <- solve(prob)
  result$value
  result$status
  result$num_iters
  
  
  #Extraer resultados
  porcentajes = data.frame(origen_voto = colnames(votos_ant),
                           a_pinera = result$getValue(delta_pinera),
                           a_guiller = result$getValue(delta_guillier),
                           a_noVoto = result$getValue(delta_no)
                           # si_voto = sol$x[1:9] + sol$x[10:18],
                           # no_voto = 1 - sol$x[1:9] - sol$x[10:18]
  )
  
  totales = data.frame(origen_voto = colnames(votos_ant),porcentajes[,-1] * colSums(votos_ant))
  
  detalle = gather(dataset,origen_voto,votos,-region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa) %>% 
    left_join(porcentajes) %>% 
    mutate(guillier_sv = votos * a_guiller,
           noVoto_sv = votos * a_noVoto,
           pinera_sv = votos * a_pinera) %>% 
    select(-a_noVoto,-a_guiller,-a_pinera) %>% 
    gather(destino,votos_sv_est, -region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa,-origen_voto,- votos) %>% 
    mutate(
      porcentaje_entreegado = votos_sv_est/votos,
      tendencia_origen = if_else(str_detect(origen_voto,"^artes|^goic|^guillier|^meo|^navarro|^sanchez"),"izquierda",as.character(origen_voto)),
      tendencia_origen = if_else(str_detect(tendencia_origen,"^pinera|^kast"),"derecha",tendencia_origen),
      tendencia_origen = if_else(str_detect(tendencia_origen,"^noVoto|^nulos|^blanco"),"nula",tendencia_origen)
    )
  
  return(list(porcentajes = porcentajes,
              totales = totales,
              detalle = detalle))
}

clusters = 20

#Llamar a la función para cada región
data_modelo = split(dataset,fits_kmeans[[clusters]]$cluster)

fit = future_lapply(data_modelo,get_model)

totales = map_dfr(fit, function(x) x[["totales"]]) %>% 
  gather(destino,votos,-origen_voto) %>% 
  group_by(origen_voto,destino) %>% 
  summarise(votos = sum(votos))

porcentajes_origen = totales %>% 
  group_by(origen_voto) %>% 
  mutate(porcentaje_origen = votos/sum(votos)) %>% 
  ungroup()

ggplot(porcentajes_origen,aes(origen_voto,porcentaje_origen,group=destino,fill=destino)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau()
ggsave("modelo_homo.png",width = 11.5,height = 5,dpi = 300)

#metricas
totales_detalle = map_dfr(fit, function(x) x[["detalle"]]) 

estimacion = totales_detalle %>% 
  group_by(region,circ_sen,distrito,comuna,circ_elec,local_vot,mesa,destino) %>% 
  summarise(votos_sv_est = sum(votos_sv_est,na.rm = T)) %>% 
  filter(destino != "noVoto_sv")

real = select(dataset, -goic_pv ,-kast_pv ,-pinera_pv ,-guillier_pv,-sanchez_pv ,-meo_pv ,-artes_pv ,-navarro_pv ,-noVoto_pv) %>% 
  gather(destino,votos_sv,-region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa)

scr_modelo = estimacion %>% left_join(real) %>% ungroup()%>% summarise(sum(  (votos_sv_est-votos_sv)^2   ))

# tendencia
1 - scr_modelo/scr_modelo_basico

# guardar RDS

saveRDS(totales_detalle,"dataset_estimacion.RDS")
