library(tidyverse)
library(broom)
library(ggthemes)

dataset = readRDS("dataset.RDS")

#Juntamos los no votantes
dataset = mutate(dataset, 
                 noVoto_pv = nulos_pv  + blanco_pv + noVoto_pv,
                 noVoto_sv = nulos_sv  + blancos_sv + noVoto_sv) %>% 
  select(-nulos_pv, -blanco_pv,-nulos_sv,-blancos_sv)


dataset = gather(dataset,candidato,votos,-region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa)
dataset = mutate(dataset, vuelta = if_else(str_detect(candidato,"_pv$"),"pv","sv"),
                 tendencia = if_else(str_detect(candidato,"^artes|^goic|^guillier|^meo|^navarro|^sanchez"),"izquierda",candidato),
                 tendencia = if_else(str_detect(candidato,"^pinera|^kast"),"derecha",tendencia),
                 tendencia = if_else(str_detect(candidato,"^noVoto|^nulos|^blanco"),"nula",tendencia),
                 candidato = str_remove(candidato,"_sv$|_pv$")
) %>% spread(vuelta,votos)


modelo_basico = dataset %>% 
  filter(tendencia != "nula") %>% 
  group_by(tendencia,region,circ_sen,distrito, comuna, circ_elec,local_vot,mesa) %>% 
  summarise(votos_est = sum(pv,na.rm = T), votos = sum(sv,na.rm = T))

scr_modelo_basico = sum((modelo_basico$votos_est - modelo_basico$votos)^2)
