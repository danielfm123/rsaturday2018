library(tidyverse)
library(broom)
library(ggthemes)

dataset = readRDS("dataset_estimacion.RDS")

aux = dataset %>% 
  filter(!str_detect(origen_voto,"_sv$")) %>% 
  mutate(tendencia_destino = replace(destino,destino == 'guillier_sv','izquierda'),
         tendencia_destino = replace(tendencia_destino,destino == 'pinera_sv','derecha'),
         tendencia_destino = replace(tendencia_destino,destino == 'noVoto','nula'),
         origen_voto = str_remove(origen_voto,"_pv$")) %>% 
  group_by(origen_voto,tendencia_destino) %>% 
  summarise(votos = sum(votos_sv_est))

ggplot(aux %>%filter(tendencia_destino != 'noVoto_sv'),
       aes(tendencia_destino,votos,fill=reorder(origen_voto,origen_voto) )) +
  geom_bar(stat = 'identity') +
  labs(fill = 'tipo origen') +
  xlab('destino') +
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
ggsave("final_abierto.png",width = 4,height = 4.5,dpi = 300)

ggplot(aux ,
       aes(tendencia_destino,votos,fill=reorder(origen_voto,origen_voto) )) +
  geom_bar(stat = 'identity') +
  labs(fill = 'tipo origen') +
  xlab('destino') +
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
ggsave("final_abierto_nulos.png",width = 6,height = 4.5,dpi = 300)

# Grafico Final
aux = dataset %>% 
  mutate(tendencia_destino = replace(destino,destino == 'guillier_sv','izquierda'),
         tendencia_destino = replace(tendencia_destino,destino == 'pinera_sv','derecha'),
         tendencia_destino = replace(tendencia_destino,destino == 'noVoto','nula'),
         tendencia_origen_comparada = if_else(tendencia_destino == tendencia_origen,'igual','oposicion'),
         tendencia_origen_comparada = if_else(tendencia_origen == 'nula','nula',tendencia_origen_comparada)) %>% 
  group_by(origen_voto,tendencia_destino) %>% 
  filter(!is.na(votos_sv_est)) %>% 
  summarise(votos = sum(votos_sv_est))

#Reception de Votos de votos
ggplot(aux %>% filter(tendencia_origen != "nula" |  tendencia_destino != "noVoto_sv"),
       aes(tendencia_destino, 
           votos,
           fill=origen_voto )) + 
  geom_bar(stat = 'identity') +
  labs(fill = 'tipo origen') +
  xlab('destino') +
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
ggsave("final_partido.png",width = 4,height = 4.5,dpi = 300)

# Final
aux = dataset %>% 
  mutate(tendencia_destino = replace(destino,destino == 'guillier_sv','izquierda'),
         tendencia_destino = replace(tendencia_destino,destino == 'pinera_sv','derecha'),
         tendencia_destino = replace(tendencia_destino,destino == 'noVoto','nula')) %>% 
  group_by(tendencia_origen,tendencia_destino) %>% 
  filter(!is.na(votos_sv_est)) %>% 
  summarise(votos = sum(votos_sv_est))

#Reception de Votos de votos
ggplot(aux %>% filter(tendencia_origen != "nula" |  tendencia_destino != "noVoto_sv"),
       aes(tendencia_destino, 
           votos,
           fill=reorder(tendencia_origen,desc(tendencia_origen) ))) + 
  geom_bar(stat = 'identity') +
  labs(fill = 'tipo origen') +
  xlab('destino') +
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
ggsave("final_partido_con_nv.png",width = 6,height = 4.5,dpi = 300)


