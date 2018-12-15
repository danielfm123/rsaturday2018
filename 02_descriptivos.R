library(tidyverse)
library(ggthemes)
dataset = readRDS("dataset.RDS")

dataset = gather(dataset,candidato,votos,-region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa)
dataset = mutate(dataset, vuelta = if_else(str_detect(candidato,"_pv$"),"pv","sv"),
                 tendencia = if_else(str_detect(candidato,"^artes|^goic|^guillier|^meo|^navarro|^sanchez"),"izquierda",candidato),
                 tendencia = if_else(str_detect(candidato,"^pinera|^kast"),"derecha",tendencia),
                 tendencia = if_else(str_detect(candidato,"^noVoto|^nulos|^blanco"),"nula",tendencia),
                 candidato = str_remove(candidato,"_sv$|_pv$")
)



## Votos Totales
aux = dataset %>% 
  group_by(candidato,vuelta,tendencia) %>% 
  summarise(votos = sum(votos))

ggplot(aux,aes(candidato,votos,fill=tendencia)) + 
  geom_bar(stat="identity") + 
  facet_grid(cols = vars(vuelta)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau()
ggsave("votos1.png",width = 11.5,height = 5,dpi = 300)

## Votos por Candidato
aux = dataset %>% 
  filter(tendencia != "nula") %>% 
  group_by(candidato,tendencia,vuelta) %>% 
  summarise(votos = sum(votos))

ggplot(aux,aes(candidato,votos,fill=tendencia)) + 
  geom_bar(stat="identity") + 
  facet_grid(cols = vars(vuelta)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau() +
ggsave("votos2.png",width = 11.5,height = 5,dpi = 300)

## Resultados por Tendencia
aux = dataset %>% 
  filter(tendencia != "nula") %>% 
  group_by(tendencia,vuelta,candidato) %>% 
  summarise(votos = sum(votos))

ggplot(aux,aes(tendencia,votos,fill=candidato)) + 
  geom_bar(stat="identity") + 
  facet_grid(cols = vars(vuelta)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau()
ggsave("votos3.png",width = 11.5,height = 5,dpi = 300)
ggsave("votos4.png",width = 6,height = 4.5,dpi = 300)
