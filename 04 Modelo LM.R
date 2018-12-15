library(tidyverse)
library(broom)

dataset = readRDS("dataset.RDS")

#Juntamos los no votantes
dataset = mutate(dataset, 
                 noVoto_pv = nulos_pv  + blanco_pv + noVoto_pv,
                 noVoto_sv = nulos_sv  + blancos_sv + noVoto_sv) %>% 
  select(-nulos_pv, -blanco_pv,-nulos_sv,-blancos_sv)
head(dataset[sample(nrow(dataset)),],15)


#Pinera
fit_pinera = lm(pinera_sv ~ goic_pv + kast_pv + pinera_pv + guillier_pv + sanchez_pv + meo_pv + artes_pv + navarro_pv + noVoto_pv -1 ,dataset)
pinera = tidy(fit_pinera)[c("term","estimate")]

#Guiller
fit_guiller = lm(guillier_sv ~ goic_pv + kast_pv + pinera_pv + guillier_pv + sanchez_pv + meo_pv + artes_pv + navarro_pv + noVoto_pv -1 ,dataset)
guiller = tidy(fit_guiller)[c("term","estimate")]

#No votaron
fit_nv = lm(noVoto_sv ~ goic_pv + kast_pv + pinera_pv + guillier_pv + sanchez_pv + meo_pv + artes_pv + navarro_pv + noVoto_pv -1 ,dataset)
nv = tidy(fit_nv)[c("term","estimate")]

#Juntar Resultados
modelo_lm = left_join(pinera,guiller,c("term" = "term"),suffix=c("_pinera","_guiller")) %>% 
  left_join(nv,c("term" = "term"),suffix=c("","_nv")) %>% 
  rename(estimate_nv = estimate)

modelo_lm = modelo_lm %>% 
  gather(destino,porcentaje,-term)

ggplot(modelo_lm,aes(term,porcentaje,group=destino,fill=destino)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau()
ggsave("modeloLM.png",width = 11.5,height = 5,dpi = 300)

scr_rm = sum(fit_pinera$residuals^2) + sum(fit_guiller$residuals^2)

1 - scr_rm/scr_modelo_basico
