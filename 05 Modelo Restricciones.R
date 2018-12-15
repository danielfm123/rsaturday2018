library(CVXR)
library(tidyverse)

dataset = readRDS("dataset.RDS")

dataset = mutate(dataset, 
                 noVoto_pv = nulos_pv  + blanco_pv + noVoto_pv,
                 noVoto_sv = nulos_sv  + blancos_sv + noVoto_sv) %>% 
  select(-nulos_pv, -blanco_pv,-nulos_sv,-blancos_sv)

# Indices de variables desicion
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
)
detalle = gather(dataset,origen_voto,votos,-region,-circ_sen,-distrito,-comuna,-circ_elec,-local_vot,-mesa,-pinera_sv,-guillier_sv,-noVoto_sv) %>% 
  left_join(porcentajes) %>% 
  mutate(guillier_sv_est = votos * a_guiller,
         noVoto_sv_est = votos * a_noVoto,
         pinera_sv_est = votos * a_pinera) %>% 
  group_by(region,circ_sen,distrito,comuna,circ_elec,local_vot,mesa) %>% 
  summarise(pinera_sv =mean(pinera_sv), guillier_sv = mean(guillier_sv), noVoto_sv = mean(noVoto_sv),votos = mean(votos),
            pinera_sv_est = sum(pinera_sv_est), guillier_sv_est = sum(guillier_sv_est), noVoto_sv_est = sum(noVoto_sv_est))


data_gr = porcentajes %>% gather(destino,porcentaje,-origen_voto)
ggplot(data_gr,aes(origen_voto,porcentaje,group=destino,fill=destino)) + 
  geom_bar(stat = "identity") + 
  scale_fill_tableau() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))+
  scale_fill_tableau()
ggsave("modelo_restr.png",width = 11.5,height = 5,dpi = 300)


scr_modelo_restr = sum((detalle$pinera_sv-detalle$pinera_sv_est)^2) + 
  sum((detalle$guillier_sv-detalle$guillier_sv_est)^2)

1 - scr_modelo_restr/scr_modelo_basico
