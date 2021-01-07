#Script para geração dos mapas usados no artigo da revista medium indicada no link abaixo
#https://medium.com/@fernandobarbalho/mapas-contando-hist%C3%B3ria-o-pib-dos-munic%C3%ADpios-brasileiros-aebb82f06086?source=friends_link&sk=cfb19f1276b5b8aea5a65672fb897dc8
library(readxl)
library(tidyr)
library(tidyverse)
library(viridis)
library(purrr)


#consumir os dados do ibge
#dados disponíveis em https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html
dados_municipios_ibge_2010_2017 <- read_excel("base_de_dados_2010_2017_xls/dados_municipios_ibge_2010_2017.xls")

names(dados_municipios_ibge_2010_2017)

dados_economicos<- dados_municipios_ibge_2010_2017 %>%
  select(c(1,7,8,33:37,39,40))

names(dados_economicos)<- c("ano","cod_mun","nome_mun","agro","industria","servicos","admnistracao","var_pib","pib_total","pib_pc")


#reescala as variáveis
dados_cluster<-
  dados_economicos %>%
  filter(ano == 2017) %>%
  mutate_at(c(4:10),  ~(scale(.) %>% as.vector))


set.seed(1972)




sil_info<-
  map_dbl(3:6, function(k){
    print(k)
    model_loop<- cluster::pam(dados_cluster[4:10],k=k)
    model_loop$silinfo$avg.width
  })
  

model<-
  cluster::pam(dados_cluster[4:10],k=6)

dados_cluster$cluster<- model$clustering


# o modelo escolhido foi o de seis clusters pro apresentar o melhor model$silinfo$avg.width entre os modelos testados

save(list="dados_cluster", file = "dados_cluster.RData")


#reorganizando os clusters para dar um sentido de variável ordinal
dados_cluster_graph<-
  dados_cluster %>%
  gather(key = "item_pib", value = "valor",-c(1:3,11))%>%
  mutate(cluster =case_when(
    cluster ==5 ~1,
    cluster ==3 ~2,
    cluster ==1 ~3,
    cluster ==4 ~4,
    cluster ==2 ~5,
    cluster ==6 ~6
  ))



#testando possibilidades 1
dados_cluster %>%
  gather(key = "item_pib", value = "valor",-c(1:3,11))%>%
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib_original", value = "valor_original",-c(1:3))
  )%>% 
  ggplot() +
  geom_jitter(aes(x=item_pib_original, y =valor_original, color= factor(cluster))) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light()


#testando possibilidades 2
dados_cluster %>%
  gather(key = "item_pib", value = "valor",-c(1:3,11))%>%
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib_original", value = "valor_original",-c(1:3))
  ) %>%
  ggplot() +
  geom_jitter(aes(x=item_pib_original, y =valor_original, color= factor(cluster))) +
  scale_color_viridis(discrete = TRUE, direction = -1) +
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  facet_wrap(cluster ~.)


#Testatndo possibilidades 3
dados_cluster %>%
  gather(key = "item_pib", value = "valor",-c(1:3,11))%>%
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib_original", value = "valor_original",-c(1:3))
  ) %>%
  ggplot() +
  geom_jitter(aes(x=cluster, y =valor_original, color= factor(cluster))) +
  scale_color_viridis(discrete = TRUE) +
  #scale_y_log10() +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90)
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  facet_wrap(item_pib_original ~., scales = "free_y")


#gráfico repreentando os clusters
dados_cluster_graph%>%
  mutate(cluster= factor(cluster)) %>%
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3))
  ) %>%
  ggplot() +
  geom_jitter(aes(x=item_pib, y =valor, color= cluster), alpha =0.5) +
  scale_color_viridis(discrete = TRUE) +
  #scale_y_log10() +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  facet_wrap(cluster ~., scales = "free_y")




#pacotes necessários para os mapas

library(geobr)
library(sf)


#shape files dos municípios
muni <- read_municipality( year=2010 )

#shape files dos estados
states <- read_state(year=2014)

#Um teste
muni %>% 
  filter(abbrev_state %in% c("SP", "MG","BA")) %>%
  ggplot()+
  
  geom_sf(fill= NA, color="gray", size=.15, show.legend = FALSE)+
  geom_sf(data= states%>% filter(abbrev_state %in% c("SP", "MG","BA")),aes(color=abbrev_state),fill= NA,  size=.15, show.legend = FALSE)+
  
  
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 

#mais testes
muni %>%
  #ggplot() +
  geom_sf(  color=NA, size=.08, show.legend = FALSE) +
  geom_sf(data= muni %>% filter(abbrev_state %in% c("SP","MG","BA"))  , aes(fill= abbrev_state ), color="lightblue", size=.15, show.legend = FALSE)+
  #geom_sf(data= states%>% filter(abbrev_state %in% c("SP","MG","BA")),color="yellow", size=.15, show.legend = FALSE)+
  
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 


#shapes files dos municípios de MG
muni_mg <- read_municipality( code_muni = "MG", year= 2000, simplified = FALSE)

#shapes files dos municípios de SP
muni_sp<- read_municipality( code_muni = "SP", year= 2010, simplified = FALSE)

#mapa geral do Brasil com os clusters
muni %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun)) %>%
  mutate(cluster= factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  ggplot()+
  geom_sf( aes(fill=cluster), color=NA, size=.08, show.legend = TRUE) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 

#mapa de minas gerais
muni_mg %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun)) %>%
  mutate(cluster= factor(cluster)) %>%
  ggplot() +
  geom_sf( aes(fill=cluster), color="lightblue", size=.01, show.legend = TRUE) +
  #geom_sf(data= states,fill=NA, color="red", size=.15, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 

#mapa de são paulo
muni_sp %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun))%>%
  mutate(cluster= factor(cluster)) %>%
  ggplot() +
  geom_sf( aes(fill=cluster), color="lightblue", size=.01, show.legend = TRUE) +
  #geom_sf(data= states,fill=NA, color="red", size=.15, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 




#mapa do Oeste
muni %>% 
  filter(abbrev_state %in% c("GO", "MT","MS", "PA")) %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun)) %>%
  mutate(cluster= factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  ggplot()+
  geom_sf( aes(fill=cluster), color="lightblue", size=.08, show.legend = TRUE) +
  geom_sf(data= states%>% filter(abbrev_state %in% c("GO", "MT","MS", "PA")),color = "red",fill= NA,  size=.15, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 


#Testes com RJ
muni %>% 
  filter(abbrev_state %in% c("RJ")) %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun)) %>%
  mutate(cluster= factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  ggplot()+
  geom_sf( aes(fill=cluster), color="lightblue", size=.08, show.legend = TRUE) +
  geom_sf(data= states%>% filter(abbrev_state %in% c("RJ")),color = "red",fill= NA,  size=.15, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 


#Mapa do NE

states_NE<- c("MA","PI","CE","RN","PB","PE","AL","SE","BA")
muni %>% 
  filter(abbrev_state %in% states_NE) %>%
  left_join(dados_cluster_graph%>%
              rename(code_muni = cod_mun)) %>%
  mutate(cluster= factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  ggplot()+
  geom_sf( aes(fill=cluster), color="lightblue", size=.08, show.legend = TRUE) +
  geom_sf(data= states%>% filter(abbrev_state %in% states_NE),color = "red",fill= NA,  size=.08, show.legend = FALSE)+
  scale_fill_viridis(discrete = TRUE)+
  labs(subtitle="Clusters de componentes do PIB", size=8) +
  theme_minimal() 
