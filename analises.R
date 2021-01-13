analise_cluster<-
dados_cluster %>%
  mutate(id= row_number())


fab<-
analise_cluster %>%
  filter(id %in% c(4111,3937,3338,4484, 2923,4348))

model_width<-model[["silinfo"]][["widths"]]

model_width<- as.data.frame (model_width)

model_width <-
  model_width %>%
  mutate(pos = row)

model_width$id <- row.names(model_width)


#box_plot por um cluster específico
dados_cluster_graph %>%
  mutate(cluster= factor(cluster)) %>% 
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3)) #%>%
      #filter(item_pib_original == "agro")
  ) %>%
  filter(item_pib %in% c("agro","admnistracao","industria","servicos")) %>%
  filter(cluster ==6) %>%
  filter(ano == 2017) %>%
  ggplot(aes(x=item_pib, y =valor_original, color= cluster)) +
  geom_boxplot( show.legend = FALSE, size =0.1) +
  geom_jitter(alpha =0.1, show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )#+
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))# +
  #facet_wrap(item_pib ~., scales = "free_y")



#consumo de tabelz com as cidades medoides dos seis clusters
mediodes_mun <- read_delim("mediodes_mun.csv", 
                           ";", escape_double = FALSE, col_types = cols(cluster = col_character()), 
                           trim_ws = TRUE)

#criação de dataframe base para as análises
df_graph<-
  dados_cluster_graph %>%
  mutate(cluster= factor(cluster)) %>% 
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3)) #%>%
    #filter(item_pib_original == "agro")
  ) %>%
  filter(ano == 2017) 


#identiricação dos itens de pib que comporão o gráfico
arg_item_pib<- c("agro","admnistracao","industria","servicos")

#Gráfico que mostra as nuvens de pontos que serão coloridos
df_graph%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "#DADADA", alpha =0.1)+
  #geom_jitter(data= df_graph%>%filter(cluster==arg_cluster,item_pib %in% arg_item_pib ),aes(color= cluster), show.legend = FALSE, alpha=alfa, size = 2) +
  #geom_point(data= mediodes_mun%>%filter(cluster==arg_cluster)%>% inner_join(df_graph%>% filter(cluster==arg_cluster,item_pib %in% arg_item_pib)),aes(color= nome_mun), size =3)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  #scale_color_viridis(discrete = TRUE, direction = -1, option = "B") +
  #scale_color_discrete_diverging(palette = "Blue-Red 2", rev= TRUE)+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black" ), #
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )



arg_item_pib<- c("pib_pc")

#nuvem de pontos que serão coloridos para o pib_pc
df_graph%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "#DADADA", alpha =0.1, width = 0.15)+
  #geom_jitter(data= df_graph%>%filter(cluster==arg_cluster,item_pib %in% arg_item_pib ),aes(color= cluster), show.legend = FALSE, alpha=alfa, size = 2) +
  #geom_point(data= mediodes_mun%>%filter(cluster==arg_cluster)%>% inner_join(df_graph%>% filter(cluster==arg_cluster,item_pib %in% arg_item_pib)),aes(color= nome_mun), size =3)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  #scale_color_viridis(discrete = TRUE, direction = -1, option = "B") +
  #scale_color_discrete_diverging(palette = "Blue-Red 2", rev= TRUE)+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_blank()
  )


#refinando o dataset de medoides para pegar o nome dos municípios
mediodes_mun_extended<-
  mediodes_mun %>%
  inner_join(df_graph) %>%
  rename(nome_mun_medoide= nome_mun) %>%
  #rename(cod_mun_medoide = cod_mun) %>%
  distinct(cluster, cod_mun, nome_mun_medoide)




arg_item_pib<- c("agro","admnistracao","industria","servicos")

#Colorindo as faixas por cluster
df_graph %>%
  left_join(mediodes_mun_extended) %>%
  mutate(super_grupo=ifelse(cluster<="3",1,2))%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  #geom_jitter( color = "white", alpha =0.4)+
  geom_jitter(aes(color= cluster), alpha=1) +
  #geom_point(aes(color= factor(nome_mun_medoide,levels = c("Santa Maria do Suaçuí","Bernardino de Campos","Mandirituba","Nova Veneza","Assis Chateaubriand","Blumenau"))), size =2, show.legend = FALSE)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  #scale_color_discrete_diverging(palette = "Berlin")+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill ="black" ),#"#696969"
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
    
  )+#+
  #coord_flip() +
  facet_wrap(super_grupo~.)

arg_item_pib<- c("pib_pc")

#Colorindo as faixas por cluster para pib_pc
df_graph %>%
  left_join(mediodes_mun_extended) %>%
  mutate(super_grupo=ifelse(cluster<="3",1,2))%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  #geom_jitter( color = "white", alpha =0.4)+
  geom_jitter(aes(color= cluster), alpha=0.6) +
  #geom_point(aes(color= factor(nome_mun_medoide,levels = c("Santa Maria do Suaçuí","Bernardino de Campos","Mandirituba","Nova Veneza","Assis Chateaubriand","Blumenau"))), size =2, show.legend = FALSE)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  #scale_color_discrete_diverging(palette = "Berlin")+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill ="black" ),#"#696969"
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
    
  )+#+
  #coord_flip() +
  facet_wrap(super_grupo~.)

#localizando os municipios medoides
arg_item_pib<- c("agro","admnistracao","industria","servicos")

df_graph %>%
  left_join(mediodes_mun_extended) %>%
  mutate(super_grupo=ifelse(cluster<="3",1,2))%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "white", alpha =0.1)+
  #geom_jitter(aes(color= cluster), alpha=0.1, show.legend = FALSE) +
  geom_point(aes(color= factor(nome_mun_medoide,levels = c("Santa Maria do Suaçuí","Bernardino de Campos","Mandirituba","Nova Veneza","Assis Chateaubriand","Blumenau"))), size =4, show.legend = TRUE)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  #scale_color_discrete_diverging(palette = "Berlin")+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill ="#333333" ),#"#696969"
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
    
  ) +
  labs(color = "Cidades representativas")


arg_item_pib<- c("pib_pc")

df_graph %>%
  left_join(mediodes_mun_extended) %>%
  mutate(super_grupo=ifelse(cluster<="3",1,2))%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color ="#DADADA", alpha =0.1, width = 0.15)+
  #geom_jitter(aes(color= cluster), alpha=0.1, show.legend = FALSE) +
  geom_point(aes(color= factor(nome_mun_medoide,levels = c("Santa Maria do Suaçuí","Bernardino de Campos","Mandirituba","Nova Veneza","Assis Chateaubriand","Blumenau"))), size =4, show.legend = TRUE)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  #scale_color_discrete_diverging(palette = "Berlin")+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),#"#696969""#333333" 
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
    
  ) +
  labs(color = "Cidades representativas")


#Mapas
library(geobr)

#shape files dos municípios
muni <- read_municipality( year=2010 )

muni<- read_municipal_seat()

#shape files dos estados
states <- read_state(year=2014)


data = muni%>%left(mediodes_mun%>%
                          rename(code_muni = cod_mun))

#mapa com as localizações das cidades medoides

muni %>%
  ggplot() +
  geom_sf(data = states, color = "lightblue", fill = NA ) +
  geom_sf(color = "#DADADA", fill= NA, alpha =1, size= 0.1) +
  #geom_sf(data = muni%>%left_join(mediodes_mun%>%
                                   # rename(code_muni = cod_mun)), aes(color= name_muni)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  ggplot() +
  geom_sf(data = states, color = "lightblue", fill = NA ) +
  #geom_sf(color = "#DADADA", fill= NA, alpha =1, size= 1) +
  geom_sf( aes(color=cluster), fill= NA, size=0.1)+
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )


muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  ggplot() +
  geom_sf(data = states, color = "lightblue", fill = NA ) +
  #geom_sf(color = "#DADADA", fill= NA, alpha =1, size= 1) +
  geom_sf( aes(color=cluster), fill= NA, size=0.1)+
  scale_color_viridis(discrete = TRUE,   na.translate = F, option = "D") +
  theme_light() +
  theme(
    #panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )



cowplot::plot_grid(map_1, map_2)
#Mapa cluster 1
#,"admnistracao","industria","servicos"
mid_cluster_1 <- sum(df_graph[df_graph$cod_mun == 3158201 & df_graph$item_pib%in% c("pib_pc"),7])


df_mapa<- muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "1",
         item_pib %in% c("pib_pc")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup() 

#mapa  do PIB total do primeiro cluster
df_mapa %>%
  ggplot()+
  geom_sf(  aes(color=pib), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_cluster_1)+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB per capita", size=8) +
  theme_minimal()  +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank()
  )
  

#Mapa cluster 2 agro
#,"admnistracao","industria","servicos"
mid_cluster_2 <- sum(df_graph[df_graph$cod_mun == 3506300 & df_graph$item_pib%in% c("agro"),7])


#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "2",
         item_pib %in% c("agro")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=pib), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_cluster_2)+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB agropecuária", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )

#Mapa cluster 2 pib_pc
#,"admnistracao","industria","servicos"
mid_cluster_2 <- sum(df_graph[df_graph$cod_mun == 3506300 & df_graph$item_pib%in% c("pib_pc"),7])


#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "2",
         item_pib %in% c("pib_pc")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=pib), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_cluster_2)+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB per capita", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )


#Mapa cluster 3 agro
#,"admnistracao","industria","servicos"
mid_cluster_3 <- sum(df_graph[df_graph$cod_mun == 4114302 & df_graph$item_pib%in% c("agro"),7])


#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "3",
         item_pib %in% c("agro")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=log(pib)), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=log(mid_cluster_3))+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB agropecuária", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )

#Mapa cluster 3 pib_pc
#,"admnistracao","industria","servicos"
mid_cluster_3 <- sum(df_graph[df_graph$cod_mun == 4114302 & df_graph$item_pib%in% c("pib_pc"),7])


#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "3",
         item_pib %in% c("pib_pc")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=pib), fill=NA,  show.legend = TRUE,size=1,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu",mid=mid_cluster_3)+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB per Capita", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )


#Mapa cluster 4 pib_pc
#,"admnistracao","industria","servicos"
mid_cluster_4 <- sum(df_graph[df_graph$cod_mun == 4211603 & df_graph$item_pib%in% c("pib_pc"),7])


pib_pc_4<-df_graph[ df_graph$item_pib == "pib_pc" & df_graph$cluster =='4',7]
mid_cluster_4 <- median(pib_pc_4$valor_original)
mid_cluster_4 <- mean(pib_pc_4$valor_original)

muni <- read_municipality( year=2010 )
muni <- read_municipal_seat()

#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "4",
         item_pib== "pib_pc") %>%
  mutate(pib =valor_original)%>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=pib), fill=NA,  show.legend = TRUE,size =2, alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  scale_color_continuous_divergingx (palette = "RdYlBu", mid = mid_cluster_4,labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  #scale_fill_continuous_divergingx (palette = "RdYlBu", mid = mid_cluster_4,labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( fill="PIB per Capita", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )


NROW(df_graph[df_graph$valor_original>=mid_cluster_4,])
NROW(df_graph[df_graph$valor_original<=mid_cluster_4,])

#Mapa cluster 5 pib_pc
#,"admnistracao","industria","servicos"
mid_cluster_5 <- sum(df_graph[df_graph$cod_mun == 4102000 & df_graph$item_pib%in% c("agro"),7])



#mapa  do PIB total do primeiro cluster
muni %>%
  left_join(df_graph%>%
              rename(code_muni = cod_mun)) %>%
  #mutate(cluster= factor(cluster)) %>%
  filter(cluster == "5",
         item_pib %in% c("agro")) %>%
  group_by(code_muni) %>%
  
  summarise(
    pib = sum(valor_original)
  ) %>%
  ungroup()%>%
  ggplot()+
  geom_sf( aes(color=pib), fill=NA,  show.legend = TRUE,size=2,alpha =1) +
  geom_sf(data= states,fill=NA, color="lightblue", size=.15, show.legend = FALSE)+
  #scale_color_continuous_sequential (palette= "Plasma" )+
  scale_color_continuous_divergingx (palette = "RdYlBu")+
  #scale_fill_gradient2( )+
  #scale_color_gradient2( midpoint = log(mid_cluster_1))+
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,5*10^3,10^4,5*10^4,10^5)) +
  #scale_fill_viridis(trans = "log10")+
  labs( color="PIB agropecuária", size=8) +
  theme_minimal()  +
  theme(
    panel.grid = element_blank()
  )
