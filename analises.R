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


dados_cluster_graph %>%
  mutate(cluster= factor(cluster)) %>% 
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3)) #%>%
    #filter(item_pib_original == "agro")
  ) %>%
  filter(item_pib %in% c("pib_pc")) %>%
  filter(cluster ==6) %>%
  filter(ano == 2017) %>%
  ggplot(aes(x=item_pib, y =valor_original, color= cluster)) +
  geom_boxplot( show.legend = FALSE) +
  geom_jitter(alpha =0.1, show.legend = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )+
scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) #+
#facet_wrap(item_pib ~., scales = "free_y")


mediodes_mun <- read_delim("mediodes_mun.csv", 
                           ";", escape_double = FALSE, col_types = cols(cluster = col_character()), 
                           trim_ws = TRUE)

df_graph<-
  dados_cluster_graph %>%
  mutate(cluster= factor(cluster)) %>% 
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3)) #%>%
    #filter(item_pib_original == "agro")
  ) %>%
  filter(ano == 2017) 


arg_item_pib<- c("agro","admnistracao","industria","servicos")

df_graph%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "white", alpha =0.1)+
  #geom_jitter(data= df_graph%>%filter(cluster==arg_cluster,item_pib %in% arg_item_pib ),aes(color= cluster), show.legend = FALSE, alpha=alfa, size = 2) +
  #geom_point(data= mediodes_mun%>%filter(cluster==arg_cluster)%>% inner_join(df_graph%>% filter(cluster==arg_cluster,item_pib %in% arg_item_pib)),aes(color= nome_mun), size =3)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  #scale_color_viridis(discrete = TRUE, direction = -1, option = "B") +
  #scale_color_discrete_diverging(palette = "Blue-Red 2", rev= TRUE)+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#333333"),
    panel.grid = element_blank(),
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )#+



arg_item_pib<- c("pib_pc")

df_graph%>%
  filter(item_pib %in% arg_item_pib) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "white", alpha =0.1)+
  #geom_jitter(data= df_graph%>%filter(cluster==arg_cluster,item_pib %in% arg_item_pib ),aes(color= cluster), show.legend = FALSE, alpha=alfa, size = 2) +
  #geom_point(data= mediodes_mun%>%filter(cluster==arg_cluster)%>% inner_join(df_graph%>% filter(cluster==arg_cluster,item_pib %in% arg_item_pib)),aes(color= nome_mun), size =3)+
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  #scale_color_viridis(discrete = TRUE, direction = -1, option = "B") +
  #scale_color_discrete_diverging(palette = "Blue-Red 2", rev= TRUE)+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#333333"),
    panel.grid = element_blank(),
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_blank()
  )#+



mediodes_mun_extended<-
  mediodes_mun %>%
  inner_join(df_graph) %>%
  rename(nome_mun_medoide= nome_mun) %>%
  #rename(cod_mun_medoide = cod_mun) %>%
  distinct(cluster, cod_mun, nome_mun_medoide)


arg_super_grupo<-2
arg_item_pib<- c("agro","admnistracao","industria","servicos")


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
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
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
    
  )+#+
  coord_flip() +
  facet_grid(super_grupo~.)

arg_item_pib<- c("pib_pc")


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
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE), breaks=c(10^3,10^4,10^5,10^6,10^7,10^8,10^9)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    panel.background = element_rect(fill ="#333333" ),#"#696969"
    panel.grid = element_blank(),
    #axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom"
    
  )+#+
  coord_flip() +
  facet_grid(super_grupo~.)
