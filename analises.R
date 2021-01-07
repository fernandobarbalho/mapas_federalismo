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


df_graph<-
  dados_cluster_graph %>%
  mutate(cluster= factor(cluster)) %>% 
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original",-c(1:3)) #%>%
    #filter(item_pib_original == "agro")
  ) %>%
  #filter(item_pib %in% c("agro","admnistracao","industria","servicos")) %>%
  #filter(cluster ==6) %>%
  filter(ano == 2017)
  
df_graph%>%
  filter(item_pib %in% c("agro","admnistracao","industria","servicos")) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "lightgrey", alpha =1)+
  geom_jitter(data= df_graph%>%filter(cluster=='1',item_pib %in% c("agro","admnistracao","industria","servicos")),aes(color= cluster), show.legend = FALSE, alpha=0.1) +
  geom_point(data= df_graph%>%filter(cod_mun==3158201,item_pib %in% c("agro","admnistracao","industria","servicos")),aes(color= nome_mun))+
  
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  #scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )#+

df_graph%>%
  filter(item_pib %in% c("pib_pc")) %>%
  ggplot(aes(x=item_pib, y =valor_original)) +
  geom_jitter( color = "lightgrey", alpha =1)+
  geom_jitter(data= df_graph%>%filter(cluster=='6',item_pib %in% c("pib_pc")),aes(color= cluster), show.legend = TRUE, alpha=0.5) +
  #geom_boxplot( show.legend = FALSE,  fill= NA, outlier.colour = NA, color = "red") +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
