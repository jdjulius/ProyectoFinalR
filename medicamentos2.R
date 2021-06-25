
gm3 <- importaciones %>% 
  filter(tipo == 2) %>% 
  group_by(anio,) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x=anio, y=total)) + 
  geom_bar(aes(fill = anio), stat='identity')  +
  scale_y_continuous(labels = comma) +
  labs(title = "Total de importaciones en medicamentos por año",
       subtitle = "2016-2020",
       x = NULL,
       y = "Total de importaciones en medicamentos ($)") +
  theme_minimal()

gm3 + scale_color_manual(values = cbp1)


ggplotly(gm3)



gm7 <- importaciones %>% 
  filter(tipo == 2) %>% 
  group_by(continente) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=continente, y=total)) + 
  geom_bar( stat='identity', color = "#91d5ff", fill = "#8898be") +
  labs(title = "Cantidad total de importaciones por continente del 2016-2020 ",
       subtitle = "2020",
       x = NULL,
       y = "Total de importaciones en medicamientos") +
  theme_minimal()

ggplotly(gm7) 

gm3 <- importaciones %>% 
  filter(tipo == 2) %>% 
  group_by(anio,) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x=anio, y=total)) + 
  geom_bar(aes(fill = anio), stat='identity')  +
  scale_y_continuous(labels = comma) +
  labs(title = "Total de importaciones en medicamentos por año",
       subtitle = "2016-2020",
       x = NULL,
       y = "Total de importaciones en medicamentos ($)") +
  theme_minimal()

gm3 + scale_color_manual(values = cbp1)


ggplotly(gm3)


gm7 <- importaciones %>% 
  filter(tipo == 2) %>% 
  group_by(continente) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=continente, y=total)) + 
  geom_bar( stat='identity', color = "#91d5ff", fill = "#8898be") +
  labs(title = "Cantidad total de importaciones por continente del 2016-2020 ",
       subtitle = "2020",
       x = NULL,
       y = "Total de importaciones en medicamientos") +
  theme_minimal()

ggplotly(gm7) 

importaciones %>% 
  ggplot(aes(x = unidades, y = monto)) +
  geom_point() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_smooth(method="lm", size=1.5)


importaciones %>% 
  ggplot(aes(x = unidades, y = monto, color=continente)) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_point() +
  labs(title = "Cantidad total de importaciones por continente del 2016-2020 ",
       subtitle = "2020",
       x = NULL,
       y = "Total de importaciones en medicamientos") +
  theme_minimal()

importaciones %>% 
  ggplot(aes(anio, monto))+
  geom_count(col="tomato3", show.legend=F) +
  scale_y_continuous(labels = comma) +
  labs(title = "Cantidad total de importaciones por continente del 2016-2020 ",
       subtitle = "2020",
       x = NULL,
       y = "Total de importaciones en medicamientos") +
  theme_minimal()


importaciones %>%
  plot_ly(x = ~anio, y = ~monto, color = ~continente,
          hoverinfo = "text",
          mode="markers",
          text = ~paste0(pais, "<br>", "Monto: ", format(round(as.numeric(round(monto,0)), 1), big.mark=","), "<br>",
                         "Mes: ", format(round(as.numeric(round(mes,0)), 1), big.mark=","))) %>%
  layout(xaxis = list(title = "Meses",zeroline = FALSE, dtick = 1, ticklen = 5, range=c(2015,2021)), 
         yaxis = list(title = "Total de importaciones en medicamientos ($)",zeroline = FALSE))



importaciones %>% 
  filter(tipo == 2) %>% 
  group_by(anio,pais) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(10) %>% 
  ggplot(aes(area = total, fill = anio, label = pais)) +
  geom_treemap() + 
  geom_treemap_text(fontface = "italic",min.size = 4, colour = "white", place = "centre", grow = TRUE)



g16 <-importaciones %>% 
  filter(tipo == 2, anio == 2016) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "Total importaciones ($) en 2016") +
  theme_minimal()+
  coord_flip()

g17 <-importaciones %>% 
  filter(tipo == 2, anio == 2017) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "Total importaciones ($)  en 2017") +
  theme_minimal()+
  coord_flip()

g18 <-importaciones %>% 
  filter(tipo == 2, anio == 2018) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "Total importaciones ($)  en 2018") +
  theme_minimal()+
  coord_flip()

g19 <-importaciones %>% 
  filter(tipo == 2, anio == 2019) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "Total importaciones ($)  en 2019") +
  theme_minimal()+
  coord_flip()

g20 <-importaciones %>% 
  filter(tipo == 2, anio == 2020) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "Total importaciones ($)  en 2020") +
  theme_minimal()+
  coord_flip()

figure <- ggarrange(g16, g17,g18, g19,g20,ncol = 2, nrow = 3)
figure



g16 <-importaciones %>% 
  filter(tipo == 2, anio == 2016) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>%
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "2016") +
  theme_minimal()+
  coord_flip()

g17 <-importaciones %>% 
  filter(tipo == 2, anio == 2017) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>%
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "2017") +
  theme_minimal()+
  coord_flip()


g18 <-importaciones %>% 
  filter(tipo == 2, anio == 2018) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>%
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "2018") +
  theme_minimal()+
  coord_flip()


g19 <-importaciones %>% 
  filter(tipo == 2, anio == 2019) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>%
  summarise(total = sum(monto)) %>%
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "2019") +
  theme_minimal()+
  coord_flip()


g20 <-importaciones %>% 
  filter(tipo == 2, anio == 2020) %>% 
  group_by(anio, pais) %>% 
  select(anio, pais,monto) %>%
  summarise(total = sum(monto)) %>%
  arrange(desc(total)) %>% 
  head(5) %>% 
  ggplot(aes(x=reorder(pais,total), y=total)) + 
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6),  limits = c(0, 60000000),breaks = seq(0,60000000, 10000000)) +
  geom_bar( stat='identity', color = "#0D335D", fill = "#0D335D") +
  labs(x = NULL,
       y = "2020") +
  theme_minimal()+
  coord_flip()

figure <- ggarrange(g16, g17,g18, g19,g20,ncol = 2, nrow = 3)
figure


gm6 <- importaciones %>% 
  select(tipo, año = fct_anio, pais, monto) %>% 
  filter(tipo == 2, año %in% c("2018","2019","2020"), pais %in% c("México", "Alemania", "India", "España", "Francia", "El Salvador")) %>%       group_by(pais) %>% 
  mutate(total_p = sum(monto)) %>%
  ungroup() %>% 
  mutate(faccion = monto /total_p ) %>% 
  group_by(año, pais) %>% 
  summarise(porcentaje = round(sum(faccion) * 100),total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x = pais, y = total, fill = año),stat="identity") +
  geom_text(aes(x = pais, y = total,label = paste0(porcentaje, "%")), position = position_dodge(0.9)) +
  annotate(
    "text", label = "31%", x = 1.2, y =100, size = 6, colour = "black"
  )+
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6), limits =c(0,140000000), breaks = seq(0,140000000, 20000000))

ggplotly(gm6) 

