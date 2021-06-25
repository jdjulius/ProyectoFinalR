
importaciones %>% 
  group_by(anio, mes) %>% 
  select(anio, mes, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() +
  geom_bar(aes(x=mes, y=total,fill=anio), stat='identity', position='dodge') 




importaciones %>% 
  group_by(pais) %>% 
  select(pais,monto) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  head(n=10) %>% 
  ggplot(aes(pais, total)) +
  geom_bar(stat = "identity",fill = rgb(0.2, 0.2, 1, 0.3), color = "blue") +
  coord_flip() + 
  theme_minimal()



importaciones %>% 
  group_by(anio) %>% 
  select(anio, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line(colour="red")




g2 <- importaciones %>% 
  filter(anio == 2020) %>% 
  group_by(anio, mes) %>% 
  select(anio, mes, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x = mes, y = total)) +
  geom_line(size=1.1)+
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = seq(1, 12, 1))+
  theme_minimal() +
  theme(legend.position = c(0.9, 0.2))

ggplotly(g2)





importaciones %>% 
  filter(anio == 2020) %>% 
  count(continente) %>%
  mutate(conti = fct_rev(fct_reorder(continente, n, .desc = TRUE))) %>%
  plot_ly(x=~n, y = ~continente, hoverinfo = "x", text = ~n) %>%
  add_bars(color = I("#3fa3ab")) %>%
  layout(xaxis = list(title = "Número de países",
                      zeroline = FALSE, 
                      range = c(0,3500)),
         yaxis = list(title = "",
                      ticklen = 5,
                      tickcolor = "transparent"))




importaciones %>% 
  filter(anio==2020) %>% 
  plot_ly(x = ~monto, y = ~unidades, color = ~continente,
          hoverinfo = "text",
          text = ~paste0(continente, "<br>", "PIB per cápita: ", round(monto,0), "<br>",
                         "Valor importado en $  ", round(unidades,0))) %>%
  add_markers(colors = "Dark2", marker = list(opacity = 0.5)) %>%
  layout(xaxis = list(title = "PIB per cápita (en dólares)",
                      zeroline = FALSE),
         yaxis = list(title = "Cantidad de Unidades",
                      zeroline = FALSE))




importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(anio) %>% 
  select(anio, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line(colour="Blue", size=1.2)




importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(anio, pais, continente) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  datatable(
    colnames = c('Año','País', 'Continente','Total ($)'),
    rownames = FALSE, filter = 'top',
    extensions = 'Buttons', options = list(
      columnDefs = list(list(width = '50px')),
      pageLength = 9,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )



gt3 <- importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(descripcion, anio) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=anio, y=total), stat='identity', position='dodge')

ggplotly(gt3)




importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(continente, anio, descripcion) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=continente, y=total), stat='identity')






importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(anio) %>% 
  select(anio, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line(colour="Blue", size=1.2)



importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(anio, pais, continente) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  datatable(
    colnames = c('Año','País', 'Continente','Total ($)'),
    rownames = FALSE, filter = 'top',
    extensions = 'Buttons', options = list(
      columnDefs = list(list(width = '50px')),
      pageLength = 9,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )



gt3 <- importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(descripcion, anio) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=anio, y=total), stat='identity', position='dodge')

ggplotly(gt3)



importaciones %>% 
  filter(tipo == 3) %>% 
  group_by(continente, anio, descripcion) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=continente, y=total), stat='identity')





importaciones %>% 
  filter(tipo == 1) %>% 
  group_by(anio) %>% 
  select(anio, monto) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot(aes(x = anio, y = total)) +
  geom_line(colour="Blue", size=1.2)


importaciones %>% 
  filter(tipo == 1) %>% 
  group_by(anio, pais, continente) %>% 
  summarise(total = sum(monto)) %>% 
  arrange(desc(total)) %>% 
  datatable(
    colnames = c('Año','País', 'Continente','Total ($)'),
    rownames = FALSE, filter = 'top',
    extensions = 'Buttons', options = list(
      columnDefs = list(list(width = '50px')),
      pageLength = 9,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )

gc3 <- importaciones %>% 
  filter(tipo == 1) %>% 
  group_by(descripcion, anio) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=anio, y=total), stat='identity', position='dodge')

ggplotly(gc3)




importaciones %>% 
  filter(tipo == 1) %>% 
  group_by(continente, anio, descripcion) %>% 
  summarise(total = sum(monto)) %>% 
  ggplot() + 
  geom_bar(aes(x=continente, y=total), stat='identity')





importaciones %>% ggplot(aes(x = monto)) + 
  geom_histogram(color = "#ffffff", fill = "#0D335D") +
  scale_x_log10(labels =scales::percent_format() )+
  labs(x = NULL,
       y = "Total de importaciones") +
  theme_minimal()