library(tidyverse)
library(ggrepel)
library(patchwork)

ipress <- readRDS("extra/ipress.rds")

antig <- readRDS("20220115/antig.rds") %>%
  filter(!is.na(precio) &
           precio > 0.02) %>% # eliminar valores cercanos a cero
  left_join(
    ipress,
    by = c("codigo_ipress" = "COD_IPRESS")
  ) %>%
  group_by(DEPARTAMENTO) %>%
  summarise(
    n = n(),
    n_distinct = n_distinct(codigo_ipress),
    min = min(precio, na.rm = TRUE),
    max = max(precio, na.rm = TRUE),
    sd = sd(precio, na.rm = TRUE),
    med = median(precio, na.rm = TRUE),
    mean = mean(precio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    dpto_lbl = paste0(DEPARTAMENTO, " (N: ", n, ")"),
    dpto_lbl = fct_reorder(dpto_lbl, mean, .desc = FALSE)
  )

pcr <- readRDS("20220115/pcr.rds") %>%
  filter(!is.na(precio) &
           precio > 0.02) %>% # eliminar valores cercanos a cero
  left_join(
    ipress,
    by = c("codigo_ipress" = "COD_IPRESS")
  ) %>%
  group_by(DEPARTAMENTO) %>%
  summarise(
    n = n(),
    n_distinct = n_distinct(codigo_ipress),
    min = min(precio, na.rm = TRUE),
    max = max(precio, na.rm = TRUE),
    sd = sd(precio, na.rm = TRUE),
    med = median(precio, na.rm = TRUE),
    mean = mean(precio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    dpto_lbl = paste0(DEPARTAMENTO, " (N: ", n, ")"),
    dpto_lbl = fct_reorder(dpto_lbl, mean, .desc = FALSE)
  )

pcr_plot <- ggplot(
  pcr %>%
    filter(!is.na(DEPARTAMENTO)),
  aes(x = mean, y = dpto_lbl)
) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    color = "grey40",
    na.rm = TRUE,
    size = 1,
    height = .5
  ) +
  scale_x_continuous(
    labels = scales::label_dollar(
      prefix = "S/ ",
      accuracy = .01),
    n.breaks = 6
  ) +
  geom_text_repel(
    aes(label = sprintf("%.2f", mean)),
    nudge_y = .5
  ) +
  geom_point(size = 4, color = "red") +
  theme_bw(15) +
  labs(
    title = "Pruebas PCR",
    x = "",
    y = ""
  )
#pcr_plot

antig_plot <- ggplot(
  antig %>%
    filter(!is.na(DEPARTAMENTO)) %>%
    filter(mean > 0),
  aes(x = mean, y = dpto_lbl)
) +
  geom_errorbarh(
    aes(xmin = min, xmax = max),
    color = "grey40",
    na.rm = TRUE,
    size = 1,
    height = .5
  ) +
  geom_text_repel(
    aes(label = sprintf("%.2f", mean)),
    nudge_y = .5
  ) +
  geom_point(size = 4, color = "brown") +
  scale_x_continuous(
    labels = scales::label_dollar(
      prefix = "S/ ",
      accuracy = .01),
    n.breaks = 6
  ) +
  theme_bw(15) +
  labs(
    title = "Pruebas antigénicas",
    x = "",
    y = ""
  )
#antig_plot

comb_plot <- (pcr_plot + antig_plot) +
  plot_annotation(
    title = "COVID-19 (Perú): Costos de pruebas por Departamento",
    subtitle = "Fuente: SUSALUD (https://app20.susalud.gob.pe:8083/wb-visualizador-precios/consulta.htm)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2022-01-15)"
  ) &
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 16, color = "gray40"),
    plot.caption = element_text(size = 14, family = "Inconsolata")
  )

#comb_plot

ggsave(
  comb_plot,
  filename = "covid19-pruebas-costos-por-departamento.png",
  width = 18,
  height = 10
)

