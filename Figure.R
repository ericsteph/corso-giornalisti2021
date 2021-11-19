library(tidyverse)
library(readxl)

# d <- read_excel("G:/Giornalisti/ES/excel/base.xlsx")
d <- read_excel("./excel/base2.xlsx")

breaks_fig <- c("2009, II trim.", "2011, II trim.", "2013, II trim.",
                "2015, II trim.", "2017, II trim.", "2019, II trim.", "2021, II trim.")

etichette <- c("2009\n(II trim.)", "2011\n(II trim.)", "2013\n(II trim.)", "2015\n(II trim.)",
               "2017\n(II trim.)", "2019\n(II trim.)", "2021\n(II trim.)")

etichette0 <- c("2009", "2011", "2013", "2015",
               "2017", "2019", "2021")

etichette2 <- c("2009\naprile", "2011\naprile", "2013\naprile", "2015\naprile",
               "2017\naprile", "2019\naprile", "2021\naprile")


# Figura 1, evoluzione disoccupati iscritti in Svizzera e in Ticino

p1 <- d %>%
  select(QUANDO, GEO, Diso_seco) %>%
  ggplot() +
  aes(x = QUANDO, y = Diso_seco, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(rows = vars(GEO), scales = "free_y") +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Disoccupati iscritti in Svizzera e in Ticino,\nper trimestre, dal 2008",
       subtitle = "Dati in migliaia",
       caption = "Fonte: Statistica dei disoccupati iscritti, Seco, Berna") +
  theme_bw() +
  theme(
        plot.title = element_text(size = rel(0.8), face = "bold"),
        plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
        plot.caption = element_text(size = rel(0.6), colour = "gray"),
        axis.text = element_text(size = rel(0.5)))


# Figura 2, evoluzione del tasso di disoccupazione Seco, in Svizzera e in Ticino

p2 <- d %>%
  select(QUANDO, GEO, Tx_diso_seco) %>%
  ggplot() +
  aes(x = QUANDO, y = Tx_diso_seco, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(cols = vars(GEO)) +
  scale_x_discrete(breaks = c("2009, II trim.", "2012, II trim.",
                              "2015, II trim.", "2018, II trim.", "2021, II trim."),
                   labels = c("2009", "2012", "2015", "2018", "2021")) +
  labs(x = "", y = "",
       title = "Tasso di disoccupazione in Svizzera e in Ticino,\nper trimestre, dal 2008",
       subtitle = "Dati in percentuale",
       caption = "Fonte: Statistica dei disoccupati iscritti, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 3, evoluzione iscritti NON disoccupati in Svizzera e in Ticino

p3 <- d %>%
  select(QUANDO, GEO, Iscritti) %>%
  ggplot() +
  aes(x = QUANDO, y = Iscritti, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(rows = vars(GEO), scales = "free_y") +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Persone in cerca di impiego non disoccupate in Svizzera e in Ticino,\nper trimestre, dal 2008",
       subtitle = "Dati in migliaia",
       caption = "Fonte: Statistica dei disoccupati iscritti, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))

# Figura 4, evoluzione degli iscritti agli URC in Svizzera e in Ticino

p4 <- d %>%
  select(QUANDO, GEO, Iscritti, Diso_seco) %>%
  mutate(ISCR = Iscritti + Diso_seco) %>%
  ggplot() +
  aes(x = QUANDO, y = ISCR, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(rows = vars(GEO), scales = "free_y") +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Persone in cerca di impiego in Svizzera e in Ticino,\nper trimestre, dal 2008",
       subtitle = "Dati in migliaia",
       caption = "Fonte: Statistica dei disoccupati iscritti, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 5, Disoccupazione ai sensi dell'ILO vs. seco in Svizzera e in Ticino
labels <- c(Tx_diso_seco = "Tasso di disoccupazione,\nsecondo la Seco", Tx_diso_ilo = "Tasso di disoccupazione,\nai sensi dell'ILO")

p5 <- d %>%
  select(QUANDO, GEO, Tx_diso_seco, Tx_diso_ilo) %>%
  pivot_longer(
    cols = Tx_diso_seco:Tx_diso_ilo,
    names_to = "Tx_diso") %>%
  ggplot() +
  aes(x = QUANDO, y = value, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess") +
  geom_line(na.rm = TRUE, size = .8) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette0) +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8),
                   labels = c("0,0%", "2,0%", "4,0%", "6,0%","8,0%"),
                   limits = c(0, 9)) +
  facet_grid(rows = vars(GEO), cols = vars(Tx_diso), labeller = labeller(Tx_diso = labels)) +
  labs(x = "", y = "",
       title = "Tasso di disoccupazione ai sensi dell'ILO e secondo la Seco,\nin Svizzera e in Ticino, per trimestre, dal 2008",
       subtitle = "Dati in percentuale",
       caption = "Fonti: Rilevazione sulla Forza di lavoro in Svizzera (Rifos), Ust, Neuch\u00E2tel;\nStatistica dei disoccupati iscritti, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 6, Evoluzione sul numero dei disoccupati in Svizzera
p6 <- d %>%
  select(QUANDO, GEO, IFC) %>%
  filter(GEO %in% "Svizzera") %>%
  ggplot() +
  aes(x = QUANDO, y = IFC, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess"
              , span = .35
              ) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette2) +
  labs(x = "", y = "",
       title = "Percezione dell'evoluzione del numero di disoccupati in Svizzera nei prossimi 12 mesi\nogni tre mesi, dal 2008",
       subtitle = "Indice",
       caption = "Fonte: Indice del clima di fiducia dei consumatori, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


a <- 4 * 1.85
b <- 3 * 1.85

# Salvo le figure
ggsave(file="plot/p1.svg", plot=p1, width=a, height=b)
ggsave(file="plot/p2.svg", plot=p2, width=a, height=b)
ggsave(file="plot/p3.svg", plot=p3, width=a, height=b)
ggsave(file="plot/p4.svg", plot=p4, width=a, height=b)
ggsave(file="plot/p5.svg", plot=p5, width=a, height=b)
ggsave(file="plot/p6.svg", plot=p6, width=a, height=b)



