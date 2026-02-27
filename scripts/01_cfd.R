# =============================================================================
# scripts/01_cfd.R — Compositional Flow Diagrams
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Visualizza la composizione delle figure storiche nel dataset
#         Networked Pantheon al variare del tempo, tramite area chart
#         stratificate. Analisi per: genere (per secolo e per decennio nel
#         periodo 1900-1990), dominio accademico e continente di nascita.
#
# Input:  data-raw/networked_pantheon_db/Nodes.csv
#
# Output: Grafici visualizzati a schermo (nessun file scritto su disco).
#         Sezioni 3 e 4 producono grafici inline.
#         Sezione 5 produce il pannello combinato finale per il genere.
#
# Dipendenze: tidyverse, gridExtra, cowplot, wesanderson
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         Per il dizionario completo delle variabili vedere:
#           data-raw/networked_pantheon_db/README.txt
# =============================================================================

# --- Librerie -----------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(cowplot)
library(wesanderson)

options(digits = 3)

# --- Dati ---------------------------------------------------------------------

# Caricamento nodi (figure storiche con metadati).
# Vengono escluse colonne ridondanti: indici HPI pre-calcolati, pageviews
# aggregate, dati geografici già presenti in forma testuale.
data <- read.csv2("data-raw/networked_pantheon_db/Nodes.csv")
data <- data[, -c(2, 4, 5, 8, 10, 11, 21, 22, 24:27, 29:41)]

# 'century': anno di nascita arrotondato al secolo (floor al multiplo di 100)
data[, 5] <- floor(data$birthyear / 100) * 100
names(data)[5] <- "century"

# Conversione a numerico per le colonne pertinenti
for (i in c(1, 5, 7, 8, 9, 15:17)) {
  data[, i] <- as.numeric(data[, i])
}

# Correzioni puntuali di errori nel dataset originale
data$Id <- data$Id + 1
data[data$Name == "Catullus", ]$deathyear <- -54    # anno di morte mancante
data[data$Name == "Catullus", ]$agespan   <- 30
data[10881, ]$century <- -1000                       # errore di arrotondamento

# Normalizzazione etichette di dominio (varianti testuali presenti nel dataset)
data$domain[data$domain   == "SCIENCE & TECHNOLOGY"] <- "SCIENCE"
data$domain[data$domain   == "BUSINESS & LAW"]        <- "BUSINESS"
data$domain2[data$domain2 == "SCIENCE & TECHNOLOGY"] <- "SCIENCE"
data$domain2[data$domain2 == "BUSINESS & LAW"]        <- "BUSINESS"

# Correzione di un errore di encoding in un nome paese
data$countryName[data$countryName == 'S\u00c3\u008eO TOM\u00c3\u008e AND PR\u00c3\u008dNCI'] <- "SAO TOME AND PRINCIPE"
data$countryName <- ifelse(data$countryName == "", "UNKNOWN", data$countryName)

# =============================================================================
# 1. GENERE — composizione per secolo (1000–1900)
# =============================================================================
# I secoli anteriori al 1000 d.C. vengono aggregati nella categoria '<1000'.
# Il vettore 'tab' è la versione appiattita della tabella (2 generi × 50 secoli),
# scansionata per colonne: tab[81:100] corrisponde ai 10 secoli 1000-1900.

ta    <- table(data$gender, data$century)
tab   <- as.vector(ta)
somme <- colSums(ta)
tab2  <- as.vector(ta / rbind(somme, somme))

# Proporzioni aggregate per il periodo precedente all'anno 1000
female_pre1000 <- sum(ta[1, 1:30]) / sum(somme[1:30])
male_pre1000   <- sum(ta[2, 1:30]) / sum(somme[1:30])

dati <- data.frame(
  Century = rep(c("<1000", seq(1000, 1900, 100)), each = 2),
  Genere  = rep(c("Femmina", "Maschio"), times = 11),
  Count   = c(sum(tab[seq(1, 80, 2)]), sum(tab[seq(2, 80, 2)]), tab[81:100]),
  Perc    = c(female_pre1000, male_pre1000, tab2[81:100])
)
dati$Century  <- factor(dati$Century, levels = dati$Century[seq(1, 22, 2)])
arranged_data <- dati %>% group_by(Century) %>% arrange(Genere)

# La legenda viene estratta da una versione temporanea del grafico percentuale
# per essere riutilizzata nel pannello combinato finale (sezione 5).
p_leg_gender <- ggplot(arranged_data,
                       aes(x = as.numeric(Century), y = Perc, fill = Genere)) +
  geom_area() +
  labs(fill = "Genere") +
  theme_minimal() +
  theme(
    legend.text      = element_text(size = 13),
    legend.title     = element_text(size = 14),
    legend.key.size  = unit(0.8, "cm"),
    legend.spacing.x = unit(0.5, "cm"),
    legend.spacing.y = unit(0.5, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous(breaks = 1:11, labels = levels(arranged_data$Century))
p5 <- ggdraw(get_legend(p_leg_gender))

p1 <- ggplot(arranged_data, aes(x = as.numeric(Century), y = Count, fill = Genere)) +
  geom_area() +
  labs(title = "Individui per genere, nei secoli",
       x = "Secolo", y = "Numero di individui", fill = "Genere") +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y     = element_text(size = 12),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = 1:11, labels = levels(arranged_data$Century))

p2 <- ggplot(arranged_data, aes(x = as.numeric(Century), y = Perc, fill = Genere)) +
  geom_area() +
  labs(title = "Percentuale di individui per genere, nei secoli",
       x = "Secolo", y = "Percentuale di individui", fill = "Genere") +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y     = element_text(size = 12),
    axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = 1:11, labels = levels(arranged_data$Century))

# =============================================================================
# 2. GENERE — composizione per decennio (1900–1990)
# =============================================================================

data2        <- data[data$century == 1900, ]
data2$decade <- trunc(data2$birthyear / 10) * 10

ta_dec     <- table(data2$gender, data2$decade)
tab_dec    <- as.vector(ta_dec)
somme_dec  <- colSums(ta_dec)
tab2_dec   <- as.vector(ta_dec / rbind(somme_dec, somme_dec))

dati2 <- data.frame(
  Decennio = rep(seq(1900, 1990, 10), each = 2),
  Genere   = rep(c("Femmina", "Maschio"), times = 10),
  Count    = tab_dec,
  Perc     = tab2_dec
)
arranged_data2 <- dati2 %>% group_by(Decennio) %>% arrange(Genere)

theme_dec <- theme(
  axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
  axis.text.y     = element_text(size = 12),
  axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 10)),
  axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 10)),
  plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
  legend.position = "none"
)

p3 <- ggplot(arranged_data2, aes(x = Decennio, y = Count, fill = Genere)) +
  geom_area() +
  labs(title = "Individui per genere, nei decenni del 1900",
       x = "Decennio", y = "Numero di individui", fill = "Genere") +
  theme_minimal() + theme_dec +
  scale_x_continuous(breaks = seq(1900, 1990, 10))

p4 <- ggplot(arranged_data2, aes(x = Decennio, y = Perc, fill = Genere)) +
  geom_area() +
  labs(title = "Percentuale di individui per genere, nei decenni del 1900",
       x = "Decennio", y = "Percentuale di individui", fill = "Genere") +
  theme_minimal() + theme_dec +
  scale_x_continuous(breaks = seq(1900, 1990, 10))

# =============================================================================
# 3. DOMINIO ACCADEMICO — composizione per secolo
# =============================================================================

nam_dom    <- c("INSTITUTIONS", "ARTS", "SPORTS", "SCIENCE",
                "HUMANITIES", "PUBLIC FIGURE", "BUSINESS", "EXPLORATION")
colori_dom <- wes_palette("Zissou1", 8, "continuous")[8:1]

ta_dom    <- table(data$domain, data$century)[nam_dom, ]
tab_dom   <- as.vector(ta_dom)
somme_dom <- colSums(ta_dom)
tab2_dom  <- as.vector(ta_dom / matrix(rep(somme_dom, 8), nrow = 8, byrow = TRUE))

# Aggregazione del periodo pre-0 d.C. per ciascun dominio
pre0_dom    <- sapply(seq_along(nam_dom),
                      function(i) sum(ta_dom[i, 1:30]) / sum(somme_dom[1:30]))
count_pre0  <- sapply(1:8, function(i) sum(tab_dom[seq(i, 240, 8)]))

dati_dom <- data.frame(
  Century = rep(c("<0", seq(0, 1900, 100)), each = 8),
  Dominio = rep(nam_dom, times = 21),
  Count   = c(count_pre0,  tab_dom[241:400]),
  Perc    = c(pre0_dom,    tab2_dom[241:400])
)
dati_dom$Dominio  <- factor(dati_dom$Dominio,  levels = nam_dom[8:1])
dati_dom$Century  <- factor(dati_dom$Century,  levels = dati_dom$Century[seq(1, 168, 8)])
arranged_dom      <- dati_dom %>% group_by(Century) %>% arrange(Dominio)

p_leg_dom <- ggplot(arranged_dom,
                    aes(x = as.numeric(Century), y = Perc, fill = Dominio)) +
  geom_area() +
  labs(fill = "Dominio") +
  theme_minimal() +
  theme(
    legend.text      = element_text(size = 11),
    legend.title     = element_text(size = 14),
    legend.key.size  = unit(0.7, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.spacing.y = unit(0.4, "cm")
  ) +
  scale_fill_manual(values = colori_dom) +
  guides(color = guide_legend(override.aes = list(size = 5)))
leg_dom <- ggdraw(get_legend(p_leg_dom))

theme_dom <- theme(
  axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
  axis.text.y     = element_text(size = 12),
  axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 10)),
  axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 10)),
  plot.title      = element_text(hjust = 0.5, size = 16, face = "bold"),
  legend.position = "none"
)

p_dom1 <- ggplot(arranged_dom, aes(x = as.numeric(Century), y = Count, fill = Dominio)) +
  geom_area() +
  labs(title = "Individui per dominio, nei secoli",
       x = "Secolo", y = "Numero di individui", fill = "Dominio") +
  theme_minimal() + theme_dom +
  scale_fill_manual(values = colori_dom) +
  scale_x_continuous(breaks = 1:21, labels = levels(arranged_dom$Century))

p_dom2 <- ggplot(arranged_dom, aes(x = as.numeric(Century), y = Perc, fill = Dominio)) +
  geom_area() +
  labs(title = "Percentuale di individui per dominio, nei secoli",
       x = "Secolo", y = "Percentuale di individui", fill = "Dominio") +
  theme_minimal() + theme_dom +
  scale_fill_manual(values = colori_dom) +
  scale_x_continuous(breaks = 1:21, labels = levels(arranged_dom$Century))

grid.arrange(p_dom1, p_dom2, leg_dom, nrow = 1, widths = c(1, 1, 0.22))

# =============================================================================
# 4. CONTINENTE — composizione per secolo
# =============================================================================

nam_cont    <- c("EUROPE", "NORTH AMERICA", "ASIA", "UNKNOWN",
                 "AFRICA", "SOUTH AMERICA", "OCEANIA")
colori_cont <- wes_palette("Zissou1", 7, type = "continuous")[7:1]

ta_cont    <- table(data$continentName, data$century)[nam_cont, ]
tab_cont   <- as.vector(ta_cont)
somme_cont <- colSums(ta_cont)
tab2_cont  <- as.vector(ta_cont / matrix(rep(somme_cont, 7), nrow = 7, byrow = TRUE))

pre0_cont    <- sapply(seq_along(nam_cont),
                       function(i) sum(ta_cont[i, 1:30]) / sum(somme_cont[1:30]))
count_pre0c  <- sapply(1:7, function(i) sum(tab_cont[seq(i, 210, 7)]))

dati_cont <- data.frame(
  Century    = rep(c("<0", seq(0, 1900, 100)), each = 7),
  Continente = rep(nam_cont, times = 21),
  Count      = c(count_pre0c, tab_cont[211:350]),
  Perc       = c(pre0_cont,   tab2_cont[211:350])
)
dati_cont$Continente <- factor(dati_cont$Continente, levels = nam_cont[7:1])
dati_cont$Century    <- factor(dati_cont$Century,    levels = dati_cont$Century[seq(1, 147, 7)])
arranged_cont        <- dati_cont %>% group_by(Century) %>% arrange(Continente)

p_leg_cont <- ggplot(arranged_cont,
                     aes(x = as.numeric(Century), y = Perc, fill = Continente)) +
  geom_area() +
  labs(fill = "Continente") +
  theme_minimal() +
  theme(
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11),
    legend.key.size  = unit(0.6, "cm"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.spacing.y = unit(0.35, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_fill_manual(values = colori_cont)
leg_cont <- ggdraw(get_legend(p_leg_cont))

theme_cont <- theme(
  axis.text.x     = element_text(angle = 45, hjust = 1, size = 12),
  axis.text.y     = element_text(size = 12),
  axis.title.x    = element_text(size = 14, face = "bold", margin = margin(t = 10)),
  axis.title.y    = element_text(size = 14, face = "bold", margin = margin(r = 10)),
  plot.title      = element_text(hjust = 0.5, size = 18, face = "bold"),
  legend.position = "none"
)

p_cont1 <- ggplot(arranged_cont,
                  aes(x = as.numeric(Century), y = Count, fill = Continente)) +
  geom_area() +
  labs(title = "Individui per continente, nei secoli",
       x = "Secolo", y = "Numero di individui", fill = "Continente") +
  theme_minimal() + theme_cont +
  scale_fill_manual(values = colori_cont) +
  scale_x_continuous(breaks = 1:21, labels = levels(arranged_cont$Century))

p_cont2 <- ggplot(arranged_cont,
                  aes(x = as.numeric(Century), y = Perc, fill = Continente)) +
  geom_area() +
  labs(title = "Percentuale di individui per continente, nei secoli",
       x = "Secolo", y = "Percentuale di individui", fill = "Continente") +
  theme_minimal() + theme_cont +
  scale_fill_manual(values = colori_cont) +
  scale_x_continuous(breaks = 1:21, labels = levels(arranged_cont$Century))

grid.arrange(p_cont1, p_cont2, leg_cont, nrow = 1, widths = c(1, 1, 0.2))

# =============================================================================
# 5. PANNELLO FINALE — Genere per secolo e per decennio (1900)
# =============================================================================
# Combina p1/p2 (genere per secolo, sezione 1) con p3/p4 (genere per decennio,
# sezione 2) in un layout a griglia, con legenda condivisa (p5).

grid.arrange(
  arrangeGrob(p1, p3, ncol = 1, heights = c(1, 1)),
  arrangeGrob(p2, p4, ncol = 1, heights = c(1, 1)),
  p5,
  ncol = 3,
  widths = c(1, 1, 0.2),
  layout_matrix = rbind(c(1, 2, 3), c(1, 2, 3)))
