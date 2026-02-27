# =============================================================================
# scripts/05a_exploratory_descriptive.R -- Analisi descrittiva esplorativa
# =============================================================================
# Tipo:   Analisi + export Gephi
#
# Scopo:  Statistiche descrittive e visualizzazioni sui dati Pantheon:
#         distribuzioni per paese, citta, continente, secolo, genere, dominio,
#         occupation; analisi Views (Wikipedia page views), HPI e BCI
#         (Historical Popularity Index, Base Cultural Index) per gruppi.
#         Regressioni semplici centralita vs. popolarita.
#         Export della rete principale in formato CSV per Gephi.
#
# Input:  R/network_utils.R (dati: data, grafi: gg2, grafo)
#
# Output: output/nodes_gephi.csv   -- tabella nodi per Gephi
#         output/edges_gephi.csv   -- edge list per Gephi
#         Figure a schermo (nessun altro file su disco).
#
# Dipendenze: ggplot2, dplyr, cowplot, wesanderson, scales, ggrepel,
#             colorspace, xtable, gridExtra, readr
#             (oltre a igraph via network_utils.R)
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         Le sezioni Views/HPI/BCI usano gg2 (rete senza isolati, named).
#         mean_distance()/distances()/global_efficiency() sono computazionalmente
#         pesanti (qualche minuto su gg2 con 11.340 nodi).
# =============================================================================
source("R/network_utils.R")
library(readr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(wesanderson)
library(scales)
library(ggrepel)
library(colorspace)
library(xtable)
library(gridExtra)
# =============================================================================
# Export rete principale per Gephi
# =============================================================================
# Nodi: colonne Id, Name, domain, occupation, gender, countryName, HPI
write_csv(
  data[, c(1, 2, 4, 5, 6, 10, 13)],
  "output/nodes_gephi.csv"
)
# Archi: edge list del grafo principale (grafo con isolati)
# Gephi import: File > Import Spreadsheet > Edges table (Source, Target)
write_csv(
  as.data.frame(as_edgelist(grafo)),
  "output/edges_gephi.csv"
)
# =============================================================================
# Statistiche descrittive e visualizzazioni
# =============================================================================
# Tabelle
{
{
tab = table(data$countryName)
tabb = sort(tab, decreasing = T)
tabella = tabb[1:15] %>% as.data.frame()
levels(tabella$Var1) = c(levels(tabella$Var1), 'OTHER')
tabella[16,] = c('OTHER', sum(tabb[16:194]))
tabella$Freq = as.numeric(tabella$Freq)
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
tabella$Continente = factor(c('NORTH AMERICA', 'EUROPE', 'EUROPE', 'EUROPE', 'EUROPE', 'UNKNOWN', 'EUROPE', 'EUROPE', 'ASIA', 'EUROPE', 'EUROPE', 'EUROPE', 'ASIA', 'EUROPE', 'ASIA', 'OTHER'),
                      levels = c('EUROPE', 'NORTH AMERICA', 'ASIA', 'UNKNOWN', 'AFRICA', 'SOUTH AMERICA', 'OCEANIA'))
}
p2 = ggplot(tabella, aes(x = Var1, y = Freq, fill = Continente)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza dei Paesi", x = "Paese", y = "Frequenza", colours='Continente') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = 'black') +
  scale_fill_manual(values = c('EUROPE' = '#3A9AB2', 'NORTH AMERICA' = "#85B7B9", 'ASIA' = "#ADC397", 'UNKNOWN' = "#DCCB4E"))


{
tab = table(data$birthcity)
length(tab)
sort(tab[tab>20], decreasing = T)
tabb = sort(tab, decreasing = T)
tabella = tabb[1:16] %>% as.data.frame()
tabella[17,] = c('OTHER', sum(tabb[17:194]))
tabella$Freq = as.numeric(tabella$Freq)
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
tabella$Continente = factor(c('OTHER', 'NORTH AMERICA', 'EUROPE', 'NORTH AMERICA', 'EUROPE', 'EUROPE', 'NORTH AMERICA', 'EUROPE', 'ASIA', 'EUROPE', 'NORTH AMERICA', 'EUROPE', 'NORTH AMERICA', 'EUROPE', 'EUROPE', 'SOUTH AMERICA', 'OTHER'),
                            levels = c('EUROPE', 'NORTH AMERICA', 'ASIA', 'UNKNOWN', 'AFRICA', 'SOUTH AMERICA', 'OCEANIA'))

}
p3 = ggplot(tabella[2:16,] %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Continente)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza delle CittÃ ", subtitle = 'Senza "Other"', x = "CittÃ ", y = "Frequenza", colours='Continente') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), legend.position = 'none') +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = 'black') +
  scale_fill_manual(values = c('EUROPE' = '#3A9AB2', 'NORTH AMERICA' = "#85B7B9", 'ASIA' = "#ADC397", 'SOUTH AMERICA' = "#ED6E04"))

{
tab = table(data$continentName)
tabb = sort(tab, decreasing = T)
tabella = tabb %>% as.data.frame()
tabella$Freq = as.numeric(tabella$Freq)
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
}
{
p1 = ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza dei Continenti", x = "Continente", y = "Frequenza", fill='Continente') +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("Zissou1", 7, type = "continuous")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, color = 'black') +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.key.size = unit(0.8, "cm"),
          legend.spacing.x = unit(0.5, "cm"),
          legend.spacing.y = unit(0.5, "cm") ) +
    guides(
      color = guide_legend(override.aes = list(size = 5)))
  
leg = get_legend(p1)
}
p5 = ggdraw(leg)

p1 = ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza dei Continenti", 
       x = "Continente", 
       y = "Frequenza") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  scale_fill_manual(values = wes_palette("Zissou1", 7, type = "continuous")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, color = 'black')

grid.arrange(p1,p2,p5,p3, nrow=2,ncol=2, widths=c(0.4,0.6))
.
{
tabella = matrix((c(seq(-3500,2000,100), rep(NA, 56))), ncol=2)
for(i in tabella[,1]){
  tabella[(i+3500)/100+1,2] = sum(data$century==as.numeric(i))
}
tabella = data.frame('V1' = c('<0', tabella[36:55,1]),
                     'V2' = c(sum(tabella[1:35,2]), tabella[36:55,2]))
tabella$V1 = factor(tabella$V1, levels=tabella$V1)
}
{
colori = colorspace::sequential_hcl(28, palette = "Blues")[1:21]
idx = order(tabella$V2, decreasing = T)
colori = colori[order(idx)]
}
p1 = ggplot(tabella, aes(x = V1, y = V2, fill = V1)) +
  geom_bar(stat = "identity") +
  labs(title = "Numero di individui per Secolo", x = "Secolo", y = "NumerositÃ ") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = 'none'
  ) + 
  scale_fill_manual(values = colori)

p2 = ggplot(tabella, aes(x = V1, y = V2 %>% log(10), fill = V1)) +
  geom_bar(stat = "identity") +
  labs(title = "Log-Numero di individui per Secolo", x = "Secolo", y = "Log-NumerositÃ ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none',
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = colori)

grid.arrange(p1, p2, nrow = 1)



tab5 = table(data$gender)
tab5
prop.table(tab5)


{
tab = table(data$domain)
tabb = sort(tab, decreasing = T)
tabella = tabb %>% as.data.frame()
tabella$Freq = as.numeric(tabella$Freq)
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
}
{
p1 = ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza dei Domini", x = "Dominio", y = "Frequenza", fill='Dominio') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 13),
        legend.key.size = unit(0.7, "cm"),
        legend.spacing.x = unit(0.4, "cm"),
        legend.spacing.y = unit(0.4, "cm")) +
    guides(color = guide_legend(override.aes = list(size = 4.5))) +
  scale_fill_manual(values = wes_palette("Zissou1", 8, type = "continuous")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, color = 'black')
leg = get_legend(p1)
}
p5 = ggdraw(leg)
p1 = ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza dei Domini", x = "Dominio", y = "Frequenza") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  scale_fill_manual(values = wes_palette("Zissou1", 8, type = "continuous")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, color = 'black')

{
tab = table(data$occupation)
tabb = sort(tab, decreasing = T)
tabella = tabb[1:15] %>% as.data.frame()
levels(tabella$Var1) = c(levels(tabella$Var1), 'OTHER')
tabella[16,] = c('OTHER', sum(tabb[16:88]))
tabella$Freq = as.numeric(tabella$Freq)
tabella$Domain = factor(c('INSTITUTIONS', 'ARTS', 'SPORTS', 'HUMANITIES', 'INSTITUTIONS',
                       'ARTS', 'ARTS', 'HUMANITIES', 'SCIENCE', 'ARTS', 'INSTITUTIONS',
                       'SCIENCE', 'ARTS', 'ARTS', 'SPORTS', 'OTHER'),
                     levels=c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'OTHER'))
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
}
p2 = ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill=Domain)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza di Occupation", x = "Occupation", y = "Frequenza", colours='Domain') +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  scale_fill_manual(values = c('INSTITUTIONS' = "#3A9AB2", 'ARTS' = "#7DB5BC", 'SPORTS' = "#A2C0A5", 'SCIENCE' = "#C5C872", 'HUMANITIES' = "#E1BC21", 'OTHER' = 'grey')) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = 'black')

grid.arrange(p1, p2, p5, nrow = 1, widths=c(1,1,0.2))

{
tab = sort(table(data$occupation[data$birthyear >= 1900]), decreasing=T)
tabb = sort(tab, decreasing = T)
tabella = tabb[1:15] %>% as.data.frame()
levels(tabella$Var1) = c(levels(tabella$Var1), 'OTHER')
tabella[16,] = c('OTHER', sum(tabb[16:50]))
tabella$Freq = as.numeric(tabella$Freq)
tabella$Domain = factor(c('ARTS', 'SPORTS', 'INSTITUTIONS', 'ARTS', 'HUMANITIES',
                          'ARTS', 'SPORTS', 'ARTS', 'SCIENCE', 'SCIENCE', 'SPORTS',
                          'SCIENCE', 'SCIENCE', 'SPORTS', 'SPORTS', 'OTHER'),
                        levels=c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'OTHER'))
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
}
ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Domain)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza di Occupation dopo il 1900", x = "Occupation", y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('INSTITUTIONS' = "#3A9AB2", 'ARTS' = "#7DB5BC", 'SPORTS' = "#A2C0A5", 'SCIENCE' = "#C5C872", 'HUMANITIES' = "#E1BC21", 'OTHER' = 'grey')) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = 'black')

{
tab = sort(table(data$occupation[data$birthyear < 1900]), decreasing=T)
tabb = sort(tab, decreasing = T)
tabella = tabb[1:15] %>% as.data.frame()
levels(tabella$Var1) = c(levels(tabella$Var1), 'OTHER')
tabella[16,] = c('OTHER', sum(tabb[16:64]))
tabella$Freq = as.numeric(tabella$Freq)
tabella$Domain = factor(c('INSTITUTIONS', 'HUMANITIES', 'INSTITUTIONS', 'HUMANITIES',
                          'INSTITUTIONS', 'ARTS', 'ARTS', 'SCIENCE', 'SCIENCE',
                          'SCIENCE', 'SCIENCE', 'PUBLIC FIGURE', 'INSTITUTIONS',
                          'ARTS', 'EXPLORATION', 'OTHER'),
                        levels=c('INSTITUTIONS', 'ARTS', 'SCIENCE', 'HUMANITIES',
                                 'PUBLIC FIGURE', 'EXPLORATION','OTHER'))
tabella$percentage = tabella$Freq / sum(tabella$Freq) * 100
}
ggplot(tabella %>% as.data.frame(), aes(x = Var1, y = Freq, fill = Domain)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequenza di Occupation prima del 1900", x = "Occupation", y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c('INSTITUTIONS' = "#3A9AB2", 'ARTS' = "#7DB5BC", 'SPORTS' = "#A2C0A5", 'SCIENCE' = "#C5C872", 'HUMANITIES' = "#E1BC21", 'PUBLIC FIGURE' = "#E79305", 'BUSINESS' = "#ED6603", 'EXPLORATION' = "#F11B00", 'OTHER' = 'grey')) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = 'black')



# Frequenze relative dei 4 occupation piÃ¹ presenti
a = rep(NA, 20)
names(a) = seq(0,1900,100)
for (i in seq(0,1900,100)){
  tabl = sort(table(data$occupation[data$century == i]), decreasing=T)
  a[i/100+1] = sum(tabl[1:4])/sum(tabl)
}
a
plot(a)



library(scales)
library(ggrepel)

# posso aggiungere anche il gender ai barplot? dividendo le barre in due:
# i seguenti grafici diventano inutili
# perÃ² occhio alle percentuali! rischiano di diventare too much/inutili
{
df <- data.frame(
  category = rep(c('A', 'B', 'C', 'D'), each = 2),
  gender = rep(c('Maschio', 'Femmina'), times = 4),
  count = c(10, 15, 20, 25, 30, 35, 40, 45)
)
# Crea il barplot con barre divise per genere
p <- ggplot(df, aes(x = category, y = count, fill = gender)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(x = "Categoria", y = "Conteggio", fill = "Genere", title = "Barplot con Barre Divise per Genere") +
  theme_minimal()
# Mostra il grafico
print(p)
}

{
df = data.frame(
  Domain = rep(c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'),each=2),
  Genere = rep(c('Male', 'Female'), 8),
  Freq = c(3242, 214, 2072, 793, 1635, 121, 1323, 43, 1209, 120, 166, 192, 103, 5, 95, 7)
)
df$Domain = factor(df$Domain, levels = c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'))
df <- df %>%
  group_by(Genere) %>%
  mutate(Percent = Freq / sum(Freq) * 100)
}
p1 = ggplot(df, aes(x = Domain, y = Freq, fill = Genere)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Frequenze nei Domini per Genere",
       x = "Dominio", y = "Frequenza") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, by = 500))

{
df = data.frame(
  Domain = rep(c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'),each=2),
  Genere = rep(c('Male', 'Female'), 8),
  Freq = c(996, 101, 1517, 761, 1614, 121, 622, 25, 367, 68, 92, 95, 70, 5, 27, 7)
)
df$Domain = factor(df$Domain, levels = c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'))
df <- df %>%
  group_by(Genere) %>%
  mutate(Percent = Freq / sum(Freq) * 100)
}
p2 = ggplot(df, aes(x = Domain, y = Freq, fill = Genere)) +
  geom_bar(stat = "identity", position = "stack") +
  #geom_text(aes(label = paste0(round(Percent), "%")), 
  #          position = position_stack(vjust = 0.5), size=3, check_overlap = T) +
  labs(title = "Nel 1900",
       #subtitle = 'Le percentuali sono relative al Genere',
       x = "Dominio", y = "Frequenza") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 14),
    legend.key.size = unit(0.7, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.spacing.y = unit(0.4, "cm") ) +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, by = 500))

{
df = data.frame(
  Domain = rep(c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'),each=2),
  Genere = rep(c('Male', 'Female'), 8),
  Freq = c(2246, 112, 555, 31, 21, 0, 701, 18, 842, 52, 74, 97, 33, 0, 68, 0)
)
df$Domain = factor(df$Domain, levels = c('INSTITUTIONS', 'ARTS', 'SPORTS', 'SCIENCE', 'HUMANITIES', 'PUBLIC FIGURE', 'BUSINESS', 'EXPLORATION'))
df <- df %>%
  group_by(Genere) %>%
  mutate(Percent = Freq / sum(Freq) * 100)
}
p3 = ggplot(df, aes(x = Domain, y = Freq, fill = Genere)) +
  geom_bar(stat = "identity", position = "stack") +
  #geom_text(aes(label = paste0(round(Percent), "%")), 
  #          position = position_stack(vjust = 0.5), size=3, check_overlap = T) +
  labs(title = "Prima del 1900",
       #subtitle = 'Le percentuali sono relative al Genere',
       x = "Dominio", y = "Frequenza") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = 'none') +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, by = 500))

grid.arrange(p1, p3, p2, nrow = 1, widths = c(0.3,0.3,0.4))

# Distribuzione delle occupazioni per periodo (confronto pre/post 1900)
{
nove = ifelse(data$century>=1900, '1900', '<1900')
nomi = table(data$occupation) %>% sort(decreasing = T) %>% names %>% head(15)
tab = table(nove, data$occupation)[,nomi]
nomi2 = names(sort(table(data$occupation), decreasing = T))[16:88]
table(nove, data$occupation)[,nomi2] %>% t %>% colSums()

df = data.frame(
  Occupation = rep(c(nomi, 'OTHER'), each=2),
  Secolo = rep(c('<1900', '1900'), 16),
  Freq = c(tab, 1127, 1402))
df$Occupation = factor(df$Occupation, levels = c(nomi, 'OTHER'))
df <- df %>%
  group_by(Secolo) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

ggplot(df, aes(x = Occupation, y = Freq, fill = Secolo)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percent), "%")), 
            position = position_stack(vjust = 0.5), size=3, check_overlap = T) +
  labs(title = "Frequenze delle occupation differenziate tra pre e post 1900",
       subtitle = 'Le percentuali sono relative al periodo storico', x = "Occupation", y = "Frequenza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

{
tab = table(data$domain[data$gender=='Male'])
tabella = tab %>% as.data.frame()
tabella <- tabella %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")), color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini Maschi")

tab = table(data$domain[data$gender=='Female'])
tabella2 = tab %>% as.data.frame()
tabella2 <- tabella2 %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")),
                  color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini Femmine")
}
{
tab = table(data$domain[data$century<1900 & data$gender=='Male'])
tabella = tab %>% as.data.frame()
tabella <- tabella %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")),
                  color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini")

tab = table(data$domain[data$century<1900 & data$gender=='Female'])
tabella = tab %>% as.data.frame()
tabella <- tabella %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")),
                  color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini")

tab = table(data$domain[data$century==1900 & data$gender=='Male'])
tabella = tab %>% as.data.frame()
tabella <- tabella %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")),
                  color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini")

tab = table(data$domain[data$century==1900 & data$gender=='Female'])
tabella = tab %>% as.data.frame()
tabella <- tabella %>%
  mutate(percentage = Freq / sum(Freq) * 100,
    ypos = 100 - (cumsum(percentage) - 0.5 * percentage))
ggplot(tabella, aes(x = "", y = percentage, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(y = ypos, label = paste0(round(percentage, 1), "%")),
                  color = 1, nudge_x=0.75) +
  scale_fill_brewer(palette = "Set3") +
  theme_void() +
  theme(legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  ggtitle("Domini")
}

nomi = names(sort(table(data$domain), decreasing = T))
tab10 = table(data$domain[data$countryName=='UNITED STATES'])[nomi]
tab11 = table(data$domain[data$countryName=='UNITED KINGDOM'])[nomi]
tab12 = table(data$domain[data$countryName=='FRANCE'])[nomi]
tab13 = table(data$domain[data$countryName=='ITALY'])[nomi]
tab14 = table(data$domain[data$countryName=='GERMANY'])[nomi]
matrice = cbind(round(prop.table(tab10),2),
                round(prop.table(tab11),2),
                round(prop.table(tab12),2),
                round(prop.table(tab13),2),
                round(prop.table(tab14),2))
colnames(matrice) = c('UNITED STATES', 'UNITED KINGDOM', 'FRANCE', 'ITALY', 'GERMANY')
matrice
# questa per analizzare gli unici paesi oltre le 500 unitÃ  (valuta se aggiungerne ma dubito)

# Distribuzione dei domini nei principali paesi (figure nate dopo il 1900)
{
tab101 = sort(table(data$domain[data$birthyear>=1900 & data$countryName=='UNITED STATES']),decreasing = T)
tab111 = sort(table(data$domain[data$birthyear>=1900 & data$countryName=='UNITED KINGDOM']),decreasing = T)
tab121 = sort(table(data$domain[data$birthyear>=1900 & data$countryName=='FRANCE']),decreasing = T)
tab131 = sort(table(data$domain[data$birthyear>=1900 & data$countryName=='ITALY']),decreasing = T)
tab141 = sort(table(data$domain[data$birthyear>=1900 & data$countryName=='GERMANY']),decreasing = T)
round(prop.table(tab101),2)[1:5]
round(prop.table(tab111),2)[1:5]
round(prop.table(tab121),2)[1:5]
round(prop.table(tab131),2)[1:5]
round(prop.table(tab141),2)[1:5]
tab102 = sort(table(data$domain[data$birthyear<1900 & data$countryName=='UNITED STATES']),decreasing = T)
tab112 = sort(table(data$domain[data$birthyear<1900 & data$countryName=='UNITED KINGDOM']),decreasing = T)
tab122 = sort(table(data$domain[data$birthyear<1900 & data$countryName=='FRANCE']),decreasing = T)
tab132 = sort(table(data$domain[data$birthyear<1900 & data$countryName=='ITALY']),decreasing = T)
tab142 = sort(table(data$domain[data$birthyear<1900 & data$countryName=='GERMANY']),decreasing = T)
round(prop.table(tab102),2)[1:5]
round(prop.table(tab112),2)[1:5]
round(prop.table(tab122),2)[1:5]
round(prop.table(tab132),2)[1:5]
round(prop.table(tab142),2)[1:5]
}

# questo invece penso sia utile: paesi che contribuiscono di piÃ¹ ad ogni dominio
tab10 = sort(table(data$countryName[data$domain=='ARTS']),decreasing = T)[1:5]
tab11 = sort(table(data$countryName[data$domain=='SPORTS']),decreasing = T)[1:5]
tab12 = sort(table(data$countryName[data$domain=='INSTITUTIONS']),decreasing = T)[1:5]
tab13 = sort(table(data$countryName[data$domain=='SCIENCE']),decreasing = T)[1:5]
tab14 = sort(table(data$countryName[data$domain=='HUMANITIES']),decreasing = T)[1:5]
tab15 = sort(table(data$countryName[data$domain %in% c('BUSINESS', 'PUBLIC FIGURE', 'EXPLORATION')]),decreasing = T)[1:5]
round(prop.table(tab10),2)
round(prop.table(tab11),2)
round(prop.table(tab12),2)
round(prop.table(tab13),2)
round(prop.table(tab14),2)
round(prop.table(tab15),2)

df = data.frame(
  'INSTITUTIONS' = names(tab12),
  'Perc.Inst.' = round(tab12/3456,2) %>% c,
  'ARTS' = names(tab10),
  'Perc.Arts' = round(tab10/2865,2) %>% c,
  'SPORTS' = names(tab11),
  'Perc.Sports' = round(tab11/1756,2) %>% c,
  'SCIENCE' = names(tab13),
  'Perc.Sci.' = round(tab13/1366,2) %>% c,
  'HUMANITIES' = names(tab14),
  'Perc.Hum.' = round(tab14/1329,2)  %>% c,
  'OTHER' = names(tab15),
  'Perc.Oth.' = round(tab15/568, 2) %>% c)

round(tab15/568, 2)

rownames(df) = NULL
df
colSums(df[,c(2,4,6,8,10,12)])
xtable(df)
}

# Views
{
{
vi = data$averageViews
names(vi) = data$Name
unique(vi) %>% length
sort(vi, decreasing = T) %>% head(30)

nomi = table(data$countryName) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$averageViews[data$countryName == i])
}
vettore[21] = mean(data$averageViews[!(data$countryName %in% nomi)])
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$averageViews[data$countryName == i])
}
vettore2[21] = sum(data$averageViews[!(data$countryName %in% nomi)])
tabella = data.frame('Paese'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Paese <- factor(tabella$Paese, levels = tabella$Paese)
}
{
colori1 = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella$Tot[1:20], decreasing = T)
colori = c(colori1[order(idx)], 'grey')
}
p2 = ggplot(tabella, aes(x = Paese, y = Tot, fill = Paese)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "Views medie totali e medie per Paese", x = "Paese", y = "Views totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Paese, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Views Totali",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "Views Medie"))


{
nomi = table(data$continentName) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 7)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$averageViews[data$continentName == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 7)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$averageViews[data$continentName == i])
}
tabella = data.frame('Cont'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Cont = factor(tabella$Cont, levels = tabella$Cont)
}
{
colori = colorspace::sequential_hcl(10, palette = "Blues")[1:7]
idx = order(tabella$Tot, decreasing = T)
}
p1 = ggplot(tabella, aes(x = Cont, y = Tot, fill = Cont)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "Views totali e medie per Continente", x = "Continente", y = "Views totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Cont, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Views Totali",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "Views Medie"))


{
nomi = table(data$domain) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 8)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$averageViews[data$domain == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 8)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$averageViews[data$domain == i])
}
tabella = data.frame(Domain =names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Domain = factor(tabella$Domain, levels = tabella$Domain)
}
{
colori = colorspace::sequential_hcl(12, palette = "Blues")[1:8]
idx = order(tabella$Tot, decreasing = T)
}
p4 = ggplot(tabella, aes(x = Domain, y = Tot, fill = Domain)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "Views totali e medie per Dominio", x = "Dominio", y = "Views totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Domain, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Views Totali",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "Views Medie"))


{
nomi = seq(0,1900,100)
vettore = rep(NA, length(nomi))
names(vettore) = as.character(nomi)
for (i in nomi){
  vettore[as.character(i)] = mean(data$averageViews[data$century == as.numeric(i)], na.rm = T)
}
vettore = c(mean(data$averageViews[data$century < 0], na.rm = T), vettore)
names(vettore)[1] = '<0'
vettore2 = rep(NA, length(nomi))
names(vettore2) = as.character(nomi)
for (i in nomi){
  vettore2[as.character(i)] = sum(data$averageViews[data$century == as.numeric(i)], na.rm = T)
}
vettore2 = c(sum(data$averageViews[data$century < 0], na.rm = T), vettore2)
names(vettore2)[1] = '<0'
tabella = data.frame(Century = c('<0', nomi), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Century = factor(tabella$Century, levels = tabella$Century)
}
{
colori = colorspace::sequential_hcl(35, palette = "Blues")[1:21]
idx = order(tabella$Ave, decreasing = T)
}
p3 = ggplot(tabella, aes(x = Century, y = Tot, fill = Century)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "Views totali e medie per Secolo", x = "Secolo", y = "Views totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Century, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Views Totali",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "Views Medie"))

grid.arrange(p1,p2,p3,p4, nrow=2)

# genere, inutile
{
nomi = c('Male', 'Female')
vettore = rep(NA, length(nomi))
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$averageViews[data$gender == i])
}
vettore
barplot(vettore)
vettore2 = rep(NA, length(nomi))
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$averageViews[data$gender == i])
}
vettore2
barplot(vettore2)
}

# occupation, utile?
{
nomi = table(data$occupation) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$averageViews[data$occupation == i])
}
vettore[21] = mean(data$averageViews[!(data$occupation %in% nomi)])
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$averageViews[data$occupation == i])
}
vettore2[21] = sum(data$averageViews[!(data$occupation %in% nomi)])
tabella = data.frame(Occupation = c(nomi, 'OTHER'), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Occupation = factor(tabella$Occupation, levels = tabella$Occupation)
tabella$Ave = ifelse(is.na(tabella$Ave), 0, tabella$Ave)
}
{
colori = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella$Ave, decreasing = T)
colori = c(colori[order(idx)], grey)
}
ggplot(tabella, aes(x = Occupation, y = Tot, fill = Occupation)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "Views totali e medie per Occupation", x = "Occupation", y = "Views totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  geom_line(aes(x = Occupation, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "Views Totali",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "Views Medie"))



plot(degree(gg2), data$averageViews)
summary(lm(data$averageViews ~ degree(gg2)))

plot(betweenness(gg2), data$averageViews)
summary(lm(data$averageViews ~ betweenness(gg2)))

plot(page_rank(gg2)$vector, data$averageViews)
summary(lm(data$averageViews ~ page_rank(gg2)$vector))
abline(lm(data$averageViews ~ page_rank(gg2)$vector))

}

# HPI
{
{
hpi = data$HPI
names(hpi) = data$Name
unique(hpi) %>% length
sort(hpi, decreasing = T) %>% head(30)

nomi = table(data$continentName) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 7)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$HPI[data$continentName == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 7)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$HPI[data$continentName == i])
}
tabella1 = data.frame('Cont'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella1) = NULL
tabella1$Cont = factor(tabella1$Cont, levels = tabella1$Cont)
}
{
colori = colorspace::sequential_hcl(10, palette = "Blues")[1:7]
idx = order(tabella1$Tot, decreasing = T)
}
p1 = ggplot(tabella1, aes(x = Cont, y = Tot, fill = Cont)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "HPI totale e medio per Continente", x = "Continente", y = "HPI totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Cont, y = Ave * max(Tot) / max(Ave) - 50000, group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella1$Ave) / max(tabella1$Tot) + 50000*max(tabella1$Ave) / max(tabella1$Tot), 
                      name = "HPI medio"))

{
nomi = table(data$countryName) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$HPI[data$countryName == i])
}
vettore[21] = mean(data$HPI[!(data$countryName %in% nomi)])
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$HPI[data$countryName == i])
}
vettore2[21] = sum(data$HPI[!(data$countryName %in% nomi)])
tabella2 = data.frame('Paese'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella2) = NULL
tabella2$Paese <- factor(tabella2$Paese, levels = tabella2$Paese)
}
{
colori1 = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella2$Tot[1:20], decreasing = T)
colori = c(colori1[order(idx)], 'grey')
}
p2 = ggplot(tabella2, aes(x = Paese, y = Tot, fill = Paese)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "HPI medio e totale per Paese", x = "Paese", y = "HPI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Paese, y = Ave * max(Tot) / max(Ave) -20000, group = 1),
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella2$Ave) / max(tabella2$Tot) +20000*max(tabella2$Ave) / max(tabella2$Tot),
                        name = "HPI medio"))


{
nomi = seq(0,1900,100)
vettore = rep(NA, length(nomi))
names(vettore) = as.character(nomi)
for (i in nomi){
  vettore[as.character(i)] = mean(data$HPI[data$century == as.numeric(i)], na.rm = T)
}
vettore = c(mean(data$HPI[data$century < 0], na.rm = T), vettore)
names(vettore)[1] = '<0'
vettore2 = rep(NA, length(nomi))
names(vettore2) = as.character(nomi)
for (i in nomi){
  vettore2[as.character(i)] = sum(data$HPI[data$century == as.numeric(i)], na.rm = T)
}
vettore2 = c(sum(data$HPI[data$century < 0], na.rm = T), vettore2)
names(vettore2)[1] = '<0'
tabella3 = data.frame(Century = c('<0', nomi), Ave = vettore, Tot = vettore2)
row.names(tabella3) = NULL
tabella3$Century = factor(tabella3$Century, levels = tabella3$Century)
tabella3$Ave = ifelse(is.na(tabella3$Ave), 0, tabella3$Ave)
}
{
colori = colorspace::sequential_hcl(100, palette = "Blues")[1:56]
idx = order(tabella3$Ave, decreasing = T)
}
p3 = ggplot(tabella3, aes(x = Century, y = Tot, fill = Century)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "HPI totale e medio per Secolo", x = "Secolo", y = "HPI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Century, y = Ave * max(Tot) / max(Ave) - 100000/2, group = 1),
            color = "red", size = 1) +
  scale_y_continuous(
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella3$Ave) / max(tabella3$Tot) +  100000/2 *max(tabella3$Ave) / max(tabella3$Tot), 
                        name = "HPI medio"))

{
nomi = table(data$domain) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 8)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$HPI[data$domain == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 8)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$HPI[data$domain == i])
}
tabella4 = data.frame(Domain =names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella4) = NULL
tabella4$Domain = factor(tabella4$Domain, levels = tabella4$Domain)
}
{
colori = colorspace::sequential_hcl(12, palette = "Blues")[1:8]
idx = order(tabella4$Tot, decreasing = T)
}
p4 = ggplot(tabella4, aes(x = Domain, y = Tot, fill = Domain)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "HPI totale e medio per Dominio", x = "Dominio", y = "HPI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(color = "blue", size = 14, face='bold', margin=margin(r=10)),
        axis.title.y.right = element_text(color = "red", size = 14, face='bold', margin=margin(r=10)), 
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = 'none') +
  geom_line(aes(x = Domain, y = Ave * max(Tot) / max(Ave) - 28000, group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella4$Ave) / max(tabella4$Tot) + 28000*(max(tabella4$Ave) / max(tabella4$Tot)), 
                        name = "HPI medio"))

grid.arrange(p1,p2,p3,p4, nrow=2)



# genere, inutile
{
nomi = c('Male', 'Female')
vettore = rep(NA, length(nomi))
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$HPI[data$gender == i])
}
vettore
barplot(vettore)
vettore2 = rep(NA, length(nomi))
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$HPI[data$gender == i])
}
vettore2
barplot(vettore2)
}

# occupation, utile?
{
nomi = table(data$occupation) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$HPI[data$occupation == i])
}
vettore[21] = mean(data$HPI[!(data$occupation %in% nomi)])
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$HPI[data$occupation == i])
}
vettore2[21] = sum(data$HPI[!(data$occupation %in% nomi)])
tabella = data.frame(Occupation = c(nomi, 'OTHER'), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Occupation = factor(tabella$Occupation, levels = tabella$Occupation)
tabella$Ave = ifelse(is.na(tabella$Ave), 0, tabella$Ave)
}
{
colori = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella$Ave, decreasing = T)
colori = c(colori[order(idx)])
}
ggplot(tabella, aes(x = Occupation, y = Tot, fill = Occupation)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = colori) +
labs(title = "HPI totale e medio per Occupation", x = "Occupation", y = "HPI totale") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
geom_line(aes(x = Occupation, y = Ave * max(Tot) / max(Ave), group = 1), 
          color = "red", size = 1) +
scale_y_continuous(
  name = "HPI totale",
  sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                      name = "HPI medio"))



plot(degree(gg2), data$HPI)
summary(lm(data$HPI ~ degree(gg2)))

plot(betweenness(gg2), data$HPI)
summary(lm(data$HPI ~ betweenness(gg2)))

plot(page_rank(gg2)$vector, data$HPI)
summary(lm(data$HPI ~ page_rank(gg2)$vector))
abline(lm(data$HPI ~ page_rank(gg2)$vector))

}

# HPI senza sport
# metti sottotitolo vuoto a HPI con sport, per confronto
{
dati = data[data$domain!='SPORTS',]
{
nomi = table(dati$continentName) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 7)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(dati$HPI[dati$continentName == i], na.rm = T)
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 7)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$HPI[dati$continentName == i], na.rm = T)
}
tabella11 = data.frame('Cont'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella11) = NULL
tabella11$Cont = factor(tabella11$Cont, levels = tabella11$Cont)
}
{
colori = colorspace::sequential_hcl(10, palette = "Blues")[1:7]
idx = order(tabella11$Tot, decreasing = T)
}
p11 = ggplot(tabella11, aes(x = Cont, y = Tot, fill = Cont)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "HPI totale e medio per Continente", x = "Continente", y = "HPI totali",
       subtitle = 'Senza sportivi') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"),
        legend.position = 'none', plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_line(aes(x = Cont, y = Ave * max(tabella1$Tot) / max(tabella1$Ave) - 50000, group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(limits=c(0,143946),
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella1$Ave) / max(tabella1$Tot) + 50000*max(tabella1$Ave) / max(tabella1$Tot), 
                      name = "HPI medio"))

{
nomi = table(dati$countryName) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(dati$HPI[data$countryName == i], na.rm = T)
}
vettore[21] = mean(data$HPI[!(dati$countryName %in% nomi)], na.rm = T)
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(dati$HPI[dati$countryName == i], na.rm = T)
}
vettore2[21] = sum(dati$HPI[!(dati$countryName %in% nomi)], na.rm = T)
tabella22 = data.frame('Paese'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella22) = NULL
tabella22$Paese <- factor(tabella22$Paese, levels = tabella22$Paese)
}
{
colori1 = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella22$Tot[1:20], decreasing = T)
colori = c(colori1[order(idx)], 'grey')
}
p22 = ggplot(tabella22, aes(x = Paese, y = Tot, fill = Paese)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "HPI medio e totale per Paese", x = "Paese", y = "HPI totale",
       subtitle = 'Senza sportivi') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"),
        legend.position = 'none', plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_line(aes(x = Paese, y = Ave * max(tabella2$Tot) / max(tabella2$Ave) -20000, group = 1),
            color = "red", linewidth = 1) +
  scale_y_continuous(limits = c(0,59559),
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella2$Ave) / max(tabella2$Tot) +20000*max(tabella2$Ave) / max(tabella2$Tot),
                        name = "HPI medio"))


{
nomi = seq(0,1900,100)
vettore = rep(NA, length(nomi))
names(vettore) = as.character(nomi)
for (i in nomi){
  vettore[as.character(i)] = mean(dati$HPI[dati$century == as.numeric(i)], na.rm = T)
}
vettore = c(mean(dati$HPI[dati$century < 0], na.rm = T), vettore)
names(vettore)[1] = '<0'
vettore2 = rep(NA, length(nomi))
names(vettore2) = as.character(nomi)
for (i in nomi){
  vettore2[as.character(i)] = sum(dati$HPI[dati$century == as.numeric(i)], na.rm = T)
}
vettore2 = c(sum(dati$HPI[dati$century < 0], na.rm = T), vettore2)
names(vettore2)[1] = '<0'
tabella33 = data.frame(Century = c('<0', nomi), Ave = vettore, Tot = vettore2)
row.names(tabella33) = NULL
tabella33$Century = factor(tabella33$Century, levels = tabella33$Century)
tabella33$Ave = ifelse(is.na(tabella33$Ave), 0, tabella33$Ave)
}
{
colori = colorspace::sequential_hcl(100, palette = "Blues")[1:56]
idx = order(tabella33$Ave, decreasing = T)
}
p33 = ggplot(tabella33, aes(x = Century, y = Tot, fill = Century)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "HPI totale e medio per Secolo", x = "Secolo", y = "HPI totale",
       subtitle = 'Senza sportivi') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"),
        legend.position = 'none', plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_line(aes(x = Century, y = Ave * max(tabella3$Tot) / max(tabella3$Ave) - 100000/2, group = 1),
            color = "red", size = 1) +
  scale_y_continuous(limits=c(0,133383),
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella3$Ave) / max(tabella3$Tot) +  100000/2 *max(tabella3$Ave) / max(tabella3$Tot), 
                        name = "HPI medio"))

{
nomi = table(dati$domain) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 7)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(dati$HPI[dati$domain == i], na.rm = T)
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 7)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(dati$HPI[dati$domain == i], na.rm = T)
}
tabella44 = data.frame(Domain = names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella44) = NULL
tabella44$Domain = factor(tabella44$Domain, levels = tabella44$Domain)
}
{
colori = colorspace::sequential_hcl(12, palette = "Blues")[1:7]
idx = order(tabella44$Tot, decreasing = T)
}
p44 = ggplot(tabella44, aes(x = Domain, y = Tot, fill = Domain)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "HPI totale e medio per Dominio", x = "Dominio", y = "HPI totale",
       subtitle = 'Senza sportivi') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red"),
        legend.position = 'none', plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_line(aes(x = Domain, y = Ave * max(tabella4$Tot) / max(tabella4$Ave) - 28000, group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(#limits=c(0,82128),
    name = "HPI totale",
    sec.axis = sec_axis(~ . * max(tabella4$Ave) / max(tabella4$Tot) + 28000*(max(tabella4$Ave) / max(tabella4$Tot)), 
                        name = "HPI medio"))

grid.arrange(p11,p22,p33,p44, nrow=2)

grid.arrange(p1,p11 ,p2,p2, p3,p33, p4,p44, nrow=2)
grid.arrange(p1,p11, nrow=1)
grid.arrange(p2,p22, nrow=1)
grid.arrange(p3,p33, nrow=1)
grid.arrange(p4,p44, nrow=1)
}


# BCI â€” Base Cultural Index: distribuzione e classifiche
{
{
bci = data$BCI
names(bci) = data$Name
unique(bci) %>% length
sort(bci, decreasing = T) %>% head(30)

nomi = table(data$countryName) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$BCI[data$countryName == i])
}
vettore[21] = mean(data$BCI[!(data$countryName %in% nomi)])
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$BCI[data$countryName == i])
}
vettore2[21] = sum(data$BCI[!(data$countryName %in% nomi)])
tabella = data.frame('Paese'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Paese <- factor(tabella$Paese, levels = tabella$Paese)
}
{
colori1 = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella$Tot[1:20], decreasing = T)
colori = c(colori1[order(idx)], 'grey')
}
ggplot(tabella, aes(x = Paese, y = Tot, fill = Paese)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "BCI medio e totale per Paese", x = "Paese", y = "BCI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  geom_line(aes(x = Paese, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "BCI totale",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "BCI medio"))


{
nomi = table(data$continentName) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 7)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$BCI[data$continentName == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 7)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$BCI[data$continentName == i])
}
tabella = data.frame('Cont'=names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Cont = factor(tabella$Cont, levels = tabella$Cont)
}
{
colori = colorspace::sequential_hcl(10, palette = "Blues")[1:7]
idx = order(tabella$Tot, decreasing = T)
}
ggplot(tabella, aes(x = Cont, y = Tot, fill = Cont)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori) +
  labs(title = "BCI totale e medio per Continente", x = "Continente", y = "BCI totali") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  geom_line(aes(x = Cont, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "BCI totale",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "BCI medio"))


{
nomi = table(data$domain) %>% sort(decreasing = T) %>% names()
vettore = rep(NA, 8)
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$BCI[data$domain == i])
}
vettore
sort(vettore, decreasing = T)
vettore2 = rep(NA, 8)
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$BCI[data$domain == i])
}
tabella = data.frame(Domain =names(vettore), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Domain = factor(tabella$Domain, levels = tabella$Domain)
}
{
colori = colorspace::sequential_hcl(12, palette = "Blues")[1:8]
idx = order(tabella$Tot, decreasing = T)
}
ggplot(tabella, aes(x = Domain, y = Tot, fill = Domain)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "BCI totale e medio per Dominio", x = "Dominio", y = "BCI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  geom_line(aes(x = Domain, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "BCI totale",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "BCI medio"))



{
nomi = seq(-3500,2000,100)
vettore = rep(NA, length(nomi))
names(vettore) = as.character(nomi)
for (i in nomi){
  vettore[as.character(i)] = mean(data$BCI[data$century == as.numeric(i)], na.rm = T)
}
vettore2 = rep(NA, length(nomi))
names(vettore2) = as.character(nomi)
for (i in nomi){
  vettore2[as.character(i)] = sum(data$BCI[data$century == as.numeric(i)], na.rm = T)
}
tabella = data.frame(Century = nomi, Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Century = factor(tabella$Century, levels = tabella$Century)
tabella$Ave = ifelse(is.na(tabella$Ave), 0, tabella$Ave)
}
{
colori = colorspace::sequential_hcl(100, palette = "Blues")[1:56]
idx = order(tabella$Ave, decreasing = T)
}
ggplot(tabella, aes(x = Century, y = Tot, fill = Century)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colori[order(idx)]) +
  labs(title = "BCI totale e medio per Secolo", x = "Secolo", y = "BCI totale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  geom_line(aes(x = Century, y = Ave * max(Tot) / max(Ave), group = 1), 
            color = "red", size = 1) +
  scale_y_continuous(
    name = "BCI totale",
    sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                        name = "BCI medio"))


# genere, inutile
{
nomi = c('Male', 'Female')
vettore = rep(NA, length(nomi))
names(vettore) = nomi
for (i in nomi){
  vettore[i] = mean(data$BCI[data$gender == i])
}
vettore
barplot(vettore)
vettore2 = rep(NA, length(nomi))
names(vettore2) = nomi
for (i in nomi){
  vettore2[i] = sum(data$BCI[data$gender == i])
}
vettore2
barplot(vettore2)
}

# occupation, utile?
{
nomi = table(data$occupation) %>% sort(decreasing = T) %>% names() %>%  head(20)
vettore = rep(NA, 21)
names(vettore) = c(nomi, 'OTHER')
for (i in nomi){
  vettore[i] = mean(data$BCI[data$occupation == i])
}
vettore[21] = mean(data$BCI[!(data$occupation %in% nomi)])
vettore2 = rep(NA, 21)
names(vettore2) = c(nomi, 'OTHER')
for (i in nomi){
  vettore2[i] = sum(data$BCI[data$occupation == i])
}
vettore2[21] = sum(data$BCI[!(data$occupation %in% nomi)])
tabella = data.frame(Occupation = c(nomi, 'OTHER'), Ave = vettore, Tot = vettore2)
row.names(tabella) = NULL
tabella$Occupation = factor(tabella$Occupation, levels = tabella$Occupation)
tabella$Ave = ifelse(is.na(tabella$Ave), 0, tabella$Ave)
}
{
colori = colorspace::sequential_hcl(30, palette = "Blues")[1:20]
idx = order(tabella$Ave, decreasing = T)
colori = c(colori[order(idx)])
}
ggplot(tabella, aes(x = Occupation, y = Tot, fill = Occupation)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = colori) +
labs(title = "BCI totale e medio per Occupation", x = "Occupation", y = "BCI totale") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
geom_line(aes(x = Occupation, y = Ave * max(Tot) / max(Ave), group = 1), 
          color = "red", size = 1) +
scale_y_continuous(
  name = "BCI totale",
  sec.axis = sec_axis(~ . * max(tabella$Ave) / max(tabella$Tot), 
                      name = "BCI medio"))



plot(degree(gg2), data$BCI)
summary(lm(data$BCI ~ degree(gg2)))

plot(betweenness(gg2), data$BCI)
summary(lm(data$BCI ~ betweenness(gg2)))

plot(page_rank(gg2)$vector, data$BCI)
summary(lm(data$BCI ~ page_rank(gg2)$vector))
abline(lm(data$BCI ~ page_rank(gg2)$vector))

}


me = mean_distance(gg2)
dista = distances(gg2)
ge = global_efficiency(gg2)
sss = similarity(gg2)

