# =============================================================================
# scripts/04b_indices_by_group.R ÔÇö Indici di centralit├á: tabelle e correlazioni
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Tabella top-10 per ciascun indice di centralit├á (degree, betweenness,
#         pagerank, harmonic, eigen, HPI). Distribuzione del grado (smoothed).
#         Scatter plot tra coppie di indici (grado vs betweenness/pagerank/eigen).
#         Correlazioni Pearson/Kendall/Spearman tra tutti gli indici.
#         Analisi comparative di reciprocit├á, transitivit├á e densit├á per
#         sottografi per dominio, secolo e paese (con e senza isolati).
#
# Input:  R/network_utils.R (grafi: gg2, conn, con + dati)
#
# Output: Figure e tabelle a schermo (nessun file scritto su disco).
#
# Dipendenze: igraph (via network_utils.R), circlize, RColorBrewer, xtable,
#             gridExtra, ComplexHeatmap, ggrepel, corrplot, cowplot, superheat
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         networkR sostituito con igraph::authority_score / hub_score.
#         selectDomain/Century/Continentname/Countryname ÔåÆ select_graph().
# =============================================================================
source("R/network_utils.R")library(circlize)
library(RColorBrewer)
library(xtable)
library(gridExtra)
library(ComplexHeatmap)
library(ggrepel)
library(corrplot)
library(cowplot)

# Funzioni
{
col_fun = function(mat) colorRamp2(c(min(mat), median(mat), max(mat)), c("white", "yellow", "red"))
col_fun2 = function(mat) colorRamp2(c(min(mat), (max(mat)-min(mat))/2+min(mat), max(mat)), c("white", "yellow", "red"))
mappa = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_title = paste(tit, 'in cui entrano gli archi'),
          row_title = paste(tit, 'da cui escono gli archi'),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))})}
mappa2 = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun2(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_title = paste(tit, 'in cui entrano gli archi'),
          row_title = paste(tit, 'da cui escono gli archi'),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))})
}
mappa3 = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          show_heatmap_legend=F,
          column_title = paste(tit, 'in cui entrano gli archi'),
          row_title = paste(tit, 'da cui escono gli archi'))
}
mappa4 = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_title = paste(tit, 'in cui entrano gli archi'),
          row_title = paste(tit, 'da cui escono gli archi'),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 3.5))}
  )}

get_legend_grob <- function(plot) {
  tmp <- ggplotGrob(plot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
}

# Tabella indici top10
{
auth_v <- igraph::authority_score(gg2)$vector
hub_v  <- igraph::hub_score(gg2)$vector
auth = data.frame(data$Name, auth_v)
auth = auth[order(auth[,2], decreasing=T),]
hub = data.frame(data$Name, hub_v)
hub = hub[order(hub[,2], decreasing=T),]

authority = auth$data.Name[1:10]
hubs = hub$data.Name[1:10]

grado = sort(igraph::degree(gg2), decreasing=T)[1:10] %>% names
ingrado = sort(igraph::degree(gg2, mode='in'), decreasing=T)[1:10] %>% names
outgrado = sort(igraph::degree(gg2, mode='out'), decreasing=T)[1:10] %>% names

betwe = sort(betweenness(gg2, directed=T), decreasing=T)[1:10] %>% names

harmo = sort(harmonic_centrality(gg2, mode = 'out'), decreasing=T)[1:10]  %>% names

eig = sort(eigen_centrality(gg2, directed=T)$vector, decreasing=T)[1:10]  %>% names

pager = sort(page_rank(gg2, directed=T)$vector, decreasing=T)[1:10] %>% names

hpi = (data$Name[order(-data$HPI)]) %>% head(10)

bci = (data$Name[order(-data$BCI)]) %>% head(10)

indici = data.frame(
  'Degree' = grado,
  #'In-Degree' = ingrado,
  #'Out-Degree' = outgrado,
  'Betweenness' = betwe,
  'PageRank' = pager,
  'Harmonic' = harmo,
  'Eigen' = eig,
  #'Hub' = hubs,
  #'Authority' = authority,
  #'BCI' = bci,
  'HPI' = hpi
)
rownames(indici) = NULL
# indegree simile a degree, outdegree simile a harmonic, hits simile a eigen

xtable(indici[,1:3])
xtable(indici[,4:6])
}

# Deg distrib
{
density_data <- density(gr, adjust = 0.25, from=0, cut=0)
df_density <- data.frame(x = density_data$x, y = density_data$y)
{
media_grado <- mean(gr)
mediana_grado <- median(gr)
ggplot(df_density, aes(x = x, y = y)) +
  geom_line(color = "steelblue", linewidth = 1, alpha=0.9) +
  labs(title = "Distribuzione lisciata del grado dei nodi",
       x = "Grado",
       y = "DensitÃ  lisciata") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold")) +
  geom_vline(xintercept = media_grado, color = "#D4002E", linetype = "dashed", linewidth = 0.8, alpha=0.65) +
  geom_vline(xintercept = mediana_grado, color = "#449023", linetype = "dashed", linewidth = 0.8, alpha=0.65) +
  annotate("text", x = media_grado + 7, y = 0.032, 
           label = paste("Media:", round(media_grado, 1)), 
           vjust = 1, hjust = -0.1, color = "#D4002E") +
  annotate("text", x = media_grado + 7, y = 0.022,
           label = paste("Mediana:", round(mediana_grado, 1)), 
           vjust = 1, hjust = -0.1, color = "#449023")
}


df_long <- data.frame(
  value = c(gr, ogr, igr),
  group = factor(rep(c("Grado", "Out-Degree", "In-Degree"), each = length(gr)))
)

ggplot(df_long, aes(x = value, color = group)) +
  geom_density(bw=0.5, linewidth=.8) +
  scale_color_manual(values = c("#D4002E", "#449023", "steelblue")) +
  theme_minimal() +
  labs(title = "DensitÃ  lisciata di grado, oout-degree e in-degree",
       x = "Grado",
       y = "DensitÃ  lisciata")
}


# Grafici indici (bet vs grado)
{
gr = degree(gg2)
b = betweenness(gg2, directed = T)
b = (b-min(b))/(max(b)-min(b))
pr = page_rank(gg2, directed = T)$vector
pr = (pr-min(pr))/(max(pr)-min(pr))
ec = eigen_centrality(gg2, directed = T)$vector
hc = harmonic_centrality(gg2, mode = 'out', normalized = T)
hc = (hc-min(hc))/(max(hc)-min(hc))

df = data.frame(
  Grado = gr,
  Betwe = b,
  Pager = pr,
  Eig = ec,
  Harm = hc,
  Dom = data$domain
)
pal = c('INSTITUTIONS' = "#3A9AB2", 'ARTS' = "#7DB5BC", 'SPORTS' = "#A2C0A5",
        'SCIENCE' = "#C5C872", 'HUMANITIES' = "#E1BC21", 'PUBLIC FIGURE' = "#E79305",
        'BUSINESS' = "#ED6603", 'EXPLORATION' = "#F11B00")
}

{
idx = sort(pr) %>% tail(6) %>% names
lab = ifelse(names(pr) %in% idx, names(pr), NA)
nu = rep(0, 11340)
nu[9740] = 0.01
df$Label = lab

p = ggplot(df, aes(x = Grado, y = Pager, color = Dom)) +
  geom_point(size = 1, alpha=0.7) +
  geom_line(method = "lm", formula = y ~ x, se = FALSE, color = "#ff7363", 
              linetype = 'dashed', orientation = "y", stat='smooth', alpha=0.6) +
  scale_color_manual(values = pal, breaks=names(pal)) + theme_minimal() +
  labs(title = "Relazione tra Grado e Pagerank",
       x = "Grado", y = "Pagerank", color = "Dominio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "bottom", 
        legend.direction = "horizontal") +
  guides(color = guide_legend(override.aes = list(size = 5)))
leg = get_legend_grob(p)

p5 = ggdraw(leg)

p1 = ggplot(df, aes(x = Grado, y = Pager, color = Dom)) +
  geom_point(size = 1, alpha=0.7) +
  geom_line(method = "lm", formula = y ~ x, se = FALSE, color = "#ff7363", 
            linetype = 'dashed', orientation = "y", stat='smooth', alpha=0.6) +
  geom_text_repel(aes(label = Label), na.rm=T,
                  box.padding = 0.15, point.padding = 0.15,
                  force = 0.2, force_pull = 1.85, nudge_y = nu, show.legend = FALSE, size = 3.1) +
  scale_color_manual(values = pal, breaks=names(pal)) + theme_minimal() +
  labs(title = "Relazione tra Grado e Pagerank",
       x = "Grado", y = "Pagerank", color = "Dominio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "") +
  guides(color = guide_legend(override.aes = list(size = 4)))
}

{
idx = c(names(tail(sort(pr),13))[c(9:13)], sort(b) %>% tail(3) %>% names)
lab = ifelse(names(pr) %in% idx, names(pr), NA)
df$Label = lab

p3 = ggplot(df, aes(x = Betwe, y = Pager, color = Dom)) +
  geom_point(size = 1, alpha=0.7) +
  geom_line(method = "lm", formula = y ~ x, se = FALSE, color = "#ff7363", 
            linetype = 'dashed', orientation = "y", stat='smooth', alpha=0.6) +
  geom_text_repel(aes(label = Label), na.rm=T,
                  box.padding = 0.15, point.padding = 0.15,
                  force = 0.2, force_pull = 1.85, nudge_y = -0.01, show.legend = FALSE, size = 3) +
  scale_color_manual(values = pal, breaks=names(pal)) + theme_minimal() +
  labs(title = "Relazione tra Pagerank e Betweenness",
       x = "Betweenness", y = "Pagerank", color = "Dominio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "") +
  guides(color = guide_legend(override.aes = list(size = 4))) 
}

{
idx = c(sort(gr) %>% tail(4) %>% names, sort(b) %>% tail(5) %>% names)
lab = ifelse(names(gr) %in% idx, names(gr), NA)
nu = rep(0, 11340)
nu[8648] = 1
df$Label = lab
}
p2 = ggplot(df, aes(x = Grado, y = Betwe, color = Dom)) +
  geom_point(size = 1, alpha=0.7) +
  geom_line(method = "lm", formula = y ~ x, se = FALSE, color = "#ff7363", 
            linetype = 'dashed', orientation = "y", stat='smooth', alpha=0.6) +
  geom_text_repel(aes(label = Label), na.rm=T,
                  box.padding = 0.15, point.padding = 0.15,nudge_x = nu,
                  force = 3, force_pull = 1, nudge_y = 0, show.legend = FALSE, size = 3.1) +
  scale_color_manual(values = pal, breaks=names(pal)) + theme_minimal() +
  labs(title = "Relazione tra Grado e Betweenness",
       x = "Grado", y = "Betweenness", color = "Dominio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "") +
  guides(color = guide_legend(override.aes = list(size = 4))) 

{
idx = c(sort(pr) %>% tail(5) %>% names, sort(ec) %>% tail(4) %>% names)
lab = ifelse(names(pr) %in% idx, names(pr), NA)
nu = rep(0, 11340)
nu[8648] = 0.13
nu[9485] = -0.05
df$Label = lab
}
p4 = ggplot(df, aes(x = Eig, y = Pager, color = Dom)) +
  geom_point(size = 1, alpha=0.7) +
  geom_line(method = "lm", formula = y ~ x, se = FALSE, color = "#ff7363", 
            linetype = 'dashed', orientation = "y", stat='smooth', alpha=0.6) +
  geom_text_repel(aes(label = Label), na.rm=T, nudge_x = nu,
                  box.padding = 0.1, point.padding = 0.1,
                  force = 4, force_pull = .2, show.legend = FALSE, size = 3) +
  scale_color_manual(values = pal, breaks=names(pal)) + theme_minimal() +
  labs(title = "Relazione tra Eigen Centrality e Pagerank",
       x = "Eigen centrality", y = "Pagerank", color = "Dominio") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.position = "") +
  guides(color = guide_legend(override.aes = list(size = 4)))

layout = grid.arrange(
  grobs = list(p1, p2, p3, p4, p5),
  layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
  heights = c(4, 4, 1))
print(layout)

# Correlazioni indici
{
auth_v <- igraph::authority_score(gg2)$vector
hub_v  <- igraph::hub_score(gg2)$vector

igr = degree(gg2, mode='in')
ogr = degree(gg2, mode='out')
hub = hub_v
hub = (hub-min(hub))/(max(hub)-min(hub))
auth = auth_v
auth = (auth-min(auth))/(max(auth)-min(auth))

df = data.frame(Deg. = gr, InD. = igr, OutD. = ogr, Betw. = b,
                PageR. = pr, Eigen = ec, Harm. = hc,
                Hub = hub, Auth. = auth)
pear = cor(df)
kend = cor(df, method = 'ken')  # per i rank
spea = cor(df, method = 'spe')  # per non linear

corrplot.mixed(pear, lower = "number", upper = "circle", tl.col = "black")
corrplot.mixed(kend, lower = "number", upper = "circle", tl.col = "black")
corrplot.mixed(spea, lower = "number", upper = "circle", tl.col = "black")
}

# giÃ  in "Esplorative"
{
# Reciprocity, transitivity, density
{
dom = data$domain
cen = data$century
con = data$continentName
cou = data$countryName

{
u = names(sort(table(data$domain), decreasing = T))
retr = matrix(NA, 3, 8)
rownames(retr) = c('Rec', 'Tran', 'Den')
colnames(retr) = u
for (i in colnames(retr)){
  dom = select_graph("domain", i)
  retr[1,i] = reciprocity(dom)
  retr[2,i] = transitivity(dom)
  retr[3,i] = edge_density(dom)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
mappa(a,'')
mappa3(a,'')

{
u = sort(unique(data$century))[-c(1:30,51)]
retr = matrix(NA, 3, 20)
rownames(retr) = c('Rec', 'Tran', 'Den')
colnames(retr) = u
for (i in colnames(retr)){
  cen = select_graph("century", as.numeric(i))
  retr[1,i] = reciprocity(cen)
  retr[2,i] = transitivity(cen)
  retr[3,i] = edge_density(cen)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))}
mappa(a,'')
mappa3(a,'')

{
u = names(sort(table(data$continentName), decreasing = T))
retr = matrix(NA, 3, 7)
rownames(retr) = c('Rec', 'Tran', 'Den')
colnames(retr) = u
for (i in colnames(retr)){
  con = select_graph("continentName", i)
  retr[1,i] = reciprocity(con)
  retr[2,i] = transitivity(con)
  retr[3,i] = edge_density(con)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
mappa(a,'')
mappa3(a,'')

{
u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
retr = matrix(NA, 3, 15)
rownames(retr) = c('Rec', 'Tran', 'Den')
colnames(retr) = u
for (i in colnames(retr)){
  cou = select_graph("countryName", i)
  retr[1,i] = reciprocity(cou)
  retr[2,i] = transitivity(cou)
  retr[3,i] = edge_density(cou)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
mappa(a,'')
mappa3(a,'')
}

{
# Reciprocity e transitivity
{
dom = data$domain
cen = data$century
cou = data$countryName

u = unique(data$domain)
retr = matrix(NA, 4, 8)
rownames(retr) = c('Rec', 'Tran', 'RecAll', 'TranAll')
colnames(retr) = u

for (i in colnames(retr)){
  dom = select_graph("domain", i)
  all = select_graph("domain", i, mode = "all")
  retr[1,i] = reciprocity(dom)
  retr[2,i] = transitivity(dom)
  retr[3,i] = reciprocity(all)
  retr[4,i] = transitivity(all)
}
(a = t(retr)[order(-retr[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 2)
mappa(a,'')

u = sort(unique(data$century))[-c(1:30,51)]
retr = matrix(NA, 4, 20)
rownames(retr) = c('Rec', 'Tran', 'RecAll', 'TranAll')
colnames(retr) = u

for (i in colnames(retr)){
  dom = select_graph("century", as.numeric(i))
  all = select_graph("century", as.numeric(i), mode = "all")
  retr[1,i] = reciprocity(dom)
  retr[2,i] = transitivity(dom)
  retr[3,i] = reciprocity(all)
  retr[4,i] = transitivity(all)
}
(a = t(retr))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 3)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
retr = matrix(NA, 4, 15)
rownames(retr) = c('Rec', 'Tran', 'RecAll', 'TranAll')
colnames(retr) = u

for (i in colnames(retr)){
  dom = select_graph("countryName", i)
  all = select_graph("countryName", i, mode = "all")
  retr[1,i] = reciprocity(dom)
  retr[2,i] = transitivity(dom)
  retr[3,i] = reciprocity(all)
  retr[4,i] = transitivity(all)
}
(a = t(retr))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 2.3)
mappa(a)
}

# Density
{
dom = data$domain
cen = data$century
cou = data$countryName

u = unique(data$domain)
den = matrix(NA, 2, 8)
rownames(den) = c('Den', 'DenAll')
colnames(den) = u

for (i in colnames(den)){
  den[1,i] = edge_density(select_graph("domain", i))
  den[2,i] = edge_density(select_graph("domain", i, mode = "all"))
}
(a = t(den)[order(-den[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 2.3)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
den = matrix(NA, 2, 20)
rownames(den) = c('Den', 'DenAll')
colnames(den) = u

for (i in colnames(den)){
  den[1,as.character(i)] = edge_density(select_graph("century", as.numeric(i)))
  den[2,as.character(i)] = edge_density(select_graph("century", as.numeric(i), mode = "all"))
}
(a = t(den))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 2.5)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
den = matrix(NA, 2, 15)
rownames(den) = c('Den', 'DenAll')
colnames(den) = u

for (i in colnames(den)){
  den[1,i] = edge_density(select_graph("countryName", i))
  den[2,i] = edge_density(select_graph("countryName", i, mode = "all"))
}
(a = t(den))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3, left.label.text.size = 2.3)
mappa(a)
}
}
}