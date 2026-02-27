# =============================================================================
# scripts/04a_indices_global.R ÔÇö Indici globali della rete Pantheon
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Calcola e analizza gli indici strutturali della rete completa:
#         diametro, reciprocita, transitivit├á, distribuzione del grado,
#         componenti, eccentricit├á. Per la componente gigante (conn): degree,
#         closeness, betweenness, pagerank, eigen, harmonic centrality e HITS.
#         Per ciascuna metrica vengono mostrate le classifiche top-N e le
#         distribuzioni per dominio, secolo e paese.
#
# Input:  R/network_utils.R (grafi: gg, gg2, conn, con + dati)
#
# Output: Figure e tabelle a schermo (nessun file scritto su disco).
#
# Dipendenze: igraph (via network_utils.R), circlize, RColorBrewer, xtable,
#             gridExtra, ComplexHeatmap, ggrepel, corrplot, cowplot, superheat
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         networkR sostituito con igraph::authority_score / hub_score.
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

### Indici per grafo non connesso
# (devo recuperarne alcuni dal grafo connesso)
# Uso solo quelli per il grafo originale (diretto)? Penso di sÃ¬
# Valuta quindi se hanno senso tutte le heatmap. Alcune magari no, in altri casi
# magari Ã¨ meglio una tabella semplice
{
# eccentricity non ha senso per un non connesso

dyad_census(gg)
diameter(gg) # -> 15

kn1 = knn(gg)
k = sort(kn1$knn, decreasing=T)
k[1:15]
kn1$knnk
plot(c(1:449), kn1$knnk, ylab="Grado medio del 'neighbor'", xlab='Grado')
abline(v=50,col=2,lwd=2)

reciprocity(gg)
transitivity(gg)
reciprocity(gg2)
transitivity(gg2)

head(count_triangles(gg))
triangles(gg2)
length(triangles(gg2))/3

round(degree_distribution(gg2) * 10000) # per leggibilitÃ 
plot(0:449, round(degree_distribution(gg2) * 10000), type='b', ylim=c(0,520))
plot(0:449, degree_distribution(gg2)*100, type='b', ylab='% di nodi con grado x', xlab='Grado')
abline(v=mean(degree(gg2)), col=2)
abline(v=median(degree(gg2)), col=3)

auth <- igraph::authority_score(gg2)$vector
hub  <- igraph::hub_score(gg2)$vector
auth
hub
mat <- data.frame(auth, data$Name)
mat <- mat[order(mat[,1], decreasing = TRUE),]
head(mat, 20)
mat <- data.frame(hub, data$Name)
mat <- mat[order(mat[,1], decreasing = TRUE),]
head(mat, 20)
}

# Indici per grafo connesso
# per gruppi non hanno senso!
{
# Eccentricity
{
ecc = eccentricity(conn)
a = ecc[ecc<8]

tab = table(data$domain[data$Name %in% names(a)]) %>% sort(decreasing=T)
tab = tab[order(names(tab))]
tab2 = table(data$domain)[order(names(table(data$domain)))]
sum(tab)/sum(tab2)
sort(tab/tab2)

table(data$countryName[data$Name %in% names(a)]) %>% sort(decreasing=T)
tab = table(data$countryName[data$Name %in% names(a)]) %>% sort(decreasing=T)
tab = tab[order(names(tab))]
tab2 = table(data$countryName)[order(names(table(data$countryName)))]
tab2 = tab2[names(tab2) %in% names(tab)]
sum(tab)/sum(tab2)
sort(tab/tab2)

table(data$century[data$Name %in% names(a)])

(data$HPI[data$Name %in% names(a)]) %>% sort(decreasing=T) %>% head
b = data$HPI[data$Name %in% names(a)] %>% sort(decreasing=T)
d = c(NA)
for(i in b) d = append(d, sum(data$HPI > i))
d = d[-1]
hist((11340-d)/11340, breaks=25, main='Distribuzione quantilica dei 459 nodi con ecc.=7', xlab='Quantile')

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

a = rep(NA, 8)
names(a) = unique(data$domain)
b = rep(NA, 20)
names(b) = sort(unique(data$century))[-c(1:30,51)]
d = rep(NA, 25)
names(d) = table(data$countryName) %>% sort(decreasing=T) %>% head(25) %>% names()

for(i in names(a)){
  a[i] = mean(ecc[dom == i])
}

for(i in names(b)){
  b[i] = mean(ecc[cen == i])
}

for(i in names(d)){
  d[i] = mean(ecc[cou == i])
}
}

# Varie
{
dyad_census(conn)
diameter(conn)

hist(count_triangles(conn))
plot(sort(count_triangles(conn), decreasin=T), xlab='Triangoli', ylab='Grado')
plot(sort(count_triangles(gg2), decreasin=T), ylab='Numero di triangoli')
plot(count_triangles(gg2), degree(gg2), xlab='Triangoli', ylab='Grado')
plot(sort(count_triangles(gg), decreasin=T), ylab='Numero di triangoli')
plot(count_triangles(gg), degree(gg), xlab='Triangoli', ylab='Grado')

kn1 = knn(conn)
sort(kn1$knn, decreasing=T)[1:25]
kn1$knnk
plot(c(1:449), kn1$knnk)
abline(v=50,col=2,lwd=2)

radius(conn)

reciprocity(conn)

transitivity(conn)

head(count_triangles(conn))
which.max(count_triangles(conn)) # Obama
triangles(conn)
length(triangles(conn))/3

round(degree_distribution(conn) * gorder(conn)) # per leggibilitÃ 
plot(0:449, round(degree_distribution(gg) * gorder(conn)), type='b', ylim=c(0,520))
}

# Degree
{
sort(degree(conn), decreasing=T)[1:10]
deg = sort(degree(conn), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(deg)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(deg)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(deg)]), decreasing = T)
table(data$century[data$Name %in% names(deg)])

sort(degree(conn, mode = 'in'), decreasing=T)[1:20]
degin = sort(degree(conn, mode = 'in'), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(degin)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(degin)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(degin)]), decreasing = T)
table(data$century[data$Name %in% names(degin)])

sort(degree(conn, mode = 'out'), decreasing=T)[1:20]
degout = sort(degree(conn, mode = 'out'), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(degout)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(degout)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(degout)]), decreasing = T)
table(data$century[data$Name %in% names(degout)])

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
degrees = matrix(NA,3,8)
rownames(degrees) = c('All', 'In', 'Out')
colnames(degrees) = u
d = degree(conn)
dout = degree(conn, mode = 'out')
din = degree(conn, mode = 'in')

for (j in colnames(degrees)){
  degrees[1,j] = d[dom == j] %>% mean()
  degrees[2,j] = din[dom == j] %>% mean()
  degrees[3,j] = dout[dom == j] %>% mean()
}
t(degrees)[order(-degrees[1,]),]
a = t(degrees)[order(-degrees[1,]),]
a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x)))
superheat(a,
          heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
degrees = matrix(NA,3,20)
rownames(degrees) = c('All', 'In', 'Out')
colnames(degrees) = u
d = degree(conn)
dout = degree(conn, mode = 'out')
din = degree(conn, mode = 'in')

for (j in colnames(degrees)){
  degrees[1,j] = d[cen == j] %>% mean()
  degrees[2,j] = din[cen == j] %>% mean()
  degrees[3,j] = dout[cen == j] %>% mean()
}
(a = t(degrees))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a,
          heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
degrees = matrix(NA,3,15)
rownames(degrees) = c('All', 'In', 'Out')
colnames(degrees) = u
d = degree(conn)
dout = degree(conn, mode = 'out')
din = degree(conn, mode = 'in')

for (j in colnames(degrees)){
  degrees[1,j] = d[cou == j] %>% mean()
  degrees[2,j] = din[cou == j] %>% mean()
  degrees[3,j] = dout[cou == j] %>% mean()
}
(a = t(degrees))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a,
          heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)
}

# Closeness
{
clo = sort(closeness(conn, mode='all', normalized =T), decreasing=T)
clo[1:20]
clo = clo[1:106]
sort(table(data$domain[data$Name %in% names(clo)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(clo)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(clo)]), decreasing = T)
table(data$century[data$Name %in% names(clo)])

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
clovec = rep(NA,8)
names(clovec) = u
clos = closeness(conn, mode='all', normalized =T)

for (j in names(clovec)){
  clovec[j] = clos[dom == j] %>% mean()
}
sort(clovec, decreasing = T)

u = sort(unique(data$century))[-c(1:30,51)]
clovec = rep(NA,20)
names(clovec) = u
clos = closeness(conn, mode='all', normalized =T)

for (j in names(clovec)){
  clovec[j] = clos[cen == j] %>% mean()
}
clovec
sort(clovec, decreasing = T)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
clovec = rep(NA,15)
names(clovec) = u
clos = closeness(conn, mode='all', normalized =T)

for (j in names(clovec)){
  clovec[j] = clos[cou == j] %>% mean()
}
clovec
sort(clovec, decreasing = T)
}

# Betweennes
{
sort(betweenness(conn, directed=T), decreasing=T)[1:20]
bet = sort(betweenness(conn,directed=T), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(bet)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(bet)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(bet)]), decreasing = T)
table(data$century[data$Name %in% names(bet)])

sort(betweenness(conn, directed=F), decreasing=T)[1:20]
betin = sort(betweenness(conn,directed=F), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(betin)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(betin)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(betin)]), decreasing = T)
table(data$century[data$Name %in% names(betin)])

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
betw = matrix(NA,2,8)
rownames(betw) = c('Dir', 'Ind')
colnames(betw) = u
b = betweenness(conn, directed=T)
bin = betweenness(conn, directed=F)

for (j in colnames(betw)){
  betw[1,j] = b[dom == j] %>% mean()
  betw[2,j] = bin[dom == j] %>% mean()
}
(a = t(betw)[order(-betw[1,]),])
(a = t(betw)[order(betw[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.5)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
betw = matrix(NA,2,20)
rownames(betw) = c('Dir', 'Ind')
colnames(betw) = u
b = betweenness(conn, directed=T)
bin = betweenness(conn, directed=F)

for (j in colnames(betw)){
  betw[1,j] = b[cen == j] %>% mean()
  betw[2,j] = bin[cen == j] %>% mean()
}
(a = t(betw))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 3)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
betw = matrix(NA,2,15)
rownames(betw) = c('Dir', 'Ind')
colnames(betw) = u
b = betweenness(conn, directed=T)
bin = betweenness(conn, directed=F)

for (j in colnames(betw)){
  betw[1,j] = b[cou == j] %>% mean()
  betw[2,j] = bin[cou == j] %>% mean()
}
(a = t(betw))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.3)
mappa(a)
}

# Pagerank
{
sort(page_rank(conn, directed=T)$vector, decreasing=T)[1:20]
pgr = sort(page_rank(conn,directed=T)$vector, decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(pgr)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(pgr)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(pgr)]), decreasing = T)
table(data$century[data$Name %in% names(pgr)])

sort(page_rank(conn, directed=F)$vector, decreasing=T)[1:20]
pgrind = sort(page_rank(conn,directed=F)$vector, decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(pgrind)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(pgrind)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(pgrind)]), decreasing = T)
table(data$century[data$Name %in% names(pgrind)])

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
prank = matrix(NA,2,8)
rownames(prank) = c('Dir', 'Ind')
colnames(prank) = u
p = page_rank(conn, directed=T, )$vector
p = (p-min(p))/(max(p)-min(p))
pin = page_rank(conn, directed=F)$vector
pin = (pin-min(pin))/(max(pin)-min(pin))

for (j in colnames(prank)){
  prank[1,j] = p[dom == j] %>% mean()
  prank[2,j] = pin[dom == j] %>% mean()
}
(a = t(prank)[order(-prank[1,]),])
(a = t(prank)[order(prank[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.4)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
prank = matrix(NA,2,20)
rownames(prank) = c('Dir', 'Ind')
colnames(prank) = u
p = page_rank(conn, directed=T, )$vector
p = (p-min(p))/(max(p)-min(p))
pin = page_rank(conn, directed=F)$vector
pin = (pin-min(pin))/(max(pin)-min(pin))

for (j in colnames(prank)){
  prank[1,j] = p[cen == j] %>% mean()
  prank[2,j] = pin[cen == j] %>% mean()
}
(a = t(prank))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 3)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
prank = matrix(NA,2,15)
rownames(prank) = c('Dir', 'Ind')
colnames(prank) = u
p = page_rank(conn, directed=T, )$vector
p = (p-min(p))/(max(p)-min(p))
pin = page_rank(conn, directed=F)$vector
pin = (pin-min(pin))/(max(pin)-min(pin))

for (j in colnames(prank)){
  prank[1,j] = p[cou == j] %>% mean()
  prank[2,j] = pin[cou == j] %>% mean()
}
(a = t(prank))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.3)
mappa(a)
}

# Eigen
{
sort(eigen_centrality(conn, directed=T)$vector, decreasing=T)[1:20]
eig = sort(eigen_centrality(conn, directed=T)$vector, decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(eig)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(eig)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(eig)]), decreasing = T)
table(data$century[data$Name %in% names(eig)])

sort(eigen_centrality(conn, directed=F)$vector, decreasing=T)[1:20]
eig = sort(eigen_centrality(conn, directed=F)$vector, decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(eig)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(eig)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(eig)]), decreasing = T)
table(data$century[data$Name %in% names(eig)])
# i primi 152 sono tennisti!

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
eigv = matrix(NA,2,8)
rownames(eigv) = c('Dir', 'Ind')
colnames(eigv) = u
e = eigen_centrality(conn, directed=T)$vector
ein = eigen_centrality(conn, directed=F)$vector

for (j in colnames(eigv)){
  eigv[1,j] = e[dom == j] %>% mean()
  eigv[2,j] = ein[dom == j] %>% mean()
}
(a = t(eigv)[order(-eigv[1,]),])
(a = t(eigv)[order(eigv[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.5)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
eigv = matrix(NA,2,20)
rownames(eigv) = c('Dir', 'Ind')
colnames(eigv) = u
e = eigen_centrality(conn, directed=T)$vector
ein = eigen_centrality(conn, directed=F)$vector

for (j in colnames(eigv)){
  eigv[1,j] = e[cen == j] %>% mean()
  eigv[2,j] = ein[cen == j] %>% mean()
}
(a = t(eigv))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
eigv = matrix(NA,2,15)
rownames(eigv) = c('Dir', 'Ind')
colnames(eigv) = u
e = eigen_centrality(conn, directed=T)$vector
ein = eigen_centrality(conn, directed=F)$vector

for (j in colnames(eigv)){
  eigv[1,j] = e[cou == j] %>% mean()
  eigv[2,j] = ein[cou == j] %>% mean()
}
(a = t(eigv))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)
}

# Harmonic
{
sort(harmonic_centrality(conn, mode = 'all'), decreasing=T)[1:20]
har = sort(harmonic_centrality(conn, mode = 'all'), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(har)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(har)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(har)]), decreasing = T)
table(data$century[data$Name %in% names(har)])

sort(harmonic_centrality(conn, mode = 'out'), decreasing=T)[1:20]
harout = sort(harmonic_centrality(conn, mode = 'out'), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(harout)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(harout)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(harout)]), decreasing = T)
table(data$century[data$Name %in% names(harout)])

sort(harmonic_centrality(conn, mode = 'in'), decreasing=T)[1:20]
harin = sort(harmonic_centrality(conn, mode = 'in'), decreasing=T)[1:100]
sort(table(data$domain[data$Name %in% names(harin)]), decreasing = T)
sort(table(data$occupation[data$Name %in% names(harin)]), decreasing = T)
sort(table(data$countryName[data$Name %in% names(harin)]), decreasing = T)
table(data$century[data$Name %in% names(harin)])

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]

u = unique(data$domain)
harmonic = matrix(NA,3,8)
rownames(harmonic) = c('All', 'In', 'Out')
colnames(harmonic) = u
h = harmonic_centrality(conn, mode = 'all')
hout = harmonic_centrality(conn, mode = 'out')
hin = harmonic_centrality(conn, mode = 'in')

for (j in colnames(harmonic)){
  harmonic[1,j] = h[dom == j] %>% mean()
  harmonic[2,j] = hin[dom == j] %>% mean()
  harmonic[3,j] = hout[dom == j] %>% mean()
}

(a = t(harmonic)[order(-harmonic[1,]),])
(a = t(harmonic)[order(harmonic[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 3.5, left.label.text.size = 2.5)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
harmonic = matrix(NA,3,20)
rownames(harmonic) = c('All', 'In', 'Out')
colnames(harmonic) = u
h = harmonic_centrality(conn, mode = 'all')
hout = harmonic_centrality(conn, mode = 'out')
hin = harmonic_centrality(conn, mode = 'in')

for (j in colnames(harmonic)){
  harmonic[1,j] = h[cen == j] %>% mean()
  harmonic[2,j] = hin[cen == j] %>% mean()
  harmonic[3,j] = hout[cen == j] %>% mean()
}
(a = t(harmonic))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
harmonic = matrix(NA,3,15)
rownames(harmonic) = c('All', 'In', 'Out')
colnames(harmonic) = u
h = harmonic_centrality(conn, mode = 'all')
hout = harmonic_centrality(conn, mode = 'out')
hin = harmonic_centrality(conn, mode = 'in')

for (j in colnames(harmonic)){
  harmonic[1,j] = h[cou == j] %>% mean()
  harmonic[2,j] = hin[cou == j] %>% mean()
  harmonic[3,j] = hout[cou == j] %>% mean()
}
(a = t(harmonic))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)
}

# Hits
{
auth_v <- igraph::authority_score(conn)$vector
hub_v  <- igraph::hub_score(conn)$vector
auth = data.frame(auth_v, data$Name[as.numeric(names(V(con)))+1])
auth = auth[order(auth[,1], decreasing=T),]
colnames(auth) = c('Authority', 'Name')
head(auth, 20)
hub = data.frame(hub_v, data$Name[as.numeric(names(V(con)))+1])
hub = hub[order(hub[,1], decreasing=T),]
colnames(hub) = c('Hub', 'Name')
head(hub, 20)

idx = data$Id %in% names(V(con)) # indice dei nodi connessi
dom = data$domain[idx]
cen = data$century[idx]
cou = data$countryName[idx]
auth = auth_v
hub  = hub_v

u = unique(data$domain)
hit = matrix(NA,2,8)
rownames(hit) = c('Hub', 'Auth')
colnames(hit) = u

for (j in colnames(hit)){
  hit[1,j] = hub[dom == j] %>% mean()
  hit[2,j] = auth[dom == j] %>% mean()
}
(a = t(hit)[order(-hit[1,]),])
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(unique(data$century))[-c(1:30,51)]
hit = matrix(NA,2,20)
rownames(hit) = c('Hub', 'Auth')
colnames(hit) = u

for (j in colnames(hit)){
  hit[1,j] = hub[cen == j] %>% mean()
  hit[2,j] = auth[cen == j] %>% mean()
}
(a = t(hit))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)

u = sort(table(data$countryName), decreasing=T)[1:15] %>% names()
hit = matrix(NA,2,15)
rownames(hit) = c('Hub', 'Auth')
colnames(hit) = u

for (j in colnames(hit)){
  hit[1,j] = hub[cou == j] %>% mean()
  hit[2,j] = auth[cou == j] %>% mean()
}
(a = t(hit))
(a = apply(a, 2, function(x) (x-min(x))/(max(x)-min(x))))
superheat(a, heat.pal = c('white', 'yellow', 'orange', 'red'), 
          bottom.label.text.angle = 90,
          bottom.label.text.size = 2, left.label.text.size = 2)
mappa(a)
}

}