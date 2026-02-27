# =============================================================================
# scripts/03_subnetwork_analysis.R ÔÇö Analisi della struttura delle sottoreti
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Analizza la struttura di sottoreti per dominio, paese, continente
#         e secolo. Calcola reciprocita, transitivit├á e densit├á per gruppo
#         (heatmap comparative). Costruisce le matrici di archi diretti
#         cross-categoria (zdom, zcou, zcon, zcen, zgen).
#         Misura l'assortativit├á nominale per ogni raggruppamento.
#
# Input:  R/network_utils.R (dati + grafi + funzioni di selezione)
#
# Output: Heatmap e grafici a schermo (nessun file scritto su disco).
#
# Dipendenze: igraph (via network_utils.R), RColorBrewer, wesanderson,
#             xtable, gridExtra, ComplexHeatmap, circlize
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
# =============================================================================
source("R/network_utils.R")# Librerie
{
library(RColorBrewer)
library(wesanderson)
library(xtable)
library(gridExtra)
library(ComplexHeatmap)
library(circlize)
}

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
          show_heatmap_legend = F,
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))})
  }
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
          row_title = paste(tit, 'da cui escono gli archi'),)
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
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 3.5))})
}
mappa5 = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_title = paste(tit, 'in cui entrano gli archi'),
          row_title = paste(tit, 'da cui escono gli archi'),
          show_heatmap_legend = F,
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.f", mat[i, j]), x, y, gp = gpar(fontsize = 6))})
}
mappa6 = function(mat, tit){
  Heatmap(round(mat,3), name = "Valore", col = col_fun(round(mat,3)), column_title_side = "top",
          row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
          show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
          column_names_side = "top", column_names_rot = 45,
          row_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_names_gp = gpar(fontsize = 8, fontface = "bold"),
          column_title = paste(tit, 'in cui entrano gli archi \n Valori logaritmici'),
          row_title = paste(tit, 'da cui escono gli archi'),
          show_heatmap_legend = F,
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.2f", mat[i, j]), x, y, gp = gpar(fontsize = 6))})
}
select2DomainDir = function(x1,x2,y=data,id=T){
  set1 = y[y$domain == x1,]
  set2 = y[y$domain == x2,]
  righe11 = edges_n[,1] %in% set1$Id # righe che partono da x1
  righe22 = edges_n[,2] %in% set2$Id # righe che arrivano a x2
  righe = as.logical(righe11 & righe22)
  new_edges = matrix(edges_c[righe, ], ncol=2)
  g = igraph::simplify(graph_from_edgelist(new_edges))
  u = unique(c(new_edges[,1], new_edges[,2]))
  ver = c(set1$Id[!(set1$Id %in% u)],set2$Id[!(set2$Id %in% u)])
  if (!(anyNA(ver))) g = g + vertices(ver)
  if (id) return(id_to_name(g))
  else return(g)
}
select2CountryNameDir = function(x1,x2,y=data,id=T){
  set1 = y[y$countryName == x1,]
  set2 = y[y$countryName == x2,]
  righe11 = edges_n[,1] %in% set1$Id # righe che partono da x1
  righe22 = edges_n[,2] %in% set2$Id # righe che arrivano a x2
  righe = as.logical(righe11 & righe22)
  new_edges = matrix(edges_c[righe, ], ncol=2)
  g = igraph::simplify(graph_from_edgelist(new_edges))
  u = unique(c(new_edges[,1], new_edges[,2]))
  ver = c(set1$Id[!(set1$Id %in% u)],set2$Id[!(set2$Id %in% u)])
  if (!(anyNA(ver))) g = g + vertices(ver)
  if (id) return(id_to_name(g))
  else return(g)
}
select2ContinentNameDir = function(x1,x2,y=data,id=T){
  set1 = y[y$continentName == x1,]
  set2 = y[y$continentName == x2,]
  righe11 = edges_n[,1] %in% set1$Id # righe che partono da x1
  righe22 = edges_n[,2] %in% set2$Id # righe che arrivano a x2
  righe = as.logical(righe11 & righe22)
  new_edges = matrix(edges_c[righe, ], ncol=2)
  g = igraph::simplify(graph_from_edgelist(new_edges))
  u = unique(c(new_edges[,1], new_edges[,2]))
  ver = c(set1$Id[!(set1$Id %in% u)],set2$Id[!(set2$Id %in% u)])
  if (!(anyNA(ver))) g = g + vertices(ver)
  if (id) return(id_to_name(g))
  else return(g)
}
select2CenturyDir = function(x1,x2,y=data,id=T){
  set1 = y[y$century == x1,]
  set2 = y[y$century == x2,]
  righe11 = edges_n[,1] %in% set1$Id # righe che partono da x1
  righe22 = edges_n[,2] %in% set2$Id # righe che arrivano a x2
  righe = as.logical(righe11 & righe22)
  new_edges = matrix(edges_c[righe, ], ncol=2)
  g = igraph::simplify(graph_from_edgelist(new_edges))
  u = unique(c(new_edges[,1], new_edges[,2]))
  ver = c(set1$Id[!(set1$Id %in% u)],set2$Id[!(set2$Id %in% u)])
  if (!(anyNA(ver))) g = g + vertices(ver)
  if (id) return(id_to_name(g))
  else return(g)
}
select2GenderDir = function(x1,x2,y=data,id=T){
  set1 = y[y$gender == x1,]
  set2 = y[y$gender == x2,]
  righe11 = edges_n[,1] %in% set1$Id # righe che partono da x1
  righe22 = edges_n[,2] %in% set2$Id # righe che arrivano a x2
  righe = as.logical(righe11 & righe22)
  new_edges = matrix(edges_c[righe, ], ncol=2)
  g = igraph::simplify(graph_from_edgelist(new_edges))
  u = unique(c(new_edges[,1], new_edges[,2]))
  ver = c(set1$Id[!(set1$Id %in% u)],set2$Id[!(set2$Id %in% u)])
  if (!(anyNA(ver))) g = g + vertices(ver)
  if (id) return(id_to_name(g))
  else return(g)
}
}

# Tabella, giÃ  fatto
{
reciprocity(gg2, mode = 'default')
reciprocity(gg2, mode = 'ratio')
transitivity(gg2)
edge_density(gg2)
mean(igraph::degree(gg2))
diameter(gg2)

df = data.frame('Nodi' = 11340, 'Nodi connessi' = 11062, 'Archi' = 126153, 'Diametro' = 15, 'DensitÃ ' = 9.81*10^-4, 'ReciprocitÃ ' = 0.346, 'TransitivitÃ ' = 0.156, 'Grado medio' = 22.2)
xtable(df, digits = c(1,0,0,0,0,6,3,3,1))
}

# Reciprocity, transitivity, density nei gruppi (fatto)
{

{
u = names(sort(table(data$domain), decreasing = T))
retr = matrix(NA, 3, 8)
rownames(retr) = c('Density', 'Reciprocity', 'Transitivity')
colnames(retr) = u
for (i in colnames(retr)){
  dom = select_graph("domain", i)
  retr[2,i] = reciprocity(dom)
  retr[3,i] = transitivity(dom)
  retr[1,i] = edge_density(dom)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
p1 = Heatmap(a, name = "Valore", col = col_fun(a), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Statistica', row_title = 'Dominio',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", t(retr)[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = sort(unique(data$century))[-c(1:30,51)]
retr = matrix(NA, 3, 20)
rownames(retr) = c('Density', 'Reciprocity', 'Transitivity')
colnames(retr) = u
for (i in colnames(retr)){
  cen = select_graph("century", i)
  retr[2,i] = reciprocity(cen)
  retr[3,i] = transitivity(cen)
  retr[1,i] = edge_density(cen)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
p2 = Heatmap(a, name = "Valore", col = col_fun(a), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Statistica', row_title = 'Secolo',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", t(retr)[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = names(sort(table(data$continentName), decreasing = T))
retr = matrix(NA, 3, 7)
rownames(retr) = c('Density', 'Reciprocity', 'Transitivity')
colnames(retr) = u
for (i in colnames(retr)){
  con = select_graph("continentName", i)
  retr[2,i] = reciprocity(con)
  retr[3,i] = transitivity(con)
  retr[1,i] = edge_density(con)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
p3 = Heatmap(a, name = "Valore", col = col_fun(a), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold", just='center'),
        column_title = 'Statistica', row_title = 'Continente',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", t(retr)[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = sort(table(data$countryName), decreasing=T)[1:20] %>% names()
retr = matrix(NA, 3, 20)
rownames(retr) = c('Density', 'Reciprocity', 'Transitivity')
colnames(retr) = u
for (i in colnames(retr)){
  cou = select_graph("countryName", i)
  retr[2,i] = reciprocity(cou)
  retr[3,i] = transitivity(cou)
  retr[1,i] = edge_density(cou)
}
(a = apply(retr %>% t, 2, function(x) (x-min(x))/(max(x)-min(x))))
}
p4 = Heatmap(a, name = "Valore", col = col_fun(a), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Statistica', row_title = 'Paese',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.3f", t(retr)[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

p3
p4
p2
p1

{
capture_heatmap <- function(ht) {
  grid.grabExpr(draw(ht, heatmap_legend_side = "right"))
}

grob1 <- capture_heatmap(p1)
grob2 <- capture_heatmap(p2)
grob3 <- capture_heatmap(p3)
grob4 <- capture_heatmap(p4)

grid.arrange(grob3, grob4, grob2, grob1, ncol = 2)
}
}

# Analisi grado gruppi (fatto)
{

{
u = names(sort(table(data$domain), decreasing = T))
rates = matrix(NA,3,8)
rownames(rates) = c('Degree', 'In-Degree', 'Out-Degree')
colnames(rates) = u
tab = sort(table(data$domain),decreasing = T)

for (j in colnames(rates)){
  all = igraph::degree(gg2)[data$domain==j] %>% mean
  ins = igraph::degree(gg2, mode = 'in')[data$domain==j] %>% mean
  out = igraph::degree(gg2, mode = 'out')[data$domain==j] %>% mean
  rates[,j] = c(all, ins, out)
}
round(t(rates),1)
b = t(rates)
mat = apply(b, 2, function(x) (x-min(x))/(max(x)-min(x)))
}
Heatmap(mat, name = "Valore", col = col_fun(mat), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Tipo di grado', row_title = 'Dominio',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", b[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = sort(unique(data$century))[-c(1:30,51)]
rates = matrix(NA,3,20)
rownames(rates) = c('Degree', 'In-Degree', 'Out-Degree')
colnames(rates) = u
tab = table(data$century)[-c(1:30,51)]
for (j in colnames(rates)){
  all = igraph::degree(gg2)[data$century==j] %>% mean
  ins = igraph::degree(gg2, mode = 'in')[data$century==j] %>% mean
  out = igraph::degree(gg2, mode = 'out')[data$century==j] %>% mean
  rates[,j] = c(all, ins, out)
}
round(t(rates),1)
b = t(rates)
mat = apply(b, 2, function(x) (x-min(x))/(max(x)-min(x)))
}
Heatmap(mat, name = "Valore", col = col_fun(mat), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Tipo di grado', row_title = 'Secolo',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", b[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = table(data$countryName) %>% sort(decreasing=T) %>% head(20) %>% names()
rates = matrix(NA,3,20)
rownames(rates) = c('Degree', 'In-Degree', 'Out-Degree')
colnames(rates) = u
tab = table(data$countryName) %>% sort(decreasing=T) %>% head(25)
for (j in colnames(rates)){
  all = igraph::degree(gg2)[data$countryName==j] %>% mean
  ins = igraph::degree(gg2, mode = 'in')[data$countryName==j] %>% mean
  out = igraph::degree(gg2, mode = 'out')[data$countryName==j] %>% mean
  rates[,j] = c(all, ins, out)
}
round(t(rates),1)
b = t(rates)
mat = apply(b, 2, function(x) (x-min(x))/(max(x)-min(x)))
}
Heatmap(mat, name = "Valore", col = col_fun(mat), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Tipo di grado', row_title = 'Paese',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", b[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))

{
u = names(sort(table(data$continentName), decreasing = T))
rates = matrix(NA,3,7)
rownames(rates) = c('Degree', 'In-Degree', 'Out-Degree')
colnames(rates) = u
tab = sort(table(data$continentName),decreasing = T)
for (j in colnames(rates)){
  all = igraph::degree(gg2)[data$continentName==j] %>% mean
  ins = igraph::degree(gg2, mode = 'in')[data$continentName==j] %>% mean
  out = igraph::degree(gg2, mode = 'out')[data$continentName==j] %>% mean
  rates[,j] = c(all, ins, out)
}
round(t(rates),1)
b = t(rates)
mat = apply(b, 2, function(x) (x-min(x))/(max(x)-min(x)))
}
Heatmap(mat, name = "Valore", col = col_fun(mat), column_title_side = "top",
        row_title_side = "left", cluster_rows = FALSE, cluster_columns = FALSE,
        show_row_names = TRUE, show_column_names = TRUE, row_names_side = "left",
        column_names_side = "top", column_names_rot = 0,
        row_names_gp = gpar(fontsize = 8, fontface = "bold"),
        column_names_gp = gpar(fontsize = 10, fontface = "bold"),
        column_title = 'Tipo di grado', row_title = 'Continente',
        column_names_centered = TRUE,
        show_heatmap_legend = F,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.1f", b[i, j]), x, y, gp = gpar(fontsize = 8))},
        border = TRUE,
        border_gp = gpar(col = "grey", lwd = 0.5))
}


# Analisi archi val. ass., rel. alle numerositÃ  e rel. agli archi totali
a = c(
assortativity_nominal(gg2, types = as.numeric(as.factor(data$domain))),
assortativity_nominal(gg2, types = as.numeric(factor(data$century))),
assortativity_nominal(gg2, types = as.numeric(as.factor(data$continentName))),
assortativity_nominal(gg2, types = as.numeric(as.factor(data$countryName)))
)
df = data.frame('Raggruppamento' = 'AssortativitÃ ', 'Continente'=a[3], 'Paese'=a[4], 'Secolo'=a[2], 'Dominio'=a[1])
xtable(df)

assortativity_nominal(gg2, types = as.numeric(as.factor(data$gender)))

cen = seq(0,1900,100)
a = rep(NA, 20)
b = rep(NA, 20)
d = rep(NA, 20)
for (i in cen){
  idx = names(V(select_graph("century", i, id = FALSE))) %>% as.numeric()
  gra = select_graph("century", i)
  a[i/100+1] = assortativity_nominal(gra, types = as.numeric(as.factor(data$domain[idx])))
  b[i/100+1] = assortativity_nominal(gra, types = as.numeric(as.factor(data$continentName[idx])))
  d[i/100+1] = assortativity_nominal(gra, types = as.numeric(as.factor(data$countryName[idx])))
}



# zdomr --> Q
# zdomr2 --> P
# zdompro --> T
# zdompro2 --> S

{
a = length(unique(data$domain))
zdom = matrix(NA, a, a)
colnames(zdom) = row.names(zdom) = names(sort(table(data$domain), decreasing=T))
for (i in rownames(zdom)){
  for (j in colnames(zdom)){
    zdom[i,j] = gsize(select2DomainDir(i,j))
  }
}
}
draw(mappa5(zdom, 'Dominio'), padding = unit(c(2, 2, 2, 8), "mm"))
{
ta = sort(table(data$domain), decreasing=T)
zdomr = apply(zdom, 1, function(x) x/ta) %>% t
zdomr2 = apply(zdom, 2, function(x) x/ta)
}
mappa(zdomr, 'Dominio') # media di connessioni ricevute da j che provengono
# da i. Indica quanto il gruppo i "contribuisce" alle connessioni in entrata di j.
mappa(zdomr2, 'Dominio') # media di connessioni che un membro del gruppo i ha verso
# il gruppo j. Indica la tendenza media di un individuo di i a connettersi con j

zdompro = apply(zdom, 1, function(x) x/colSums(zdom)) %>% t
zdompro2 = apply(zdom, 2, function(x) x/rowSums(zdom))

mappa(zdompro, 'Dominio')  # proporzione di connessioni che il gruppo i fornisce
           # al gruppo j rispetto a tutte le connessioni in entrata che j riceve
draw(mappa(zdompro2, 'Dominio'), padding = unit(c(2, 2, 2, 8), "mm"))
# proporzione di connessioni che il gruppo i invia
# al gruppo j rispetto a tutte le connessioni in uscita che i genera

####

{
cou = table(data$countryName) %>% sort(decreasing=T) %>% head(20) %>% names()
a = length(cou)
zcou = matrix(NA, a, a)
colnames(zcou) = row.names(zcou) = cou
for (i in cou){
  for (j in cou){
    zcou[i,j] = gsize(select2CountryNameDir(i,j))
  }
}
}
draw(mappa5(zcou, 'Paese'), padding = unit(c(2, 2, 2, 15), "mm"))
{
ta = head(sort(table(data$countryName), decreasing=T),20)
zcour = t(apply(zcou, 1, function(x) x/ta))
zcour2 = apply(zcou, 2, function(x) x/ta)
}
draw(mappa(zcour, 'Paese'), padding = unit(c(2, 2, 2, 16), "mm"))
draw(mappa(zcour2, 'Paese'), padding = unit(c(2, 2, 2, 16), "mm"))

zcoupro = apply(zcou, 1, function(x) x/colSums(zcou)) %>% t
zcoupro2 = apply(zcou, 2, function(x) x/rowSums(zcou))
draw(mappa(zcoupro, 'Paese'), padding = unit(c(2, 2, 2, 16), "mm"))
draw(mappa(zcoupro2, 'Paese'), padding = unit(c(2, 2, 2, 15), "mm"))

####

{
a = length(unique(data$continentName))
zcon = matrix(NA, a, a)
colnames(zcon) = row.names(zcon) = names(sort(table(data$continentName), decreasing=T))
for (i in rownames(zcon)){
  for (j in colnames(zcon)){
    zcon[i,j] = gsize(select2ContinentNameDir(i,j))
  }
}
}
mappa5(zcon, 'Continente')
{
ta = sort(table(data$continentName), decreasing=T)
zconr = apply(zcon, 1, function(x) x/ta) %>% t
zconr2 = apply(zcon, 2, function(x) x/ta)
}
mappa(zconr, 'Continente')
mappa(zconr2, 'Continente')

zconpro = apply(zcon, 1, function(x) x/colSums(zcon)) %>% t
zconpro2 = apply(zcon, 2, function(x) x/rowSums(zcon))
mappa(zconpro, 'Continente')
mappa(zconpro2, 'Continente')

####
gtable1 <- grid.grabExpr(draw(mappa7(zcen, 'Secolo')))
gtable2 <- grid.grabExpr(draw(mappa6(logzcen, 'Secolo')))
grid.arrange(gtable1, gtable2, ncol = 1)
gtable3 <- grid.grabExpr(draw(mappa5(zcen, 'Secolo')))
gtable4 <- grid.grabExpr(draw(mappa6(logzcen, 'Secolo')))
grid.arrange(gtable3, gtable4, ncol = 1, heights = c(.475,.525))

{
u = sort(unique(data$century))[-c(1:30,51)]
a = length(u)
zcen = matrix(NA, a, a)
colnames(zcen) = row.names(zcen) = as.character(u)
for (i in u){
  for (j in u){
    zcen[as.character(i),as.character(j)] = gsize(select2CenturyDir(i,j))
  }
}
}
mappa5(zcen, 'Secolo')
logzcen = ifelse(log(zcen,10)==-Inf, 1, log(zcen,10))
mappa(logzcen, 'Secolo')
{
ta = table(data$century)[-c(1:30,51)]
zcenr = t(apply(zcen, 1, function(x) x/ta))
zcenr2 = apply(zcen, 2, function(x) x/ta)
}
mappa(zcenr, 'Secolo')
mappa(zcenr2, 'Secolo')

zcenpro = apply(zcen, 1, function(x) x/colSums(zcen)) %>% t
zcenpro2 = apply(zcen, 2, function(x) x/rowSums(zcen))
mappa(zcenpro, 'Secolo')
mappa(zcenpro2, 'Secolo')



# Tutti i Paesi
{
cou = table(data$countryName) %>% sort(decreasing=T) %>% names()
a = length(cou)
zcou = matrix(NA, a, a)
colnames(zcou) = row.names(zcou) = cou
for (i in cou){
  for (j in cou){
    zcou[i,j] = gsize(select2CountryNameDir(i,j))
  }
}
}
mappa3(zcou, 'Paese')
{
  ta = sort(table(data$countryName), decreasing=T)
  zcour = t(apply(zcou, 1, function(x) x/ta))
  zcour2 = apply(zcou, 2, function(x) x/ta)
}
mappa3(zcour, 'Paese')
mappa3(zcour2, 'Paese')


# Genere
{
a = length(unique(data$gender))
zgen = matrix(NA, a, a)
colnames(zgen) = row.names(zgen) = unique(data$gender)
for (i in colnames(zgen)){
  for (j in rownames(zgen)){
    zgen[i,j] = gsize(select2GenderDir(i,j))
  }
}
}
mappa(zgen, 'Genere')
{
ta = sort(table(data$gender), decreasing = T)
zgenr = t(apply(zgen, 1, function(x) x/ta))
zgenr2 = apply(zgen, 2, function(x) x/ta)
}
mappa(zgenr, 'Genere')
mappa(zgenr2, 'Genere')
zgenpro = apply(zgen, 1, function(x) x/colSums(zgen)) %>% t
zgenpro2 = apply(zgen, 2, function(x) x/rowSums(zgen))
mappa(zgenpro, 'Genere')
mappa(zgenpro2, 'Genere')
zgenr
zgenr2

zgen
zgenpro2
zgenpro

male = select_graph("gender", "Male")
female = select_graph("gender", "Female")

edge_density(male)
edge_density(female)

reciprocity(male)
reciprocity(female)

transitivity(male)
transitivity(female)

mean(igraph::degree(gg2)[data$gender=='Male'])
mean(igraph::degree(gg2, mode = 'in')[data$gender=='Male'])
mean(igraph::degree(gg2, mode = 'out')[data$gender=='Male'])
mean(igraph::degree(gg2,)[data$gender=='Female'])
mean(igraph::degree(gg2, mode = 'in')[data$gender=='Female'])
mean(igraph::degree(gg2, mode = 'out')[data$gender=='Female'])


###
