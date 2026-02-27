# =============================================================================
# scripts/02_graph_variables.R — Grafi pesati per variabile categoriale
# =============================================================================
# Tipo:   Analisi + export Gephi
#
# Scopo:  Costruisce le matrici di adiacenza pesate tra categorie (dominio,
#         paese, paese completo, continente, secolo) calcolando il numero di
#         archi cross-categoria nella rete Pantheon. Esporta le edge list in
#         formato CSV per visualizzazione in Gephi.
#
# Input:  R/network_utils.R (dati + grafi + funzioni di selezione)
#
# Output: network_export/category_domain.csv
#         network_export/category_country.csv
#         network_export/category_country_all.csv
#         network_export/category_continent.csv
#         network_export/category_century.csv
#
# Dipendenze: igraph (via network_utils.R), readr
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         I CSV sono importabili in Gephi: File > Import Spreadsheet > Edges table
#         (colonne: V1 = Source, V2 = Target).
# =============================================================================

source("R/network_utils.R")

library(readr)

# =============================================================================
# 1. DOMINI — matrice di connessioni cross-dominio
# =============================================================================

a <- length(unique(data$domain))
domini_rel <- domini <- matrix(NA, a, a)
colnames(domini_rel) <- row.names(domini_rel) <-
  colnames(domini)    <- row.names(domini)    <- names(sort(table(data$domain), decreasing = TRUE))

for (i in unique(data$domain)) {
  size <- gsize(select_graph("domain", i, "all"))
  for (j in unique(data$domain)) {
    inter            <- gsize(select_graph2("domain", i, j))
    domini_rel[i, j] <- inter / size
    domini[i, j]     <- inter
  }
}

# =============================================================================
# 2. PAESI (top 25) — matrice di connessioni cross-paese
# =============================================================================

cou <- table(data$countryName) %>% sort(decreasing = TRUE) %>% head(25) %>% names()
a   <- length(cou)
paesi <- paesi_rel <- matrix(NA, a, a)
colnames(paesi)    <- row.names(paesi)    <-
  colnames(paesi_rel) <- row.names(paesi_rel) <- cou

for (i in cou) {
  size <- gsize(select_graph("countryName", i, "all"))
  for (j in cou) {
    inter              <- gsize(select_graph2("countryName", i, j))
    paesi_rel[i, j]   <- inter / size
    paesi[i, j]        <- inter
  }
}

# =============================================================================
# 3. PAESI (tutti) — matrice completa + aggregazione in OTHER
# =============================================================================

# 31118 zeri su 38025, 6469 sotto lo 0.1
cou <- table(data$countryName) %>% sort(decreasing = TRUE) %>% names()
a   <- length(cou)
countries <- countries_rel <- matrix(NA, a, a)
colnames(countries)    <- row.names(countries)    <-
  colnames(countries_rel) <- row.names(countries_rel) <- cou

for (i in cou) {
  size <- gsize(select_graph("countryName", i, "all"))
  for (j in cou) {
    inter                  <- gsize(select_graph2("countryName", i, j))
    countries_rel[i, j]   <- inter / size
    countries[i, j]        <- inter
  }
}

# Aggrega tutti i paesi oltre il 25° in "OTHER" per ridurre la matrice
riga   <- countries[26:195, ] %>% colSums()
riga   <- riga[1:25]
colonna <- t(countries)[26:195, ] %>% colSums()
colonna <- c(colonna[1:25], sum(colonna[26:195]))
paesi   <- rbind(paesi, riga)
paesi   <- cbind(paesi, colonna)
colnames(paesi)[26] <- "OTHER"
rownames(paesi)[26] <- "OTHER"

# =============================================================================
# 4. CONTINENTI — matrice di connessioni cross-continente
# =============================================================================

a            <- length(unique(data$continentName))
contin       <- contin_rel <- matrix(NA, a, a)
continent_names <- unique(data$continentName)
colnames(contin) <- row.names(contin) <-
  colnames(contin_rel) <- row.names(contin_rel) <- continent_names

for (i in continent_names) {
  size <- gsize(select_graph("continentName", i, "all"))
  for (j in continent_names) {
    inter            <- gsize(select_graph2("continentName", i, j))
    contin_rel[i, j] <- inter / size
    contin[i, j]     <- inter
  }
}

# =============================================================================
# 5. SECOLI — matrice di connessioni cross-secolo
# =============================================================================

u        <- sort(unique(data$century))
a        <- length(u)
centuries <- centuries_rel <- matrix(NA, a, a)
colnames(centuries) <- row.names(centuries) <-
  colnames(centuries_rel) <- row.names(centuries_rel) <- as.character(u)

for (i in u) {
  size <- gsize(select_graph("century", i, "all"))
  for (j in u) {
    inter                                         <- gsize(select_graph2("century", i, j))
    centuries_rel[as.character(i), as.character(j)] <- inter / size
    centuries[as.character(i), as.character(j)]     <- inter
  }
}

# =============================================================================
# 6. Visualizzazione (grafo pesato domini)
# =============================================================================

rete_domini      <- graph_from_adjacency_matrix(domini,     weighted = TRUE, add.rownames = TRUE)
rete_domini_rel  <- graph_from_adjacency_matrix(domini_rel, weighted = TRUE, add.rownames = TRUE)
rete_paesi       <- graph_from_adjacency_matrix(paesi,      weighted = TRUE, add.rownames = TRUE)
rete_paesi_rel   <- graph_from_adjacency_matrix(paesi_rel,  weighted = TRUE, add.rownames = TRUE)
rete_countries   <- graph_from_adjacency_matrix(countries,     weighted = TRUE, add.rownames = TRUE)
rete_countries_rel <- graph_from_adjacency_matrix(countries_rel, weighted = TRUE, add.rownames = TRUE)
rete_contin      <- graph_from_adjacency_matrix(contin,     weighted = TRUE, add.rownames = TRUE)
rete_contin_rel  <- graph_from_adjacency_matrix(contin_rel, weighted = TRUE, add.rownames = TRUE)
rete_centuries   <- graph_from_adjacency_matrix(centuries,     weighted = TRUE, add.rownames = TRUE)
rete_centuries_rel <- graph_from_adjacency_matrix(centuries_rel)

V(rete_domini)$num <- table(data$domain) %>% sort(decreasing = TRUE)
plot(rete_domini, vertex.size = V(rete_domini)$num / 100,
     edge.width = E(rete_domini)$weight %>% log(), edge.arrow.size = 0.5,
     edge.arrow.width = 0.7,
     layout = layout_with_fr(rete_domini, weights = E(rete_domini)$weights))

plot(rete_countries, edge.arrow.size = 0.5,
     edge.arrow.width = 0.7,
     layout = layout_with_fr(rete_countries, weights = E(rete_countries)$weights))
rete_countries_con <- delete_vertices(rete_countries,
                                      V(rete_countries)[components(rete_countries)$membership != 1])
plot(rete_countries_con, edge.arrow.size = 0.5,
     edge.arrow.width = 0.7,
     layout = layout_with_fr(rete_countries_con),
     vertex.label.cex = 0.6, vertex.size = c(rep(8, 10), rep(5, 176)),
     vertex.color = c(rep(2, 10), rep(3, 176)), vertex.label = NA)

# =============================================================================
# 7. Export Gephi — edge list non pesate per ogni categoria
# =============================================================================

rete_domini    <- graph_from_adjacency_matrix(domini,    add.rownames = TRUE)
rete_paesi     <- graph_from_adjacency_matrix(paesi,     add.rownames = TRUE)
rete_countries <- graph_from_adjacency_matrix(countries, add.rownames = TRUE)
rete_contin    <- graph_from_adjacency_matrix(contin,    add.rownames = TRUE)
rete_centuries <- graph_from_adjacency_matrix(centuries, add.rownames = TRUE)

write_csv(as_edgelist(rete_domini)    %>% as.data.frame(), file = "network_export/category_domain.csv")
write_csv(as_edgelist(rete_paesi)     %>% as.data.frame(), file = "network_export/category_country.csv")
write_csv(as_edgelist(rete_countries) %>% as.data.frame(), file = "network_export/category_country_all.csv")
write_csv(as_edgelist(rete_contin)    %>% as.data.frame(), file = "network_export/category_continent.csv")
write_csv(as_edgelist(rete_centuries) %>% as.data.frame(), file = "network_export/category_century.csv")
