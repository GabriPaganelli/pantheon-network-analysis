# =============================================================================
# scripts/05b_exploratory_communities.R — Community detection
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Applica algoritmi di community detection alla componente gigante
#         della rete Pantheon e confronta i risultati: label propagation,
#         leading eigenvector, fast greedy, fluid communities, Leiden,
#         Louvain, walktrap (a vari step). Esplora la composizione delle
#         community trovate da cluster_fluid_communities.
#
# Input:  R/network_utils.R (grafi: conn, conind, connind)
#
# Output: Oggetti di community in memoria (m1–m11). Nessun file su disco.
#
# Dipendenze: igraph (via network_utils.R)
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         cluster_fluid_communities richiede un grafo non orientato connesso.
#         I parametri di resolution/steps influenzano fortemente il numero
#         di community risultanti (vedere commenti inline).
# =============================================================================

source("R/network_utils.R")

# =============================================================================
# Community detection — confronto algoritmi
# =============================================================================

# print, length, sizes, membership, cut_at, as.dendrogram, as.hclust, groups, modularity

#m = cluster_edge_betweenness(conn)
#m = cluster_infomap(conn)
#m = cluster_optimal(conn)
#cluster_spinglass() ???
m1 <- cluster_label_prop(conn, mode = "all")   # 110 cluster
length(unique(m1$membership))
#m = cluster_label_prop(conn, mode='out')
m2 <- cluster_label_prop(conn, mode = "in")    # 223 cluster
length(unique(m2$membership))
m3 <- cluster_leading_eigen(conn)              # 151 cluster
length(unique(m3$membership))
m4 <- cluster_fast_greedy(connind)             # 50 cluster
length(unique(m4$membership))
m5 <- cluster_fluid_communities(conind, no.of.communities = 20)
m6 <- cluster_leiden(connind, objective_function = "CPM", resolution_parameter = 0.0001)
m6$nb_clusters  # qui serve respar piccolo per pochi cluster (1 -> singolette)
m7 <- cluster_leiden(connind, objective_function = "modularity", resolution_parameter = 1)
m7$nb_clusters  # 23 con respar = 1
m8 <- cluster_louvain(connind, resolution = 1)
length(unique(m8$membership))  # 24 con res = 1
m9  <- cluster_walktrap(conn)             # 305 cluster
length(unique(m9$membership))
m10 <- cluster_walktrap(conn, steps = 10) # 147 cluster
length(unique(m10$membership))
m11 <- cluster_walktrap(conn, steps = 50) # 51 cluster
length(unique(m11$membership))

# Esplorazione composizione community (fluid, k=20)
table(m5$membership)
idx <- ifelse(m5$membership == 1,  TRUE, FALSE)
data$Name[as.numeric(m5$names)[idx] + 1] %>% head(30)
idx <- ifelse(m5$membership == 2,  TRUE, FALSE)
data$Name[as.numeric(m5$names)[idx] + 1] %>% head(30)
idx <- ifelse(m5$membership == 20, TRUE, FALSE)
data$Name[as.numeric(m5$names)[idx] + 1] %>% head(30)
data$occupation[as.numeric(m5$names)[idx] + 1] %>% table() %>% sort(decreasing = TRUE)
