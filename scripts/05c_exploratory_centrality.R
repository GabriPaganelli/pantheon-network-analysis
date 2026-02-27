# =============================================================================
# scripts/05c_exploratory_centrality.R ÔÇö Indici di centralit├á sulla rete globale
# =============================================================================
# Tipo:   Analisi
#
# Scopo:  Calcola e ordina gli indici di centralit├á sul grafo completo (gg, gg2)
#         e sulla componente gigante (conn): degree, closeness, betweenness,
#         pagerank, eigen, harmonic, HITS (authority/hub).
#         Mostra le classifiche top-50/20 per ciascun indice.
#
# Input:  R/network_utils.R (grafi: gg, gg2, conn, con + dati)
#
# Output: Classifiche in console (nessun file su disco).
#
# Dipendenze: igraph (via network_utils.R)
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         networkR sostituito con igraph::authority_score / hub_score.
# =============================================================================
source("R/network_utils.R")# Components
{
components(gg)$csize
components(gg)$no
components(gg2)$csize
components(gg2)$no
sum(components(gg)$membership != 1)
components(conn)$csize
components(conn)$no
}

# Indici per grafo non connesso
{
ecc = eccentricity(gg)
sort(ecc, decreasing=T)[1:50]
ecc1 = eccentricity(gg, mode='in')
sort(ecc1, decreasing=T)[1:50]
ecc2 = eccentricity(gg, mode='out')
sort(ecc2, decreasing=T)[1:50]

dyad_census(gg)
diameter(gg) # -> 15

kn1 = knn(gg)
kn2 = knn(gg, mode='out')
kn3 = knn(gg, mode='in')

sort(kn1$knn, decreasing=T)[1:40]
sort(kn2$knn, decreasing=T)[1:40]
sort(kn3$knn, decreasing=T)[1:40]

kn1$knnk
kn2$knnk
kn3$knnk

# radius(gg)              # 0
# radius(gg, mode='in')   # 0
# radius(gg, mode='out')  # 0

reciprocity(gg)

transitivity(gg)

triad_census(gg)
triad_census(gg2)

head(count_triangles(gg))
triangles(gg2)
length(triangles(gg2))/3

sum(which_mutual(gg))

plot(table(degree(gg2)))

#sort(power_centrality(gg, rescale=T,), decreasing=T)[1:50] # serve piÃ¹ memoria
sort(degree(gg), decreasing=T)[1:50] # se /(N-1), degree centrality (std)
sort(closeness(gg))[1:50] # ma non ha senso perchÃ¨ non Ã¨ connesso
sort(betweenness(gg), decreasing=T)[1:50]
sort(page_rank(gg)$vector, decreasing=T)[1:50]
sort(eigen_centrality(gg)$vector, decreasing=T)[1:50]
sort(harmonic_centrality(gg), decreasing=T)[1:50]
sort(hub_score(gg)$vector, decreasing=T)[1:50]
#s = subgraph_centrality(gg)

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
{
ecc = eccentricity(conn)
sort(ecc, decreasing=T)[1:20]
sort(ecc)[1:15]
ecc1 = eccentricity(conn, mode='in')
sort(ecc1, decreasing=T)[1:20]
sort(ecc1)[1:15]
ecc2 = eccentricity(conn, mode='out')
sort(ecc2, decreasing=T)[1:20]
sort(ecc2)[1:15]

dyad_census(conn)
diameter(conn)
is_connected(conn)    # T !

kn1 = knn(conn)
kn2 = knn(conn, mode='out')
kn3 = knn(conn, mode='in')

sort(kn1$knn, decreasing=T)[1:25]
sort(kn2$knn, decreasing=T)[1:25]
sort(kn3$knn, decreasing=T)[1:25]

kn1$knnk
kn2$knnk
kn3$knnk

radius(conn)
radius(conn, mode='in')
radius(conn, mode='out')

reciprocity(conn)

transitivity(conn)

triad_census(conn)

head(count_triangles(conn))
triangles(conn)
length(triangles(conn))/3

sum(which_mutual(conn))

plot(table(degree(conn)))

#sort(power_centrality(conn, rescale=T,), decreasing=T)[1:50] # serve piÃ¹ memoria
sort(degree(conn), decreasing=T)[1:50] # se /(N-1), degree centrality (std)
sort(closeness(conn))[1:50]
sort(betweenness(conn), decreasing=T)[1:50]
sort(page_rank(conn)$vector, decreasing=T)[1:50]
sort(eigen_centrality(conn)$vector, decreasing=T)[1:50]
sort(harmonic_centrality(conn), decreasing=T)[1:50]
sort(hub_score(conn)$vector, decreasing=T)[1:50]
#s = subgraph_centrality(conn)

auth_conn <- igraph::authority_score(conn)$vector
hub_conn  <- igraph::hub_score(conn)$vector
mat <- data.frame(auth_conn, data$Name[as.numeric(names(V(con)))+1])
mat <- mat[order(mat[,1], decreasing = TRUE),]
head(mat, 20)
mat <- data.frame(hub_conn, data$Name[as.numeric(names(V(con)))+1])
mat <- mat[order(mat[,1], decreasing = TRUE),]
head(mat, 20)