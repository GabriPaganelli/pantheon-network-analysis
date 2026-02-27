# =============================================================================
# R/network_utils.R — Helper condiviso: dati, grafi, funzioni di selezione
# =============================================================================
# Tipo:   Helper (da sourcecare, non eseguire direttamente)
#
# Scopo:  Carica e pulisce i dati grezzi, costruisce gli 8 oggetti grafo
#         principali e definisce tutte le funzioni di selezione/utilità usate
#         dagli script di analisi.
#
#         Tutti gli script nella cartella scripts/ (tranne 01_cfd.R) iniziano
#         con:  source("R/network_utils.R")
#
# Input:  data-raw/networked_pantheon_db/Edges.csv
#         data-raw/networked_pantheon_db/Nodes.csv
#
# Output: Variabili globali in memoria:
#   Dati:   data, edges_n, edges_c
#   Grafi:  grafo, grafoind, g, g2, gg, gg2, ggg, ggg2
#           con, conind, conn, connind
#   Altro:  lonely
#   Funzioni: select_graph, select_graph2, select_years, select_age
#             id_to_name, data_indexes, person
#
# Dipendenze: igraph
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         Per il dizionario delle colonne del dataset vedere:
#           data-raw/networked_pantheon_db/README.txt
# =============================================================================

library(igraph)

options(digits = 3)

# --- Dati ---------------------------------------------------------------------

x       <- read.csv2("data-raw/networked_pantheon_db/Edges.csv")
edges_n <- as.matrix(x[, 1:2]) + 1
edges_n <- edges_n[edges_n[, 1] != edges_n[, 2], ]   # rimozione self-loop
edges_c <- apply(edges_n, 2, as.character)

data <- read.csv2("data-raw/networked_pantheon_db/Nodes.csv")
data <- data[, -c(2, 4, 5, 8, 10, 11, 21, 22, 24:27, 29:41)]

data[, 5]      <- floor(data$birthyear / 100) * 100
names(data)[5] <- "century"

for (i in c(1, 5, 7, 8, 9, 15:17)) {
  data[, i] <- as.numeric(data[, i])
}

data$Id <- data$Id + 1
data[data$Name == "Catullus", ]$deathyear <- -54
data[data$Name == "Catullus", ]$agespan   <- 30
data[10881, ]$century <- -1000

data$domain[data$domain   == "SCIENCE & TECHNOLOGY"] <- "SCIENCE"
data$domain[data$domain   == "BUSINESS & LAW"]        <- "BUSINESS"
data$domain2[data$domain2 == "SCIENCE & TECHNOLOGY"] <- "SCIENCE"
data$domain2[data$domain2 == "BUSINESS & LAW"]        <- "BUSINESS"

data$countryName[data$countryName == 'S\u00c3\u008eO TOM\u00c3\u008e AND PR\u00c3\u008dNCI'] <- "SAO TOME AND PRINCIPE"
data$countryName <- ifelse(data$countryName == "", "UNKNOWN", data$countryName)

# --- Funzioni di utilità ------------------------------------------------------

# Sostituisce gli Id numerici dei nodi con i nomi delle figure storiche.
id_to_name <- function(x) {
  vertex_attr(x)$name <- data$Name[as.numeric(vertex_attr(x)$name)]
  return(x)
}

# Ritorna gli indici di riga in 'data' dei nodi di un grafo (utile per join).
data_indexes <- function(g) {
  return(which(data$Name %in% names(V(g))))
}

# Ritorna il sottografo (diretto) di un singolo individuo: tutti gli archi
# in cui quella persona è sorgente o destinazione.
person <- function(x) {
  i         <- data$Id[data$Name == x]
  righe     <- as.logical(edges_n[, 1] %in% i | edges_n[, 2] %in% i)
  new_edges <- edges_c[righe, ]
  if (length(new_edges) == 2)              # caso arco singolo: serve matrix
    return(graph_from_edgelist(t(as.matrix(new_edges))))
  return(graph_from_edgelist(new_edges))
}

# Nodi isolati: figure storiche senza nessun arco nel grafo.
lonely <- local({
  righe <- !(data$Id %in% unique(edges_n))
  data[righe, ]
})

# --- Funzioni di selezione ----------------------------------------------------
#
# Tutte le funzioni di selezione delegano la costruzione del grafo a
# .build_graph() (funzione interna). Il parametro `mode` controlla quali
# archi includere; `col` e `val` determinano il sottoinsieme di nodi.

# Funzione interna: costruisce il grafo da un set di Id e una maschera booleana.
.build_graph <- function(node_ids, righe, id) {
  if (!any(righe))
    return(if (id) id_to_name(make_empty_graph() + vertices(unique(node_ids)))
           else make_empty_graph() + vertices(unique(node_ids)))
  new_edges <- matrix(edges_c[righe, ], ncol = 2)
  g   <- igraph::simplify(graph_from_edgelist(new_edges))
  u   <- unique(c(new_edges[, 1], new_edges[, 2]))
  ver <- unique(node_ids[!(node_ids %in% u)])
  if (!anyNA(ver) && length(ver) > 0) g <- g + vertices(ver)
  if (id) return(id_to_name(g))
  return(g)
}

#' Costruisce un sottografo filtrando per variabile e modalità di selezione archi.
#'
#' @param col  Nome della colonna di \code{data} su cui filtrare.
#'   Valori validi: "occupation", "countryName", "continentName", "gender",
#'   "century", "industry", "domain", "domain2".
#' @param val  Valore o vettore di valori da selezionare.
#' @param mode Criterio di inclusione degli archi:
#'   \describe{
#'     \item{"inner"}{entrambi i nodi nel gruppo (default — sottografo indotto)}
#'     \item{"out"}{solo il nodo sorgente nel gruppo}
#'     \item{"in"}{solo il nodo destinazione nel gruppo}
#'     \item{"but"}{esattamente uno dei due nodi nel gruppo (xor)}
#'     \item{"all"}{almeno uno dei due nodi nel gruppo (ego-network del gruppo)}
#'   }
#' @param y   Data frame da usare (default: \code{data} globale).
#' @param id  Se \code{TRUE}, converte gli Id in nomi (default: \code{TRUE}).
#' @return Oggetto igraph.
select_graph <- function(col, val, mode = "inner", y = data, id = TRUE) {
  set    <- y[y[[col]] %in% val, ]
  r_from <- edges_n[, 1] %in% set$Id
  r_to   <- edges_n[, 2] %in% set$Id
  righe  <- switch(mode,
    inner = as.logical(r_from &  r_to),
    out   = r_from,
    `in`  = r_to,
    but   = xor(r_from, r_to),
    all   = as.logical(r_from | r_to),
    stop("'mode' non valido: usare 'inner', 'out', 'in', 'but', 'all'")
  )
  .build_graph(set$Id, righe, id)
}

#' Seleziona archi che attraversano due categorie diverse della stessa variabile.
#' Utile per analizzare le connessioni cross-gruppo (es. Scienza → Arte).
#'
#' @param col Nome della colonna di \code{data}.
#' @param x1  Prima categoria.
#' @param x2  Seconda categoria.
#' @inheritParams select_graph
#' @return Oggetto igraph con gli archi tra x1 e x2 (bidirezionali).
select_graph2 <- function(col, x1, x2, y = data, id = TRUE) {
  set1    <- y[y[[col]] == x1, ]
  set2    <- y[y[[col]] == x2, ]
  r1_from <- edges_n[, 1] %in% set1$Id
  r1_to   <- edges_n[, 2] %in% set1$Id
  r2_from <- edges_n[, 1] %in% set2$Id
  r2_to   <- edges_n[, 2] %in% set2$Id
  righe   <- as.logical((r1_from & r2_to) | (r1_to & r2_from))
  .build_graph(unique(c(set1$Id, set2$Id)), righe, id)
}

#' Seleziona un sottografo per intervallo di anni di nascita.
#'
#' @param range Vettore numerico \code{c(anno_inizio, anno_fine)}.
#' @inheritParams select_graph
#' @return Oggetto igraph.
select_years <- function(range, mode = "inner", y = data, id = TRUE) {
  set    <- y[y$birthyear >= range[1] & y$birthyear <= range[2], ]
  r_from <- edges_n[, 1] %in% set$Id
  r_to   <- edges_n[, 2] %in% set$Id
  righe  <- switch(mode,
    inner = as.logical(r_from &  r_to),
    out   = r_from,
    `in`  = r_to,
    but   = xor(r_from, r_to),
    all   = as.logical(r_from | r_to),
    stop("'mode' non valido: usare 'inner', 'out', 'in', 'but', 'all'")
  )
  .build_graph(set$Id, righe, id)
}

#' Seleziona un sottografo per decennio di longevità.
#' Il decennio è calcolato come \code{trunc(agespan/10)*10}.
#' Esclude figure con \code{deathyear == 2018} (sentinella per viventi).
#'
#' @param decade Decennio di vita (es. \code{70} per individui vissuti 70–79 anni).
#' @inheritParams select_graph
#' @return Oggetto igraph.
select_age <- function(decade, mode = "inner", y = data, id = TRUE) {
  set    <- y[trunc(y$agespan / 10) * 10 == decade & y$deathyear != 2018, ]
  r_from <- edges_n[, 1] %in% set$Id
  r_to   <- edges_n[, 2] %in% set$Id
  righe  <- switch(mode,
    inner = as.logical(r_from &  r_to),
    out   = r_from,
    `in`  = r_to,
    but   = xor(r_from, r_to),
    all   = as.logical(r_from | r_to),
    stop("'mode' non valido: usare 'inner', 'out', 'in', 'but', 'all'")
  )
  .build_graph(set$Id, righe, id)
}

# --- Costruzione degli oggetti grafo principali -------------------------------
#
# Convenzione di naming:
#   grafo / grafoind  : grafo completo (include nodi isolati), Id / Name
#   g    / g2         : grafo senza isolati, Id, direzionato / non direzionato
#   gg   / gg2        : come g/grafo ma con nomi (Name) al posto degli Id
#   ggg  / ggg2       : come g2/grafoind ma con nomi
#   con  / conind     : componente gigante di g / g2
#   conn / connind    : componente gigante di gg / ggg

archi    <- c(t(edges_c))
grafo    <- igraph::simplify(add_edges(make_empty_graph() + vertices(data$Id), archi))
grafoind <- as.undirected(grafo, mode = "collapse")

isolated <- which(degree(grafo) == 0)
g        <- delete_vertices(grafo, isolated)
g2       <- as.undirected(g, mode = "collapse")

gg   <- id_to_name(g)
gg2  <- id_to_name(grafo)
ggg  <- id_to_name(g2)
ggg2 <- id_to_name(grafoind)

con     <- delete_vertices(g,   V(g)[components(g)$membership     != 1])
conind  <- delete_vertices(g2,  V(g2)[components(g2)$membership   != 1])
conn    <- delete_vertices(gg,  V(gg)[components(gg)$membership   != 1])
connind <- delete_vertices(ggg, V(ggg)[components(ggg)$membership != 1])
