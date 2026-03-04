# La Rete della Fama: Network Analysis sui dati Pantheon
# *The Network of Fame: Network Analysis on the Pantheon Dataset*

---

## Italiano

### Panoramica

Codice R per la tesi triennale *"La Rete della Fama: Network Analysis sui dati Pantheon"*
(Gabriele Paganelli, Università di Padova, A.A. 2023/2024) — [leggi la tesi](thesis.pdf).

Analisi della rete biografica **Networked Pantheon**: 11.340 figure storiche connesse
da 126.153 archi diretti derivati dalla co-citazione su Wikipedia. Il progetto esplora
la struttura della rete, gli indici di centralità, le community e i fattori associati
alla popolarità storica (HPI, Views, BCI); poi tenta una modellazione tramite ERGM.

**Dataset:** Beytía, P. & Schobin, J. (2018). *Networked Pantheon: A Relational Database
of Globally Famous People.* Research Data Journal for the Humanities and Social Sciences,
3(1), 85–98. [DOI: 10.1163/24523666-01000009](https://doi.org/10.1163/24523666-01000009)

### Metodologia

- **Analisi descrittiva** — distribuzioni per dominio, continente, paese, secolo, genere
- **Indici di centralità** — degree, betweenness, closeness, PageRank, HITS
- **Community detection** — algoritmi di clustering su rete non diretta
- **Analisi sottoreti** — sottografi per categoria, heatmap delle connessioni cross-gruppo
- **ERGM** (Exponential Random Graph Models) — modellazione dei fattori associati a HPI,
  Views e BCI controllando per struttura della rete

### Struttura del repository

```
pantheon-network-analysis/
├── README.md
├── LICENSE
├── thesis.pdf
│
├── data-raw/
│   ├── networked_pantheon_db/
│   │   ├── Edges.csv                      # 126.153 archi (co-citazione Wikipedia)
│   │   ├── Nodes.csv                      # 11.340 nodi (figure storiche + attributi)
│   │   ├── README.txt                     # Dizionario colonne dataset originale
│   │   └── networked_pantheon.gephi       # Progetto Gephi originale
│   └── images/                            # Screenshot visualizzazioni Gephi
│
├── R/
│   └── network_utils.R                    # Helper condiviso: dati, grafi, funzioni
│
├── scripts/
│   ├── 01_cfd.R                           # Distribuzioni cumulate di frequenza
│   ├── 02_graph_variables.R               # Reti categoria-categoria per Gephi
│   ├── 03_subnetwork_analysis.R           # Analisi sottoreti (heatmap)
│   ├── 04a_indices_global.R               # Indici globali
│   ├── 04b_indices_by_group.R             # Indici per gruppo
│   ├── 05a_exploratory_descriptive.R      # Statistiche descrittive + export Gephi
│   ├── 05b_exploratory_communities.R      # Community detection
│   ├── 05c_exploratory_centrality.R       # Centralità avanzate + HITS
│   └── 06_ergm.R                          # Modelli ERGM
│
├── output/
│   ├── nodes_gephi.csv                    # Tabella nodi per Gephi (da 05a)
│   └── edges_gephi.csv                    # Edge list per Gephi (da 05a)
│
└── network_export/
    ├── category_domain.csv                # Rete pesata tra domini (da 02)
    ├── category_continent.csv             # Rete pesata tra continenti (da 02)
    ├── category_country.csv               # Rete pesata tra paesi (da 02)
    └── category_century.csv               # Rete pesata tra secoli (da 02)
```

### Installazione

```r
install.packages(c(
  "igraph", "tidyverse", "wesanderson", "gridExtra", "cowplot",
  "RColorBrewer", "xtable", "ComplexHeatmap", "circlize", "corrplot",
  "ggrepel", "superheat", "colorspace", "scales",
  "network", "ergm", "intergraph"
))
```

Analisi svolta con R ≥ 4.3 e igraph ≥ 1.5.

### Utilizzo

Eseguire **dalla root del repository**.
Tutti gli script (tranne `01_cfd.R`) iniziano con `source("R/network_utils.R")`.

```r
# Fase 1 — Analisi principali (indipendenti tra loro)
source("scripts/01_cfd.R")
source("scripts/02_graph_variables.R")     # → network_export/category_*.csv
source("scripts/03_subnetwork_analysis.R")

# Fase 2 — Indici di rete
source("scripts/04a_indices_global.R")
source("scripts/04b_indices_by_group.R")

# Fase 3 — Analisi esplorativa
source("scripts/05a_exploratory_descriptive.R")  # → output/nodes_gephi.csv
                                                  # → output/edges_gephi.csv
source("scripts/05b_exploratory_communities.R")
source("scripts/05c_exploratory_centrality.R")

# Fase 4 — ERGM (opzionale, tempi di calcolo inammissibili per alcune sezioni)
source("scripts/06_ergm.R")
```

Per la visualizzazione in Gephi importare `output/nodes_gephi.csv`,
`output/edges_gephi.csv` e i file in `network_export/`.

### Risultati

- Analisi strutturale: distribuzione dei gradi, clustering
- Centralità: top-figure per betweenness, PageRank e HITS
- Community: partizione della rete per dominio e continente
- ERGM: fattori strutturali e attributi associati a HPI, Views e BCI
- Export CSV per visualizzazione interattiva in Gephi

### Autore

**Gabriele Paganelli** — Academic Portfolio
Tesi triennale, Università di Padova, A.A. 2023/2024.

---

## English

### Overview

R code for the bachelor's thesis *"The Web of Fame: Network Analysis on the Pantheon Dataset"*
(Gabriele Paganelli, University of Padova, A.Y. 2023/2024) — [read the thesis](thesis.pdf).

Analysis of the **Networked Pantheon** biographical network: 11,340 historical figures
connected by 126,153 directed edges derived from Wikipedia co-citations. The project
explores network structure, centrality indices, community detection, and factors
associated with historical popularity (HPI, Views, BCI); tentative modeling is performed via ERGM.

**Dataset:** Beytía, P. & Schobin, J. (2018). *Networked Pantheon: A Relational Database
of Globally Famous People.* Research Data Journal for the Humanities and Social Sciences,
3(1), 85–98. [DOI: 10.1163/24523666-01000009](https://doi.org/10.1163/24523666-01000009)

### Methodology

- **Descriptive analysis** — distributions by domain, continent, country, century, gender
- **Centrality indices** — degree, betweenness, closeness, PageRank, HITS
- **Community detection** — clustering algorithms on undirected network
- **Subnetwork analysis** — category subgraphs, cross-group connectivity heatmaps
- **ERGM** (Exponential Random Graph Models) — modelling factors associated with HPI,
  Views, and BCI while controlling for network structure

### Repository Structure

```
pantheon-network-analysis/
├── README.md
├── LICENSE
├── thesis.pdf
│
├── data-raw/
│   ├── networked_pantheon_db/
│   │   ├── Edges.csv                      # 126,153 edges (Wikipedia co-citations)
│   │   ├── Nodes.csv                      # 11,340 nodes (historical figures + attributes)
│   │   ├── README.txt                     # Original dataset column dictionary
│   │   └── networked_pantheon.gephi       # Original Gephi project
│   └── images/                            # Gephi visualisation screenshots
│
├── R/
│   └── network_utils.R                    # Shared helper: data, graphs, utility functions
│
├── scripts/
│   ├── 01_cfd.R                           # Cumulative Frequency Distribution
│   ├── 02_graph_variables.R               # Category-category networks for Gephi
│   ├── 03_subnetwork_analysis.R           # Subnetwork analysis (heatmaps)
│   ├── 04a_indices_global.R               # Global network indices
│   ├── 04b_indices_by_group.R             # Indices by group (domain, continent…)
│   ├── 05a_exploratory_descriptive.R      # Descriptive statistics + Gephi export
│   ├── 05b_exploratory_communities.R      # Community detection
│   ├── 05c_exploratory_centrality.R       # Advanced centrality + HITS
│   └── 06_ergm.R                          # ERGM models
│
├── output/
│   ├── nodes_gephi.csv                    # Node table for Gephi (from 05a)
│   └── edges_gephi.csv                    # Edge list for Gephi (from 05a)
│
└── network_export/
    ├── category_domain.csv                # Weighted domain-domain network (from 02)
    ├── category_continent.csv             # Weighted continent network (from 02)
    ├── category_country.csv               # Weighted country network (from 02)
    └── category_century.csv               # Weighted century network (from 02)
```

### Installation

```r
install.packages(c(
  "igraph", "tidyverse", "wesanderson", "gridExtra", "cowplot",
  "RColorBrewer", "xtable", "ComplexHeatmap", "circlize", "corrplot",
  "ggrepel", "superheat", "colorspace", "scales",
  "network", "ergm", "intergraph"
))
```

Tested with R ≥ 4.3 and igraph ≥ 1.5.

### Usage

Run all scripts **from the repository root**.
All scripts (except `01_cfd.R`) begin with `source("R/network_utils.R")`.

```r
# Phase 1 — Main analyses (independent)
source("scripts/01_cfd.R")
source("scripts/02_graph_variables.R")     # → network_export/category_*.csv
source("scripts/03_subnetwork_analysis.R")

# Phase 2 — Network indices
source("scripts/04a_indices_global.R")
source("scripts/04b_indices_by_group.R")

# Phase 3 — Exploratory analysis
source("scripts/05a_exploratory_descriptive.R")  # → output/nodes_gephi.csv
                                                  # → output/edges_gephi.csv
source("scripts/05b_exploratory_communities.R")
source("scripts/05c_exploratory_centrality.R")

# Phase 4 — ERGM (optional, some parts are impossibly computationally intensive)
source("scripts/06_ergm.R")
```

For Gephi visualisation, import `output/nodes_gephi.csv`, `output/edges_gephi.csv`,
and the files in `network_export/`.

### Results

- Structural analysis: degree distribution, clustering coefficients
- Centrality: top figures by betweenness, PageRank, and HITS
- Communities: network partition by domain and continent
- ERGM: structural and attribute-level factors associated with HPI, Views, and BCI
- CSV exports for interactive visualisation in Gephi

### Author

**Gabriele Paganelli** — Academic Portfolio
Bachelor's thesis, University of Padova, A.Y. 2023/2024.
