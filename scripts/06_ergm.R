# =============================================================================
# scripts/06_ergm.R - Modellazione ERGM (Exponential Random Graph Model)
# =============================================================================
# Tipo:   Modellazione
#
# Scopo:  Stima modelli ERGM su sottoreti selezionate (intera rete, tennis,
#         1900, Africa, Italy, Germany, Spain, Canada, Singer, Racecar, Painter,
#         Math, Basket, Switzerland, Physicist, Astronomer, Business, 1700,
#         Ireland, Sample). Analisi di goodness-of-fit e diagnostica MCMC.
#
# Input:  R/network_utils.R (dati + grafi + funzioni di selezione)
#
# Output: Risultati dei modelli in console (nessun file scritto su disco).
#         I modelli Ireland e Sample includono GOF e simulazioni.
#
# Dipendenze: igraph (via network_utils.R), network, ergm, intergraph, Rglpk
#
# Note:   Eseguire dalla root del repository (cartella pantheon-network-analysis/).
#         ATTENZIONE: script computazionalmente pesante (ore di calcolo).
#         Tutti i modelli usano parallel=10 (adattare alla macchina in uso).
#         set.seed(42) garantisce la riproducibilita dei risultati MCMC.
# =============================================================================

source("R/network_utils.R")
# Librerie
{
library(network)
library(ergm)
library(intergraph)
library(Rglpk)
}

# I modelli ERGM usano MCMC internamente: il seed garantisce riproducibilità.
set.seed(42)

# ergmTerm?<term>

# Reti fallite

# Rete intera
{
g
idx = as.numeric(names(V(g)))
gr = asNetwork(g)
gr %v% 'countryname' = data$countryName[idx]
gr %v% 'continentname' = data$continentName[idx]
gr %v% 'gender' = data$gender[idx]
gr %v% 'views' = data$averageViews[idx]
gr %v% 'domain' = data$domain[idx]
gr %v% 'century' = data$century[idx]

m1 = ergm(gr ~ edges + nodematch('gender') + nodematch('countryname')
          + nodematch('continentname') + nodematch('domain') + nodecov('views')
          + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)

}
# Rete Tennis
{
player = select_graph("occupation", "TENNIS PLAYER", id = FALSE)
idx = as.numeric(names(V(player)))
tennis = asNetwork(select_graph("occupation", "TENNIS PLAYER"))
tennis %v% 'countryname' = data$countryName[idx]
tennis %v% 'continentname' = data$continentName[idx]
tennis %v% 'gender' = data$gender[idx]
tennis %v% 'views' = data$averageViews[idx]
mfull = ergm(tennis ~ edges # + nodematch('century') + nodematch('domain')
             + nodematch('gender') + nodematch('countryname')
             + nodematch('continentname') + nodecov('views') + ddsp(1:3) + desp(1:3)
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(4, fixed=T) + dgwnsp(1, fixed=T) + istar(2:5) + ostar(2:5)
             #+ nodefactor('domain')
             + nodefactor('gender') + nodefactor('continentname'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(tennis ~ edges + nodefactor('gender') + nodematch('countryname')
          + nodematch('continentname'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(tennis ~ edges + nodefactor('gender') + nodematch('countryname')
          + nodematch('continentname') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(tennis ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodematch('gender') + nodecov('views') + ddsp(1:10),
          control = control.ergm(parallel=10))
summary(m3)

}
# Rete 1900
{
nove = select_graph("century", 1900, id = FALSE)
idx = as.numeric(names(V(nove)))
novec = asNetwork(select_graph("century", 1900))
novec %v% 'countryname' = data$countryName[idx]
novec %v% 'continentname' = data$continentName[idx]
novec %v% 'gender' = data$gender[idx]
novec %v% 'views' = data$averageViews[idx]
novec %v% 'domain' = data$domain[idx]
m1 = ergm(novec ~ edges + nodematch('domain') + nodematch('continentname')
          + nodematch('gender') + nodecov('views') + nodematch('countryname'),
          control = control.ergm(parallel=10))
summary(m1)
}
# Rete Africa
{
afr = select_graph("continentName", "AFRICA", id = FALSE)
idx = as.numeric(names(V(afr)))
africa = asNetwork(select_graph("continentName", "AFRICA"))
africa %v% 'countryname' = data$countryName[idx]
africa %v% 'century' = data$century[idx]
africa %v% 'gender' = data$gender[idx]
africa %v% 'domain' = data$domain[idx]
africa %v% 'views' = data$averageViews[idx]
m1 = ergm(africa ~ edges + m2star + dsp(10) + nodematch('domain') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
}
# Rete Italy
{
ita = select_graph("countryName", "ITALY", id = FALSE)
idx = as.numeric(names(V(ita)))
italy = asNetwork(select_graph("countryName", "ITALY"))
italy %v% 'century' = data$century[idx]
italy %v% 'gender' = data$gender[idx]
italy %v% 'domain' = data$domain[idx]
italy %v% 'views' = data$averageViews[idx]
m1 = ergm(italy ~ edges + m2star + dsp(10) + nodematch('domain') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
}
# Rete Germany
{
ger = select_graph("countryName", "GERMANY", id = FALSE)
idx = as.numeric(names(V(ger)))
germany = asNetwork(select_graph("countryName", "GERMANY"))
germany %v% 'century' = data$century[idx]
germany %v% 'gender' = data$gender[idx]
germany %v% 'domain' = data$domain[idx]
germany %v% 'views' = data$averageViews[idx]
m1 = ergm(germany ~ edges + m2star + dsp(3:6) + nodematch('domain') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
}
# Rete Spain
{
spa = select_graph("countryName", "SPAIN", id = FALSE)
idx = as.numeric(names(V(spa)))
spain = asNetwork(select_graph("countryName", "SPAIN"))
spain %v% 'century' = data$century[idx]
spain %v% 'gender' = data$gender[idx]
spain %v% 'domain' = data$domain[idx]
spain %v% 'views' = data$averageViews[idx]
m1 = ergm(spain ~ edges + nodematch('domain') + nodematch('century')
          + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(spain ~ edges + dsp(1:3) + nodematch('domain') + nodematch('century')
          + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=8))
summary(m2)
}
# Rete Canada
{
can = select_graph("countryName", "CANADA", id = FALSE)
idx = as.numeric(names(V(can)))
canada = asNetwork(select_graph("countryName", "CANADA"))
canada %v% 'century' = data$century[idx]
canada %v% 'gender' = data$gender[idx]
canada %v% 'domain' = data$domain[idx]
canada %v% 'views' = data$averageViews[idx]
m1 = ergm(canada ~ edges + nodematch('domain') + nodematch('century')
          + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(canada ~ edges + dsp(1:3) + nodematch('domain')
          + nodematch('gender'),
          control = control.ergm(parallel=8))
summary(m2)
}
# Rete Singer
{
sing = select_graph("occupation", "SINGER", id = FALSE)
idx = as.numeric(names(V(sing)))
singer = asNetwork(select_graph("occupation", "SINGER"))
singer %v% 'countryname' = data$countryName[idx]
singer %v% 'continentname' = data$continentName[idx]
singer %v% 'gender' = data$gender[idx]
singer %v% 'views' = data$averageViews[idx]
m1 = ergm(singer ~ edges + m2star + dsp(3:5) + nodematch('gender') + nodematch('continentname'),
          control = control.ergm(parallel=10))
summary(m1)
}
# Rete Racecar
{
racecar = select_graph("occupation", "RACECAR DRIVER", id = FALSE)
idx = as.numeric(names(V(racecar)))
race = asNetwork(select_graph("occupation", "RACECAR DRIVER"))
race %v% 'countryname' = data$countryName[idx]
race %v% 'continentname' = data$continentName[idx]
race %v% 'views' = data$averageViews[idx]
m1 = ergm(race ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(race ~ edges + dsp(1:3) + nodematch('countryname') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m2)
}
# Rete Painter
{
paint = select_graph("occupation", "PAINTER", id = FALSE)
idx = as.numeric(names(V(paint)))
painter = asNetwork(select_graph("occupation", "PAINTER"))
painter %v% 'countryname' = data$countryName[idx]
painter %v% 'continentname' = data$continentName[idx]
painter %v% 'views' = data$averageViews[idx]
painter %v% 'gender' = data$gender[idx]
painter %v% 'century' = data$century[idx]

mfull = ergm(painter ~ edges + nodematch('century') #+ nodematch('domain')
             + nodematch('gender') + nodematch('countryname')
             + nodematch('continentname') + nodecov('views') + ddsp(1:3) + desp(1:3)
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + dgwnsp(1, fixed=T) + istar(2:5) + ostar(2:5)
             #+ nodefactor('domain')
             + nodefactor('gender') + nodefactor('continentname'),
             control = control.ergm(parallel=10))
summary(mfull)

m1 = ergm(painter ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(painter ~ edges + nodematch('century')
          + nodematch('countryname') + nodematch('continentname') + nodecov('views')
          + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
          + gwodegree(1, fixed=T)+ istar(2:5) + ostar(2:4)
          + nodefactor('gender') + nodefactor('continentname'),
          control = control.ergm(parallel=10))
summary(m2)
}
# Rete Math
{
mathem = select_graph("occupation", "MATHEMATICIAN", id = FALSE)
idx = as.numeric(names(V(mathem)))
math = asNetwork(select_graph("occupation", "MATHEMATICIAN"))
math %v% 'countryname' = data$countryName[idx]
math %v% 'continentname' = data$continentName[idx]
math %v% 'views' = data$averageViews[idx]
math %v% 'gender' = data$gender[idx]
math %v% 'century' = data$century[idx]
m1 = ergm(math ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(math ~ edges + dsp(1:3) + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m2)
}
# Rete Basket
{
bask = select_graph("occupation", "BASKETBALL PLAYER", id = FALSE)
bask = delete_vertices(bask, which(degree(bask)==0))
idx = as.numeric(names(V(bask)))
basket = asNetwork(select_graph("occupation", "BASKETBALL PLAYER"))
basket = id_to_name(bask) %>% asNetwork()
basket %v% 'countryname' = data$countryName[idx]
basket %v% 'continentname' = data$continentName[idx]
basket %v% 'views' = data$averageViews[idx]

mfull = ergm(basket ~ edges + nodematch('continentname') + nodecov('views')
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('continentname'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(basket ~ edges + nodematch('continentname')
          + nodecov('views') + nodefactor('continentname'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(astro ~ edges + nodematch('continentname') + nodefactor('continentname')
          + nodecov('views') + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) ,
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(astro ~ edges + nodecov('views') + nodematch('continentname')
          + nodefactor('continentname') + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3)
m4 = ergm(astro ~ edges + nodecov('views') + nodematch('continentname')
          + nodefactor('continentname') + istar(2:5) + ostar(2:5),
          control = control.ergm(parallel=10))
summary(m4)
}
# Rete Switzerland
{
swi = select_graph("countryName", "SWITZERLAND", id = FALSE)
swi = delete_vertices(swi, which(degree(swi)==0))
idx = as.numeric(names(V(swi)))
switz = asNetwork(select_graph("countryName", "SWITZERLAND"))
switz = asNetwork(id_to_name(swi))
switz %v% 'century' = data$century[idx]
switz %v% 'gender' = data$gender[idx]
switz %v% 'domain' = data$domain[idx]
switz %v% 'views' = data$averageViews[idx]

mfull = ergm(switz ~ edges + nodematch('domain') + nodematch('century')
             + nodematch('gender') + nodecov('views') +ddsp(1:3) + desp(1:3)
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + dgwnsp(1, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('domain') + nodefactor('gender'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(switz ~ edges + nodematch('domain') + nodematch('century')
          + nodecov('views')
          + nodefactor('gender'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(switz ~ edges + nodematch('domain') + nodematch('century')
          + nodecov('views')
          + nodefactor('gender') + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) ,
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(switz ~ edges + nodematch('domain') + nodematch('century')
          + nodecov('views')
          + nodefactor('gender') + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3)
m4 = ergm(switz ~ edges + nodematch('domain') + nodematch('century')
          + nodecov('views')
          + nodefactor('gender') + istar(2:3) + ostar(2:3),
          control = control.ergm(parallel=10))
summary(m4)
}
# Rete Phys
{
physics = select_graph("occupation", "PHYSICIST", id = FALSE)
physics = delete_vertices(physics, which(degree(physics)==0))
idx = as.numeric(names(V(physics)))
phys = asNetwork(select_graph("occupation", "PHYSICIST"))
phys = asNetwork(id_to_name(physics))
phys %v% 'countryname' = data$countryName[idx]
phys %v% 'continentname' = data$continentName[idx]
phys %v% 'views' = data$averageViews[idx]
phys %v% 'gender' = data$gender[idx]
phys %v% 'century' = data$century[idx]

mfull = ergm(ireland ~ edges + nodematch('century') + nodematch('continentname')
             + nodematch('gender') + nodecov('views')
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + dgwnsp(1, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('gender'),
             control = control.ergm(parallel=10))
summary(mfull)

m1 = ergm(phys ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodefactor('gender') + nodematch('century'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(phys ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century')
          + dgwesp(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m2)
m2 = ergm(phys ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century')
          + dgwdsp(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(phys ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century')
          + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3)
m4 = ergm(phys ~ edges + nodematch('countryname') + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + nodematch('century')
          + istar(2:5) + ostar(2:5),
          control = control.ergm(parallel=10))
summary(m4)

}
# Rete Astro
{
astronomi = select_graph("occupation", "ASTRONOMER", id = FALSE)
astronomi = delete_vertices(astronomi, which(degree(astronomi)==0))
idx = as.numeric(names(V(astronomi)))
astro = asNetwork(select_graph("occupation", "ASTRONOMER"))
astro = asNetwork(id_to_name(astronomi))
astro %v% 'countryname' = data$countryName[idx]
astro %v% 'continentname' = data$continentName[idx]
astro %v% 'views' = data$averageViews[idx]
astro %v% 'gender' = data$gender[idx]
astro %v% 'century' = data$century[idx]

mfull = ergm(astro ~ edges + nodematch('century') + nodematch('continentname')
             + nodematch('gender') + nodecov('views')
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(4, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('gender') + nodefactor('continentname'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(astro ~ edges + nodematch('century') + nodematch('continentname')
          + nodecov('views') + nodefactor('gender'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(astro ~ edges + nodematch('century') 
          + nodematch('continentname')
          + nodecov('views') + nodematch('gender') + dgwdsp(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(astro ~ edges + nodematch('century')
          + nodecov('views') + nodefactor('gender')+ nodematch('continentname')
          + nodefactor('continentname') + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3)
m4 = ergm(astro ~ edges + nodematch('century') + nodematch('gender')
          + nodecov('views') + nodefactor('gender') + nodematch('continentname')
          + nodefactor('continentname') + istar(c(2,3,5)) + ostar(c(2,3,5)),
          control = control.ergm(parallel=10))
summary(m4)

}
# Rete Business
{
bus = select_graph("occupation", "BUSINESSPERSON", id = FALSE)
bus = delete_vertices(bus, which(degree(bus)==0))
idx = as.numeric(names(V(bus)))
business = asNetwork(select_graph("occupation", "BUSINESSPERSON"))
business = asNetwork(id_to_name(bus))
business %v% 'countryname' = data$countryName[idx]
business %v% 'continentname' = data$continentName[idx]
business %v% 'views' = data$averageViews[idx]

mfull = ergm(business ~ edges + nodematch('continentname') + nodecov('views')
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('continentname'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(business ~ edges + nodematch('continentname')
          + nodecov('views') + nodefactor('continentname'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(business ~ edges + nodematch('continentname') + nodefactor('continentname')
          + nodecov('views') + dgwdsp(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m2)
m3 = ergm(business ~ edges + nodecov('views') + nodematch('continentname')
          + nodefactor('continentname') + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3)

}

# QUeste quelle che ho tenuto
# Rete 1700
{
  sette = select_graph("century", 1700, id = FALSE)
  idx = as.numeric(names(V(sette)))
  settec = asNetwork(select_graph("century", 1700))
  settec %v% 'countryname' = data$countryName[idx]
  settec %v% 'continentname' = data$continentName[idx]
  settec %v% 'gender' = data$gender[idx]
  settec %v% 'views' = data$averageViews[idx]
  settec %v% 'domain' = data$domain[idx]
  msette = ergm(settec ~ edges + nodematch('domain') + nodematch('continentname')
                + nodematch('gender') + nodecov('views') + nodematch('countryname')
                + nodefactor('domain') + nodefactor('continentname') + nodefactor('gender'),
                control = control.ergm(parallel=10))
  summary(msette)
  summary(msette)$coeff
  summary(msette)$coeff %>% xtable
  
  
  mcmc.diagnostics(msette)
  fit = gof(msette, control = control.gof.ergm(nsim = 500, MCMC.burnin = 1000, parallel=10))
  fit
  par(mfrow=c(2,3))
  plot(fit)
  par(mfrow=c(2,3))
  plot(fit, plotlogodds = T)
  
  par(mfrow=c(2,2))
  plot(1,1)
  plot(1,1)
  plot(1,1)
  plot(fit, plotlogodds = T, which=2:5, main='')
  par(mfrow=c(1,1))
  
  par(mfrow=c(1,2))
  sim = simulate(msette)
  plot(settec, main ='Osservata')
  plot(sim, main ='Simulata', displayisolates = T)
  par(mfrow=c(1,1))
}
# Rete Ireland
{
ire = select_graph("countryName", "IRELAND", id = FALSE)
ire = delete_vertices(ire, which(degree(ire)==0))
idx = as.numeric(names(V(ire)))
ireland = asNetwork(select_graph("countryName", "IRELAND"))
ireland = asNetwork(id_to_name(ire))
ireland %v% 'century' = data$century[idx]
ireland %v% 'gender' = data$gender[idx]
ireland %v% 'domain' = data$domain[idx]
ireland %v% 'views' = data$averageViews[idx]
mfull = ergm(ireland ~ edges + nodematch('domain') + nodematch('century')
             + nodematch('gender') + nodecov('views') +ddsp(1:3) + desp(1:3)
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + dgwnsp(1, fixed=T) + istar(2:5) + ostar(2:5)
             + + nodefactor('domain') + nodefactor('gender'),
             control = control.ergm(parallel=10))
summary(mfull)
m1 = ergm(ireland ~ edges + nodematch('domain') + nodematch('century')
          + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m1) # tutti i termini significativi
m2 = ergm(ireland ~ edges + ddsp(1:3) + nodematch('domain') + nodematch('century')
          + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=8))
summary(m2) # ddsp non significativo
m3 = ergm(ireland ~ edges + mutual + desp(1:2)+ nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=8))
summary(m3) # solo desp(1) significativo
m4 = ergm(ireland ~ edges + mutual + dgwdsp(2, fixed=T) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m4) # dgwdsp non significativo
m5 = ergm(ireland ~ edges + mutual + dgwesp(1, fixed=T) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m5) # dgwesp significativo
m6 = ergm(ireland ~ edges + mutual + gwidegree(1, fixed=T) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m6) # gwidegree non significativo
m7 = ergm(ireland ~ edges + mutual + gwodegree(1, fixed=T) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m7) # gwodegree non significativo
m8 = ergm(ireland ~ edges + mutual + dgwnsp(1, fixed=T) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m8) # dgwnsp significativo
m9 = ergm(ireland ~ edges + mutual + istar(2:5) + nodematch('domain')
          + nodematch('century') + nodematch('gender') + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m9) # istar non significativo
m10 = ergm(ireland ~ edges + mutual + ostar(2:5) + nodematch('domain')
           + nodematch('century') + nodematch('gender') + nodecov('views'),
           control = control.ergm(parallel=10))
summary(m10) # ostar non significativo
m11 = ergm(ireland ~ edges + mutual + nodematch('domain') + nodematch('century')
           + nodematch('gender') + nodecov('views') + nodefactor('domain')
           + nodefactor('gender'),
           control = control.ergm(parallel=10))
summary(m11) # nodefactor migliora il fit


mire = ergm(ireland ~ edges + mutual + twopath + nodematch('domain')
            + nodematch('century') + dgwesp(.25, fixed=T) + nodefactor('domain')
            + nodefactor('gender'),
            control = control.ergm(parallel=10))
names(mire$coefficients)[4:11] = c('U_domain', 'U_century', 'gwesp', 'domain_Humanities', 'domain_Institutions', 'domain_Science', 'domain_Sports', 'gender_Male')
summary(mire)
summary(mire)$coef
summary(mire)$coef[,c(1,2,5)] %>% xtable

mcmc.diagnostics(mire)
fit2 = gof(mire, control = control.gof.ergm(nsim = 500, MCMC.burnin = 2000, parallel=10))
fit2
par(mfrow=c(2,3))
plot(fit2)
par(mfrow=c(2,3))
plot(fit2, plotlogodds = T)
par(mfrow=c(2,2))
plot(1,1)
plot(1,1)
plot(1,1)
plot(fit2, plotlogodds = T, main = '')
par(mfrow=c(1,1))

par(mfrow=c(2,3))
sim1 = simulate(mire)
sim2 = simulate(mire)
sim3 = simulate(mire)
sim4 = simulate(mire)
sim5 = simulate(mire)
plot(ireland, main ='Rete osservata', cex.main=2)
plot(sim1, main ='Rete simulata 1', cex.main=2)
plot(sim2, main ='Rete simulata 2', cex.main=2)
plot(sim3, main ='Rete simulata 3', cex.main=2)
plot(sim4, main ='Rete simulata 4', cex.main=2)
plot(sim5, main ='Rete simulata 5', cex.main=2)
par(mfrow=c(1,1))

}
# Rete Sample
{
set.seed(19)
sam = sample.int(11340, 115) %>% sort
{
archi = (edges_n[,1] %in% sam | edges_n[,2] %in% sam)
lista = edges_c[archi,]
rand = igraph::simplify(graph_from_edgelist(lista, directed = T))
rand = delete_vertices(rand, which(degree(rand)<4))
rand = delete_vertices(rand, which(degree(rand)==0))
randgraph = asNetwork(rand)
plot(randgraph)
idx = degree(rand) %>% names %>% as.numeric

randgraph %v% 'domain' = data[idx,]$domain
randgraph %v% 'continentname' = data[idx,]$continentName
randgraph %v% 'countryname' = data[idx,]$countryName
randgraph %v% 'century' = data[idx,]$century
randgraph %v% 'gender' = data[idx,]$gender
randgraph %v% 'views' = data[idx,]$averageViews

data[idx,]$domain %>% table
data[idx,]$continentName %>% table
data[idx,]$countryName %>% table
data[idx,]$century %>% table
data[idx,]$gender %>% table
data[idx,]$averageViews %>% sort %>% plot
}

mfull = ergm(randgraph ~ edges + nodematch('century') + nodematch('domain')
             + nodematch('gender') + nodematch('countryname')
             + nodematch('continentname') + nodecov('views')
             + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T) + gwidegree(1, fixed=T)
             + gwodegree(1, fixed=T) + istar(2:5) + ostar(2:5)
             + nodefactor('domain') + nodefactor('gender') + nodefactor('continentname'),
             control = control.ergm(parallel=10))
# ? nodefactor ?
m1 = ergm(randgraph ~ edges + mutual + nodematch('domain') + nodematch('continentname')
          + nodematch('countryname') + nodematch('century') + nodematch('gender')
          + nodecov('views'),
          control = control.ergm(parallel=10))
summary(m1)
m2 = ergm(randgraph ~ edges + mutual + nodematch('domain') + nodematch('continentname')
          + nodematch('countryname') + nodematch('century') + nodematch('gender')
          + nodecov('views') + dgwdsp(1, fixed=T) + dgwesp(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m2) # gwdsp e gwesp non significativi
m3 = ergm(randgraph ~ edges + mutual + nodematch('domain') + nodematch('continentname')
          + nodematch('countryname') + nodematch('century') + nodematch('gender')
          + nodecov('views') + gwidegree(1, fixed=T) + gwodegree(1, fixed=T),
          control = control.ergm(parallel=10))
summary(m3) # gwidegree e gwodegree significativi

m4 = ergm(randgraph ~ edges + mutual + twopath + nodematch('domain') + nodematch('continentname')
          + nodematch('century') + nodematch('gender')
          + nodecov('views') + gwidegree(.25, fixed=T) + gwodegree(.25, fixed=T)
          + nodefactor('domain'),
          control = control.ergm(parallel=10))
names(m4$coefficients)[4:16] = c('U_domain', 'U_continent', 'U_century', 'U_gender', 'views', 'gwID', 'gwOD', 'domain_Exploration', 'domain_Humanities', 'domain_Institutions', 'domain_PublicFigure', 'domain_Science', 'domain_Sports')
summary(m4)
summary(m4)$coef[,c(1,2,5)] %>% xtable

mcmc.diagnostics(m4)
fit4 = gof(m4, control = control.gof.ergm(nsim = 500, MCMC.burnin = 2000, parallel=10))
fit4
par(mfrow=c(2,3))
plot(fit4)
par(mfrow=c(2,2))
plot(1,1)
plot(1,1)
plot(1,1)
plot(fit4, plotlogodds = T, main = '')
par(mfrow=c(1,1))

par(mfrow=c(2,3))
sim1 = simulate(m4)
sim2 = simulate(m4)
sim3 = simulate(m4)
sim4 = simulate(m4)
sim5 = simulate(m4)
plot(randgraph, main ='Rete osservata', cex.main=2)
plot(sim1, main ='Rete simulata 1', cex.main=2)
plot(sim2, main ='Rete simulata 2', cex.main=2)
plot(sim3, main ='Rete simulata 3', cex.main=2)
plot(sim4, main ='Rete simulata 4', cex.main=2)
plot(sim5, main ='Rete simulata 5', cex.main=2)
par(mfrow=c(1,1))
}




