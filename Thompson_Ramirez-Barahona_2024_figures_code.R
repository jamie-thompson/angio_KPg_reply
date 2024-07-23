library(ape)
library(tidyverse)
library(magrittr)
library(phytools)

## This object contains the Smith and Brown (2018) phylogeny we analysed, along with trees simulated by Hagen (2024) under 10% and 40% survival
# Code to regenerate these trees is provided below but commented out, this RData has the exact versions we used
load("Thompson_Ramirez-Barahona_2024_data.RData")

## To read in Smith and Brown (2018) tree and Hagen (2024) trees
# t <- ape::read.tree("ultra_Angio_SB_prune.tre")
# tt <- ape::read.tree("hagen_no_extinct_tree40.tre")
# ttt <- ape::read.tree("hagen_no_extinct_tree10.tre")
# 
# hagen40 <- nodeHeights(tt)[,1] %>% as_tibble() %>% rename("hagen" = value) %>% 
#   mutate(hagen = max(hagen)-hagen) %>% mutate(RootWard = 66/max(hagen))
# hagen10 <- nodeHeights(ttt)[,1] %>% as_tibble() %>% rename("hagen" = value) %>% 
#   mutate(hagen = max(hagen)-hagen) %>% mutate(RootWard = 66/max(hagen))
# SB <- nodeHeights(t)[,1] %>% as_tibble() %>% rename("Us" = value) %>% 
#   mutate(Us = max(Us)-Us) %>% mutate(RootWard = ifelse(Us < 66, NA, 66 / Us))
# 
# hagen40 %<>% mutate(Bins = (cut(hagen,breaks = seq(0,130,2),labels = seq(0,130,2)[-1],include.lowest=TRUE))) %>% 
#   count(Bins) %>% mutate(Bins = as.numeric(as.character(Bins)))
# hagen10 %<>% mutate(Bins = (cut(hagen,breaks = seq(0,116,2),labels = seq(0,116,2)[-1],include.lowest=TRUE))) %>% 
#   count(Bins) %>% mutate(Bins = as.numeric(as.character(Bins)))
# SB %<>% mutate(Bins = (cut(Us,breaks = seq(0,140,2),labels = seq(0,140,2)[-1],include.lowest=TRUE))) %>% 
#   count(Bins) %>% mutate(Bins = as.numeric(as.character(Bins)))

# # To simulate new versions of the six trees we made (exact ones are in the RData alongside trees made by Hagen (2024) and Smith and Brown (2018))
# sims = 1
# taxa = 70000
# root = 140
# spec = 0.18
# ext = 0.09
# samp = 0.23
# mass = 66
# survival = 0.4
# strat = "uniform"
# age_taxa <- tess.sim.taxa.age(n=sims, nTaxa = taxa, age = root, lambda = spec, mu = ext, samplingProbability = samp, MRCA=TRUE, samplingStrategy = strat)
# taxa_taxa <- tess.sim.taxa(n=sims, nTaxa = taxa, max = root, lambda = spec, mu = ext, samplingProbability = samp,MRCA=TRUE,samplingStrategy = strat)
# taxa_complete <- tess.sim.taxa(n=sims, nTaxa = taxa, max = root, lambda = spec, mu = ext, samplingProbability = 1, MRCA=TRUE,samplingStrategy = strat)
# age_taxa_mass <- tess.sim.taxa.age(n=sims, nTaxa = taxa, age = root, lambda = spec, mu = ext, samplingProbability = samp,MRCA=TRUE,massExtinctionTimes = root - mass, massExtinctionSurvivalProbabilities = survival,samplingStrategy = strat)
# taxa_taxa_mass <- tess.sim.taxa(n=sims, nTaxa = taxa, max = root, lambda = spec, mu = ext, samplingProbability = samp,MRCA=TRUE, massExtinctionTimes = root - mass, massExtinctionSurvivalProbabilities = survival,samplingStrategy = strat)
# taxa_complete_mass <- tess.sim.taxa(n=sims, nTaxa = taxa, max = root, lambda = spec, mu = ext, samplingProbability = 1, MRCA=TRUE, massExtinctionTimes = root - mass, massExtinctionSurvivalProbabilities = survival,samplingStrategy = strat)

hagen_sims <- ggplot() + 
  geom_bar(data=lista[[1]], aes(x= Bins, y = n), stat="identity",
           fill="red",col="NA",alpha=0.5) +
  geom_bar(data=lista[[2]], aes(x= Bins, y = n), stat="identity",
           fill="dodgerblue",col="NA",alpha=0.5) +
  geom_point(data = lista[[3]], aes(x= Bins, y = n),stat="identity") +
  scale_y_continuous(trans="log10",labels=function(x) sprintf("%.0f", (x))) +
  scale_x_continuous(breaks = seq(10,140,10),labels=function(x) sprintf("%.0f", (x))) +
  theme(#axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()) +
  labs(y="Number of branching events",
       x="Age in millions of years") +
  NULL

# Get root height for all sims
maxas <- lista[[6]]  %>% group_by(Simulation) %>% summarise(max = max(Heights)) %>%   mutate(Simulation = factor(Simulation,levels = c("TaxaComplete","TaxaCompleteMass","Taxa","TaxaMass","AgeTaxa","AgeTaxaMass"))) 

colors <- MetBrewer::met.brewer("Hiroshige",n=8)
colors <- rep(colors[c(1,3,6)],each=2)

our_sims <- lista[[6]] %>%
  mutate(Bins = cut(Heights, breaks=seq(0,150,2),labels=seq(0,150,2)[-1],include.lowest=TRUE)) %>% group_by(Simulation,Bins) %>% count() %>% 
  ggplot(aes(color=Simulation,fill=Simulation,y=n,x=as.numeric(as.character(Bins)))) + 
  geom_bar(stat="identity") +
  facet_wrap(~Simulation,nrow = 3,ncol = 2) +
  scale_y_continuous(trans="log10") +
  geom_vline(xintercept = 66, linetype="dashed") +
  geom_line(data=lista[[3]],aes(y=n,x=as.numeric(as.character(Bins))),inherit.aes=FALSE) +
  geom_vline(data = maxas, aes(xintercept = max,col=Simulation)) +
  geom_text(data = maxas, aes(x = max,y = 1000,col=Simulation,
                              label= round(max,0),fontface="bold"),nudge_x = -10) +
  scale_fill_manual(values=colors)+
  scale_color_manual(values=colors) +
  theme(
    strip.background = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line()) +
  labs(y="",x="Age in millions of years") +
  NULL