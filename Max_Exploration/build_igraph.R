

#' Building iGraph using cleaned interaction data. Data cleaned and processed 
#' using the code from `CanMW.R` found in `Chapter-2/`. (script: `AdjacencyMatrix.R`)
#' `adj_matrix.rds`adjacency matrix
#' `adj_matrix_val.rds` Validated only
#' The data is found in the OneDrive


library(ggplot2)
library(dplyr)
library(tidyr)
library(igraph)

#1. Read in data 
# Path to the data
path <- "C:/Users/max0j/McGill University/Laura's Lab_Group - Documents/Max/Chapter-2/Output/"

# this is the strict data
canMW <- read.csv("./Max_Exploration/CanMW_cleaned.csv")


adjMat <- readRDS(paste0(path, "adj_matrix_val.rds"))
# for adjMat, # i (row) = prey, j (col) = predator

# Cleaning
# Are there rows that are all 0?
# are there cols that are all 0?
# to remove: only species in which colSums == rowSums == 0

summary(adjMat)
i <- Matrix::rowSums(adjMat)
j <- Matrix::colSums(adjMat)

# specieslist:
sp <- colnames(adjMat)

#species where i and j ==0
to_keep <- sp[!((sp %in% (names(i[i==0]))) & (sp %in% names(j[j == 0])))]

# remove corresponding rows/cols
adjMat_cleaned <- adjMat[to_keep, to_keep]

#2. Create igraph object
#' Given an adjacency matrix where ROWs are PREY and COLUMNS are PREDATORS, 
#' `igraph::graph_from_adjacency_matrix(, mode = "directed")` produces a 
#' directed graph with the edge from the Prey to the Predator (PREY -> PREDATOR)

graph <- graph_from_adjacency_matrix(adjMat_cleaned, mode = "directed", diag = FALSE, add.colnames = NULL, add.rownames = NULL)
plot(graph)
tkplot(graph)
rglplot(graph)

#3. check for cyclessss
is_acyclic(graph)
find_cycle(graph, mode = "total")
canMW %>% 
  filter(Predator == ("Vulpes vulpes"|"Ursus americanus")|Prey == ("Vulpes vulpes"|"Zonotrichia albicollis")) %>% 
  mutate(val = ifelse(GloBi == 1 | BirdDiet == 1 | CarniDiet ==1 | NLfoodweb ==1 | NunavikMW== 1, TRUE, FALSE)) %>%
  filter(val == TRUE)

is_dag(graph)
simple_cycles(graph)

# https://stackoverflow.com/questions/55091438/r-igraph-find-all-cycles :
FindCycles = function(g) {
  Cycles = NULL
  for(v1 in V(g)) {
    if(degree(g, v1, mode="in") == 0) { next }
    GoodNeighbors = neighbors(g, v1, mode="out")
    GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
    for(v2 in GoodNeighbors) {
      TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
      TempCyc = TempCyc[which(sapply(TempCyc, length) > 3)]
      TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
      Cycles  = c(Cycles, TempCyc)
    }
  }
  Cycles
}
