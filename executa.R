library(devtools)
library(purrr)
source("simulador.R")
devtools::source_url("https://ime.unicamp.br/~ra137784/ME918/2023s2/lab01_ig.R")
planos <- input_lab01()       
str(planos) # várias listas com planos de simulação
amostras <- map(planos, simula_distribuicao)
