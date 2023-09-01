library(jsonlite)
r1 <- read_json("dados/relatorio1.json")
str(r1)
library(yaml)
configuracoes <- read_yaml("configuracoes.yaml")
library(purrr)
le_json <- function(i){
  arquivo <- read_json(file.path("dados",configuracoes$arquivos[[i]]))
}

res <- map(1:length(configuracoes$arquivos), function(x)le_json(x))

df_aux <- function(x){
  map(res[[x]], data.frame)
}

registros <- list_rbind(map(1:length(res), function(x) {
  aux <- df_aux(x)
  list_rbind(aux)
  }))

str(registros)
library(tidyverse)
registros <- registros %>% unique() %>% 
  mutate(horario = lubridate::ymd_hms(horario))


registros %>% separate(col = horario,
                       into = c("Data", "Hora"), sep = " ") %>% 
  filter(evento == "recalibragem") %>% 
  ggplot(aes(x = Hora, y = evento))+
  geom_point()+
  facet_wrap(~Data)

registros %>% arrange(horario) %>% 
  mutate(evento_rec = evento == "recalibragem") %>% 
  mutate(n_rec = cumsum(evento_rec)) %>% 
  ggplot(aes(x = horario, y = n_rec))+
  geom_line()+
  theme_bw()+
  labs(y = "número de recalibragens (ao todo)")

registros <- registros %>% arrange(horario) %>% 
  mutate(evento_rec = evento == "recalibragem") %>% 
  mutate(n_rec = cumsum(evento_rec))

tempo_entre_rec <- registros %>% filter(evento == "recalibragem") %>% 
  mutate(hora_prox_rec = lead(horario)) %>% select(n_rec, hora_prox_rec)

registros <- full_join(registros, tempo_entre_rec)
registros <- registros %>% mutate(tempo_ate_rec = as.numeric(hora_prox_rec - horario)/3600) %>% 
  filter(tempo_ate_rec < 8)

for(i in 1:length(configuracoes$ignorar)){
  inicio  <- lubridate::ymd_hms(configuracoes$ignorar[[i]]$inicio)
  fim  <- lubridate::ymd_hms(configuracoes$ignorar[[i]]$fim)
  registros <- registros %>% filter(horario<fim)
}

ajusta_modelos <- function(){
  combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
  l <- unlist(apply(combin, 1, list), recursive = FALSE)
  combinacoes <- lapply(l, function(x) names(x)[x])
  modelos <- list()
  for(i in 1:length(combinacoes)){
  registros_lm <- registros %>% select(c("intensidade", combinacoes[[i]])) %>%
    drop_na()
  modelos[[i]] <- lm(intensidade~., registros_lm)
  }
  return(modelos)
}
modelos <- ajusta_modelos()

prediz_intensidade <- function(vars){
  combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
  l <- unlist(apply(combin, 1, list), recursive = FALSE)
  combinacoes <- lapply(l, function(x) names(x)[x])
  argumento <- ""
  
}
predict(modelos[[5]], data.frame(v1=2,v2=3,v4=1))
