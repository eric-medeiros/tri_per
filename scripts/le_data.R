library(readxl)
library(lubridate)
library(dplyr)
library(forcats)
library(stringr)

caminho_excel <- "data/dados.xlsx"

saida <- read_excel(caminho_excel, sheet = 1)

saida$SAIDA <- as.integer(saida$SAIDA)
saida$DATA <- dmy(saida$DATA)
saida$LOCAL <- as.factor(saida$LOCAL)
saida$Hcampo <- as.period(round(as.period(saida$Hcampo*3600)),"hours")
saida$Hboto <- as.period(round(as.period(as.double(saida$Hboto)*3600)),"hours")
saida$Nfoto <- as.integer(saida$Nfoto)

saida_fix <- saida %>%
  filter(LOCAL != "TRA")


ids <- read_excel(caminho_excel, sheet = 2)

ids$DATAHORA <- dmy_hm(ids$DATAHORA)
ids$LOCAL <- as.factor(ids$LOCAL)
ids$IND <- as.character(ids$IND)
ids$DATA <- as.Date(ids$DATAHORA)

ids_fix <- ids %>%
  filter(LOCAL != "TRA")


dist <- read_excel(caminho_excel, sheet = 3)

dist$IND <- as.character(dist$IND)
dist$DIST <- as.factor(dist$DIST)

dados <- ids_fix %>%
  left_join(saida_fix, by = c("DATA","LOCAL")) %>%
  select(c(5,4,2,6,7,8,3,1)) %>%
  left_join(dist, by = "IND") %>%
  mutate(IND = as.factor(IND)) %>%
  nest_by(SAIDA, DATA, LOCAL, Hcampo, Hboto, Nfoto)


dados <- dados %>%
  mutate(n_ID_saida = nrow(data),
         n_ID_total = 0,
         n_ID_novos = 0,
         n_ReID_saida = 0,
         n_ReID_total = 0,
         IND_saida = "NA",
         IND_total = "NA",
         IND_novos = "NA")


ID_cum <- factor(levels = str_sort(levels(dados$data[[1]]$IND), numeric = TRUE))
ReID_cum <- factor(levels = str_sort(levels(dados$data[[1]]$IND), numeric = TRUE))

for(i in 1:nrow(dados)) {
  
  id_antes <- ID_cum
  ID_cum <- unique(fct_c(ID_cum, dados$data[[i]]$IND))
  id_depois <- ID_cum
  
  reid_antes <- ReID_cum
  ReID_cum <- unique(fct_c(ReID_cum, dados$data[[i]]$IND[dados$data[[i]]$IND %in% id_antes]))
  reid_depois <- ReID_cum 

  dados$n_ID_total[[i]] <-  sum(fct_count(fct_drop(ID_cum))$n)

  dados$n_ID_novos[[i]] <- sum(fct_count(fct_drop(id_depois))$n) - sum(fct_count(fct_drop(id_antes))$n)
  
  dados$n_ReID_saida[[i]] <- sum(fct_count(fct_drop(reid_depois))$n) - sum(fct_count(fct_drop(reid_antes))$n)
  
  dados$n_ReID_total[[i]] <-  sum(fct_count(fct_drop(ReID_cum))$n)
  
  dados$IND_saida[[i]] <- paste(dados$data[[i]]$IND, collapse = ", ")
  
  dados$IND_total[[i]] <- paste(fct_inseq(fct_unique(fct_drop(ID_cum))), collapse = ", ")
  
  dados$IND_novos[[i]] <- paste(setdiff(id_depois, id_antes), collapse = ", ")

 }

dados$"ID/H" <- dados$n_ID_saida/(hour(dados$Hcampo)+(minute(dados$Hcampo)/60))

dados %>%
  group_by(LOCAL) %>%
  summarise( media = mean())
