# Library load.
library(tidyverse)
setwd("C:/Users/neris/Desktop/RESEARCH/Projects/Articulo_diversity/Klebsiella_global_genomics")


source("code/design.R")
# Load metadata database ####


base_datos <- readr::read_delim("data/ALL_metadata_PASS_20221116.csv") %>%
  tidyr::replace_na(list(Continent = "Unknown",Isolation_source = "Unknown")) # metadata de las muestras


base_datos <- base_datos %>%
  mutate(ORIGIN = case_when(
    type2 == "NLSAR" ~ "NLSAR",
    Country == "Spain" ~ "Spain",
    T ~ Continent
  )) %>%
  filter(!(str_detect(Nombre_archivo_neris, "HCV2_51")))


# Load genomic database ####

datos_genomicos <- read_delim("data/ISOLATES_QC_ALLINFO.csv") %>%
  rename(Nombre_archivo_neris = Accession) # datos de Kleborate

datos_genomicos$Nombre_archivo_neris <-
  gsub("LONG_", "", datos_genomicos$Nombre_archivo_neris)

# Merge databases ####

All.genomic.db <- left_join(base_datos, datos_genomicos) %>%
  filter(!(str_detect(Nombre_archivo_neris, "HCV2_51")))


# Edit merge bd ####
library(stringi)

All.genomic.db$ST.1 <- gsub("-.*", "", All.genomic.db$ST)


All.genomic.db$Bla_Carb_acquired2 <-
  stringi::stri_replace_all_fixed(
    All.genomic.db$Bla_Carb_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )


All.genomic.db$Bla_ESBL_acquired2 <-
  stringi::stri_replace_all_fixed(
    All.genomic.db$Bla_ESBL_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )

All.genomic.db$Bla_acquired2 <-
  stringi::stri_replace_all_fixed(
    All.genomic.db$Bla_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )




All.genomic.db <- mutate(
  All.genomic.db,
  ST_1 = case_when(ST.1 %in% STGROUPING[-13] ~ ST.1,
                   T ~ "Other"),
  ST_2 = case_when(ST.1 %in% STGROUPING_2[-15] ~ ST.1,
                   T ~ "Other")
)

#write.csv(All.genomic.db, "ALL_INFO_march23.tsv", row.names = F, quote = F)



#All.genomic.db <- All.genomic.db %>% filter(Bioproject != "PRJNA415530" | is.na(Bioproject))


# EDICION PARA EL PAPER 10 NOV 2023
base_datos = base_datos %>%
  filter(type != "SPAIN042022" & type != "SRA") %>% 
  mutate(
    ORIGIN,
    ORIGIN = case_when(
      ORIGIN == "Spain" ~ "Europe",
      T ~ ORIGIN
    ) ) 
# EDICION PARA EL PAPER 10 NOV 2023
All.genomic.db = All.genomic.db %>%
  filter(type != "SPAIN042022" & type != "SRA") %>% 
  mutate(
    ORIGIN,
    ORIGIN = case_when(
      ORIGIN == "Spain" ~ "Europe",
      T ~ ORIGIN
    ) ) 

selected_Sts <- All.genomic.db %>%
  filter(ORIGIN != "Unknown") %>%
  group_by(ORIGIN) %>% count(ST.1) %>%
  slice_max(order_by = n,
            n = 10,
            with_ties = T) %>%
  filter(n > 5) %>%
  pull(ST.1)

selected_STs <-
  All.genomic.db %>%
  count(ST.1) %>%
  slice_max(order_by = n,
            n = 16,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(ST.1)

STORDER_ALL <- All.genomic.db %>%
  filter(ST.1 %in% selected_STs) %>%
  count(ST.1) %>%
  arrange(desc(n)) %>%
  pull(ST.1) %>% append("Other")


STORDER_ALL2 <- All.genomic.db %>%
  #filter(ST.1 %in% selected_STs) %>%
  count(ST.1) %>%
  arrange(desc(n)) %>%
  pull(ST.1) %>% append("Other")




