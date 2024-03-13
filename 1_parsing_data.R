# Library load.
library(tidyverse)


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

datos_total <- left_join(base_datos, datos_genomicos) %>%
  filter(!(str_detect(Nombre_archivo_neris, "HCV2_51")))


# Edit merge bd ####

datos_total$ST.1 <- gsub("-.*", "", datos_total$ST)


datos_total$Bla_Carb_acquired2 <-
  stri_replace_all_fixed(
    datos_total$Bla_Carb_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )


datos_total$Bla_ESBL_acquired2 <-
  stri_replace_all_fixed(
    datos_total$Bla_ESBL_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )

datos_total$Bla_acquired2 <-
  stri_replace_all_fixed(
    datos_total$Bla_acquired,
    pattern = c("*", "?", "^"),
    replacement = c("", "", ""),
    vectorize = F
  )




datos_total <- mutate(
  datos_total,
  ST_1 = case_when(ST.1 %in% STGROUPING[-13] ~ ST.1,
                   T ~ "Other"),
  ST_2 = case_when(ST.1 %in% STGROUPING_2[-15] ~ ST.1,
                   T ~ "Other")
)

#write.csv(datos_total, "ALL_INFO_march23.tsv", row.names = F, quote = F)



#datos_total <- datos_total %>% filter(Bioproject != "PRJNA415530" | is.na(Bioproject))


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
datos_total = datos_total %>%
  filter(type != "SPAIN042022" & type != "SRA") %>% 
  mutate(
    ORIGIN,
    ORIGIN = case_when(
      ORIGIN == "Spain" ~ "Europe",
      T ~ ORIGIN
    ) ) 

selected_Sts <- datos_total %>%
  filter(ORIGIN != "Unknown") %>%
  group_by(ORIGIN) %>% count(ST.1) %>%
  slice_max(order_by = n,
            n = 10,
            with_ties = T) %>%
  filter(n > 5) %>%
  pull(ST.1)

selected_STs <-
  datos_total %>%
  count(ST.1) %>%
  slice_max(order_by = n,
            n = 16,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(ST.1)

STORDER_ALL <- datos_total %>%
  filter(ST.1 %in% selected_STs) %>%
  count(ST.1) %>%
  arrange(desc(n)) %>%
  pull(ST.1) %>% append("Other")


STORDER_ALL2 <- datos_total %>%
  #filter(ST.1 %in% selected_STs) %>%
  count(ST.1) %>%
  arrange(desc(n)) %>%
  pull(ST.1) %>% append("Other")




