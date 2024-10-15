# Jordi Sevilla Fortuny

# Cargar librerias ####

library(pacman)


pacman::p_load(
  "tidyverse",
  # manejo de datos y graficas
  "readr",
  # leer datos
  "showtext",
  # Descargar fuentes
  "ggbreak",
  # Romper ejes ggplot
  "flextable",
  # tablas epidemiologicas
  "janitor",
  # Poner bonita la tabla epi
  "webshot",
  # Imprimir la tabla epi
  "scales",
  # poner eje en porcentaje
  "ggpubr",
  # GGarrange
  "plotly",
  # Graficas interactivas
  "maps",
  # Mapa del mundo con ggplot
  "scatterpie",
  # poner pie chart en el mapa
  #"maptools",
  "sf",
  # formato para intersectar puntos con poligonos
  "ggrepel",
  # evitar solapamiento de labels
  "fastDummies",
  # variables dummy
  "stringi",
  # reemplazar patrones en texto
  "cowplot",
  # figuras
  "treemap",
  "grDevices",
  "gridExtra",
  "gt",
  "gtExtras"
)
gpclibPermit()



# Cargar bases de datos ####


 base_datos <- readr::read_delim("C:/Users/neris/Desktop/LABORATORIO/Articulo_diversity/ALL_metadata_PASS_20221116.csv") %>%
   tidyr::replace_na(list(Continent = "Unknown",Isolation_source = "Unknown")) # metadata de las muestras
#
 datos_genomicos <- read_delim("C:/Users/neris/Desktop/LABORATORIO/Articulo_diversity/ISOLATES_QC_ALLINFO.csv") %>%
   rename(Nombre_archivo_neris = Accession) # datos de Kleborate

base_datos <- base_datos %>%
  mutate(ORIGIN = case_when(
    type2 == "NLSAR" ~ "NLSAR",
    Country == "Spain" ~ "Spain",
    T ~ Continent
  )) %>%
  filter(!(str_detect(Nombre_archivo_neris, "HCV2_51")))

datos_genomicos$Nombre_archivo_neris <-
  gsub("LONG_", "", datos_genomicos$Nombre_archivo_neris)

datos_total <- left_join(base_datos, datos_genomicos) %>%
  filter(!(str_detect(Nombre_archivo_neris, "HCV2_51")))


ncontig_hist <- 
  datos_total %>%
  ggplot(aes(x=contigs))+
  geom_bar(fill="#084c61")+
  scale_fill_identity()+
   geom_vline(xintercept=20, 
               color = "darkred")+        
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))  +
  xlab("Contigs")+
  ylab("Assemblies") 
  
#saveplot(filename = "CH3_ncontigshist", ncontig_hist, "t")
  

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

STGROUPING = c(
  "ST512",
  "ST392",
  "ST17",
  "ST29",
  "ST219",
  "ST101",
  "ST405",
  "ST147",
  "ST15",
  "ST258",
  "ST11",
  "ST307",
  "Other"
)

STGROUPING_2 = c(
  "ST512",
  "ST392",
  "ST17",
  "ST29",
  "ST219",
  "ST101",
  "ST405",
  "ST147",
  "ST15",
  "ST16",
  "ST258",
  "ST11",
  "ST307",
  "ST437",
  "ST37",
  "Other"
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

# Ajustes de gráficos ####
  ## color #####
    RESVIRCOL <-
      c(
        WT = "gray",
        VIR = "#CCB1E4",
        ESBL = "#E9A7A9",
        "ESBL + VIR" = "#9A85C9",
        CARBA = "#C0677D" ,
        "CARBA + VIR" = "#615A9A",
        "CARBA + COL" = "#602643",
        "CARBA + COL + VIR" = "#533E84"
      )



  
  GROUPCOLORS <-
    c(
      Susceptible = "#cdd7d6",
      Bla =  "#1EA0AE" ,
      ESBL = "#084c61",
      Omp = "#ffc857" ,
      Carb = "#db3a34"
    )
  

  
    ORIGINCOL <-
      c(
        NLSAR = "#dc3977",
        Spain = "#7c1d6f",
        Europe = "#f0746e",
        Asia = "#fcde9c",
        Americas = "#7ccba2" ,
        Africa = "#02764b" ,
        Oceania = "#ffa500" ,
        Unknown = "lightgray"
      )
    
    ORIGINCOL <-
      c(
        NLSAR = "#dc3977",
        Spain = "#7c1d6f",
        Europe = "#825197",
        Asia = "#7ccba2" ,
        Americas = "#045275" ,
        Africa =  "#ffa500" ,
        Oceania = "#fcde9c" ,
        Unknown = "lightgray"
      )
    
    ORIGINCOL <-
      c(
        NLSAR = "#dc3977",
        Spain = "#7c1d6f",
        Europe = "#825197",
        Asia = "#7ccba2",
        Americas = "#fcde9c" ,
        Africa = "#ffa500",
        Oceania = "#02764b" ,
        Unknown = "lightgray"
      )
    
    
    source <-
      c(
        Human = "#F7464A",
        Animal = "#FDB45C",
        Environment = "#46BFBD",
        Food = "#8397C5" ,
        unknown = "lightgray"
      )
    
    GROUPCOLORS <-
      c(
        Susceptible = "#cdd7d6",
        Bla =  "#1EA0AE" ,
        ESBL = "#084c61",
        Omp = "#ffc857" ,
        Carb = "#db3a34"
      )
    

  ## order ####
    
    STORDERv1_2 <-
      c(
        "Other",
        "ST512",
        "ST45",
        "ST392",
        "ST17",
        "ST29",
        "ST219",
        "ST101",
        "ST405",
        "ST147",
        "ST15",
        "ST258",
        "ST11",
        "ST307"
      )
    STORDERv1 <-
      c(
        "ST11",
        "ST258",
        "ST307",
        "ST15",
        "ST147",
        "ST512",
        "ST101",
        "ST16",
        "ST405",
        "ST23",
        "ST37",
        "ST45",
        "ST14",
        "ST392",
        "ST437",
        "ST231",
        "ST395",
        "ST17",
        "ST340",
        "ST219",
        "ST29",
        "ST34",
        "ST326",
        "ST86",
        "ST152",
        "ST25",
        "ST39",
        "ST48"
      )
source( "C:/Users/neris/Desktop/LABORATORIO/Articulo_diversity/DESING_SOURCE.R")

# START ####
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



RESTYPEORDER <- c("Susceptible", "Omp", "Bla", "ESBL", "Carb")
ORIGINORDER <-
  c("NLSAR",
    "Spain",
    "Europe",
"Americas" ,
    "Africa" ,    "Asia",
        "Oceania",
    "Unknown")
fORIGINORDER <- factor(ORIGIN, levels = ORIGINORDER)



# CONTEOS ####
  datos_total %>% count()
  datos_total %>% count(ORIGIN) %>% mutate(perct = n/9936)
  
  
  datos_total %>% filter(ORIGIN != "NLSAR")  %>% 
  count((Isolation_source))
        
  
    datos_total %>%
      filter(!is.na(Collection_year)) %>%
      filter((ORIGIN != "NLSAR")) %>%
      #count()
    
      mutate(
        Collection_year = case_when(
          Collection_year < 2010 ~ 2000,
          Collection_year >= 2010 ~ 2100,
          T ~ Collection_year
        )
      ) %>%
      count(Collection_year)
  
    
    datos_total %>%   
      filter((ORIGIN != "NLSAR")) %>%
    #filter(ORIGIN == "Europe")  %>% 
      count(Country)  %>%  arrange(desc(n))
    
   temp =  datos_total %>%   
     filter((ORIGIN != "NLSAR")) %>%
      filter(Country == "Spain")  

  base_datos %>% count(ORIGIN)

  base_datos %>% count(type2)

 # Gráficos espaciales ####

  ## mapa y pie chart isolation source ####

base_datos$Isolation_source <-
  factor(base_datos$Isolation_source, levels = levels(factor(base_datos$Isolation_source))[c(4, 5, 1, 2, 3)])

    ### Pie chart isolation source ####

#  pie <-

datapie <-     filter(base_datos, type2 == "BD") %>%
  group_by(Isolation_source) %>%
  summarise(n = n())  %>%
  arrange(desc(Isolation_source)) %>%
  mutate(prop = n / sum(n) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.8 * prop)

ggplot(datapie,  aes(x = "", y = prop, fill = Isolation_source)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  
  geom_text(aes(y = ypos, label = Isolation_source),
            color = "white",
            size = 6)

slices <- pie$n
lbls <- pie$Isolation_source
pct <- round(slices / sum(slices) * 100, 2)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls, "%", sep = "") # ad % to labels

colors <- c("#F7464A", "lightgray", "#FDB45C", "#46BFBD", "#8397C5")


pie(slices, labels = lbls, col = colors)





    ### Mapa del mundo coloreado por region ####

p_mapl <-  ggplot(map_data("world")) +
  geom_polygon(aes(x = long, y = lat, group = group),
               color = "gray80",
               fill = "gray80") +
  #  geom_point(data = filter(base_datos, !is.na(Continent), type2 == "BD") %>% count(Lat, Lon, Isolation_source), aes(x = Lon, y = Lat, color = Isolation_source, size = n)) +
  geom_point(data = base_datos, aes(x = Lon, y = Lat, color = ORIGIN)) +
  # scale_color_manual(values = source) +
  scale_color_manual(values = ORIGINCOL) +
  #scale_fill_manual(values = source) +
  labs(x = "", y = "", color = "Isolation source") +
  theme_void() +        theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    legend.box = "vertical",
    legend.position = "bottom"
  )

p_mapl

l_pmap <- get_legend(p_mapl)

p_map <- p_mapl + theme(legend.position = "none")

p_map

#

  ## mapa españa type 2 ####

#mapa <- rgdal::readOGR("/home/jordi/practicas/redMIVA/bases_de_datos/lineas_limite/SHP_ETRS89/recintos_provinciales_inspire_peninbal_etrs89/recintos_provinciales_inspire_peninbal_etrs89.shp")
# mapa de españa por provincias

mapa_df <- fortify(model = mapa, region = "NATCODE")

info_provincias <- mapa@data
info_provincias %>% head()

info_provincias <- info_provincias %>%
  mutate(
    pais       = stringr::str_sub(
      string = NATCODE,
      start = 1,
      end = 2
    ),
    c_autonoma = stringr::str_sub(
      string = NATCODE,
      start = 3,
      end = 4
    ),
    provincia  = stringr::str_sub(
      string = NATCODE,
      start = 5,
      end = 6
    ),
    municipio  = stringr::str_sub(
      string = NATCODE,
      start = 7,
      end = -1
    )
  ) %>%
  rename(nombre_provincia = NAMEUNIT) %>%
  select(NATCODE, nombre_provincia, c_autonoma, provincia, municipio)

mapa_df <- mapa_df %>%
  left_join(info_provincias, by = c("id" = "NATCODE"))

points <-
  filter(base_datos,!is.na(Lon), Country == "Spain", Lat < 46)

# Con la libreria sf encuentro en que poligono esta cada punto para poder agrupar por provincias
#https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package

#mapa_sf <- read_sf("/home/jordi/practicas/redMIVA/bases_de_datos/lineas_limite/SHP_ETRS89/recintos_provinciales_inspire_peninbal_etrs89/recintos_provinciales_inspire_peninbal_etrs89.shp")
# Mapa de españa por provincias en formato con sf
mapa_sf <- mapa_sf %>%
  left_join(info_provincias, by = c("NATCODE"))

points_sf <-
  st_as_sf(
    filter(base_datos,!is.na(Lon), Country == "Spain", Lat < 46),
    coords = c("Lon", "Lat"),
    crs = st_crs(mapa_sf)
  )

province_list <- st_intersects(points_sf, mapa_sf)

points$provincia <-
  as.character(mapa_sf$provincia[unlist(province_list)])

#errors <- filter(points, type2 == "NLSAR", provincia == 28 )

#write_csv(errors, file = "/home/jordi/practicas/analisis_muestras_neris/bases_datos/errores_Lat_Lon_NLSAR.csv")

points <-
  mutate(
    points,
    provincia = case_when(
      City == "Valencia" ~ "46",
      City == "Castellon" ~ "12",
      City == "Alicante" ~ "03",
      City == " Valencia" ~ "46",
      City == " Castellon" ~ "12",
      City == " Alicante" ~ "03",
      T ~ provincia
    )
  )
points <- points %>%
  group_by(provincia, type2) %>%
  summarise(n = n())


# data frame con relación codigo de provincia ~ provincia
#https://public.opendatasoft.com/explore/dataset/provincias-espanolas/table/?sort=provincia

# codigo_provincias <- read_delim("/Users/sevillanito/Downloads/provincias-espanolas (1).csv", delim = ";") %>%
#   select("Geo Point", Codigo) %>%
#   separate(col = "Geo Point", into = c("Lat", "Lon"), sep = ",")  # dataframe con los códigos y las lat-lon de cada provincia

points <-
  left_join(points, codigo_provincias, by = c("provincia" = "Codigo"))


mapa_df %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "gray90") +
  geom_point(
    data = points,
    aes(
      x = as.numeric(Lon),
      y = as.numeric(Lat),
      color = type2,
      size = n
    ),
    inherit.aes = F,
    alpha = 0.7
  ) +
  scale_size_continuous(range = c(1, 15)) +
  coord_map("mercator") +
  scale_color_manual(values = TYPECOL) +
  labs(x = "",
       y = "",
       color = "Type",
       size = "Nº of cases") +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )

  ## Figura pais ~ año ####
    ### version 1 ####
time <- base_datos %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  group_by(Collection_year, ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = factor(ORIGIN, rev(names(ORIGINCOL)))) +
  geom_point(aes(colour = ORIGIN, size = n), alpha = 0.9) +
  scale_colour_manual(values = ORIGINCOL) +
  scale_size_continuous(range = c(0.1, 25)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(
    x = "Collection year",
    colour = "Origin",
    y = "",
    size = "Nº of cases"
  )
time
counts <- base_datos %>%
  filter(!is.na(Collection_year)) %>%
  group_by(ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = n,
      y = factor(ORIGIN, rev(names(ORIGINCOL))),
      fill = ORIGIN) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n, x = n + 300), size = 5) +
  scale_fill_manual(values = ORIGINCOL, guide = "none") +
  labs(y = "", x = "Nº of cases") +
  theme(axis.text.y = element_blank()) +
  xlim(c(0, 4000))
counts
ggarrange(
  time,
  counts,
  common.legend = F,
  align = "hv",
  legend.grob = get_legend(time),
  legend = "right",
  widths = c(3, 2)
)

    ### Versión 2 ####
histo_time <- base_datos %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  ggplot() +
  aes(x = Collection_year, fill = factor(ORIGIN, rev(names(ORIGINCOL)))) +
  geom_bar() +
  scale_fill_manual(values = ORIGINCOL) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(x = "Collection year", fill = "Origin", y = "Nº of cases")
histo_time

histo_origin <- base_datos %>%
  ggplot() +
  aes(fill = ORIGIN, x = factor(ORIGIN, rev(names(ORIGINCOL)))) +
  geom_bar() +
  scale_fill_manual(values = ORIGINCOL, guide = "none") +
  labs(x = "Origin", y = "")
histo_origin

ggarrange(
  histo_time,
  histo_origin,
  common.legend = T,
  align = "hv",
  legend = "right"
)

    ### Versión 3 NERIS ####



histo_time <- base_datos %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  ggplot() +
  aes(x = Collection_year, fill = factor(ORIGIN, rev(names(ORIGINCOL)))) +
  geom_bar() +
  scale_fill_manual(values = ORIGINCOL) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(x = "Collection year", fill = "Origin", y = "Nº of cases")  +
  geom_text(aes(label = n, y = n + 10)) #+
theme(
  legend.position = "none",
  axis.text = element_blank(),
  axis.title.x = element_blank()
)
histo_time

counts <- base_datos %>%
  #filter(!is.na(Collection_year)) %>%
  group_by(ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = n,
      y = factor(ORIGIN, rev(names(ORIGINCOL))),
      fill = ORIGIN) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n, x = n - 5), size = 3) +
  scale_fill_manual(values = ORIGINCOL, guide = "none") +
  labs(y = "", x = "Nº of cases") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(1000, 3000),
    limits = (c(0, 4100))
  ) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") + coord_cartesian(clip = "off")
counts

time <- base_datos %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  group_by(Collection_year, ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = factor(ORIGIN, rev(names(ORIGINCOL)))) +
  geom_point(aes(colour = ORIGIN, size = n), alpha = 0.9) +
  scale_colour_manual(values = ORIGINCOL) +
  scale_size(range = c(0, 9))  +      #scale_size_continuous(range = c(0.1,25)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(
    x = "Collection year",
    colour = "Origin",
    y = "",
    size = "Nº of cases"
  ) +
  theme(legend.position = "none")
time

a <- plot_grid(
  p_map,
  histo_time ,
  counts,
  rel_widths = c(2, 1.5, 5),
  nrow = 2,
  axis = "rl",
  align = "v"
)
a

pglobal <-
  ggarrange(
    p_map,
    # First row with line plot
    # Second row with box and dot plots
    ggarrange(
      time,
      counts,
      NULL,
      nrow = 1,
      labels = c("B", "C"),
      widths = c(0.6, 0.3, 0.06),
      align = "h",
      font.label = list(color = "black", size = 10)
    ),
    nrow = 2,
    labels = "A",
    # Label of the line plot,
    font.label = list(color = "black", size = 10),
    heights = c(1, 0.9)
    
  )

pglobal



#saveplot(filename = "CH4_globalmap", pglobal, size = "h")


  ## número de bioprojects por pais ####

base_datos %>%
  filter(!is.na(Bioproject)) %>%
  dplyr::distinct(Bioproject, .keep_all = T) %>%
  group_by(ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = factor(ORIGIN, names(ORIGINCOL)),
      y = n,
      fill = ORIGIN) +
  geom_col() +
  scale_fill_manual(values = ORIGINCOL, guide = "none") +
  labs(x = "Origin", y = "Nº of bioprojects")


sophiabpj <-
  read.csv(
    "/home/neris/Laboratorio/0_Klebsiella/1_Analisis/7_ALLIN_DEF/0_bbdd_genomes/sophia_david_biosample_result.txt",
    header = F
  ) %>%
  mutate(Bioproject = "PRJEB10018")

left_join(base_datos, sophiabpj, by = c("Biosample" = "V1")) %>%
  mutate(Bioproject = coalesce(Bioproject.x, Bioproject.y)) %>%
  count(ORIGIN, Bioproject) %>% arrange(desc(n))


  ## distribución tamaño de bioprojects pais ####

bioproject <- base_datos %>%
  filter(!is.na(Bioproject)) %>%
  group_by(ORIGIN, Bioproject) %>%
  dplyr::summarise(n = n())

bioproject$n <-
  cut(bioproject$n, breaks = c(0, 5, 10, 100, 200, 500, 2000))

bioproject %>%
  group_by(n, ORIGIN) %>%
  summarise(n_2 = n()) %>%
  ggplot() +
  aes(x = n, y = n_2 , fill = n) +
  geom_col() +
  geom_text(aes(label = n_2, y = n_2 + 50)) +
  facet_wrap(vars(ORIGIN)) +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(
          angle = 60,
          vjust = 1,
          hjust = 1
        )) +
  scale_fill_discrete(guide = "none") +
  labs(x = "Nº of cases", y = "Nº of Bioprojects")


# ST  ####
  ## ST by ORIGIN ####

    selected_STs_2 <-
      datos_total %>%
      count(ST.1) %>%
      slice_max(order_by = n,
                n = 20,
                with_ties = T) %>%
      arrange(desc(n)) %>%
      pull(ST.1)

    
    library(forcats)
    p1_ST_ORIGIN <-
      datos_total %>%
      group_by(ST.1, ORIGIN) %>%
      dplyr::summarise(n = n()) %>%
      ggplot() +
      aes(
        x = n,
        y = forcats::fct_explicit_na(factor(ST.1, levels = rev(selected_STs_2)), "Other"),
        fill = factor(ORIGIN, levels = rev(ORIGINORDER))
      ) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = ORIGINCOL) +
      labs(y = NULL, x = "Genomes", fill = "Origin")+
      theme(legend.position = "bottom") 
    p1_ST_ORIGIN
    #saveplot("Originlegend", p1_ST_ORIGIN, "w")
    

    ## St por base datos ####

table(datos_total$type2, datos_total$ST_1) %>%
  as.data.frame() %>%
  mutate(Freq = case_when(Var1 == "BD" ~ -Freq,
                          T ~ Freq)) %>%
  ggplot(aes(
    x = Freq,
    fill = Var2,
    y = factor(Var2,  STORDERv1_2)
  )) +
  geom_bar(stat = "identity",  position = "identity")  +
  scale_fill_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING]) +
  labs(y = "ST", x = "Isolates") +
  scale_y_discrete(position = "left") +
  geom_vline(xintercept = 0) +
  scale_x_break(c(-4800,-1700), expand = F) +
  scale_x_continuous(
    breaks = c(-5000, -1000, -500, 0, 500),
    labels = c("5000", "1000", "500", "0", "500"),
    position = "bottom"
  ) +
  guides(fill = guide_legend(title = "ST")) +
  theme(
    axis.line.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

datos_total %>% count(ORIGIN, ST_1) %>% filter(ST_1 == "ST307")
datos_total %>% count(ST) %>% arrange(desc(n)) %>% filter(n == 1)
datos_total %>% count(ST) %>% filter(!str_detect(ST, "LV")) %>% arrange(desc(n)) %>% filter(n ==
                                                                                              1)
datos_total %>% count(ST) %>% filter(str_detect(ST, "LV")) %>% arrange(desc(n)) %>% filter(n ==
                                                                                             1)
gt(
  datos_total %>% count(ST.1) %>% arrange(desc(n)) %>% slice_max(
    order_by = n,
    n = 10,
    with_ties = T
  )
)



ggplot(datos_total) +
  aes(x = ST.1) +
  geom_bar(aes(fill = ORIGIN)) +
  scale_fill_manual(values = ORIGINCOL)

temp <- datos_total %>%
  count(ORIGIN, ST)



#counts <-
datos_total %>%
  filter(!is.na(Collection_year)) %>%
  count(ST_2, ORIGIN) %>%
  ggplot() +
  aes(
    y = n,
    x = reorder(ST_2,-n, sum),
    fill = factor(ORIGIN, levels = rev(ORIGINORDER))
  ) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = ORIGINCOL, limits = ORIGINORDER) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  labs(x = "Lineages", y = "Genomes") +
  theme(legend.position = "none")

#counts <-
datos_total %>%
  filter(!is.na(Collection_year)) %>%
  count(ST_2, ORIGIN) %>%
  ggplot() +
  aes(y = n,
      x = reorder(ST_2,-n, sum),
      fill = ST_2) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = STCOLORS) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.6)) +
  labs(x = "Lineages", y = "Genomes") +
  theme(legend.position = "none")

      ### ggvenn ####
install.packages("ggVennDiagram")                # Install & load ggvenn
library("ggvenn")

library(ggVennDiagram)

temp <-
  datos_total %>%
  select(ORIGIN, ST.1) %>% distinct() %>% mutate(val = TRUE) %>%
  pivot_wider(names_from = "ORIGIN", values_from = "val") %>%
  mutate(across(-ST.1,  ~ replace_na(.x, FALSE)))

ggplot(temp) +
  geom_venn(aes(A = `NLSAR`, B = `Spain`,  C = `Europe`)) #, D =`Asia` )) #`Americas`, E=, F= `Oceania`, G=`Africa`))

ggplot(temp) +
  geom_venn(aes(A = `NLSAR`, B = `Spain`,  C = `Europe`)) #, D =`Asia` )) #`Americas`, E=, F= `Oceania`, G=`Africa`))


ggVennDiagram(x, label = "none", edge_size = 2)


#

      ### Figura ST ~ Año ####
      ### Versión 1 ####
time <- datos_total %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  group_by(Collection_year, ST_2) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = ST_2) +
  geom_point(aes(colour = ST_2, size = n), alpha = 0.9) +
  scale_colour_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING_2], guide = "none") +
  scale_size_continuous(range = c(0.1, 25)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(
    x = "Collection year",
    colour = "ST",
    y = "",
    size = "Nº of cases"
  )

counts <- datos_total %>%
  filter(!is.na(Collection_year)) %>%
  group_by(ST_2, ORIGIN) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = n, y = ST_2, fill = ORIGIN) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = ORIGINCOL) +
  labs(y = "", x = "Nº of cases", fill = "Origin") +
  theme(axis.text.y = element_blank())

ggarrange(
  time,
  counts,
  common.legend = F,
  legend = "bottom",
  align = "hv",
  widths = c(3, 2)
)

      ### Versión 2 ####
      time <- datos_total %>%
        filter(!is.na(Collection_year)) %>%
        mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                           T ~ Collection_year)) %>%
        group_by(Collection_year, ST_2) %>%
        dplyr::summarise(n = n()) %>%
        ggplot() +
        aes(x = Collection_year, y = ST_2) +
        geom_point(aes(colour = ST_2, size = n), alpha = 0.9) +
        scale_colour_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING_2], guide = "none") +
        scale_size_continuous(range = c(0.1, 25), guide = "none") +
        scale_x_continuous(
          breaks = c(2000, 2005, 2010, 2015, 2020),
          labels = c("1900-2000", "2005", "2010", "2015", "2020")
        ) +
        labs(
          x = "Collection year",
          colour = "ST",
          y = "",
          size = "Nº of cases"
        )
      
      counts <- datos_total %>%
        filter(!is.na(Collection_year)) %>%
        group_by(ST_2, ORIGIN) %>%
        dplyr::summarise(n = n()) %>%
        ggplot() +
        aes(x = n, y = ST_2, fill = ST_2) +
        geom_col(width = 0.7) +
        scale_fill_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING_2], guide = "none") +
        labs(y = "", x = "Nº of cases", fill = "Origin")  +
        facet_wrap(vars(ORIGIN), ncol = 4)
      
      ggarrange(
        time,
        counts,
        ncol = 1,
        common.legend = F,
        legend = "bottom",
        align = "hv",
        heights = c(1, 1.7)
      )
      
      ### donut #####
      datos_total %>%
        count(ST_2, ORIGIN) %>%
        ggplot() +
        aes(y = n,
            x = reorder(ST_2, n, sum),
            fill = ST_2) +
        geom_col(width = 0.7) +
        scale_fill_manual(values = STCOLORS) +
        theme(legend.position = "none") +
        coord_polar(theta = "y") +
        scale_y_continuous(limits = c(0, 1550)) +
        facet_wrap( ~ ORIGIN, nrow = 2) +
        theme(
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_blank()
        ) #,
      legend.position = "none")
      
      
      ### donut  first 10  of each origin #####
      temp <- datos_total %>%
        group_by(ORIGIN) %>% count(ST.1) %>%
        slice_max(order_by = n,
                  n = 10,
                  with_ties = T) %>% filter(n > 5)
      
      unique(temp$ST.1)
      gt(temp %>% group_by(ST.1) %>% summarise(sum(n)) %>% arrange(desc(`sum(n)`)))
      
      ggplot(temp) +
        aes(y = n,
            x = reorder(ST.1, n, sum),
            fill = ST.1) +
        geom_col(width = 0.7) +
        scale_fill_manual(values = STCOLORS) +
        theme(legend.position = "none") +
        coord_polar(theta = "y") +
        #scale_y_continuous(limits = c(0,1550), breaks = c(500, 1000, 1500)) +
        facet_wrap( ~ ORIGIN, nrow = 2)  +
        #theme_bw()+
        theme(
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_blank() ,
          legend.position = "none"
        )
      
      selected_STs_5 <-
        datos_total %>%
        filter(ORIGIN != "Unknown") %>%
        group_by(ORIGIN) %>% count(ST.1) %>%
        slice_max(order_by = n,
                  n = 5,
                  with_ties = T) %>%
        filter(n > 5) %>%  arrange(desc(n)) %>%
        pull(ST.1)
      
      ### gt table ####
     custom_gt_table =
      datos_total %>%
        rename(Region = ORIGIN) %>%
        group_by(Region) %>%
        mutate("Genomes" = n()) %>%
        group_by(Region, ST.1) %>%
        mutate(n = n()) %>%
        select(Region, ST.1, "Genomes" , n) %>% distinct() %>%
        group_by(Region) %>%
        mutate(prop = round(n / sum(n) * 100, digits = 2)) %>%
        filter(ST.1 %in% selected_STs_5) %>%
        mutate("Other" =  100 - sum(prop)) %>%
        pivot_wider(id_cols = -n, names_from = ST.1, values_from = prop) %>%
        mutate_if(is.numeric, ~ replace_na(., 0)) %>%
        arrange(factor(Region, levels = ORIGINORDER)) %>%
        select("Genomes", c(selected_STs_5), "Other")
      
      custom_gt_table  %>%
        as.data.frame()    %>%           #head() %>%
        gt() %>%
        gt_color_rows(ST11:ST86,
                      domain = c(0, 42),
                      palette = c("white", "#9a170d"))  %>%
        gt_color_rows(
          Other,
          domain = c(20, 80),
          palette = c("white", "#045275"),
          direction = 1
        )  %>%
        
        opt_table_font(font = google_font(name = "Roboto")) #%>%
        #gtsave(., paste0(figpath, "CH4_gt_5STs.docx"))
      #%>%
      row_group_order(ORIGINORDER) #%>%
      gt_plt_bar_pct(column = total, scaled = TRUE)
      
      
    ### gt table to circle hisotgram ####
        
      custom_gt_table %>%
      pivot_longer(!Region, names_to = "ST.1", values_to = "prop") %>%
        filter(ST.1 != "Genomes")  %>%
      filter(Region != "Unknown")  %>%
        mutate(prop_category = cut(prop, 
                                   breaks = c(-Inf, 2, 10, 25, 100), 
                                   labels = c("0-2", "2-10", "10-25", "25-100")))  %>%
        
      ggplot( aes(x = factor(Region, levels = (ORIGINORDER)),
                 y = factor(ST.1, levels= rev(STORDER_ALL2)))) + 
        #geom_point(aes(colour = prop, size = prop)) + 
        geom_point(aes(colour = prop_category), size =10) + 
        labs(x=NULL, y = NULL) +
        geom_text(aes(label = round(prop, 1 )), color = "white", size = 3) +  # Add text labels
        scale_colour_manual(
          values = c("#cdd7d6", "#1EA0AE", "#084c61", "#db3a34"),
          guide = "legend"
        ) +
        scale_x_discrete(position = "top")  +
        theme_bw() +
        theme(legend.position = "bottom")
      
        
      
      
      # Europe
      gteurope <-   datos_total %>%
        filter(Continent == "Europe" & ORIGIN != "NLSAR") %>%
        group_by(Country) %>%
        mutate(country_N = n()) %>%
        filter(country_N > 50) %>%
        group_by(Country, ST.1) %>%
        mutate(n = n()) %>%
        select(Country, ST.1, country_N, n) %>% distinct() %>%
        group_by(Country) %>%
        mutate(prop = round(n / sum(n) * 100, digits = 2)) %>%
        filter(ST.1 %in% selected_STs_5) %>%
        mutate(totalsum =  sum(prop)) %>%
        pivot_wider(-n, names_from = ST.1, values_from = prop) %>%
        mutate_if(is.numeric, ~ replace_na(., 0)) %>% arrange(desc(country_N)) %>%
        as.data.frame() %>%
        #head() %>%
        gt() %>%
        gt_color_rows(ST11:ST86, domain = c(0, 60))  %>%
        opt_table_font(font = google_font(name = "Roboto"))
      
        #gtsave(gteurope, "CH4_gt_europe.tex")
      
      
      unique(selected_STs_5)
      
      datapie <-   datos_total %>%
        group_by(ORIGIN) %>% count(ST.1) %>%
        mutate(prop = n / sum(n) * 100) %>%
        filter(ST.1 %in% selected_STs_5)
      
      gt(datapie)
      unique(datapie$ST.1)
      
      gt(datapie %>% group_by(ST.1) %>% summarise(total = sum(n)) %>% arrange(desc(total)))
      
      #ggplot(datapie,  aes(x="", y=prop, fill=ST.1)) +
      ggplot(datapie,  aes(
        x = 2,
        y = prop,
        fill = factor(ST.1, levels = rev(STORDERv1))
      )) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        facet_wrap( ~ factor(ORIGIN, levels = ORIGINORDER), nrow = 2) +
        scale_fill_manual(values = STCOLORS, limits = (STORDERv1)) +
        scale_color_identity() +
        xlim(.2, 2.5) +
        theme(legend.position = "none")
      
      
      ggplot(datapie) +
        aes(x = factor(ST.1, levels = rev(STORDERv1)),
            y = prop,
            fill = ST.1) +
        geom_col(width = 1) +
        scale_fill_manual(values = STCOLORS, limits = rev(STORDERv1)) +
        theme(legend.position = "none") +
        coord_polar(theta = "y") +
        scale_y_continuous(limits = c(0, 50), breaks = c(25, 50)) +
        facet_wrap( ~ factor(ORIGIN, levels = ORIGINORDER), nrow = 2)  +
        #theme_void()+
        theme(
          panel.grid.major.x = element_blank() ,
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_blank() ,
          legend.position = "none"
        )
      
  ## rarefaction curves #####
      library(vegan)
      data(package = "vegan") ## names of data sets in the package
      data(dune) # Vegetation and Environment in Dutch Dune Meadows
      str(dune) #a data frame of observations of 30 species at 20 sites
      
      dune2 <-
        datos_total %>%
        count(ORIGIN, ST.1) %>%
        pivot_wider(names_from = ST.1, values_from = n) %>%
        mutate_if(is.numeric, ~ replace_na(., 0)) %>%
        mutate_if(is.integer, as.numeric) %>%
        select(-ORIGIN) %>% as.data.frame()
      
      dune2 <-
        datos_total %>%
        count(ORIGIN, ST.1) %>%
        pivot_wider(names_from = ST.1, values_from = n) %>%
        mutate_if(is.numeric, ~ replace_na(., 0)) %>%
        mutate_if(is.integer, as.numeric) %>%
        remove_rownames %>% column_to_rownames(var = "ORIGIN") %>% as.data.frame()
      
      datos_total %>%
        count(ORIGIN, ST.1) %>%
        pivot_wider(names_from = ST.1, values_from = n) %>%
        mutate_if(is.numeric, ~ replace_na(., 0)) %>%
        mutate_if(is.integer, as.numeric) %>%
        pull(ORIGIN)
      
      str(dune2)
      
      dfD <-
        vegan::diversity(dune2, index = "simpson") %>% as.data.frame() %>% tibble::rownames_to_column("ORIGIN") %>% rename(Div = ".") %>%
        mutate(text = paste0(ORIGIN, " (", as.character(round(Div, 2)), ")"))
      
      colnames(dfD)
      spAbund <- rowSums(dune2)
      raremin <- min(rowSums(dune2))
      sRare <- rarefy(dune2, raremin)
      rcdf <- rarecurve(dune2,  label = T, tidy = T)
      
      data_ends <-
        rcdf %>% group_by(Site) %>% slice_max(order_by = c(Sample) , n = 1)   %>% full_join(., dfD, by =
                                                                                              c("Site" = "ORIGIN"))
      
      rfcurves <-
        ggplot(filter(rcdf, Site != "Unknown")) +
        geom_line(aes(x = Sample, y = Species, color = Site),
                  linewidth = 1.5,
                  lineend = "round") +
        geom_line(
          data = filter(rcdf,  Site != "Unknown" &
                          Site == "Africa"),
          aes(x = Sample, y = Species, color = Site),
          size = 1.5,
          lineend = "round"
        ) +
        geom_line(
          data = filter(rcdf,  Site != "Unknown" &
                          Site == "Oceania"),
          aes(x = Sample, y = Species, color = Site),
          size = 1.5,
          lineend = "round"
        ) +
        
        scale_color_manual(values = ORIGINCOL)  +
        scale_fill_manual(values = alpha(ORIGINCOL, 0.8))  +
        geom_label_repel(
          data = filter(data_ends,  Site != "Unknown"),
          aes(
            label = text,
            x = Sample,
            fill = (Site),
            y = Species ,
            family = 'Roboto'
          ),
          size = 3,
          #nudge_x = 10,
          #min.segment.length = 1, 
          label.size = 0, 
          label.r = 0.4, 
          #direction = c("x"), 
          xlim = c(500, 4000)
        ) +
        scale_x_continuous(limits = c(0, 4500), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 450), expand = c(0, 0)) +
        theme(legend.position = "none") +
        ylab("Sequence types") + xlab("Genomes")
      
      
      rfcurves
      
      plotxsad <- ggarrange(
        rfcurves,
        p1_ST_ORIGIN,
        labels = c("A", "B"),
        font.label = list(color = "black", size = 10)
      )
      plotxsad
      #saveplot(filename = "CH4_rarefactioncures_ST", plotxsad, size = 85)
      
      
  
  ## mash ####
    ### Mash distance summaries ####

# Calculate distance summaries between samples
# Eliminar con un sed la ruta de los nombres y luego leer la matriz de ditancias


#Esta parte es mejor hacerla en el super

num_cols <-
  read.csv(
    "/home/neris/Laboratorio/0_Klebsiella/1_Analisis/7_ALLIN_DEF/1_ISOLATE_FILTER/5_ALL_042022/mash/mash_dist_13134_edited.txt",
    nrows = 1,
    header = FALSE,
    sep = "\t"
  )
num_cols = 1 + as.integer(num_cols[, 2])
data = as.matrix(
  read.table(
    "/home/neris/Laboratorio/0_Klebsiella/1_Analisis/7_ALLIN_DEF/1_ISOLATE_FILTER/5_ALL_042022/mash/mash_dist_13134_edited.txt",
    fill = T,
    col.names = rep('', num_cols),
    skip = 1,
    row.names = 1
  )
)
makeSymm <- function(m) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  return(m)
}
datasym = makeSymm(data)
colnames(datasym) <- rownames(datasym)
#Hacer el summary
df1_summary <- as.data.frame(apply(datasym, 2, summary))
#     write.csv(t(df1_summary), "mash_dist2_summary.csv")


  ## mapa del mundo  por ST  ####
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

ggplot(map_data("world")) +
  geom_polygon(aes(x = long, y = lat, group = group),
               color = "grey30",
               fill = "gray99") +
  geom_point(
    data = filter(datos_total,!is.na(Continent)),
    aes(x = Lon, y = Lat, color = ST_1),
    size = 1.5
  ) +
  scale_color_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING]) +
  labs(x = "", y = "", color = "ST") +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank()
  )

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"), ]
Europe_countrynames <-
  Europe %>% distinct(name_en) %>% pull(name_en)
temp <-
  filter(datos_total,!is.na(Continent)) %>% group_by(ST.1, Lat, Lon) %>% count()

ggplot(Europe) +
  geom_sf() +
  coord_sf(xlim = c(-25, 50),
           ylim = c(35, 70),
           expand = FALSE) +
  geom_point(data = temp, aes(
    x = Lon,
    y = Lat,
    color = ST.1,
    size = n
  )) +
  scale_color_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING])




    ### Europe map and pie charts ####
    library(maps)
    world_df <- map_data('world')
    
    world_Europe_countrynames <- world_df %>%
      mutate(
        region = case_when(
          region == "UK" ~ "United Kingdom",
          region == "North Macedonia" ~ "Republic of Macedonia",
          TRUE ~ region
        )
      ) %>%
      filter(region %in% Europe_countrynames) #%>%
    select(long, lat, region) %>%
      distinct() #%>% pull(region)
    
    anti_join(Europe_countrynames,
              world_Europe_countrynames,
              by = c("name_en" = "region"))
    
    #Comprobar que estan todos
    datos_total %>% mutate(Country = case_when(Country == "Macedonia" ~ "Republic of Macedonia",
                                               TRUE ~ Country)) %>%
      filter(Continent == "Europe" &
               !(Country %in% world_Europe_countrynames)) %>%
      distinct(Country)
    
    datos_total2 = datos_total %>% mutate(Country = case_when(Country == "Macedonia" ~ "Republic of Macedonia",
                                                              TRUE ~ Country)) %>%
      filter(Continent == "Europe")
    
    # merge
    
    temp <-
      left_join(datos_total2,
                world_Europe_countrynames,
                by = c("Country" = "region"))
    
    p <- ggplot(Europe) +
      geom_sf() +
      coord_sf(xlim = c(-25, 50),
               ylim = c(35, 70),
               expand = FALSE) +
      stat_midpoint(
        data = temp,
        aes(
          label = Country,
          group = Country,
          x = long,
          y = lat
        ),
        geom = "text",
        colour = "black"
      )
    
    temp2 <-
      ggplot_build(p)$data[[2]] %>% select(label, x, y) %>%
      right_join(., datos_total2, by = c("label" = "Country")) %>% group_by(ST.1, x, y) %>% count()
    
    
    ggplot(Europe) +
      geom_sf() +
      coord_sf(xlim = c(-25, 50),
               ylim = c(35, 70),
               expand = FALSE) +
      geom_point(data = temp2, aes(
        x = x,
        y = y,
        color = ST.1,
        size = n
      )) +
      scale_color_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING])
    
    
    
    
    
    
    
    
    ### gt table europe ST11 ####
    datos_total %>%
      filter(!is.na(Continent) &
               ORIGIN == "Europe" & ST.1 == "ST11") %>%
      group_by(ST.1) %>% count(Country) %>%
      mutate(prop = round(n / sum(n) * 100, digits = 2)) %>%
      pivot_wider(-n, names_from = ST.1, values_from = prop) %>%
      mutate_if(is.numeric, ~ replace_na(., 0)) %>%
      as.data.frame() %>%
      #head() %>%
      gt() %>%
      gt_color_rows(ST11, domain = c(0, 100))  %>%
      opt_table_font(font = google_font(name = "Roboto")) #%>%
    row_group_order(ORIGIN, groups = ORIGINORDER) #%>%
    gt_plt_bar_pct(column = total, scaled = TRUE)
    
    
  ## ST CG258 ####
CG258STs <- c(
  "ST1084",
  "ST11",
  "ST1199",
  "ST258",
  "ST340",
  "ST379",
  "ST395",
  "ST418",
  "ST437",
  "ST512",
  "ST833",
  "ST855"
)

temp <- datos_total %>% filter(ST.1 %in% CG258STs)
rm(temp)

# ARG ####
  ## Total genes ####
    ### Densidad ####
    ggplot(datos_total) +
      aes(x = num_resistance_genes, y = ..scaled..) +
      geom_density(aes(fill = type2), alpha = 0.5) +
      scale_fill_manual(values = TYPECOL) +
      labs(x = "Number of resistance genes")
    
    ### Barras ####
    ggplot(datos_total) +
      aes(x = num_resistance_genes,) +
      geom_bar(aes(fill = type2)) +
      scale_fill_manual(values = TYPECOL) +
      labs(x = "Number of resistance genes")
    
    ### Neris ####
    ggplot(datos_total) +
      aes(x = num_resistance_genes) +
      geom_bar(aes(fill = ORIGIN)) +
      scale_fill_manual(values = ORIGINCOL) +
      labs(x = "Number of resistance genes")
    
    datos_total %>%
      mutate(ranges = cut(num_resistance_genes, seq(0, 60, 3)))  %>%
      count(ranges) %>%
      mutate(prop = round(n / sum(n) * 100, digits = 2))
    
    datos_total %>% count(Bla_Carb_acquired)   %>% arrange(desc(n))
    datos_total %>% count(Bla_ESBL_acquired)  %>% summarise(sum(n))
    datos_total %>% count(num_resistance_genes) %>% arrange((n))
    
    datos_total %>%  filter(num_resistance_genes > 3) %>%  summarise(mean(num_resistance_genes))
    temp <- datos_total %>%  filter(num_resistance_genes > 20)
    
    
  ## unique ARG pero sample ####
  
  nmax <- (max(datos_total$num_resistance_genes) + 1)
  
  uniqegenes2 <-
    datos_total %>%
    select(Nombre_archivo_neris, ORIGIN, AGly_acquired:Bla_chr) %>%
    unite(
      col = GENES,
      AGly_acquired:Bla_chr,
      sep = ";",
      remove = T
    ) %>%
    separate_rows(GENES, sep = ";") %>% distinct() %>% filter(GENES != "-")
  
  uniqegenes <- uniqegenes2 %>% count(Nombre_archivo_neris, ORIGIN)
  
  uniqegenes %>% mutate(ranges = cut(n, seq(0, 60, 5)))  %>%
    count(ranges) %>%
    mutate(prop = round(n / sum(n) * 100, digits = 2))
  
  uniqegenes %>%  filter(n > 5) %>%  summarise(mean(n))
  tempo <-
    uniqegenes2 %>% group_by(Nombre_archivo_neris) %>% filter(n() > 20)
  
  
  
  ggplot(uniqegenes) +
    aes(x = n, y = ..scaled..) +
    geom_density(aes(fill = ORIGIN), alpha = 0.5) +
    scale_fill_manual(values = TYPECOL) +
    labs(x = "Number of resistance genes")
  
  ggplot(uniqegenes) +
    aes(x = n,) +
    geom_bar(aes(fill = ORIGIN)) +
    scale_fill_manual(values = TYPECOL) +
    labs(x = "Number of resistance genes")
  
  ## AMR Finder plus ####
    ### Comparison kleborate AMR  Finder plus #####
      comand = "sed 's/\(GC._[0-9]*\.[0-9]\)\(_.*_genomic\)/\1/' AL_AMRfinderplus_cat.tsv > AL_AMRfinderplus_edited.tsv | sed 's/LONG_//'"
      
      file_Res <-
        read.csv(
          "C:/Users/neris/Desktop/LABORATORIO/Articulo_diversity/AL_AMRfinderplus_edited.tsv",
          sep = "\t",
          header = T
        ) %>%
        distinct() %>% filter(Scope  != "Scope") %>%
        mutate(Class = case_when(is.na(Class) ~ Element.type,
                                 TRUE ~ Class))       %>%
        mutate(Name = str_remove_all(Name, "_ass.*"))    %>%
        mutate( X..Coverage.of.reference.sequence = as.numeric(X..Coverage.of.reference.sequence),
                X..Identity.to.reference.sequence = as.numeric(X..Identity.to.reference.sequence)) %>%
        mutate(Gene.symbol2 = case_when(( X..Coverage.of.reference.sequence < 100 & X..Identity.to.reference.sequence < 100 ) ~ paste0(Gene.symbol, "*?"),
                                          X..Coverage.of.reference.sequence < 100 ~ paste0(Gene.symbol, "*"),
                                          X..Identity.to.reference.sequence < 100 ~ paste0(Gene.symbol, "?"),
                                          TRUE ~ Gene.symbol)) %>%
        filter(Element.type == "AMR") %>%
      select(Name, Subclass, Gene.symbol2) %>% pivot_wider(., names_from = Subclass, values_from = Gene.symbol2, values_fn  = ~paste(., collapse = "; ")) %>%
        right_join( ., datos_total,   by = c("Name" = "Nombre_archivo_neris"))
      
        
        tempo <- 
          file_Res %>% 
          select(Name, CARBAPENEM, Bla_Carb_acquired) %>%
          mutate(CARBAPENEM = str_remove(CARBAPENEM, "omp.*;?")) %>%
           mutate_all(na_if,"") %>%
          filter(Bla_Carb_acquired != "-") %>%
          separate_rows(Bla_Carb_acquired, sep =";") %>%
          distinct() %>%
            filter(!str_detect(`CARBAPENEM`, Bla_Carb_acquired))
         
        
        
    
      ### CONTEOS ####
      
      
          temp2 <-
            file_Res %>%
            select(Name, `BETA-LACTAM`) %>%
            group_by(Name) %>%
            summarize_all(paste, collapse = '; ') %>%
            mutate(`BETA-LACTAM` = str_remove_all(`BETA-LACTAM`, " ")) %>%
            separate_rows(`BETA-LACTAM`, sep = ";") %>% distinct() %>% count(`BETA-LACTAM`) %>% arrange(desc(n))
          
          temp2 %>% filter()
          
          temp = temp2 %>% group_by(Name, ORIGIN) %>% count() %>% ungroup()
          
          
          ggplot((temp)) +
            aes(x = n) +
            geom_bar(aes(fill = ORIGIN)) +
            labs(x = "Number of resistance genes") +
            scale_fill_manual(values = ORIGINCOL)
          
          tempo = temp2 %>% group_by(Name) %>%  filter(n() == 4) %>% ungroup () %>% count(Gene.symbol)
          
          temp2 %>% count(Name) %>%  filter(n > 4) %>% summarise(mean(n))
          
          
          temp %>%  select(n) %>% mutate(ranges = cut(n, seq(0, 60, 5)))  %>%
            count(ranges) %>%
            mutate(prop = round(n / sum(n) * 100, digits = 2))
          
      # MDR
        temp <- file_Res %>%
          filter(Element.type == "AMR" &
                   Element.subtype != "POINT" & Gene.symbol != "emrD") %>%
          select(Name, Class) %>% distinct() %>% count(Name)
        
        file_Res %>% filter(Element.type == "AMR" &
                              Element.subtype != "POINT" & Gene.symbol != "emrD") %>%
          filter(Subclass == "CARBA") %>% separate(
            Gene.symbol,
            sep = "-",
            into = c("Gene", "allele"),
            remove = F
          )  %>% group_by(type2) %>%
          count(Gene) %>% arrange(desc(n))
        
  
 
        
  
        
    ## ggaluvial ####
  library(ggalluvial)
  #https://corybrunson.github.io/ggalluvial/articles/order-rectangles.html
  
  selected_STs <-  datos_total %>%
    #group_by(ORIGIN) %>%
    count(ST.1) %>%
    slice_max(order_by = n,
              n = 7,
              with_ties = T) %>%
    filter(n > 5) %>%
    pull(ST.1)
  
  STORDER_ggalluvial <-
    c("ST11",  "ST307", "ST15", "ST147", "ST101", "ST512", "ST258")
  
  
  Carb_jitter <-
    datos_total %>%
    select(ORIGIN, ST.1, Bla_Carb_acquired) %>%
    separate_rows(Bla_Carb_acquired, sep = ";") %>%
    #filter(Bla_Carb_acquired != "-") %>%
    group_by(Bla_Carb_acquired) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    mutate(Bla_Carb_acquired = case_when(n >= 50 ~ Bla_Carb_acquired,
                                         T ~ "Other"))  %>%
    filter(ST.1 %in% selected_STs)
  
  
  iso_filt <-
    Carb_jitter %>% group_by(ST.1) %>% filter(n() >= 30) %>% ungroup() %>%
    select(ORIGIN, ST.1,  Bla_Carb_acquired)  %>%
    mutate_all(., ~ replace_na(., "unknown"))
  
  iso_alluvial <- iso_filt  %>%
    count(ORIGIN, ST.1, Bla_Carb_acquired) %>%
    rename(Freq = n)
  
  
  stratum1_levels  <- ORIGINORDER
  
  stratum2_levels  <- STORDER_ggalluvial
  
  stratum3_levels  <-
    iso_filt %>% group_by(ORIGIN, ST.1, K_locus) %>% count() %>%
    arrange(desc(n)) %>% as.data.frame() %>% select(K_locus) %>% distinct() %>% unlist(use.names = F)
  
  stratum4_levels  <-
    iso_filt %>% group_by(ORIGIN, ST.1, Bla_Carb_acquired) %>% count() %>%
    arrange(desc(n)) %>% as.data.frame() %>% select(Bla_Carb_acquired) %>% distinct() %>% unlist(use.names = F)
  
  iso_alluvial$ORIGIN <-
    factor(iso_alluvial$ORIGIN, levels = stratum1_levels)
  iso_alluvial$ST.1 <-
    factor(iso_alluvial$ST.1, levels = stratum2_levels)
  iso_alluvial$K_locus <-
    factor(iso_alluvial$K_locus, levels = stratum3_levels)
  iso_alluvial$Bla_Carb_acquired <-
    factor(iso_alluvial$Bla_Carb_acquired, levels = stratum4_levels)
  
  
  ggplot(iso_alluvial,
         aes(
           y = Freq,
           axis1 = ORIGIN,
           axis2 = ST.1,
           axis3 = Bla_Carb_acquired
         )) +
    #geom_lode() + geom_flow(aes(fill = ORIGIN), curve_type = "cubic") +
    geom_alluvium(aes(fill = ORIGIN), width = 1 / 12) +
    geom_stratum(width = 1 / 12) + #, fill = "black", color = "grey") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    #geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
    scale_fill_manual(values = ORIGINCOL) +
    theme_void()
  
  ggplot(iso_alluvial,
         aes(
           y = Freq,
           axis1 = ORIGIN,
           axis2 = ST.1,
           axis3 = Bla_Carb_acquired
         )) +
    geom_flow(aes(fill = ORIGIN), width = .4, curve_type = "quintic") +
    geom_stratum(width = .4) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3)   +
    scale_fill_manual(values = ORIGINCOL) +
    theme_void()
  
  
  
  # to order by size
  
  iso_filt <-
    isolates_CG_NLSAR %>% group_by(ST) %>% filter(n() >= 30) %>% ungroup()
  
  iso_alluvial <-
    iso_filt %>% select(Sublineage, Clonal.group, ST, K_locus) %>%
    mutate(subject = row_number()) %>% relocate(subject, .before = Sublineage) %>%
    pivot_longer(.,
                 cols = 2:5,
                 names_to = "collection",
                 values_to = "category")
  
  ggplot(iso_alluvial,
         aes(
           x = collection,
           stratum = category,
           alluvium = subject
         )) +
    geom_alluvium(aes(fill = category)) +
    stat_stratum(aes(fill = category), decreasing = TRUE) +
    stat_stratum(geom = "text", aes(label = category), decreasing = TRUE) +
    theme(
      legend.position = "none",
      text = element_text(size = 16,  family = "Roboto"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank()
    )
  
  
  
  
  
  
  ## jitter plots genes de resistencia ####
    ### Para Carba ####
    
    Carb_jitter <-
      select(datos_total, type2, ST_2, Bla_Carb_acquired) %>%
      separate_rows(Bla_Carb_acquired, sep = ";") %>%
      filter(Bla_Carb_acquired != "-") %>%
      group_by(Bla_Carb_acquired) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(Bla_Carb_acquired = case_when(n >= 45 ~ Bla_Carb_acquired,
                                           T ~ "Other"))
    
    
    Carb_jitter$Bla_Carb_acquired <-
      factor(Carb_jitter$Bla_Carb_acquired , levels = levels(factor(Carb_jitter$Bla_Carb_acquired))[c(7, 1, 11, 2, 3, 4, 5, 6, 8, 9, 10)])
    
    a <- ggplot(Carb_jitter) +
      aes(x = factor(ST_2, STGROUPING_2),
          y = Bla_Carb_acquired,
          color = type2) +
      geom_jitter(alpha = 0.3) +
      scale_color_manual(values = TYPECOL) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.6),
        panel.grid = element_blank(),
        legend.position = "none"
      ) +
      geom_vline(
        xintercept = seq(1.5, 15 - 0.5, 1),
        linewidth = 0.2,
        colour = "grey"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        linewidth = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    Carb_counts <- Carb_jitter %>%
      group_by(Bla_Carb_acquired, type2) %>%
      summarise(n = n())
    Carb_counts$type2 <-
      factor(Carb_counts$type2, levels = levels(factor(Carb_counts$type2))[c(2, 1)])
    
    b <- ggplot(Carb_counts) +
      aes(x = n, y = Bla_Carb_acquired, fill = type2) +
      geom_col() +
      scale_fill_manual(values = TYPECOL) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    plot_grid(
      a ,
      b,
      nrow = 1,
      axis = "tb",
      align = "h",
      labels = c("A", "B")
    )
    
    Carb_jitter 
    
    ### Para ESBL ####
    
    
    ESBL_jitter <-
      select(datos_total, type2, ST_2, Bla_ESBL_acquired) %>%
      separate_rows(Bla_ESBL_acquired, sep = ";") %>%
      filter(Bla_ESBL_acquired != "-") %>%
      group_by(Bla_ESBL_acquired) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(Bla_ESBL_acquired = case_when(n >= 20 ~ Bla_ESBL_acquired,
                                           T ~ "Other"))
    
    
    ESBL_jitter$Bla_ESBL_acquired <-
      factor(ESBL_jitter$Bla_ESBL_acquired, levels = levels(factor(ESBL_jitter$Bla_ESBL_acquired))[c(10, 17, 16, 15, 14, 13, 12, 11, 9, 8, 7, 6, 5, 4, 3, 2, 1)])
    
    a <- ggplot(ESBL_jitter) +
      aes(x = factor(ST_2, STGROUPING_2),
          y = Bla_ESBL_acquired,
          color = type2) +
      geom_jitter(alpha = 0.3) +
      scale_color_manual(values = TYPECOL) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.6),
        panel.grid = element_blank(),
        legend.position = "none"
      ) +
      geom_vline(
        xintercept = seq(1.5, 15 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    ESBL_counts <- ESBL_jitter %>%
      group_by(Bla_ESBL_acquired, type2) %>%
      summarise(n = n())
    
    ESBL_counts$type2 <-
      factor(ESBL_counts$type2, levels = levels(factor(ESBL_counts$type2))[c(2, 1)])
    
    b <- ggplot(ESBL_counts) +
      aes(x = n, y = Bla_ESBL_acquired, fill = type2) +
      geom_col() +
      scale_fill_manual(values = TYPECOL) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    plot_grid(
      a ,
      b,
      nrow = 1,
      axis = "tb",
      align = "h",
      labels = c("A", "B")
    )
    
    
    ### para Bla ####
    
    
    Bla_jitter <- select(datos_total, type2, ST_2, Bla_acquired) %>%
      separate_rows(Bla_acquired, sep = ";") %>%
      filter(Bla_acquired != "-") %>%
      group_by(Bla_acquired) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(Bla_acquired = case_when(n >= 20 ~ Bla_acquired,
                                      T ~ "Other"))
    
    
    Bla_jitter$Bla_acquired <-
      factor(Bla_jitter$Bla_acquired, levels = levels(factor(Bla_jitter$Bla_acquired))[c(8, 13, 1, 6, 7, 17, 16, 15, 14, 2, 3, 4, 5, 9, 10, 11, 12)])
    
    a <- ggplot(Bla_jitter) +
      aes(x = factor(ST_2, STGROUPING_2),
          y = Bla_acquired,
          color = type2) +
      geom_jitter(alpha = 0.3) +
      scale_color_manual(values = TYPECOL) +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.6),
        panel.grid = element_blank(),
        legend.position = "none"
      ) +
      geom_vline(
        xintercept = seq(1.5, 15 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    Bla_counts <- Bla_jitter %>%
      group_by(Bla_acquired, type2) %>%
      summarise(n = n())
    
    Bla_counts$type2 <-
      factor(Bla_counts$type2, levels = levels(factor(Bla_counts$type2))[c(2, 1)])
    
    b <- ggplot(Bla_counts) +
      aes(x = n, y = Bla_acquired, fill = type2) +
      geom_col() +
      scale_fill_manual(values = TYPECOL) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none"
      ) +
      geom_hline(
        yintercept = seq(1.5, 17 - 0.5, 1),
        lwd = 0.2,
        colour = "grey"
      ) + ylab(NULL) + xlab(NULL)
    
    plot_grid(
      a ,
      b,
      nrow = 1,
      axis = "tb",
      align = "h",
      labels = c("A", "B")
    )
    
    
  ## combinacióm carba + esbl ####

    combination <-
      select( datos_total,  Bla_Carb_acquired2, Bla_ESBL_acquired2, type2, ST.1,
        Nombre_archivo_neris, ORIGIN) %>% rename(ST_2 = ST.1)
    
    combination <-
      separate_rows(combination, Bla_Carb_acquired2, sep = ";")
    
    combination <-
      separate_rows(combination, Bla_ESBL_acquired2, sep = ";")
    #

    ### Version 1 ####
      combination_1 <- combination %>%
        unique() %>%
        filter(
          !is.na(Bla_Carb_acquired2),
          !is.na(Bla_ESBL_acquired2),
          Bla_Carb_acquired2 != "-",
          Bla_ESBL_acquired2 != "-"
        ) %>%
        unite(comb,
              Bla_Carb_acquired2,
              Bla_ESBL_acquired2,
              sep = " + ",
              remove = T)
      
      comb_order <- combination_1 %>%
        group_by(comb) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        arrange(n) %>%
        filter(n > 25) %>%
        pull(comb)
      
      counts <- combination_1 %>%
        group_by(comb) %>%
        filter(n() > 25) %>%
        ggplot() +
        aes(y = factor(comb, comb_order), fill = type2) +
        geom_bar() +
        scale_fill_manual(values = TYPECOL) +
        theme(
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom"
        ) +
        scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250),
                           limits = c(0, 1300))
      
      ST <-
        prop.table(table(combination_1$comb, combination_1$ST_2), 1) %>%
        as.data.frame() %>%
        filter(Var1 %in% comb_order) %>%
        ggplot() +
        aes(y = factor(Var1, comb_order),
            x = Freq,
            fill = Var2) +
        geom_col() +
        scale_fill_manual(values = STCOLORS[names(STCOLORS) %in% STGROUPING_2]) +
        scale_x_continuous(labels = percent) +
        theme(axis.title.x = element_blank(),
              legend.position = "bottom") +
        labs(y = "Combination ( Carbapenemase + ESBL)", fill = "ST")
      
      plot_grid(ST, counts,
        nrow = 1, axis = "tb", align = "h",
        labels = c("A", "B"), rel_widths = c(1, 0.7))
      
      
    ### Versión 2 ####
    
      combination_2 <- 
        combination %>%
        filter(!is.na(Bla_Carb_acquired2),  Bla_Carb_acquired2 != "-") %>%
        mutate(Bla_ESBL_acquired2 = str_replace_all(Bla_ESBL_acquired2, "SHV-12", "-")) %>%
        mutate(comb = case_when( Bla_ESBL_acquired2 == "-" |  is.na(Bla_ESBL_acquired2) ~ Bla_Carb_acquired2,
                                  T ~ paste(Bla_Carb_acquired2, Bla_ESBL_acquired2, sep = " + ")))

      
      com_2 <-
        combination_2 %>%
        group_by(comb) %>%
        mutate(total_comb = n()) %>%
        filter(total_comb > 50)
     

      counts <-
        com_2  %>%
        ggplot() +
        aes(y = reorder(comb, total_comb, max),
            fill = replace_na(factor(ST_2, levels = rev(STORDER_ALL)), "Other")) +
        geom_bar() +
        scale_fill_manual(values = STCOLORS, limits = STORDER_ALL) +
        theme(axis.title.x = element_blank(),
              legend.position = "none")+
        labs(fill = "ST") + ylab("") + expand_limits(x = 0)
      
      counts
      unique(com_2$ST_2)
      STORDER_ALL
      
      
      origin <-
        com_2  %>%
        ggplot() +
        aes(y = reorder(comb, total_comb, max),
            fill = factor(ORIGIN, levels = rev(ORIGINORDER))) +
        geom_bar(position = "fill") +
        scale_fill_manual(values = ORIGINCOL, limits = ORIGINORDER) +
        scale_x_continuous(labels = percent,
                           expand = c(0, 0),
                           limits = c(0, NA)) +
        theme(
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_blank(),
          legend.position = "none"
        ) +
        labs(y = "", fill = "Origin")
      
      
      
      combp <- plot_grid(counts ,origin,
        nrow = 1,
        axis = "tb",
        align = "h",
        labels = c("A", "B"),
        rel_widths = c(1, 0.5) )
      
      #saveplot(filename = "CH4_CARB_ESBLS_combination", combp, size = "h")
      
      combp
      
      com_2 %>% count(Bla_Carb_acquired)
      
    ### CTX-M-65 ####
      datos_total %>%
        filter(Bla_ESBL_acquired == "CTX-M-65" & str_detect(AGly_acquired, "rmtB")) %>% 
        count()
      datos_total %>%
        filter(Bla_ESBL_acquired == "CTX-M-65" ) %>% 
        count()
      datos_total %>%
        filter(str_detect(Bla_ESBL_acquired, "CTX-M-1$") & str_detect(Bla_Carb_acquired, "OXA-48")) %>%
        filter(ORIGIN!= "NLSAR") %>% count(City)
        count() 
        
        datos_total %>%
          filter(Bla_ESBL_acquired != "-" & Bla_Carb_acquired != "-") %>%
          count(str_detect(Bla_ESBL_acquired, "CTX-M-15$"))# %>% arrange(desc(n))
        count() 
      
        
        
        tempppp <- datos_total %>%
          filter(Bla_Carb_acquired2 != "-") %>%
      count(Bla_Carb_acquired2, ORIGIN) 
        
        
        tempppp <-  datos_total %>%
          filter(str_detect(Bla_Carb_acquired2,  "NDM")) %>%
          count(ORIGIN, Bla_Carb_acquired2) %>% arrange(desc(n))
        
        
        
        
        datos_total %>%
          filter(str_detect(Bla_Carb_acquired2,  "VIM-") ) %>%
          count(ST.1)  %>% arrange(desc(n))
        
      
    ### geom_point #####
      com_2 %>%
        group_by(comb) %>%
        mutate(total_comb = n()) %>%
        group_by(comb, ORIGIN) %>%
        mutate(comb_origin = n()) %>%
        mutate(freq = comb_origin / total_comb * 100) %>%
        distinct(ORIGIN, comb, total_comb, comb_origin,  freq) %>%
        group_by(comb) %>%
        slice_max(order_by = freq,
                  n = 1,
                  with_ties = T) %>%
        ggplot(aes(
          x = total_comb,
          y = freq,
          color = ORIGIN,
          label = comb
        )) +
        geom_point() +
        scale_color_manual(values = ORIGINCOL) +
        geom_text_repel()
      
    ### conteos #####
      
      com_2 %>% distinct(comb, ST_2) %>% group_by(ST_2) %>% count() %>% arrange(desc(n))
      com_2 %>% ungroup() %>% distinct(Bla_Carb_acquired, ST_2) %>% group_by(ST_2) %>% count() %>% arrange(desc(n))
      com_2 %>% ungroup() %>% distinct(Bla_Carb_acquired, ST_2) %>% filter(ST_2 == "ST147")
    
    
      
      select(datos_total, type2, ST_2, Bla_Carb_acquired, ORIGIN) %>%
        filter(Bla_Carb_acquired != "-") %>% 
        separate_rows(Bla_Carb_acquired, sep = ";") %>% 
        filter(str_detect(Bla_Carb_acquired, "OXA-48")) %>% count()
      
      Bla_Carb_acquired, ORIGIN) %>% filter(ORIGIN == "NLSAR")

      datos_total %>% count(ST_1 == "ST512")
      
    
    
    ### Only carb ####
combination_2 <- combination %>%
  unique() %>%
  filter(!is.na(Bla_Carb_acquired),  Bla_Carb_acquired != "-") %>%
  mutate(comb = case_when(
    Bla_ESBL_acquired == "-" |
      is.na(Bla_ESBL_acquired) ~ Bla_Carb_acquired,
    T ~ Bla_Carb_acquired
  ))

#origin <-
combination_2 %>% group_by(comb, ORIGIN) %>%
  count() %>%
  ggplot() +
  aes(y = (comb),
      x = n,
      fill = factor(ORIGIN, levels = rev(ORIGINORDER))) +
  geom_col(position = "fill") +
  scale_fill_manual(values = ORIGINCOL, limits = ORIGINORDER) +
  scale_x_continuous(labels = percent,
                     expand = c(0, 0),
                     limits = c(0, NA)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  ) +
  labs(y = "", fill = "Origin")

counts <-
  combination_2 %>%
  group_by(comb) %>%
  mutate(n = n()) %>%
  #filter(n > 30) %>%
  ggplot() +
  aes(y = reorder(comb, n, max), fill = replace_na(factor(ST_2, levels = rev(STORDER_ALL)), "Other")) +
  geom_bar() +
  scale_fill_manual(values = STCOLORS, limits = STORDER_ALL) +
  theme(axis.title.x = element_blank(),
        legend.position = "none") +
  scale_x_continuous(
    breaks = c(0, 250, 500, 750, 1000, 1250),
    limits = c(0, 1300),
    expand = c(0, 0)
  ) +
  labs(fill = "ST") + ylab("") + expand_limits(x = 0)

plot_grid(
  counts ,
  origin,
  nrow = 1,
  axis = "tb",
  align = "h",
  #labels = c("A", "B"),
  rel_widths = c(1, 0.5)
)


  ## boxplot ATB classes ####

boxplot <-
  select(
    datos_total,
    type2,
    num_resistance_classes,
    contains("Omp"),
    Bla_ESBL_acquired,
    Bla_Carb_acquired,
    Bla_acquired
  ) %>%
  mutate(
    type = case_when(
      Bla_Carb_acquired != "-" ~ "Carb",
      Bla_ESBL_acquired != "-" ~ "ESBL",
      Bla_acquired != "-" ~ "Bla",
      Omp_mutations != "-" ~ "Omp",
      T ~ "Susceptible"
    )
  )


    ### Sin puntos ####

ggplot(boxplot) +
  aes(fill = type, x = factor(type, RESTYPEORDER)) +
  geom_boxplot(aes(y = num_resistance_classes), alpha = 1) +
  scale_fill_manual(values = GROUPCOLORS, guide = "none") +
  theme(axis.title.x = element_blank()) +
  labs(y = "ATB classes")

    ### Con los puntos ####
ggplot(boxplot) +
  aes(fill = type, x = factor(type, RESTYPEORDER)) +
  geom_jitter(aes(
    x = factor(type, RESTYPEORDER),
    y = num_resistance_classes,
    colour = type2
  ),
  alpha = 0.5) +
  geom_boxplot(aes(y = num_resistance_classes), alpha = 0.7) +
  scale_fill_manual(values = GROUPCOLORS, guide = "none") +
  scale_colour_manual(values = TYPECOL) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) +
  theme(axis.title.x = element_blank()) +
  labs(y = "ATB classes") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_hline(
    yintercept = seq(1.5, 14 - 0.5, 1),
    lwd = 0.2,
    colour = "grey"
  )

  ## Distribución temporal de familias de genes ####
distribucion_genes <-
  select(
    datos_total,
    Bla_Carb_acquired,
    Bla_ESBL_acquired,
    Bla_acquired,
    Nombre_archivo_neris,
    Collection_year
  )

distribucion_genes <-
  separate_rows(distribucion_genes, Bla_Carb_acquired, sep = ";")
distribucion_genes <-
  separate_rows(distribucion_genes, Bla_ESBL_acquired, sep = ";")
    ### Carba ####

distribucion_carba <-
  separate_rows(distribucion_genes, Bla_Carb_acquired, sep = ";") %>%
  filter(!is.na(Bla_Carb_acquired), Bla_Carb_acquired != "-") %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  unique()

distribucion_carba$Bla_Carb_acquired <-
  gsub("-.*", " - like", distribucion_carba$Bla_Carb_acquired)

carba_plot <- distribucion_carba %>%
  filter(!is.na(Collection_year)) %>%
  group_by(Bla_Carb_acquired) %>%
  filter(n() > 25) %>%
  group_by(Bla_Carb_acquired, Collection_year) %>%
  summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = Bla_Carb_acquired) +
  geom_point(aes(size = n, color = Bla_Carb_acquired)) +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(range = c(2, 18)) +
  labs(y = "Carbapenemase", size = "Nº of cases") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
carba_plot

    ### ESBL ####

distribucion_ESBL <-
  separate_rows(distribucion_genes, Bla_ESBL_acquired, sep = ";") %>%
  filter(!is.na(Bla_ESBL_acquired), Bla_ESBL_acquired != "-") %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  unique()

distribucion_ESBL$Bla_ESBL_acquired <-
  gsub("-.*", " - like", distribucion_ESBL$Bla_ESBL_acquired)

ESBL_plot <- distribucion_ESBL %>%
  filter(!is.na(Collection_year)) %>%
  group_by(Bla_ESBL_acquired) %>%
  filter(n() > 25) %>%
  group_by(Bla_ESBL_acquired, Collection_year) %>%
  summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = Bla_ESBL_acquired) +
  geom_point(aes(size = n, color = Bla_ESBL_acquired)) +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(range = c(2, 18)) +
  labs(y = "ESBL", size = "Nº of cases") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
ESBL_plot

    ### Bla ####

distribucion_bla <-
  separate_rows(distribucion_genes, Bla_acquired, sep = ";") %>%
  filter(!is.na(Bla_acquired), Bla_acquired != "-") %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  unique()

distribucion_bla$Bla_acquired <-
  gsub("-.*", " - like", distribucion_bla$Bla_acquired)

bla_plot <- distribucion_bla %>%
  filter(!is.na(Collection_year)) %>%
  group_by(Bla_acquired) %>%
  filter(n() > 150) %>%
  group_by(Bla_acquired, Collection_year) %>%
  summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = Bla_acquired) +
  geom_point(aes(size = n, color = Bla_acquired)) +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(range = c(2, 18)) +
  labs(y = "ß-lactamase") +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(x = "Collection year", size = "Nº of cases")
bla_plot



ggarrange(
  carba_plot,
  ESBL_plot,
  bla_plot,
  ncol = 1,
  common.legend = T,
  align = "hv",
  legend = "right"
)



## KPC MAPS DISTRIBUTION ####
KPC_Data = datos_total %>%  
  select(Bla_Carb_acquired2, Lat, Lon, ORIGIN) %>%
  filter(str_detect(Bla_Carb_acquired2, "KPC"))

    
p_mapl <-  ggplot(map_data("world")) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   color = "gray80",
                   fill = "gray80") +
      #  geom_point(data = filter(base_datos, !is.na(Continent), type2 == "BD") %>% count(Lat, Lon, Isolation_source), aes(x = Lon, y = Lat, color = Isolation_source, size = n)) +
      geom_point(data = KPC_Data, aes(x = Lon, y = Lat, color = Bla_Carb_acquired2)) +
      # scale_color_manual(values = source) +
      #scale_color_manual(values = ORIGINCOL) +
      #scale_fill_manual(values = source) +
      labs(x = "", y = "", color = "Isolation source") +
      theme_void() +        theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        legend.box = "vertical",
        legend.position = "bottom"
      )
    
    
p_mapl
    
    l_pmap <- get_legend(p_mapl)
    
    p_map <- p_mapl + theme(legend.position = "none")
    
    p_map

# VIR ####
  ## version con facet por origin ####
  datos_total %>%
    ggplot() +
    aes(
      x = factor(virulence_score),
      y = factor(resistance_score),
      color = ORIGIN
    ) +
    geom_jitter() +
    scale_color_manual(values = ORIGINCOL, guide = "none") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 60,
            vjust = 1,
            hjust = 1
          )) +
    geom_vline(
      xintercept = seq(1.5, 7 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    geom_hline(
      yintercept = seq(1.5, 5 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    ylab("Virulence score") +
    xlab("Resistance score") +
    scale_x_discrete(labels = c("WT", "ybt", "clb", "iuc", "ybt+iuc", "ybt+iuc+clb")) +
    scale_y_discrete(labels = c("WT", "ESBL", "Carb", "Carb+col")) +
    facet_wrap(vars(factor(ORIGIN, names(ORIGINCOL))), nrow = 2)
  
  ## Versión con tamaño y color por origin ####
  Nvir <- datos_total %>%
    group_by(resistance_score, virulence_score, ORIGIN) %>%
    ggplot() +
    aes(
      x = factor(virulence_score),
      y = factor(resistance_score),
      color = ORIGIN
    ) +
    geom_jitter(size = 0.5) +
    scale_color_manual(values = ORIGINCOL) +
    geom_vline(
      xintercept = seq(1.5, 7 - 0.5, 1),
      linewidth = 0.2,
      colour = "grey"
    ) +
    geom_hline(
      yintercept = seq(1.5, 5 - 0.5, 1),
      linewidth = 0.2,
      colour = "grey"
    ) +
    #  ylab("Virulence score")+
    # xlab("Resistance score") +
    ylab(NULL) +
    xlab(NULL) +
    labs(color = "Origin", size = "Nº of cases") +
    scale_x_discrete(labels = c("WT", "ybt", "clb", "iuc", "ybt+\niuc", "ybt+\niuc+\nclb")) +
    scale_y_discrete(labels = c("WT", "ESBL", "Carb", "Carb+\nColistin")) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        angle = 60,
        vjust = 1,
        hjust = 1
      ),
      legend.position = "none"
    )
  
  Nvir
  
  saveplot("CH4_virconv", Nvir, "t")
  
  ## Versión con tamaño y color por origin ####
  datos_total %>%
    group_by(resistance_score, virulence_score, ORIGIN) %>%
    summarise(n = n()) %>%
    filter(n > 0) %>%
    ggplot() +
    aes(
      x = factor(virulence_score),
      y = factor(resistance_score),
      color = ORIGIN
    ) +
    geom_jitter(aes(size = n)) +
    scale_color_manual(values = ORIGINCOL) +
    geom_vline(
      xintercept = seq(1.5, 7 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    geom_hline(
      yintercept = seq(1.5, 5 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    ylab("Virulence score") +
    xlab("Resistance score") +
    labs(color = "Origin", size = "Nº of cases") +
    scale_x_discrete(labels = c("WT", "ybt", "clb", "iuc", "ybt+iuc", "ybt+iuc+clb")) +
    scale_y_discrete(labels = c("WT", "ESBL", "Carb", "Carb+col")) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 60,
            vjust = 1,
            hjust = 1
          ))

## Versión con tamaño y color por origin ampliado ####
  datos_total %>%
    group_by(resistance_score, virulence_score, ORIGIN) %>%
    summarise(n = n()) %>%
    filter(n > 0,
           resistance_score > 0,
           virulence_score > 2,
           !is.na(virulence_score)) %>%
    ggplot() +
    aes(
      x = factor(virulence_score),
      y = factor(resistance_score),
      color = ORIGIN
    ) +
    geom_jitter(aes(size = n)) +
    scale_size_continuous(range = c(2, 20)) +
    scale_color_manual(values = ORIGINCOL) +
    geom_vline(
      xintercept = seq(1.5, 3 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    geom_hline(
      yintercept = seq(1.5, 3 - 0.5, 1),
      lwd = 0.2,
      colour = "grey"
    ) +
    ylab("Virulence score (>2)") +
    xlab("Resistance score (>0)") +
    labs(color = "Origin", size = "Nº of cases") +
    scale_x_discrete(labels = c("iuc", "ybt+iuc", "ybt+iuc+clb")) +
    scale_y_discrete(labels = c("ESBL", "Carb", "Carb+col")) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(
            angle = 60,
            vjust = 1,
            hjust = 1
          ))
  
  ## Versión por categorias ####
  res_vir_cat <- 
  datos_total %>%
    mutate(
      category = case_when(
        resistance_score == 0 & virulence_score <= 1 ~ "WT",
        resistance_score == 0 &
          virulence_score > 1 ~ "VIR",
        resistance_score == 1 &
          virulence_score <= 1 ~ "ESBL",
        resistance_score == 1 &
          virulence_score > 1 ~ "ESBL + VIR",
        resistance_score == 2 &
          virulence_score <= 1 ~ "CARBA",
        resistance_score == 2 &
          virulence_score > 1 ~ "CARBA + VIR",
        resistance_score == 3 &
          virulence_score <= 1 ~ "CARBA + COL",
        resistance_score == 3 &
          virulence_score > 1 ~ "CARBA + COL + VIR"
      )
    )  %>%
    group_by(category, ORIGIN) %>%
    summarise(n = n()) %>%
    mutate(
      type = case_when(
        category == "WT" ~ "WT",
        category == "VIR" ~ "Virulent",
        category == "ESBL" ~ "ESBL",
        category == "ESBL + VIR" ~ "Convergent",
        category == "CARBA" ~ "Carbapenemase",
        category == "CARBA + VIR" ~ "Convergent",
        category == "CARBA + COL" ~ "Carbapenemase",
        category == "CARBA + COL + VIR" ~ "Convergent"
      )
    ) %>% as.data.frame()
  
    ### barras ####
    # por origin
    virrcatbarras <- ggplot(res_vir_cat) +
      aes(factor(type, levels = c("WT", "ESBL", "Carbapenemase",  "Virulent", "Convergent")) , n) +
      geom_col(aes(fill = factor(ORIGIN, levels = rev(ORIGINORDER)))) + xlab("") + ylab("Nº of cases") +
      scale_fill_manual(values = ORIGINCOL, limits = ORIGINORDER) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(
              angle = 60,
              vjust = 1,
              hjust = 1
            ))
    #facet_wrap(~ type, nrow = 1)
    
    virrcatbarras
    
    # por categoria
    
    pRESVIR_ORIGIN <- 
      ggplot(res_vir_cat) +
      aes(y= factor(ORIGIN, levels=rev(ORIGINORDER)), x = n) +
      geom_col(aes(fill = factor(type, levels = rev(c("WT", "ESBL", "Carbapenemase",  "Virulent", "Convergent")))) , position = "fill") +
        xlab("Type") + ylab(NULL) +
        scale_fill_manual(values = RESVIRCOL2, 
                          breaks=c("WT", "ESBL", "Carbapenemase",  "Virulent", "Convergent")) +
      theme(
        axis.text.y = element_text(),
        axis.text.x = element_text(
          angle = 60,
          vjust = 1,
          hjust = 1
        ),
        axis.text.y.right = NULL
      ) +
      labs(fill = "Type") +
      theme(axis.line=element_blank(),
            axis.ticks=element_blank(),
            legend.position="bottom",
            legend.direction = "vertical",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(), 
            axis.text.x = element_text(angle = 45, hjust=0.1, size = 8), 
            axis.text.y = element_text(color="black", size = 10))  +
      
      scale_x_continuous(position = "top") +
      guides(fill=guide_legend(ncol=1,byrow=TRUE))
    
    pRESVIR_ORIGIN
    
    ### treemap #####
    
    treemap::treemap(res_vir_cat, index = "category", vSize = "n")
    treemap::treemap(res_vir_cat,
                     index = c("ORIGIN", "category"),
                     vSize = "n")
    
    ### Doughnut ####
    # por origin
    ggplot(res_vir_cat) +
      aes(x = factor(ORIGIN, rev(names(ORIGINCOL))), y = n) +
      geom_col(aes(fill = ORIGIN)) +
      scale_y_continuous(limits = c(0, 1500),
                         breaks = c(0, 250, 500, 750, 1000, 1250, 1500)) +
      scale_fill_manual(values = ORIGINCOL) +
      coord_polar(theta = "y") +
      facet_wrap(vars(factor(
        category,
        c(
          "WT",
          "VIR",
          "ESBL",
          "ESBL + VIR",
          "CARBA",
          "CARBA + VIR",
          "CARBA + COL",
          "CARBA + COL + VIR"
        )
      )), nrow = 2, dir = "v") +
      theme(
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank()
      )
    
    # por categoria
    
    ggplot(res_vir_cat) +
      aes(x = factor(category, rev(names(RESVIRCOL))), y = n) +
      geom_col(aes(fill = factor(category, rev(names(
        RESVIRCOL
      ))))) +
      scale_y_continuous(limits = c(0, 1500),
                         breaks = c(0, 250, 500, 750, 1000, 1250, 1500)) +
      scale_fill_manual(values = RESVIRCOL) +
      coord_polar(theta = "y") +
      facet_wrap(vars(factor(ORIGIN, names(ORIGINCOL))), nrow = 2, dir = "v") +
      theme(
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(fill = "Category")
    
    ### polar ####
    
    # por origin
    
    ggplot(res_vir_cat) +
      aes(x = factor(
        category,
        c(
          "WT",
          "VIR",
          "ESBL",
          "ESBL + VIR",
          "CARBA",
          "CARBA + VIR",
          "CARBA + COL",
          "CARBA + COL + VIR"
        )
      ) , y = n) +
      geom_col(aes(fill = ORIGIN)) +
      scale_fill_manual(values = ORIGINCOL) +
      coord_polar(theta = "x") +
      theme(
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank()
      )
    
    # por categoria
    ggplot(res_vir_cat) +
      aes(x = factor(ORIGIN, names(ORIGINCOL)), y = n) +
      geom_col(aes(fill = factor(category, rev(names(
        RESVIRCOL
      ))))) +
      scale_fill_manual(values = RESVIRCOL) +
      coord_polar(theta = "x") +
      theme(
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(fill = "Category")
    
    ## CONTEOS #####
    datos_total %>% filter(Colibactin != "-" & Bla_Carb_acquired != "-" & ORIGIN == "Americas") %>% count(Bioproject) %>%
      arrange(desc(n))
    
    datos_total %>%
      filter(Yersiniabactin != "-") %>%
      #group_by(ORIGIN) %>%
      count(ST.1) %>% arrange(desc(n))
    
        mutate(pru = str_extract(Yersiniabactin, "-.*")) %>% count(pru)
    
    datos_total %>%
      filter(Colibactin != "-") %>%
      #group_by(ORIGIN) %>%
      count(ST.1) %>% arrange(desc(n))
    
    datos_total %>%
      filter(Aerobactin != "-" ) %>%
      #group_by(ORIGIN)%>%
      count(ST.1) %>% arrange(desc(n))
    
    datos_total %>%
      filter(RmpADC != "-" ) %>%
      #group_by(ORIGIN)%>%
      count(ST.1) %>% arrange(desc(n))
    
    datos_total %>%
      filter(Salmochelin != "-" ) %>%
      #group_by(ORIGIN)%>%
      count(ST.1) %>% arrange(desc(n))
    
    
    datos_total %>%
      filter(ORIGIN  == "Asia") %>%
      group_by(Colibactin, Aerobactin, Salmochelin, Yersiniabactin) %>%
      count()
    
    
    temp <- datos_total %>%
      filter(ORIGIN  == "Asia" & virulence_score > 2 & Bla_Carb_acquired != "-") %>%
      select(Colibactin, Aerobactin, Salmochelin, Yersiniabactin, Bla_Carb_acquired, Country) %>%
      group_by(Colibactin, Aerobactin, Salmochelin, Yersiniabactin, Bla_Carb_acquired) %>%
    #distinct() %>%
      summarise(count = n())
    
    
    datos_total %>% select()
    
    res_vir_cat %>% group_by(type, ORIGIN) %>% summarise(sum(n))
    
    res_vir_cat %>% group_by(type, ORIGIN) %>%
      summarise(total = sum(n)) %>%
      pivot_wider(names_from = type, values_from = total) %>%
      mutate(total=rowSums(select(., -1))) %>%
      mutate(across(-c(1, ncol(.)), ~ ./total*100))
    
    
    res_vir_cat %>% filter(type == "Virulent") %>% count(ST.1) %>% arrange(desc(n))
  
    # PLOT TILES KHOLT #####
    ## version 1 ####
    GROUPCarbCOUNT = file_Res %>% group_by(ORIGIN,) %>% summarise(n=n())
    
    bla_c = c( "\\SHV\\b")
    bla_a = c("\\OXA-1\\b", "\\TEM-1D\\b")
    esbl = c("\\CTX-M-15\\b", "\\CTX-M-1\\b", "\\CTX-M-3\\b", "\\SHV-12\\b" )  
    bla_carb = c( "\\NDM\\b", "\\OXA-48\\b", "\\KPC-2\\b", "\\KPC-3\\b")
    sul= c("\\Sul1\\b", "\\Sul2\\b")
    rif= c("arr")
    Flq=c("qnrB")
    Phe = c("[Cc]atB") 
    agly=c( "aac.6'.-Ib-cr"
            ) # "\\<aadA\\>","aadA16",
    
    ARGORDER=c("SHV", "SHV12", "OXA1", "TEM1D", "CTXM1", "CTXM3",   "CTXM15", "OXA48", "KPC-2","KPC-3", "NDM", 
               "Sul1", "Sul2", "arr", "qnrB",  "aac.3.IIa", "aac.6'.Ibcr", "aph3Ia",
               "aac.6'.Ib", "aadA", "aadA16", "aadA2", "rmtF" )
    
    ARGORDER_Prev=c("SHV", "SHV12", "OXA1", "TEM1D",  "CTXM1", "CTXM3", "CTXM15", "OXA48", "KPC2","KPC3", "NDM", 
                    "Sul1", "Sul2", "arr", "qnrB", "[Cc]atB", "aac.6'.Ibcr", 
                   "aadA2")
    
    ARGlabels=c("SHV",  "SHV-12", "OXA-1", "TEM-1D",  "CTX-M-1","CTX-M-3","CTX-M-15", "OXA-48","KPC-2","KPC-3", "NDM-like",
                "Sul1", "Sul2", "arr", "qnrB", "CatB", "aac(6')-Ibcr", 
                "aadA2")
    
     #tempo <-
       file_Res %>% mutate(pruu =str_extract(pattern = "OXA-[0-9]*[?\\*]*", `BETA-LACTAM`)) %>%
         select(Name, ORIGIN, pruu) %>%
         filter(!is.na(pruu)) %>%
       count(pruu) %>% arrange(desc(n))
       
       
    
    GCC_genes = GROUPCarbCOUNT 
    
    for (x in esbl){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN, str_detect(Bla_ESBL_acquired, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_a){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,str_detect(Bla_acquired, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_c){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  str_detect(Bla_chr, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_carb){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, Bla_Carb_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in sul){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  str_detect(Sul_acquired, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in rif){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, Rif_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in Flq){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, Flq_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in Phe){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, Phe_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in agly){
      name = (gsub("[-|\\|>|<]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, AGly_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    
    dftile2 = GCC_genes %>% select(-n,everything()) %>% 
      mutate_if(is.numeric, funs(./n*100))  %>%
      select(-n) %>% replace(is.na(.), 0)
    
    pGENESTILE <- 
    dftile2 %>%
      pivot_longer(-c(ORIGIN), names_to = "genes", values_to = "counts") %>%
      ggplot(aes(x=factor(genes, levels=ARGORDER_Prev), y=factor(ORIGIN, levels=rev(ORIGINORDER)), fill=counts)) + 
      geom_tile(colour = "black",  width=0.9, height=0.9) + # scale_fill_distiller(palette = "Reds", direction = 1, min +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=0.1, size = 8)) +
      scale_fill_gradient2(
        "Resistance",
        low = "white",
        high = "black",
        mid = "#db3a34", 
        midpoint = 50
      )  + ylab(NULL) + xlab("Resistance determinants") +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            legend.position="bottom",
            legend.direction = "vertical",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())  +
      scale_x_discrete(position = "top", labels= ARGlabels) 
    
    
    pGENESTILE
    
    
    
    
    ## version 1.2 ####
    GROUPCarbCOUNT = file_Res %>% group_by(ORIGIN,) %>% summarise(n=n())
    
    bla_c = c( "\\SHV\\b")
    bla_a = c("\\OXA-1\\b", "\\TEM-1D\\b")
    esbl = c("\\CTX-M-15\\b", "\\CTX-M-1\\b", "\\CTX-M-3\\b", "\\SHV-12\\b" )  
    bla_carb = c( "\\IMP\\b","\\VIM\\b","\\NDM\\b", "\\OXA-48\\b", "\\KPC\\b")


    ARGORDER=c("SHV",  "OXA1", "TEM1D", "SHV12","CTXM1", "CTXM3",  "CTXM15", "OXA48", "KPC","NDM", "VIM", "IMP" )
    
    ARGORDER_Prev=c("SHV",  "OXA1", "TEM1D", "SHV12", "CTXM1", "CTXM3", "CTXM15", "OXA48", "KPC", "NDM", "VIM", "IMP" )
    
    ARGlabels=c("SHV",   "OXA-1", "TEM-1D",  "SHV-12","CTX-M-1","CTX-M-3","CTX-M-15", "OXA-48","KPC-LIKE", "NDM-like",  "VIM-like", "IMP-like")
    
    #tempo <-
    file_Res %>% mutate(pruu =str_extract(pattern = "OXA-[0-9]*[?\\*]*", `BETA-LACTAM`)) %>%
      select(Name, ORIGIN, pruu) %>%
      filter(!is.na(pruu)) %>%
      count(pruu) %>% arrange(desc(n))
    
    
    
    GCC_genes = GROUPCarbCOUNT 
    
    for (x in esbl){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN, str_detect(Bla_ESBL_acquired, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_a){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,str_detect(Bla_acquired, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_c){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  str_detect(Bla_chr, x)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }
    for (x in bla_carb){
      name = (gsub("[-|\\|b]", "", x))
      
      temp = file_Res %>% 
        count(ORIGIN,  grepl(x, Bla_Carb_acquired)) %>% 
        rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
        select(ORIGIN, !!name)
      
      GCC_genes = merge(GCC_genes, temp, all = T)
    }

    
    dftile2 = GCC_genes %>% select(-n,everything()) %>% 
      mutate_if(is.numeric, funs(./n*100))  %>%
      select(-n) %>% replace(is.na(.), 0)
    
    pGENESTILE <- 
      dftile2 %>%
      pivot_longer(-c(ORIGIN), names_to = "genes", values_to = "counts") %>%
      ggplot(aes(x=factor(genes, levels=ARGORDER_Prev), y=factor(ORIGIN, levels=rev(ORIGINORDER)), fill=counts)) + 
      geom_tile(colour = "black",  width=0.9, height=0.9) + # scale_fill_distiller(palette = "Reds", direction = 1, min +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=0.1, size = 8)) +
      scale_fill_gradient2(
        "Resistance",
        low = "white",
        high = "black",
        mid = "#db3a34", 
        midpoint = 50
      )  + ylab(NULL) + xlab("Resistance determinants") +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            legend.position="bottom",
            legend.direction = "vertical",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())  +
      scale_x_discrete(position = "top", labels= ARGlabels) 
    
    
    pGENESTILE
    
    
    
    
    ## Version 2 ####   
    # ST
    selected_STs <-
      datos_total %>%
      filter(ORIGIN != "Unknown") %>%
      group_by(ORIGIN) %>% count(ST.1) %>%
      slice_max(order_by = n,
                n = 5,
                with_ties = T) %>%
      filter(n > 5) %>%  arrange(desc(n)) %>%
      pull(ST.1)
    
    library(forcats)
    p1_ST_ORIGIN <-
      datos_total %>%
      group_by(ST.1, ORIGIN) %>%
      dplyr::summarise(n = n()) %>%
      ggplot() +
      aes(x = n,
        y = factor(ORIGIN, levels=rev(ORIGINORDER)),
        fill = replace_na(factor(ST.1, levels = rev(STORDER_ALL)), "Other")) +
      geom_col(width = 0.8, position="fill") +
      scale_fill_manual(values = STCOLORS) +
      labs(y = NULL, x = "Sequence types", fill = "Origin") +
      theme(axis.line=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      scale_x_continuous(position = "top") 
    
    p1_ST_ORIGIN
    
    # Primero ARG 
    tempo <- 
      file_Res %>% select( ORIGIN, 
                           Bla_acquired, Bla_ESBL_acquired, Bla_Carb_acquired, AGly_acquired, Bla_ESBL_inhR_acquired,
                           Flq_acquired, Rif_acquired, Tmt_acquired, Tgc_acquired, Col_acquired, Col_mutations, Fcyn_acquired,
      ) %>%
      mutate( across(
        .cols = c(-ORIGIN, ),
        .fns = ~ str_replace_all(
          string = ., 
          pattern = "-", 
          replacement = "0"))) %>%
      mutate(across(.cols=c(-ORIGIN), 
                    .fns = ~ case_when(. == "0" ~ 0, 
                                       . != "0" ~ 1))) %>%
      #mutate(Bla_ESBL_acquired = case_when(#Bla_Carb_acquired == "1" ~ 0,
      #                                    Bla_ESBL_acquired == "0" ~ 0, 
      #                                     Bla_ESBL_acquired != "0" ~ 1)) %>%
      
      #filter(ORIGIN == "NLSAR" & Bla_Carb_acquired == 1) %>% count()
      group_by(ORIGIN) %>%
      mutate_if(is.numeric, funs(sum(.)))  %>%
      mutate(n=n()) %>%
      distinct() %>%     
      select(-n,everything())    %>%
      mutate_if(is.numeric, funs(./n*100)) %>%
      select(-n)
    
    
    ptempo1 <- 
      tempo %>%
      pivot_longer(-c(ORIGIN), names_to = "genes", values_to = "counts") %>%
      ggplot(aes(x=(genes), y=factor(ORIGIN, levels=rev(ORIGINORDER)), fill=counts)) + 
      geom_tile(colour = "black",  width=0.9, height=0.8) +# scale_fill_distiller(palette = "Reds", direction = 1, min +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=0.1, size = 8)) +
      scale_fill_gradient2(
        low = "white",
        high = "black",
        mid = "#db3a34", 
        midpoint = 40
      )  + ylab(NULL) + xlab("Resistance determinants") +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())  +
      scale_x_discrete(position = "top") 
    
    ## VIR ####
    
    tempo2 <- 
      file_Res %>% select( ORIGIN, Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC
                           
      ) %>%
      mutate( across(
        .cols = c(-ORIGIN, ),
        .fns = ~ str_replace_all(
          string = ., 
          pattern = "-", 
          replacement = "0"))) %>%
      mutate(across(.cols=c(-ORIGIN), 
                    .fns = ~ case_when(. == "0" ~ 0, 
                                       . != "0" ~ 1))) %>%
      #mutate(Bla_ESBL_acquired = case_when(#Bla_Carb_acquired == "1" ~ 0,
      #                                    Bla_ESBL_acquired == "0" ~ 0, 
      #                                     Bla_ESBL_acquired != "0" ~ 1)) %>%
      
      #filter(ORIGIN == "NLSAR" & Bla_Carb_acquired == 1) %>% count()
      group_by(ORIGIN) %>%
      mutate_if(is.numeric, funs(sum(.)))  %>%
      mutate(n=n()) %>%
      distinct() %>%     
      select(-n,everything())    %>%
      mutate_if(is.numeric, funs(./n*100)) %>%
      select(-n)
    
    
    ptempo2 <- 
      tempo2 %>%
      pivot_longer(-c(ORIGIN), names_to = "genes", values_to = "counts") %>%
      ggplot(aes(x=(factor(genes, levels= rev(c( "Colibactin", "RmpADC","Salmochelin", "Aerobactin", "Yersiniabactin")))), y=factor(ORIGIN, levels=rev(ORIGINORDER)), fill=counts)) + 
      geom_tile(colour = "black",  width=0.9, height=0.9) + # scale_fill_distiller(palette = "Reds", direction = 1, min +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust=0.1, size = 8)) +
      scale_fill_gradient2("Virulence", 
        low = "white",
        high = "black",
        mid = "#ffc857", 
        midpoint = 50)  + 
      ylab(NULL) + xlab( "Virulence factors") +
      theme(axis.line=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            legend.position="bottom",
             legend.direction="vertical",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()) +
      scale_x_discrete(position = "top") 
    
    ptempo2
    
    
    ### ARG, VIR density curves. #####
    uniqeARGgenes2 <-
      datos_total %>%
      select(Nombre_archivo_neris, ORIGIN, AGly_acquired:Bla_chr) %>%
      unite(
        col = ARGs,
        AGly_acquired:Bla_chr,
        sep = ";",
        remove = T
      ) %>%
      separate_rows(ARGs, sep = ";") %>% distinct() %>% filter(ARGs != "-") %>%
      count(Nombre_archivo_neris, ORIGIN)
    
    uniqegenes <- datos_total %>%
      select(Nombre_archivo_neris, ORIGIN, Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC) %>%
      mutate(across(everything(), ~str_replace_all(., "^-$", "!@!"))) %>%
      unite(
        col = VFs,
        c(Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC),
        sep = ";",
        remove = T
      ) %>%
      mutate(nVFs = 5-str_count( VFs, "!@!"))  %>% select(-VFs) %>% 
       full_join(., uniqeARGgenes2, by=c("Nombre_archivo_neris", "ORIGIN")) %>%
       rename(ARGs = n) %>%  pivot_longer(., cols=c("nVFs",  "ARGs"), names_to= "type", values_to="ngenes")
       
    
    
      puniqegenes =
        ggplot(uniqegenes) +
        aes(x = ngenes, y = ..scaled..) +
        geom_density(aes(color=type)) +
        scale_color_manual(values = c("#9a170d", "#084c61")) +
          facet_wrap(~factor(ORIGIN, levels = ORIGINORDER),  ncol = 1, strip.position = "left")+
        labs(x = "Number of genes") +
        
        theme(axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              #axis.title.x = element_blank(),
              axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank()) +
        scale_x_continuous(limits=c(0, 25),  position = "top")

      
                  
        library(ggridges)
        
        # Cut off the trailin tails. 
        # Specify `rel_min_height`: a percent cutoff
      ggplot(uniqegenes, aes(x = ngenes, y = factor(ORIGIN, levels = rev(ORIGINORDER)))) + 
          geom_density_ridges(aes(fill = type), rel_min_height = 0.01,  alpha = 0.6, scale=0.8) +
        geom_density_ridges(aes(fill = type),  stat = "binline", bins = 30, scale = 0.95, alpha = 0.6) 
        
      
      ggarrange(p1_ST_ORIGIN, ptempo1, ptempo2, nrow = 1, align = "h")
      
      ggarrange(p1_ST_ORIGIN, pGENESTILE, ptempo2, nrow = 1, align = "h", widths = c(0.6, 1.2, 0.3))
      
      
      #ggarrange(puniqegenes, pGENESTILE, ptempo2, nrow = 1, align = "h", widths = c(0.6, 1.2, 0.3), heights = c(0.5, 1, 1))
      
      plot_grid(puniqegenes, pGENESTILE, ptempo2, align = "h", axis = "rlbt", nrow = 1,  rel_widths = c(0.4, 1.2, 0.3))
    plot <-   ggarrange(pRESVIR_ORIGIN, pGENESTILE, ptempo2, NULL, nrow = 1, align = "h", widths = c(0.6, 1, 0.4, 0.1), common.legend = T, legend = "bottom")
    plot <-   ggarrange(pRESVIR_ORIGIN, pGENESTILE, ptempo2, NULL, nrow = 1, align = "h", widths = c(0.6, 1, 0.4, 0.1))
    
    plot  
    
   # saveplot(filename = "CH4_ARG_VIR", plot, size = 140)
      
      
   
      