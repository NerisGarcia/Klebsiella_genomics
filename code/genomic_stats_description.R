# Library load.
library(tidyverse)
library(ggpubr)

setwd("C:/Users/neris/Desktop/RESEARCH/Projects/Articulo_diversity/Klebsiella_global_genomics")


source("code/design.R")
# Load metadata database ####


# load data --------------------------------

all.genomic.db <- 
  read.csv("C:/Users/neris/Desktop/RESEARCH/Projects/genomic_CV/Klebsiella_transmission/data/Genomic_info.csv") %>% 
filter(source_type == "SKPCV" | source_type2 == "NCBI")  %>%
  tidyr::replace_na(list(Continent = "Unknown", 
                         Isolation_source = "Unknown", 
                         Country = "Unknown"))  %>% 
  mutate(Region = case_when(
    source_type == "SKPCV" ~ "SKPCV",
    Country == "Spain" ~ "Europe",
    T ~ Continent
  )) %>% 
  #! fix ST
  mutate(ST.1 = str_remove(ST, "-.*"))  %>% 
  mutate(Country2 = case_when(
    source_type== "SKPCV" ~"SKPCV", 
    TRUE ~ Country  )) %>% 
  #add AMR + VIR categories
  mutate(
    strain_type = case_when(
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
  mutate(
    strain_type2 = case_when(
      strain_type == "WT" ~ "WT",
      strain_type == "VIR" ~ "Virulent",
      strain_type == "ESBL" ~ "ESBL",
      strain_type == "ESBL + VIR" ~ "Convergent",
      strain_type == "CARBA" ~ "Carbapenemase",
      strain_type == "CARBA + VIR" ~ "Convergent",
      strain_type == "CARBA + COL" ~ "Carbapenemase",
      strain_type == "CARBA + COL + VIR" ~ "Convergent"
    )
  ) 


# Count QC 

all.genomic.db %>% count(source_type)
all.genomic.db %>%
  filter(source_type2 == "NCBI") %>% 
  count(QC_filter)

#! write supp mat
#write.csv(all.genomic.db, "data/Genomic_data.csv")


pass.genomic.db <- 
  all.genomic.db %>% 
  #! add Region column
  filter(QC_filter == "PASS")  

rm(all.genomic.db)

ncbi.genomic.db <- 
  pass.genomic.db %>% 
  filter( source_type2 == "NCBI") 

# Start analysis  (only PASS) -----------------------

## Counts####
### Country#####

ncbi.genomic.db %>% 
  count(Country)

### Collection year#####
ncbi.genomic.db %>% 
  count(Collection_year)



ncbi.genomic.db %>%
  filter(!is.na(Collection_year)) %>%
  mutate(
    Collection_year = case_when(
      Collection_year < 2010 ~ 2000,
      Collection_year >= 2010 ~ 2100,
      T ~ Collection_year
    )
  ) %>%
  count(Collection_year)

###Isolation source#####
ncbi.genomic.db %>% 
  count(Isolation_source)

## Collection description #########
### World map ####

p_mapl <-  ggplot(map_data("world")) +
  geom_polygon(aes(x = long, y = lat, group = group),
               color = "gray80",
               fill = "gray80") +
  #  geom_point(data = filter(pass.genomic.db, !is.na(Continent), type2 == "BD") %>% count(Lat, Lon, Isolation_source), aes(x = Lon, y = Lat, color = Isolation_source, size = n)) +
  geom_point(data = pass.genomic.db, aes(x = Lon, y = Lat, color = Region)) +
  # scale_color_manual(values = source) +
  scale_color_manual(values = RegionCOL) +
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

### time ####
time <- pass.genomic.db %>%
  filter(!is.na(Collection_year)) %>%
  mutate(Collection_year = case_when(Collection_year < 2000 ~ 2000,
                                     T ~ Collection_year)) %>%
  group_by(Collection_year, Region) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = Collection_year, y = factor(Region, rev(names(RegionCOL)))) +
  geom_point(aes(colour = Region, size = n), alpha = 0.9) +
  scale_colour_manual(values = RegionCOL) +
  scale_size(range = c(0, 9))  +      #scale_size_continuous(range = c(0.1,25)) +
  scale_x_continuous(
    breaks = c(2000, 2005, 2010, 2015, 2020),
    labels = c("1900-2000", "2005", "2010", "2015", "2020")
  ) +
  labs(
    x = "Collection year",
    colour = "Region",
    y = "",
    size = "Nº of cases"
  ) +
  theme(legend.position = "none")
time


### counts ####

counts <- pass.genomic.db %>%
  #filter(!is.na(Collection_year)) %>%
  group_by(Region) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(x = n,
      y = factor(Region, rev(names(RegionCOL))),
      fill = Region) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n, x = n - 5), size = 3) +
  scale_fill_manual(values = RegionCOL, guide = "none") +
  labs(y = "", x = "Nº of cases") +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(1000, 3000),
    limits = (c(0, 4100))
  ) +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none") + coord_cartesian(clip = "off")
counts


### merge figs ####
pglobal <-
  ggpubr::ggarrange(
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



## ST -----------------------------------------------------------
### Selected STs and cgMLST_Sublineage#####

selected_STs_2 <-
  ncbi.genomic.db %>%
  count(ST.1) %>%
  mutate(prop = n / sum(n)) %>%
  slice_max(order_by = prop,
            n = 9,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(ST.1)


selected_cgMLST_SL <- 
  ncbi.genomic.db %>%
  count(cgMLST_Sublineage) %>%
  slice_max(order_by = n,
            n = 10,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(cgMLST_Sublineage)

#### Total pie #####

ST_total_df <-
  ncbi.genomic.db %>%
  mutate(ST.2 = case_when(ST.1 %in% selected_STs_2 ~ ST.1, TRUE  ~ "Other")) %>%
  count(ST.2) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))  %>%
  # Compute the cumulative percentages (top of each rectangle)
  mutate(ymax = cumsum(prop)) %>%
  # Compute the bottom of each rectangle
  mutate(ymin = lag(ymax, default = 0)) %>%
  # Compute label position
  mutate(labelPosition = (ymax + ymin) / 2) %>%
  # Compute a good label
  mutate(label = paste0(ST.2,"\n", round(prop*100, 2), "%"))



# Make the plot
ST_pie <- 
  ggplot(ST_total_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ST.2)) +
  geom_rect() +
  #geom_label( x=5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_manual(values = STCOLORS) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


ST_pie

### Bar per region ####

pST_Region <- 
  pass.genomic.db %>% 
  mutate(ST.2 = case_when(
    ST.1 %in% selected_STs_2 ~ ST.1, TRUE  ~ "Other")
    ) %>%
  count(ST.2, Region) %>% 
  ggplot() +
  aes(y= factor(Region, levels=rev(RegionORDER)), x = n) +
  geom_col(
    aes(
    fill = ST.2),
    position = "fill") +
  xlab("Type") + ylab(NULL) +
  scale_fill_manual(values = STCOLORS, 
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



#### Map ####

country_selected <-
  pass.genomic.db %>% 
  count(Country2) %>%
  mutate(prop = round(n/sum(n)*100, 2)) %>% 
  arrange(desc(prop)) %>% 
  filter(prop > 0.9 & Country2 != "Unknown") %>% 
  pull(Country2)

country_selected

country_selected_LatLon <-
  pass.genomic.db %>% 
  filter(Country2 %in% country_selected) %>% 
  group_by(Country2) %>% 
  filter(row_number() == 1) %>%
  mutate(Lat=mean(Lat), Lon= mean(Lon)) %>% 
  select(Country, Lat, Lon)


country_pie_df <- 
  pass.genomic.db %>% 
  filter(Country2 %in% country_selected ) %>%
  mutate(ST.2 = case_when(ST.1 %in% selected_STs_2 ~ ST.1, TRUE  ~ "Other")) %>%
  group_by(Country2, ST.2) %>% 
  count() %>% 
  group_by(Country2) %>%
  mutate(Total= sum(n), Prop = n/sum(n)*100) %>% 
  ungroup() %>% 
    select(-n) %>% 
  pivot_wider(names_from = ST.2, values_from = Prop ) %>% 
  left_join(., country_selected_LatLon, by="Country2") %>% 
  mutate_at(vars(selected_STs_2), ~replace(., is.na(.), 0))
  
country_pie_df 
#---

  
  library(scatterpie)
  p_mapl <-  
    ggplot(map_data("world")) +
    geom_polygon(aes(x = long, y = lat, group = group),
                 color = "black",
                 fill = "white") +
  
  geom_scatterpie(data = country_pie_df, 
                  aes(x = Lon, y = Lat, group= Country
                      #, r=Total/100
                      ),
                  
                  cols = (append(selected_STs_2, "Other"))) +
  
      coord_equal() +
      scale_fill_manual(values = STCOLORS) +
      
      geom_text(data = country_pie_df,
                aes(x = Lon, y = Lat-7, label= Country)) +
      theme_void()+
     theme(
      legend.position = "none"
    ) +
      geom_scatterpie_legend(country_pie_df$Total/100,
                             x=-160, y=-55, 
                             labeller=function(x) 100*x)



#### Year #####

STs_year_barp <- 
  ncbi.genomic.db %>%
  filter(Collection_year > 2005 & Collection_year < 2021) %>% 
  group_by(ST.1, Collection_year) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    y =  n,
    x = Collection_year,
    fill = forcats::fct_explicit_na(factor(ST.1, levels = rev(selected_STs_2)), "Other")
  ) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = STCOLORS) +
  labs(y = NULL, x = "Geographic Region", fill = "cgMLST sL")+
  theme(legend.position = "bottom") 


STs_year_barp 
#### SL year ####  
  
ncbi.genomic.db %>%
  filter(Collection_year > 2010 & Collection_year < 2021) %>% 
  filter(ST.1 %in% selected_STs_2) %>%
  filter(cgMLST_Sublineage != "SL10009;SL14;SL258") %>% 
  group_by(ST.1, Collection_year, cgMLST_Sublineage) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    y =  n,
    x = Collection_year,
    fill = forcats::fct_explicit_na(factor(ST.1, levels = rev(selected_STs_2)), "Other")
  ) +
  geom_area()+
  scale_fill_manual(values = STCOLORS) +
  labs(y = NULL, x = "Geographic Region", fill = "cgMLST sL")+
  theme(legend.position = "bottom") +
  facet_grid(vars(cgMLST_Sublineage)) 


SLs_year_barp <- 
  pass.genomic.db %>% 
  mutate(
    cgMLST_Sublineage2 = case_when(
      cgMLST_Sublineage %in% selected_cgMLST_SL ~ cgMLST_Sublineage, 
      TRUE  ~ "Other"
    )) %>%
  filter(Collection_year > 2005 & Collection_year < 2021) %>% 
  group_by(cgMLST_Sublineage2, Collection_year) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    y =  n,
    x = Collection_year,
    fill = cgMLST_Sublineage2
  ) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = SLCOLORS) +
  labs(y = NULL, x = "Geographic Region", fill = "cgMLST sL")+
  theme(legend.position = "bottom") 


#### ST by Region ####

selected_STs_2 <-
  ncbi.genomic.db %>%
  count(ST.1) %>%
  slice_max(order_by = n,
            n = 20,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(ST.1)


library(forcats)
p1_ST_Region <-
  pass.genomic.db %>%
  group_by(ST.1, Region) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    x = n,
    y = forcats::fct_explicit_na(factor(ST.1, levels = rev(selected_STs_2)), "Other"),
    fill = factor(Region, levels = rev(RegionORDER))
  ) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = RegionCOL) +
  labs(y = NULL, x = "Genomes", fill = "Region")+
  theme(legend.position = "none") 

p1_ST_Region
#saveplot("Regionlegend", p1_ST_Region, "w")



#### 5 most found STs per region####

selected_STs_5 <-
  pass.genomic.db %>%
  filter(Region != "Unknown") %>%
  group_by(Region) %>%
  count(ST.1) %>%
  slice_max(order_by = n,
            n = 5,
            with_ties = T) %>%
  filter(n > 5) %>%  
  arrange(desc(n)) %>%
  pull(ST.1)


ST_per_region_df <-
  pass.genomic.db %>%
  group_by(Region) %>%
  mutate("Genomes" = n()) %>%
  group_by(Region, ST.1) %>%
  mutate(n = n()) %>%
  select(Region, ST.1, "Genomes" , n) %>%
  distinct() %>%
  group_by(Region) %>%
  mutate(prop = round(n / sum(n) * 100, digits = 2)) %>%
  filter(ST.1 %in% selected_STs_5) %>%
  mutate("Other" =  100 - sum(prop)) %>%
  pivot_wider(id_cols = -n, names_from = ST.1, values_from = prop) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  arrange(factor(Region, levels = RegionORDER)) %>%
  select("Genomes", c(selected_STs_5), "Other")


# ST order

STORDER_ALL2 <- 
  pass.genomic.db %>%
  filter(ST.1 %in% selected_STs_5) %>%
  count(ST.1) %>%
  arrange(desc(n)) %>%
  pull(ST.1) %>% 
  append("Other")

# St label colors

STCOLORS_reordered <- STCOLORS[match(STORDER_ALL2, names(STCOLORS))]
STCOLORS_reordered <-
  ifelse(is.na(STCOLORS_reordered), 
         "black", 
         STCOLORS_reordered)

ylabcolors <- 
  as.vector((STCOLORS_reordered) )


#plot
ST_per_region_plot <-
  ST_per_region_df %>%
  pivot_longer(!Region, names_to = "ST.1", values_to = "prop") %>%
  filter(ST.1 != "Genomes")  %>%
  filter(Region != "Unknown")  %>%
  mutate(prop_category = cut(
    prop,
    breaks = c(-Inf, 2, 10, 25, 100),
    labels = c("0-2", "2-10", "10-25", "25-100")
  ))  %>%
  ggplot(aes(
    x = factor(Region, levels = (RegionORDER)),
    y = factor(ST.1, levels = rev(STORDER_ALL2))
  )) +
  geom_point(aes(colour = prop_category), size = 10) +
  labs(x = NULL, y = NULL) +
  geom_text(aes(label = round(prop, 1)), 
            color = "white", size = 3) +  # Add text labels
  scale_colour_manual(values = c("#cdd7d6", "#64acee", "#1a4369", "#db3a34"),
                      guide = "legend") +
  scale_x_discrete(position = "bottom")  +
  #scale_y_discrete(limits=rev)+
theme(legend.position = "bottom",
        axis.text.x = 
          element_text(colour = rev(ylabcolors))) +
  coord_flip()

ST_per_region_plot


#### gt table #####
library(gt) 
library(gtExtras)


ST_per_region_df  %>%
  as.data.frame() %>%
  gt() %>%
  gtExtras::gt_color_rows(
    ST11:ST86,
    domain = c(0, 42),
    palette = c("white", "#9a170d")
  )  %>%
  gtExtras::gt_color_rows(
    Other,
    domain = c(20, 80),
    palette = c("white", "#045275"),
    direction = 1
  ) #%>%
#gtsave(., paste0(figpath, "CH4_gt_5STs.docx"))

# Europe
gteurope <-   datos_total %>%
  filter(Continent == "Europe" & Region != "NLSAR") %>%
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
  group_by(Region) %>% count(ST.1) %>%
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
  facet_wrap( ~ factor(Region, levels = RegionORDER), nrow = 2) +
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
  facet_wrap( ~ factor(Region, levels = RegionORDER), nrow = 2)  +
  #theme_void()+
  theme(
    panel.grid.major.x = element_blank() ,
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank() ,
    legend.position = "none"
  )




#### Diversity - rarefaction curves #####
library(vegan)
library(ggrepel)


dune2 <-
  pass.genomic.db %>%
  count(Region, ST.1) %>%
  pivot_wider(names_from = ST.1, values_from = n) %>%
  mutate_if(is.numeric, ~ replace_na(., 0)) %>%
  mutate_if(is.integer, as.numeric) %>%
  remove_rownames %>% 
  column_to_rownames(var = "Region") %>%
  as.data.frame()



str(dune2)

dfD <-
  vegan::diversity(dune2, index = "simpson") %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Region") %>% 
  rename(Div = ".") %>%
  mutate(text = paste0(Region, " (", as.character(round(Div, 2)), ")"))

colnames(dfD)
spAbund <- rowSums(dune2)
raremin <- min(rowSums(dune2))
sRare <- rarefy(dune2, raremin)
rcdf <- rarecurve(dune2,  label = T, tidy = T)

data_ends <-
  rcdf %>% 
  group_by(Site) %>% 
  slice_max(order_by = c(Sample) , n = 1) %>% 
  full_join(., dfD, by = c("Site" = "Region"))

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
  
  scale_color_manual(values = RegionCOL)  +
  scale_fill_manual(values = alpha(RegionCOL, 0.8))  +
  ggrepel::geom_label_repel(
    data = filter(data_ends,  Site != "Unknown"),
    aes(
      label = text,
      x = Sample,
      fill = (Site),
      y = Species ,
      family = "Montserrat Medium"
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
  theme(legend.position = "bottom") +
  ylab("Sequence types") + xlab("Genomes")


rfcurves

#saveplot(filename = "CH4_rarefactioncures_ST", plotxsad, size = 85)


#### Figure ####

ggarrange( 
  ggarrange(
    pST_Region, 
    p_mapl, 
    nrow  = 1,
    widths = c(0.3, 0.7)  ),  

  ggarrange(
    STs_year_barp,
    SLs_year_barp ,
    nrow =   1,
    align = "h"),

  ncol = 1, 
  heights = c(0.45, 0.45),
  align = "v", 
  common.legend = TRUE
)

 
####figure2 ####

ggarrange(
  ST_per_region_plot,
  rfcurves,
ncol = 2, 
widths = c(0.65, 0.35),
align = "h"
)

## AMR + VIR -----------------------------------------------------------
### Total pie ####
cat_total_df <-
  ncbi.genomic.db %>% 
  group_by(strain_type2) %>%
  count() %>% 
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop))  %>%
  # Compute the cumulative percentages (top of each rectangle)
  mutate(ymax = cumsum(prop)) %>%
  # Compute the bottom of each rectangle
  mutate(ymin = lag(ymax, default = 0)) %>%
  # Compute label position
  mutate(labelPosition = (ymax + ymin) / 2) %>%
  # Compute a good label
  mutate(label = paste0(strain_type2,"\n", round(prop*100, 2), "%"))


# Make the plot
ST_pie <- 
  ggplot(cat_total_df, 
         aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=strain_type2)) +
  geom_rect() +
  scale_fill_manual(values = RESVIRCOL2) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


ST_pie

### Bar per region ####

pRESVIR_Region <- 
  pass.genomic.db %>% 
  count(strain_type2, Region) %>% 
  ggplot() +
  aes(y= factor(Region, levels=rev(RegionORDER)), x = n) +
  geom_col(aes(fill = factor(strain_type2, levels = rev(c("WT", "ESBL", "Carbapenemase",  "Virulent", "Convergent")))) , position = "fill") +
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


### type per phylo ####
 type_SL <- 
pass.genomic.db %>% 
  mutate(
    cgMLST_Sublineage2 = case_when(
      cgMLST_Sublineage %in% selected_cgMLST_SL ~ cgMLST_Sublineage, 
      TRUE  ~ "Other"
      )) %>%
  count(strain_type2, cgMLST_Sublineage2) %>% 
  ggplot() +
  aes(
    y =  n,
    x = cgMLST_Sublineage2,
    fill = strain_type2 
  ) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = RESVIRCOL2) +
  labs(y = NULL, x = "Geographic Region", fill = "cgMLST sL")+
  theme(legend.position = "right") 

type_SL

### per year ####
type_year <- 
  pass.genomic.db %>% 
  filter(Collection_year > 2003 & Collection_year < 2021) %>% 
  group_by(strain_type2, Collection_year) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    y =  n,
    x = Collection_year,
    fill = strain_type2
  ) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = RESVIRCOL2) +
  labs(y = NULL, x = "Geographic Region", fill = "Strain type")+
  theme(legend.position = "right") 




# AMR------------------------------
file_Res <-
  read.csv("data/AL_AMRfinderplus_edited.tsv",
           sep = "\t",
           header = T) %>%
  distinct() %>%
  filter(Scope  != "Scope") %>%
  mutate(Class = case_when(is.na(Class) ~ Element.type, TRUE ~ Class)) %>%
  mutate(Name = str_remove_all(Name, "_ass.*"))    %>%
  mutate(
    X..Coverage.of.reference.sequence = as.numeric(X..Coverage.of.reference.sequence),
    X..Identity.to.reference.sequence = as.numeric(X..Identity.to.reference.sequence)
  ) %>%
  mutate(
    Gene.symbol2 = case_when((
      X..Coverage.of.reference.sequence < 100 &
        X..Identity.to.reference.sequence < 100
    ) ~ paste0(Gene.symbol, "*?"),
    X..Coverage.of.reference.sequence < 100 ~ paste0(Gene.symbol, "*"),
    X..Identity.to.reference.sequence < 100 ~ paste0(Gene.symbol, "?"),
    TRUE ~ Gene.symbol
    )
  ) %>%
  filter(Element.type == "AMR") %>%
  select(Name, Subclass, Gene.symbol2) %>% pivot_wider(
    .,
    names_from = Subclass,
    values_from = Gene.symbol2,
    values_fn  = ~ paste(., collapse = "; ")
  ) %>%
  right_join(., pass.genomic.db, by = c("Name" = "sample_name"))


# AMR tile####
GROUPCarbCOUNT = file_Res %>% group_by(Region,) %>% summarise(n=n())

bla_c = c( "\\SHV\\b")
bla_a = c("\\OXA-1\\b", "\\TEM-1D\\b")
esbl = c("\\CTX-M-15\\b", "\\CTX-M-1\\b", "\\CTX-M-3\\b", "\\SHV-12\\b" )  
bla_carb = c( "\\IMP\\b","\\VIM\\b","\\NDM\\b", "\\OXA-48\\b", "\\KPC\\b")


ARGORDER=c("SHV",  "OXA1", "TEM1D", "SHV12","CTXM1", "CTXM3",  "CTXM15", "OXA48", "KPC","NDM", "VIM", "IMP" )

ARGORDER_Prev=c("SHV",  "OXA1", "TEM1D", "SHV12", "CTXM1", "CTXM3", "CTXM15", "OXA48", "KPC", "NDM", "VIM", "IMP" )

ARGlabels=c("SHV",   "OXA-1", "TEM-1D",  "SHV-12","CTX-M-1","CTX-M-3","CTX-M-15", "OXA-48","KPC-LIKE", "NDM-like",  "VIM-like", "IMP-like")


GCC_genes = GROUPCarbCOUNT 

for (x in esbl){
  name = (gsub("[-|\\|b]", "", x))
  
  temp = file_Res %>% 
    count(Region, str_detect(Bla_ESBL_acquired, x)) %>% 
    rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
    select(Region, !!name)
  
  GCC_genes = merge(GCC_genes, temp, all = T)
}
for (x in bla_a){
  name = (gsub("[-|\\|b]", "", x))
  
  temp = file_Res %>% 
    count(Region,str_detect(Bla_acquired, x)) %>% 
    rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
    select(Region, !!name)
  
  GCC_genes = merge(GCC_genes, temp, all = T)
}
for (x in bla_c){
  name = (gsub("[-|\\|b]", "", x))
  
  temp = file_Res %>% 
    count(Region,  str_detect(Bla_chr, x)) %>% 
    rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
    select(Region, !!name)
  
  GCC_genes = merge(GCC_genes, temp, all = T)
}
for (x in bla_carb){
  name = (gsub("[-|\\|b]", "", x))
  
  temp = file_Res %>% 
    count(Region,  grepl(x, Bla_Carb_acquired)) %>% 
    rename(TF = 2, !!name := 3) %>%  filter(TF == "TRUE") %>% 
    select(Region, !!name)
  
  GCC_genes = merge(GCC_genes, temp, all = T)
}


dftile2 = GCC_genes %>% select(-n,everything()) %>% 
  mutate_if(is.numeric, funs(./n*100))  %>%
  select(-n) %>% replace(is.na(.), 0)

pGENESTILE <- 
  dftile2 %>%
  pivot_longer(-c(Region), names_to = "genes", values_to = "counts") %>%
  ggplot(aes(x=factor(genes, levels=ARGORDER_Prev), y=factor(Region, levels=rev(RegionORDER)), fill=counts)) + 
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


## Vir tile ####

tempo2 <- 
  file_Res %>% select( Region, Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC
                       
  ) %>%
  mutate( across(
    .cols = c(-Region, ),
    .fns = ~ str_replace_all(
      string = ., 
      pattern = "-", 
      replacement = "0"))) %>%
  mutate(across(.cols=c(-Region), 
                .fns = ~ case_when(. == "0" ~ 0, 
                                   . != "0" ~ 1))) %>%
  #mutate(Bla_ESBL_acquired = case_when(#Bla_Carb_acquired == "1" ~ 0,
  #                                    Bla_ESBL_acquired == "0" ~ 0, 
  #                                     Bla_ESBL_acquired != "0" ~ 1)) %>%
  
  #filter(Region == "NLSAR" & Bla_Carb_acquired == 1) %>% count()
  group_by(Region) %>%
  mutate_if(is.numeric, funs(sum(.)))  %>%
  mutate(n=n()) %>%
  distinct() %>%     
  select(-n,everything())    %>%
  mutate_if(is.numeric, funs(./n*100)) %>%
  select(-n)


ptempo2 <- 
  tempo2 %>%
  pivot_longer(-c(Region), names_to = "genes", values_to = "counts") %>%
  ggplot(aes(x=(factor(genes, levels= rev(c( "Colibactin", "RmpADC","Salmochelin", "Aerobactin", "Yersiniabactin")))), y=factor(Region, levels=rev(RegionORDER)), fill=counts)) + 
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


   tempo2 <- 
      file_Res %>% select( Region, Yersiniabactin, Colibactin, Aerobactin, Salmochelin, RmpADC
                           
      ) %>%
      mutate( across(
        .cols = c(-Region, ),
        .fns = ~ str_replace_all(
          string = ., 
          pattern = "-", 
          replacement = "0"))) %>%
      mutate(across(.cols=c(-Region), 
                    .fns = ~ case_when(. == "0" ~ 0, 
                                       . != "0" ~ 1))) %>%
      #mutate(Bla_ESBL_acquired = case_when(#Bla_Carb_acquired == "1" ~ 0,
      #                                    Bla_ESBL_acquired == "0" ~ 0, 
      #                                     Bla_ESBL_acquired != "0" ~ 1)) %>%
      
      #filter(Region == "NLSAR" & Bla_Carb_acquired == 1) %>% count()
      group_by(Region) %>%
      mutate_if(is.numeric, funs(sum(.)))  %>%
      mutate(n=n()) %>%
      distinct() %>%     
      select(-n,everything())    %>%
      mutate_if(is.numeric, funs(./n*100)) %>%
      select(-n)
    
    
    ptempo2 <- 
      tempo2 %>%
      pivot_longer(-c(Region), names_to = "genes", values_to = "counts") %>%
      ggplot(aes(x=(factor(genes, levels= rev(c( "Colibactin", "RmpADC","Salmochelin", "Aerobactin", "Yersiniabactin")))), y=factor(Region, levels=rev(RegionORDER)), fill=counts)) + 
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

    
    
### figure ####
    
    
    plot <-  
      ggarrange(
      ggarrange(
        pRESVIR_Region,
        pGENESTILE,
        ptempo2,
        NULL,
        nrow = 1,
        align = "h",
        legend = "none", 
        widths = c(0.6, 1, 0.4, 0.1)

      ),
      ggarrange(
      type_year, 
      type_SL,
      nrow= 1,
      common.legend = TRUE, 
      legend = "none", 
      align = "h",
      widths = c(0.3, 0.5, 0.5)
      ),
      nrow=2, 
      common.legend = T,
      legend = "bottom"
      )
    
    plot    

# Carbapenemases ######