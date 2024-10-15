####
# Neris Garcia
# THEME, COLORS and ETC:..

# THEME ------------------------------------------------------------------------
# Install font monserrat in your conmputer
library(extrafont)
font_import(promp = F, pattern = "Montserrat-Medium")

loadfonts(device = "win")

library(ggplot2)
theme_set(theme_classic(base_family = "Montserrat Medium"))

theme_update(
  text = element_text(family = "Montserrat Medium"),
  axis.text.x = element_text(color = "black"),
  axis.text.y = element_text(color = "black"),
  axis.ticks = element_line(color = "black"),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8)
)

update_geom_defaults("text", list(size = 3, family = theme_get()$text$family))

# Colors ####

  TYPECOL = c(SKPCV="#DC3977", DB="#045275")

  HOSPCOL = c(HGUV = "#ffa500", HAV ="#fcde9c", HCV="#f0746e", HGE="#7c1d6f", 
            HGUA="#02764b", HGUC ="#089099", HLF="#7ccba2", HRA="#dc3977", HM="#C5C5C5", 
            HUPLF="#7ccba2", HCUV = "#f0746e",  HGUE="#7c1d6f", HULR = "#dc3977" )
  
  STCOLORS = c(ST307="#93c47d",  ST11 = "#64acee", ST437 = "#21E1FF", Other="grey",
               ST15="#935fca", ST147="#d5a6bd", ST405="#ffd966", ST101 = "#e69138",  
               ST219="#db3a34", ST29="#274e13", ST33="#52796f", ST17="#744700",   ST45="#9a170d",
               ST512="#1e3f9d",  ST392="#c90076", ST258="#041c2e",ST902= "#555555", 
               ST16="#b69561", ST14="#442C5D", ST13="#37A66F", ST37="#4E0707", ST395="#A3D7CF")
  
  RESCOLORS = c( "3GC"="#1ea0ae", 
                 threeres = "#084c61",
                 Carba="#db3a34",
                 fourres = "darkred",
                 Susceptible = "darkgray")
  
  GENECOLORS = c("blaVIM" = "#F0ABA8", 
                  "blaOXA"= "#E05652",
                  "blaNDM"= "#D02A25",
                  "blaKPC" ="#9C201C",
                 #681512
                  "twocarb" ="black", 
                 "Ompk35"="#1ea0ae", 
                 "Ompk36" = "#084c61",
                  "-" = "grey")
  
  
  GENECOLORS = c("blaGES" =  "darkgray",
                  "blaIMI" =  "darkgray",
                  "blaIMP" =   "#E05652",
                  "blaKPC" =  "#681512",
                  "blaNDM" =  "#D02A25",
                  "blaOXA" =  "#9C201C",
                  "blaVIM" =  "#F0ABA8",
                  "two" = "black",
                  "ompK35" =  "#1ea0ae",
                  "ompK36" =  "#084c61",
                 "NA" = "grey")
  
  GENECOLORS1 = c("VIM-1" ="#735751", 
                  "OXA-48"="#db3a34SURV",
                  "NDM-1"= "#9a170d",
                  "NDM-23"="#9a170d",
                  "KPC-3" ="#340A09",
                  "NDM-1+OXA-48" ="black", 
                  "NDM-23+OXA-48" ="black", 
                  "-" = "grey")

  
  
  RegionCOL <-
    c(
      SKPCV = "#C0A8CB",
      Spain = "#dc3977",
      Europe = "#7c1d6f",
      Americas = "#fcde9c" ,
      Africa = "#ffa500",
      Asia = "#7ccba2",
      Oceania = "#02764b" ,
      Unknown = "lightgray"
    )
  
  
  RESVIRCOL <-
    c(
      WT = "#cdd7d6",  
      ESBL = "#db3a34", 
      CARBA = "#9a170d" ,
      VIR = "#045275" ,
      "ESBL + VIR" = "#93c47d",
      "CARBA + VIR" = "#93c47d",
      "CARBA + COL" = "#93c47d",
      "CARBA + COL + VIR" = "#93c47d"
    )
  
  RESVIRCOL2 <-
    c(
      WT = "#cdd7d6",  
      ESBL = "#045275", 
      Carbapenemase = "#db3a34" ,
      Virulent = "#ffc857" ,
      Convergent= "#93c47d"
    )

#ORDER ####
  
  ## Hospital #####
  HOSPORDER = c("HDM", "HGUC", "HUCV", "HAV", "HUPLF", "HGUV", "HULR", "HGUA", "HGUE")
  
  HOSPORDER2 = c("HDM", "HGUC", "HUPLF", "HGUA",  "HCUV", "HAV","HGUV", "HULR", "HGUE")
  
  HOSPORDER3 = c("Non-SKPCV", "HDM", "HGUC", "HUPLF", "HGUA",  "HCUV", "HAV","HGUV", "HULR", "HGUE")
  
  ## ST #####
  
  STORDERv1 = c("Other", "ST45", "ST392", "ST17", "ST29",  "ST219", "ST101",
                "ST405", "ST147", "ST15",  "ST11", "ST307")
  
  STORDERv1_2 = c("Other", "ST512", "ST45",   "ST392", "ST17", "ST29",  "ST219", 
                  "ST101", "ST405", "ST147", "ST15",   "ST258", "ST11", "ST307")
  
  STORDERv2 = c("ST307",  "ST11",  "ST15",  "ST147","ST405", "ST101", "ST219",  
                "ST29", "ST17",  "ST392", "ST45", "Other")  
  
  STORDERv3 = c("ST307",  "ST11",  "ST15",  "ST147", "ST405",  "ST219", 
                "ST29",  "ST17",  "ST392", "ST45","ST14", "ST37",  "ST902",
                "ST13", "ST512", "ST395", "ST33", "Other") 
  STORDERv4 = c("Other", "ST307",  "ST11",  "ST15",  "ST147", "ST405",  "ST219", 
                "ST29",  "ST17",  "ST392", "ST45","ST14", "ST37",  "ST902",
                "ST13", "ST512", "ST395", "ST33", NA_character_ ) 
  
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
  
  ## SL ######
  
  SLCOLORS = c("SL2258;SL307;SL322"="#93c47d", 
               "SL10009;SL14;SL258" = "#64acee", 
               "SL14;SL147;SL889"="#d5a6bd", #"#c90076", 
               "SL15"="#935fca",
               "SL14"="#442C5D", 
               "SL101;SL3157"  = "#e69138",  
              "SL17;SL337" ="#b69561", #"#744700", 
              "SL1245;SL14;SL45"="#9a170d",
              "SL10009;SL14;SL37" ="#4E0707",
              "SL23;SL3010;SL3321"= "#555555", 
              Other="lightgrey")
  
  
  ## Resistance #####
  
  RESTYPEORDER=c("Susceptible", "Bla", "Omp", "AmpC", "ESBL", "Carb")
  
  CARBTYPEORDER=c("Susceptible","Omp", "ESBL",   "Omp+ESBL","AmpC","Omp+AmpC","CarbaEnzyme"                 )
  
  ATBORDER= c( "NT", "S", "I", "R")
  
  
  ATBRESORDER=c("BlaR" , "3GCR", "CarbR", "Carb_3GCR")
  
  ATBRESORDER = c("Susceptible", "3GC", "threeres", "Carba", "fourres")
  
  RESTYPEORDER <- c("Susceptible", "Omp", "Bla", "ESBL", "Carb")
  
  
  ## Region ####
  
  RegionORDER <-
    c("SKPCV",
      "Spain",
      "Europe",
      "Americas" ,
      "Africa" ,  
      "Asia",
      "Oceania",
      "Unknown")
  
  
  RegionORDER <-
    c("SKPCV",
      "Spain",
      "Europe",
      "Asia",
      "Americas" ,
      "Africa" ,
      "Oceania",
      "Unknown")
  