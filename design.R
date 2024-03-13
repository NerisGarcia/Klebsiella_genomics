####
# Neris Garcia
# THEME, COLORS and ETC:..


# COLORS AND ORDER   ####
## Colors ####

  TYPECOL = c(NLSAR="#DC3977", DB="#045275")

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
  
  
  GENECOLORS1 = c("VIM-1" ="#735751", 
                  "OXA-48"="#db3a34SURV",
                  "NDM-1"= "#9a170d",
                  "NDM-23"="#9a170d",
                  "KPC-3" ="#340A09",
                  "NDM-1+OXA-48" ="black", 
                  "NDM-23+OXA-48" ="black", 
                  "-" = "grey")

  ORIGINCOL <-
    c(
      NLSAR = "#C0A8CB",
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

##ORDER ####
  HOSPORDER = c("HDM", "HGUC", "HUCV", "HAV", "HUPLF", "HGUV", "HULR", "HGUA", "HGUE")
  
  HOSPORDER2 = c("HDM", "HGUC", "HUPLF", "HGUA",  "HCUV", "HAV","HGUV", "HULR", "HGUE")
  
  HOSPORDER3 = c("Non-SKPCV", "HDM", "HGUC", "HUPLF", "HGUA",  "HCUV", "HAV","HGUV", "HULR", "HGUE")
  
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
  
  RESTYPEORDER=c("Susceptible", "Bla", "Omp", "AmpC", "ESBL", "Carb")
  
  CARBTYPEORDER=c("Susceptible","Omp", "ESBL",   "Omp+ESBL","AmpC","Omp+AmpC","CarbaEnzyme"                 )
  
  ATBORDER= c( "NT", "S", "I", "R")
  
  
  ATBRESORDER=c("BlaR" , "3GCR", "CarbR", "Carb_3GCR")
  
  ATBRESORDER = c("Susceptible", "3GC", "threeres", "Carba", "fourres")
  
  
  ORIGINORDER <-
    c("NLSAR",
      "Spain",
      "Europe",
      "Asia",
      "Americas" ,
      "Africa" ,
      "Oceania",
      "Unknown")