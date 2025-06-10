############################################################
#              DATAINHÄMTNING FÖR BEFOLKNINGSPROGNOS       #
############################################################
#                                                          #
#    Hämtar demografisk data från SCB:s API                #
#                                                          #
#    Datatyper som hämtas:                                 #
#    - Befolkning (medelfolkmängd och totalbefolkning)     #
#    - Födda                                               #
#    - Döda                                                #
#    - In- och utflyttningar (inrikes)                     #
#    - In- och utvandring                                  #
#    - Länsgränsflyttningar                                #
#    - Riksprognosdata                                     #
#                                                          #
#    Förväntar följande variabler från huvudskriptet:      #
#    - år_historisk: vektor med historiska år              #
#    - Kommuner: vektor med kommunkoder                    #
#    - Län: namn på länet                                  #
#    - år_prognos: vektor med prognosår                    #
#                                                          #
############################################################

### Nödvändiga paket ###
library(tidyverse)
library(pxweb)
library(purrr)
library(data.table)
library(writexl)
library(readxl)
library(zoo)
library(openxlsx)

# ===========================================================
# HJÄLPFUNKTIONER
# ===========================================================

# Förhindra vetenskaplig notation för tydlighet
options(scipen = 999)

# Funktion för att standardisera kolumnnamn (kommundata)
namnge_och_rensa_kolumner <- function(data) {
  # Kontrollera att dataramen har rätt antal kolumner
  if (ncol(data) != 5) {
    stop("Dataframe måste ha exakt fem kolumner")
  }
  
  # Standardisera kolumnnamn
  names(data) <- c("Region", "Ålder", "Kön", "År", "Värde")
  
  # Extrahera numerisk ålder från textsträng
  data$Ålder <- as.numeric(gsub("[^0-9]", "", data$Ålder))
  
  return(data)
}

# Funktion för att standardisera kolumnnamn (riksdata)
namnge_och_rensa_kolumner_riket <- function(data) {
  # Kontrollera att dataramen har rätt antal kolumner
  if (ncol(data) != 4) {
    stop("Dataframe måste ha exakt fyra kolumner")
  }
  
  # Standardisera kolumnnamn
  names(data) <- c("Kön", "Ålder", "År", "Värde")
  
  # Extrahera numerisk ålder från textsträng
  data$Ålder <- as.numeric(gsub("[^0-9]", "", data$Ålder))
  
  return(data)
}

# Kontroll av datakonsistens
kontrollera_tabell_längder <- function(kommun_lista) {
  # Definiera grupper med förväntad samma längd
  group1 <- c("medelfolkmangd", "doda", "invandring", "utvandring", "inrikes_inflyttade", "inrikes_utflyttade", "totfolkmangd")
  group2 <- c("fodda", "medelfolkmangd_modrar", "totfolkmangd_modrar")
  
  group1_längder <- sapply(kommun_lista[group1], nrow)
  group2_längder <- sapply(kommun_lista[group2], nrow)
  
  if (length(unique(group1_längder)) == 1 && length(unique(group2_längder)) == 1) {
    return("Tabellerna är korrekt formaterade för riskberäkningar!")
  } else {
    return("Varning: Tabellerna har olika längder och är inte korrekt formaterade för riskberäkningar.")
  }
}

# Funktion för beräkning av glidande medelvärde
beräkna_genomsnitt <- function(värde, width) {
  rollapply(värde, width = width, FUN = mean, fill = NA, align = 'right')
}

# Funktion för spline-utjämning av data
beräkna_spline <- function(data, y_col) {
  spline_func <- smooth.spline(data$Ålder, data[[y_col]])
  pmax(predict(spline_func, data$Ålder)$y, 0)
}

# Funktion för åldersgruppering (används vid dödsriskberäkningar)
gruppera_alder <- function(alder) {
  case_when(
    alder == 0 ~ "0",
    alder >= 1 & alder <= 4 ~ "1-4",
    alder >= 5 & alder <= 89 ~ paste0(5 * floor((alder - 5) / 5) + 5, "-", 5 * floor((alder - 5) / 5) + 9),
    alder >= 90 ~ "90+"
  )
}

# Funktion för beräkning av glidande summa
beräkna_summor <- function(värde, width) {
  rollapply(värde, width = width, FUN = sum, fill = NA, align = 'right')
}

# Funktion för att skapa mappning mellan åldersgrupper och enskilda åldrar
skapa_aldersmappning <- function() {
  tibble(
    Åldersgrupp = c("0", "1-4", paste0(seq(5, 85, 5), "-", seq(9, 89, 5)), "90+"),
    Ålder = list(0, 1:4, 5:9, 10:14, 15:19, 20:24, 25:29, 30:34, 35:39, 40:44, 
                 45:49, 50:54, 55:59, 60:64, 65:69, 70:74, 75:79, 80:84, 85:89, 90:100)
  ) |> 
    unnest(Ålder)
}

# Skapa åldersmappningen
aldersmappning <- skapa_aldersmappning()

# ===========================================================
# DATAINHÄMTNING - KOMMUNDATA
# ===========================================================

message("\n📊 STARTAR DATAINHÄMTNING...")
message(paste("  - Antal geografier:", length(Kommuner)))
message(paste("  - Historiska år:", paste(range(år_historisk), collapse = " - ")))
message(paste("  - Prognosår:", paste(range(år_prognos[1:10]), collapse = " - "), "\n"))

# Befolkning medelfolkmängd -----------------------------------------------

message("  Hämtar medelfolkmängd...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("BE0101A5"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101D/MedelfolkFodelsear",
            query = pxweb_query_list)

medelfolkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
medelfolkmangd <- namnge_och_rensa_kolumner(medelfolkmangd)
medelfolkmangd$Variabel <- "Folkmängd"

# Justering för 0-åringar (halvårseffekt)
medelfolkmangd <- medelfolkmangd |> 
  mutate(Värde = case_when(
    Ålder == 0 ~ Värde * 2,
    TRUE ~ Värde
  ))

# Medelfolkmängd mödrar ---------------------------------------------------

medelfolkmangd_modrar <- medelfolkmangd
medelfolkmangd_modrar$Variabel <- "Folkmängd mödrar"
medelfolkmangd_modrar <- medelfolkmangd_modrar |> filter(Ålder >= 15 & Ålder <= 49)
medelfolkmangd_modrar <- medelfolkmangd_modrar |> filter(Kön == "kvinnor")
medelfolkmangd_modrar <- medelfolkmangd_modrar |> select(Region, Ålder, År, Värde, Variabel)

# Totalbefolkning ---------------------------------------------------------

message("  Hämtar totalbefolkning...")

pxweb_query_list <- 
  list("Region"= Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101N1"),
       "Tid"= år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy",
            query = pxweb_query_list)

totfolkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
totfolkmangd <- namnge_och_rensa_kolumner(totfolkmangd)
totfolkmangd$Variabel <- "Total folkmängd"

# Totalbefolkning mödrar --------------------------------------------------

totfolkmangd_modrar <- totfolkmangd
totfolkmangd_modrar$Variabel <- "Total folkmängd mödrar"
totfolkmangd_modrar <- totfolkmangd_modrar |> filter(Ålder >= 15 & Ålder <= 49)
totfolkmangd_modrar <- totfolkmangd_modrar |> filter(Kön == "kvinnor")
totfolkmangd_modrar <- totfolkmangd_modrar |> select(Region, Ålder, År, Värde, Variabel)

# Födda -------------------------------------------------------------------

message("  Hämtar födelsedata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "AlderModer"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101E2"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101H/FoddaK",
            query = pxweb_query_list)

fodda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
fodda <- namnge_och_rensa_kolumner(fodda)
fodda$Variabel <- "Födda"
fodda <- fodda |> group_by(Region, Ålder, År) |> 
  summarize(Värde = sum(Värde))
fodda <- fodda |> filter(Ålder >=15 & Ålder <=49)
fodda <- drop_na(fodda)

# Döda --------------------------------------------------------------------

message("  Hämtar dödsdata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101D8"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaFodelsearK",
            query = pxweb_query_list)

doda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
doda <- namnge_och_rensa_kolumner(doda)
doda$Variabel <- "Döda"

# Invandring --------------------------------------------------------------

message("  Hämtar invandringsdata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AX"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

invandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
invandring <- namnge_och_rensa_kolumner(invandring)
invandring$Variabel <- "Invandring"

# Utvandring --------------------------------------------------------------

message("  Hämtar utvandringsdata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AY"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

utvandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
utvandring <- namnge_och_rensa_kolumner(utvandring)
utvandring$Variabel <- "Utvandring"

# Inrikes inflyttade ------------------------------------------------------

message("  Hämtar inrikes inflyttningsdata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A2"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_inflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
inrikes_inflyttade <- namnge_och_rensa_kolumner(inrikes_inflyttade)
inrikes_inflyttade$Variabel <- "Inrikes inflyttade"

# Inrikes utflyttade ------------------------------------------------------

message("  Hämtar inrikes utflyttningsdata...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A3"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_utflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
inrikes_utflyttade <- namnge_och_rensa_kolumner(inrikes_utflyttade)
inrikes_utflyttade$Variabel <- "Inrikes utflyttade"

# Antalet in- och utflyttningar över länsgräns kommun --------------------

message("  Hämtar länsgränsflyttningar...")

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("000001E7","000001E8","000001EA","000001E9"),
       "Tid"= år_historisk)

# Hämta data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/FlyttFodReg",
            query = pxweb_query_list)

# Konvertera till dataframe 
flyttningar_lansgrans <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
flyttningar_lansgrans <- as_tibble(flyttningar_lansgrans)

# Rensa och strukturera data
flyttningar_lansgrans <- flyttningar_lansgrans |> 
  rename(
    Region = "region",
    Ålder = "ålder", 
    Kön = "kön",
    År = "år",
    Inflyttningar_inom_lan = "Inflyttningar från kommuner inom länet",
    Inflyttningar_ovriga_lan = "Inflyttningar från övriga län",
    Utflyttningar_inom_lan = "Utflyttningar till kommuner inom länet",
    Utflyttningar_ovriga_lan = "Utflyttningar till övriga län"
  ) |>
  mutate(
    # Extrahera numerisk ålder
    Ålder = as.numeric(gsub("[^0-9]", "", Ålder)),
    # Konvertera kön till text
    Kön = case_when(
      Kön == "1" ~ "män",
      Kön == "2" ~ "kvinnor",
      TRUE ~ as.character(Kön)
    )
  )

# Separera inflyttningar och utflyttningar för enklare hantering
inflyttningar_lansgrans_raw <- flyttningar_lansgrans |> 
  select(Region, År, Ålder, Kön, Inflyttningar_inom_lan, Inflyttningar_ovriga_lan) |>
  rename(
    Inom_lan = Inflyttningar_inom_lan,
    Ovriga_lan = Inflyttningar_ovriga_lan
  ) |>
  mutate(
    Total = Inom_lan + Ovriga_lan
  )

utflyttningar_lansgrans_raw <- flyttningar_lansgrans |> 
  select(Region, År, Ålder, Kön, Utflyttningar_inom_lan, Utflyttningar_ovriga_lan) |>
  rename(
    Inom_lan = Utflyttningar_inom_lan,
    Ovriga_lan = Utflyttningar_ovriga_lan
  ) |>
  mutate(
    Total = Inom_lan + Ovriga_lan
  )

# Sammanslagning ----------------------------------------------------------

message("\n  Sammanställer kommundata...")

kommun_lista <- list(
  medelfolkmangd = medelfolkmangd,
  medelfolkmangd_modrar = medelfolkmangd_modrar,
  fodda = fodda,
  doda = doda,
  invandring = invandring,
  utvandring = utvandring,
  inrikes_inflyttade = inrikes_inflyttade,
  inrikes_utflyttade = inrikes_utflyttade,
  totfolkmangd = totfolkmangd,
  totfolkmangd_modrar = totfolkmangd_modrar,
  inflyttningar_lansgrans_raw = inflyttningar_lansgrans_raw,
  utflyttningar_lansgrans_raw = utflyttningar_lansgrans_raw
)

# Rensa bort rader med saknade värden
kommun_lista <- map(kommun_lista, ~ drop_na(.x))
kommun_lista <- map(kommun_lista, as_tibble)

# Kontrollera datakonsistens
kontroll_resultat <- kontrollera_tabell_längder(kommun_lista)
message(paste("  ", kontroll_resultat))

# Spara kommundata
saveRDS(kommun_lista, "Data_underlag/kommun_lista.rds")
message("  ✅ Kommundata sparat i Data_underlag/kommun_lista.rds")

# ===========================================================
# DATAINHÄMTNING - RIKSDATA (PROGNOSUNDERLAG)
# ===========================================================

message("\n📊 HÄMTAR RIKSPROGNOSDATA...")

# Dödstal -----------------------------------------------------------------

message("  Hämtar dödstal för riket...")

pxweb_query_list <- 
  list("Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("000005MX"),
       "Tid"=år_prognos)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401D/BefProgDodstalNb",
            query = pxweb_query_list)

dodstal <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
dodstal <- namnge_och_rensa_kolumner_riket(dodstal)
dodstal$Variabel <- "Dödstal"

# Födelsetal --------------------------------------------------------------

message("  Hämtar födelsetal för riket...")

pxweb_query_list <- 
  list("ModerFodregion"=c("90"),
       "Alder"=c("*"),
       "ContentsCode"=c("000005MY"),
       "Tid"= år_prognos)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401D/BefProgFruktTotNb",
            query = pxweb_query_list)

fodelsetal <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
fodelsetal$Variabel <- "Födelsetal"
fodelsetal <- fodelsetal |> select(ålder, år, Antal, Variabel)
fodelsetal <- rename(fodelsetal, Ålder = "ålder", År = "år", Värde = "Antal")
fodelsetal$Ålder <- as.numeric(gsub("[^0-9]", "", fodelsetal$Ålder))
fodelsetal <- drop_na(fodelsetal)
fodelsetal <- fodelsetal |> select(Ålder, År, Värde, Variabel)

# Invandring --------------------------------------------------------------

message("  Hämtar invandring för riket...")

pxweb_query_list <- 
  list("Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("000005MR"),
       "Tid"= år_prognos)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgOsiktDetNb",
            query = pxweb_query_list)

invandring_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
invandring_riket <- namnge_och_rensa_kolumner_riket(invandring_riket)
invandring_riket$Variabel <- "Invandring"

# Inrikes flyttningar -----------------------------------------------------

message("  Hämtar inrikes flyttningar för riket...")

pxweb_query_list <- 
  list("Alder"=c("*"),
       "Typflyttning"=c("MKommuniL","ÖLandG"),
       "Kon"=c("*"),
       "ContentsCode"=c("000000OM"),
       "Tid"=år_historisk)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttningarInrk",
            query = pxweb_query_list)

inrikesflyttningar_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
inrikesflyttningar_riket <- rename(inrikesflyttningar_riket, Antal = "Antal")
inrikesflyttningar_riket <- inrikesflyttningar_riket |> 
  group_by(ålder, kön, år) |> 
  summarize(Värde = sum(Antal))
inrikesflyttningar_riket$ålder <- as.numeric(gsub("[^0-9]", "", inrikesflyttningar_riket$ålder))
inrikesflyttningar_riket$Variabel <- "Inrikes flyttningar"
inrikesflyttningar_riket <- rename(inrikesflyttningar_riket, Ålder = "ålder", Kön = "kön", År = "år")
inrikesflyttningar_riket <- inrikesflyttningar_riket |> select(Kön, Ålder, År, Värde, Variabel)

# Prognosbefolkning -------------------------------------------------------

message("  Hämtar prognosbefolkning för riket...")

pxweb_query_list <- 
  list("InrikesUtrikes"=c("83"),
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105+"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("000005NO"),
       "Tid"=år_prognos)

# Hämta data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefolkprognRevNb",
            query = pxweb_query_list)

riket_prognosinvånare <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
riket_prognosinvånare <- riket_prognosinvånare |> select(kön, ålder, år, "Antal")
riket_prognosinvånare_grund <- namnge_och_rensa_kolumner_riket(riket_prognosinvånare)

# Sammanslagning ----------------------------------------------------------

message("\n  Sammanställer riksdata...")

riket_lista <- list(
  dodstal = dodstal,
  fodelsetal = fodelsetal,
  invandring_riket = invandring_riket,
  inrikesflyttningar_riket = inrikesflyttningar_riket,
  riket_prognosinvånare_grund = riket_prognosinvånare_grund
)

# Rensa bort rader med saknade värden
riket_lista <- map(riket_lista, ~ drop_na(.x))
riket_lista <- map(riket_lista, as_tibble)

# Spara riksdata
saveRDS(riket_lista, "Data_underlag/riket_lista.rds")
message("  ✅ Riksdata sparat i Data_underlag/riket_lista.rds")

message("\n✅ DATAINHÄMTNING KLAR!")