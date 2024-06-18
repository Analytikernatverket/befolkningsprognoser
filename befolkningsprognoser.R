### Paket ####
library(tidyverse)
library(pxweb)
library(purrr)
library(data.table)
library(writexl)
library(readxl)
library(zoo)            # här lägger jag in en kommentar

#################### FUNKTIONER ###################
# Funktioner --------------------------------------------------------------


# Funktion för att namnge kolumner och rensa "Ålder"-kolumnen
namnge_och_rensa_kolumner <- function(data) {
  # Kontrollera om dataramen har fem kolumner
  if (ncol(data) != 5) {
    stop("Dataframe måste ha exakt fem kolumner")
  }
  
  # Tilldela de nya namnen
  names(data) <- c("Region", "Ålder", "Kön", "År", "Värde")
  
  # Rensa kolumnen "Ålder" från icke-numeriska tecken
  data$Ålder <- as.numeric(gsub("[^0-9]", "", data$Ålder))
  
  # Returnera den uppdaterade dataramen
  return(data)
}


# Funktion för att namnge kolumner och rensa "Ålder"-kolumnen
namnge_och_rensa_kolumner_riket <- function(data) {
  # Kontrollera om dataramen har fem kolumner
  if (ncol(data) != 4) {
    stop("Dataframe måste ha exakt fem kolumner")
  }
  
  # Tilldela de nya namnen
  names(data) <- c("Kön", "Ålder", "År", "Värde")
  
  # Rensa kolumnen "Ålder" från icke-numeriska tecken
  data$Ålder <- as.numeric(gsub("[^0-9]", "", data$Ålder))
  
  # Returnera den uppdaterade dataramen
  return(data)
}

# Funktion för att namnge kolumner och rensa "Ålder"-kolumnen
namnge_och_rensa_kolumner_2 <- function(data) {
  # Kontrollera om dataramen har fem kolumner
  if (ncol(data) != 5) {
    stop("Dataframe måste ha exakt fem kolumner")
  }
  
  # Tilldela de nya namnen
  names(data) <- c("Region", "Ålder", "Kön", "År", "Värde", "Variabel", "Prognos")
  
  # Rensa kolumnen "Ålder" från icke-numeriska tecken
  data$Ålder <- as.numeric(gsub("[^0-9]", "", data$Ålder))
  
  # Returnera den uppdaterade dataramen
  return(data)
}



## OBS! Skriv in kommun och år nedan

######################## DATAINHÄMTNING (OBS! SKRIV IN KOMMUN; RIKET; ÅR) ###################################

År <- c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
Kommuner <- c("00","1315","1380","1381","1382","1383","1384")

# OBS! Skriv in kommuner och år
# Befolkning medelfolkmängd -----------------------------------------------

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("BE0101A5"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101D/MedelfolkFodelsear",
            query = pxweb_query_list)


medelfolkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

medelfolkmangd <- namnge_och_rensa_kolumner(medelfolkmangd)

medelfolkmangd$Variabel <- "Folkmängd"


# Medelfolkmängd mödrar ---------------------------------------------------


medelfolkmangd_modrar <- medelfolkmangd
medelfolkmangd_modrar$Variabel <- "Folkmängd mödrar"
medelfolkmangd_modrar <- medelfolkmangd_modrar |> filter(Ålder >= 15 & Ålder <= 49)
medelfolkmangd_modrar <- medelfolkmangd_modrar |> filter(Kön == "kvinnor")
medelfolkmangd_modrar <- medelfolkmangd_modrar |> select(Region, Ålder, År, Värde, Variabel)


# Totalbefolkning senaste året --------------------------------------------


pxweb_query_list <- 
  list("Region"= Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101N1"),
       "Tid"= År)

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

pxweb_query_list <- 
  list("Region"=Kommuner,
       "AlderModer"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101E2"),
       "Tid"=År)

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

## Döda

# Döda --------------------------------------------------------------------

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101D8"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaFodelsearK",
            query = pxweb_query_list)

doda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

doda <- namnge_och_rensa_kolumner(doda)

doda$Variabel <- "Döda"



# Invandring --------------------------------------------------------------

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AX"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

invandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

invandring <- namnge_och_rensa_kolumner(invandring)

invandring$Variabel <- "Invandring"

## Utvandring

# Utvandring --------------------------------------------------------------

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AY"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

utvandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

utvandring <- namnge_och_rensa_kolumner(utvandring)

utvandring$Variabel <- "Utvandring"

# Inrikes inflyttade ------------------------------------------------------


pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A2"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_inflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

inrikes_inflyttade <- namnge_och_rensa_kolumner(inrikes_inflyttade)

inrikes_inflyttade$Variabel <- "Inrikes inflyttade"

# Inrikes utflyttade ------------------------------------------------------


pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A3"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_utflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

inrikes_utflyttade <- namnge_och_rensa_kolumner(inrikes_utflyttade)

inrikes_utflyttade$Variabel <- "Inrikes utflyttade"





# Sammanslagning ----------------------------------------------------------

kommun_lista <- list(medelfolkmangd, medelfolkmangd_modrar, fodda, doda, invandring, utvandring, inrikes_inflyttade, inrikes_utflyttade, totfolkmangd, totfolkmangd_modrar)


# Droppa eventuella "tot" och liknande från ålderskategorin, dubbelkolla sedan så att alla tabeller har lika många rader.
kommun_lista <- map(kommun_lista, ~ drop_na(.x))
kommun_lista <- map(kommun_lista, as_tibble)





######################## Riksdata - prognos och inrikesflyttningar (OBS! Uppdatera år!) #################################

År <- c("2024", "2025", "2026", "2027", "2028", "2029", "2030", "2031", "2032", "2033", "2034", "2035", "2036", "2037", "2038", "2039", "2040")


# Dödstal -----------------------------------------------------------------


pxweb_query_list <- 
  list("Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("000005MX"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401D/BefProgDodstalNb",
            query = pxweb_query_list)

dodstal <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

dodstal <- namnge_och_rensa_kolumner_riket(dodstal)

dodstal$Variabel <- "Dödstal"

# Födelsetal --------------------------------------------------------------

pxweb_query_list <- 
  list("ModerFodregion"=c("90"),
       "Alder"=c("*"),
       "ContentsCode"=c("000005MY"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401D/BefProgFruktTotNb",
            query = pxweb_query_list)

fodelsetal <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

fodelsetal$Variabel <- "Födelsetal"

fodelsetal <- fodelsetal |> select(ålder, år, Fruktsamhetstal, Variabel)

fodelsetal <- rename(fodelsetal, Ålder = "ålder", År = "år", Värde = "Fruktsamhetstal")

fodelsetal$Ålder <- as.numeric(gsub("[^0-9]", "", fodelsetal$Ålder))

fodelsetal <- drop_na(fodelsetal)

fodelsetal <- fodelsetal |> select(Ålder, År, Värde, Variabel)


# Invandring --------------------------------------------------------------

pxweb_query_list <- 
  list("Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("000005MR"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgOsiktDetNb",
            query = pxweb_query_list)

invandring_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

invandring_riket <- namnge_och_rensa_kolumner_riket(invandring_riket)

invandring_riket$Variabel <- "Invandring"

# Inrikes flyttningar och inrikes invånare fram till 2040 -----------------------------------------------------


År <- c("2013","2014","2015","2016","2017","2018","2019","2020","2021","2022", "2023")

pxweb_query_list <- 
  list("Alder"=c("*"),
       "Typflyttning"=c("MKommuniL","ÖLandG"),
       "Kon"=c("*"),
       "ContentsCode"=c("000000OM"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/FlyttningarInrk",
            query = pxweb_query_list)

inrikesflyttningar_riket <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

inrikesflyttningar_riket <- rename(inrikesflyttningar_riket, Antal = "Antal inrikes flyttningar ")

inrikesflyttningar_riket <- inrikesflyttningar_riket |> 
  group_by(ålder, kön, år) |> 
  summarize(Värde = sum(Antal))

inrikesflyttningar_riket$ålder <- as.numeric(gsub("[^0-9]", "", inrikesflyttningar_riket$ålder))

inrikesflyttningar_riket$Variabel <- "Inrikes flyttningar"

inrikesflyttningar_riket <- rename(inrikesflyttningar_riket, Ålder = "ålder", Kön = "kön", År = "år")

inrikesflyttningar_riket <- inrikesflyttningar_riket |>  select(Kön, Ålder, År, Värde, Variabel)

# Inrikes invånare

pxweb_query_list <- 
  list("InrikesUtrikes"=c("83"),
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105+"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("000005NO"),
       "Tid"=c("2024","2025","2026","2027","2028","2029","2030","2031","2032","2033","2034","2035","2036","2037","2038","2039","2040"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefolkprognRevNb",
            query = pxweb_query_list)

# Convert to data.frame 
riket_prognosinvånare <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

riket_prognosinvånare <- riket_prognosinvånare |> select(kön, ålder, år, "Folkmängd 31 dec")

riket_prognosinvånare_grund <- namnge_och_rensa_kolumner_riket(riket_prognosinvånare)



# Sammanslagning ----------------------------------------------------------


riket_lista <- list(dodstal, fodelsetal, invandring_riket, inrikesflyttningar_riket, riket_prognosinvånare_grund)





############### RISKMÅTT ##############################
# Lista med kommunernas namn
loop_kommun <- unique(medelfolkmangd$Region)


# Födelsekvoter -----------------------------------------------------------


# Beräkna genomsnittfödslar för alla kommuner
fodelserisker_snitt <- kommun_lista[[3]] %>%
  group_by(Ålder) %>%
  arrange(Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

# Beräkna genomsnittsfolkmängd för kvinnor i barnafödande ålder
folkmangd_moder_snitt <- kommun_lista[[2]] %>%
  group_by(Ålder) %>%
  arrange(Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

# Beräkna födelsekvoter (fruktsamhetstal) för varje kommun och årsgenomsnitt
födelsekvoter <- fodelserisker_snitt %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / folkmangd_moder_snitt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / folkmangd_moder_snitt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / folkmangd_moder_snitt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / folkmangd_moder_snitt$Nioårsgenomsnitt
  )

# Extrahera födelsekvoterna för riket (för att beräkna kommunernas relativa födelsekvotstal som sedan appliceras på prognos)
födelsekvoter_riket <- födelsekvoter |> filter(Region == "Riket")

födelsekvoter_relativ <- födelsekvoter %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / födelsekvoter_riket$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / födelsekvoter_riket$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / födelsekvoter_riket$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / födelsekvoter_riket$Nioårsgenomsnitt
  )

födelsekvoter_relativ <- drop_na(födelsekvoter_relativ)
födelsekvoter_relativ <- födelsekvoter_relativ |> select(Region, Ålder, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt)

# Läs in riksprognosens fruksamhetstal och se till att värdet inte är vetenskapligt

födelsekvoter_riksprognos <- riket_lista[[2]]

födelsekvoter_riksprognos <- as_tibble(födelsekvoter_riksprognos)

födelsekvoter_riksprognos$Värde <- format(födelsekvoter_riksprognos$Värde, scientific = FALSE)

födelsekvoter_riksprognos <- drop_na(födelsekvoter_riksprognos)

# Din funktion för att beräkna genomsnitt
# Skapa en tom lista för att lagra resultaten
results <- list()

# Sätt options för att undvika vetenskaplig notation
options(scipen = 999)

# Iterera över alla unika år
for(year in unique(födelsekvoter_riksprognos$År)) {
  # Iterera över alla unika regioner
  for(region in unique(födelsekvoter_relativ$Region)) {
    # Filtrera data för specifikt år och region
    data_riket <- födelsekvoter_riksprognos %>% filter(År == year)
    data_region <- födelsekvoter_relativ %>% filter(Region == region)
    
    # Se till att Värde är numeriskt och inte i vetenskaplig notation
    data_riket$Värde <- as.numeric(format(data_riket$Värde, scientific = FALSE))
    
    # Kontrollera att vi har matchande åldrar för att undvika fel
    if(nrow(data_riket) == nrow(data_region) && all(data_riket$Ålder == data_region$Ålder)) {
      # Beräkna genomsnitten
      data_riket$Ettårsgenomsnitt <- data_riket$Värde * data_region$Ettårsgenomsnitt
      data_riket$Treårsgenomsnitt <- data_riket$Värde * data_region$Treårsgenomsnitt
      data_riket$Femårsgenomsnitt <- data_riket$Värde * data_region$Femårsgenomsnitt
      data_riket$Nioårsgenomsnitt <- data_riket$Värde * data_region$Nioårsgenomsnitt
      data_riket$Region <- region
      
      # Formatera kolumnerna för att undvika vetenskaplig notation
      data_riket$Ettårsgenomsnitt <- format(data_riket$Ettårsgenomsnitt, scientific = FALSE)
      data_riket$Treårsgenomsnitt <- format(data_riket$Treårsgenomsnitt, scientific = FALSE)
      data_riket$Femårsgenomsnitt <- format(data_riket$Femårsgenomsnitt, scientific = FALSE)
      data_riket$Nioårsgenomsnitt <- format(data_riket$Nioårsgenomsnitt, scientific = FALSE)
      
      # Lägg till i resultaten
      results[[paste(year, region)]] <- data_riket
    }
  }
}

födelserisker_prognosklar <- bind_rows(results)

########### Dödskvoter

# Dödstal -----------------------------------------------------------------


# Beräkna genomsnittfödslar för alla kommuner
dödsrisker_snitt <- kommun_lista[[4]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

# Beräkna folkmängd utifrån faktisk folkmängd 31 december när det gäller dödstal
folkmangd_genomsnitt <- kommun_lista[[9]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))


dödssrisker_prognosklar <-  dödsrisker_snitt %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / folkmangd_genomsnitt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / folkmangd_genomsnitt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / folkmangd_genomsnitt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / folkmangd_genomsnitt$Nioårsgenomsnitt
  )

# Extrahera födelsekvoterna för riket (för att beräkna kommunernas relativa födelsekvotstal som sedan appliceras på prognos)
dödskvoter_riket <- dödssrisker_prognosklar |> filter(Region == "Riket")


dödskvoter_relativ <- dödssrisker_prognosklar %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / dödskvoter_riket$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / dödskvoter_riket$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / dödskvoter_riket$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / dödskvoter_riket$Nioårsgenomsnitt
  )

# Ersätt NaN med 0 i specifika kolumner
dödskvoter_relativ <- dödskvoter_relativ %>%
  mutate(
    Ettårsgenomsnitt = ifelse(is.nan(Ettårsgenomsnitt), 0, Ettårsgenomsnitt),
    Treårsgenomsnitt = ifelse(is.nan(Treårsgenomsnitt), 0, Treårsgenomsnitt),
    Femårsgenomsnitt = ifelse(is.nan(Femårsgenomsnitt), 0, Femårsgenomsnitt),
    Nioårsgenomsnitt = ifelse(is.nan(Nioårsgenomsnitt), 0, Nioårsgenomsnitt)
  )

dödskvoter_relativ <- dödsrisker_snitt %>%
  left_join(folkmangd_genomsnitt, by = c("Kön", "Region", "Ålder", "År"), suffix = c("_död", "_folkm")) %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt_död / Ettårsgenomsnitt_folkm,
    Treårsgenomsnitt = Treårsgenomsnitt_död / Treårsgenomsnitt_folkm,
    Femårsgenomsnitt = Femårsgenomsnitt_död / Femårsgenomsnitt_folkm,
    Nioårsgenomsnitt = Nioårsgenomsnitt_död / Nioårsgenomsnitt_folkm
  ) %>%
  left_join(dödskvoter_riket, by = c("Kön", "Ålder", "År"), suffix = c("", "_riket")) %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / Ettårsgenomsnitt_riket,
    Treårsgenomsnitt = Treårsgenomsnitt / Treårsgenomsnitt_riket,
    Femårsgenomsnitt = Femårsgenomsnitt / Femårsgenomsnitt_riket,
    Nioårsgenomsnitt = Nioårsgenomsnitt / Nioårsgenomsnitt_riket
  ) %>%
  mutate(
    across(starts_with("Ettårsgenomsnitt"), ~ifelse(is.nan(.), 0, .))
  ) %>%
  select(Region, Ålder, Kön, År, Värde, Variabel, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt)


####

dödskvoter_riksprognos <- riket_lista[[1]]

dödskvoter_riksprognos <- as_tibble(dödskvoter_riksprognos)

dödskvoter_riksprognos <- drop_na(dödskvoter_riksprognos)

# Beräkna genomsnittliga dödstal för åldersgruppen 100-105 år
dödstal_100_plus <- dödskvoter_riksprognos %>%
  filter(Ålder >= 100 & Ålder <= 105) %>%
  group_by(Kön, År) %>%
  summarise(GenomsnittligtVärde = mean(as.numeric(Värde), na.rm = TRUE))

# Tilldela genomsnittet till 100-åringar och ta bort rader för 101-105 år
dödskvoter_riksprognos <- dödskvoter_riksprognos %>%
  filter(Ålder < 101) %>%
  left_join(dödstal_100_plus, by = c("Kön", "År")) %>%
  mutate(Värde = ifelse(Ålder == 100, GenomsnittligtVärde, as.numeric(Värde))) %>%
  select(-GenomsnittligtVärde)

# Säkerställ sortering
dödskvoter_riksprognos <- dödskvoter_riksprognos %>%
  arrange(Kön, Ålder, År)

dödskvoter_relativ <- dödskvoter_relativ %>%
  arrange(Kön, Ålder, År)

# Din funktion för att beräkna genomsnitt
# Skapa en tom lista för att lagra resultaten
results <- list()

# Sätt options för att undvika vetenskaplig notation
options(scipen = 999)

# Iterera över alla unika år
for(year in unique(dödskvoter_riksprognos$År)) {
  # Iterera över alla unika regioner
  for(region in unique(dödskvoter_relativ$Region)) {
    # Filtrera data för specifikt år och region
    data_riket <- dödskvoter_riksprognos %>% filter(År == year)
    data_region <- dödskvoter_relativ %>% filter(Region == region)
    
    ### OBS! Här sätter vi den relativa kvoten för HALLAND (90-100 åringar) då gruppen är liten 
    
    hallands_län_data <- dödskvoter_relativ %>%
      filter(Region == "Hallands län", Ålder >= 90 & Ålder <= 100, År == year)
    
    data_region <- dödskvoter_relativ %>% 
      filter(Region == region) %>%
      mutate(
        Ettårsgenomsnitt = ifelse(Ålder >= 95 & Ålder <= 100, 1, Ettårsgenomsnitt),
        Treårsgenomsnitt = ifelse(Ålder >= 95 & Ålder <= 100, 1, Treårsgenomsnitt),
        Femårsgenomsnitt = ifelse(Ålder >= 95 & Ålder <= 100, 1, Femårsgenomsnitt),
        Nioårsgenomsnitt = ifelse(Ålder >= 95 & Ålder <= 100, 1, Nioårsgenomsnitt)
      )
    
    
    # Se till att Värde är numeriskt och inte i vetenskaplig notation
    data_riket$Värde <- as.numeric(format(data_riket$Värde, scientific = FALSE))
    
    # Kontrollera att vi har matchande åldrar för att undvika fel
    if(nrow(data_riket) == nrow(data_region) && all(data_riket$Ålder == data_region$Ålder)) {
      # Beräkna genomsnitten
      data_riket$Ettårsgenomsnitt <- data_riket$Värde * data_region$Ettårsgenomsnitt
      data_riket$Treårsgenomsnitt <- data_riket$Värde * data_region$Treårsgenomsnitt
      data_riket$Femårsgenomsnitt <- data_riket$Värde * data_region$Femårsgenomsnitt
      data_riket$Nioårsgenomsnitt <- data_riket$Värde * data_region$Nioårsgenomsnitt
      data_riket$Region <- region
      
      # Formatera kolumnerna för att undvika vetenskaplig notation
      data_riket$Ettårsgenomsnitt <- format(data_riket$Ettårsgenomsnitt, scientific = FALSE)
      data_riket$Treårsgenomsnitt <- format(data_riket$Treårsgenomsnitt, scientific = FALSE)
      data_riket$Femårsgenomsnitt <- format(data_riket$Femårsgenomsnitt, scientific = FALSE)
      data_riket$Nioårsgenomsnitt <- format(data_riket$Nioårsgenomsnitt, scientific = FALSE)
      
      # Lägg till i resultaten
      results[[paste(year, region)]] <- data_riket
    }
  }
}

dödsrisker_prognosklar <- bind_rows(results)


############################################## Invandringskvoter

# Invandringskvoter -------------------------------------------------------


invandringsrisker_snitt <- kommun_lista[[5]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

invandringsrisker_riket <- invandringsrisker_snitt |> filter(Region == "Riket")

invandringsrisker_kvoter <- invandringsrisker_snitt %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / invandringsrisker_riket$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / invandringsrisker_riket$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / invandringsrisker_riket$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / invandringsrisker_riket$Nioårsgenomsnitt
  )

invandringsrisker_prognosklar <- invandringsrisker_kvoter

invandringsrisker_riksprognos <- riket_lista[[3]]

invandringsrisker_riksprognos <- as_tibble(invandringsrisker_riksprognos)

# Beräkna genomsnittliga dödstal för åldersgruppen 100-105 år
invandringsstal_100_plus <- invandringsrisker_riksprognos %>%
  filter(Ålder >= 100 & Ålder <= 105) %>%
  group_by(Kön, År) %>%
  summarise(GenomsnittligtVärde = mean(as.numeric(Värde), na.rm = TRUE))

# Tilldela genomsnittet till 100-åringar och ta bort rader för 101-105 år
invandringsrisker_riksprognos <- invandringsrisker_riksprognos %>%
  filter(Ålder < 101) %>%
  left_join(dödstal_100_plus, by = c("Kön", "År")) %>%
  mutate(Värde = ifelse(Ålder == 100, GenomsnittligtVärde, as.numeric(Värde))) %>%
  select(-GenomsnittligtVärde)

invandringsrisker_riksprognos <- drop_na(invandringsrisker_riksprognos)

# Säkerställ sortering
invandringsrisker_riksprognos_riksprognos <- invandringsrisker_riksprognos %>%
  arrange(Kön, Ålder, År)

invandringsrisker_prognosklar <- invandringsrisker_prognosklar %>%
  arrange(Kön, Ålder, År)


############################################## Utvandringskvoter
# Beräkna genomsnittfödslar för alla kommuner
utvandringsrisker_snitt <- kommun_lista[[6]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

# Utvandringskvot ---------------------------------------------------------


# Beräkna genomsnittsfolkmängd för kvinnor i barnafödande ålder
folkmangd_genomsnitt <- kommun_lista[[1]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

utvandringsrisker_prognosklar <-  utvandringsrisker_snitt %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / folkmangd_genomsnitt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / folkmangd_genomsnitt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / folkmangd_genomsnitt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / folkmangd_genomsnitt$Nioårsgenomsnitt
  )

# Beräkna genomsnittliga dödstal för åldersgruppen 100-105 år
utvandringstal_100_plus <- utvandringsrisker_prognosklar %>%
  filter(Ålder >= 100 & Ålder <= 105) %>%
  group_by(Kön, År) %>%
  summarise(GenomsnittligtVärde = mean(as.numeric(Värde), na.rm = TRUE))

# Tilldela genomsnittet till 100-åringar och ta bort rader för 101-105 år
utvandringsrisker_prognosklar <- utvandringsrisker_prognosklar %>%
  filter(Ålder < 101) %>%
  left_join(utvandringstal_100_plus, by = c("Kön", "År")) %>%
  mutate(Värde = ifelse(Ålder == 100, GenomsnittligtVärde, as.numeric(Värde))) %>%
  select(-GenomsnittligtVärde)



################ Inflyttningskvoter

# Inrikes inflyttningskvot ------------------------------------------------


inflyttningsrisker_snitt <- kommun_lista[[7]] %>%
  group_by(Ålder) %>%
  arrange(Region, Kön, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

flyttarinomsverige  <- riket_lista[[4]]

flyttarinomsverige <- flyttarinomsverige %>%
  group_by(Ålder) %>%
  arrange(Kön, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

flyttarinomsverige <- drop_na(flyttarinomsverige)

inflyttningsrisker_prognosklar <-  inflyttningsrisker_snitt %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / flyttarinomsverige$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / flyttarinomsverige$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / flyttarinomsverige$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / flyttarinomsverige$Nioårsgenomsnitt
  )


## Först hur många som bor i riket i respektive åldersklass (årsslut senaste fem åren)
riket_invånare <- kommun_lista[[9]] |>filter(Region == "Riket")
riket_invånare$År <- as.numeric(riket_invånare$År)
riket_invånare <- riket_invånare |> filter(År == "2019" | År == "2020" | År == "2021" | År == "2022" |År == "2023")
riket_invånare <- riket_invånare |> 
  group_by(Ålder,
           Kön) |> 
  summarize(Värde = mean(Värde))

## Sedan hur många som flyttade i snitt över kommungräns senaste fem åren    
riket_inrikes_flyttare <- riket_lista[[4]]
riket_inrikes_flyttare$År <- as.numeric(riket_inrikes_flyttare$År)
riket_inrikes_flyttare <- riket_inrikes_flyttare |> filter(År == "2019" | År == "2020" | År == "2021" | År == "2022" |År == "2023")
riket_inrikes_flyttare <- riket_inrikes_flyttare |> 
  group_by(Ålder,
           Kön) |> 
  summarize(Värde = mean(Värde))

riket_inrikes_flyttare <- drop_na(riket_inrikes_flyttare)

## Beräkna flyttrisk över kommungräns per åldersgrupp
riket_invånare$Flyttrisk <- riket_inrikes_flyttare$Värde/riket_invånare$Värde

riket_invånare <- as_tibble(riket_invånare)

# Applicera flyttriskerna på riksprognosen för att få ut förväntat antal flyttar över kommungräns under prognosperioden

riket_prognosinvånare <- riket_lista[[5]] # Beräkna genomsnittliga dödstal för åldersgruppen 100-105 år

invånare_100_plus <- riket_prognosinvånare %>%
  filter(Ålder >= 100 & Ålder <= 105) %>%
  group_by(Kön, År) %>%
  summarise(Värde2 = mean(as.numeric(Värde), na.rm = TRUE))

# Tilldela genomsnittet till 100-åringar och ta bort rader för 101-105 år
riket_prognosinvånare <- riket_prognosinvånare %>%
  filter(Ålder < 101) %>%
  left_join(invånare_100_plus, by = c("Kön", "År")) %>%
  mutate(Värde = ifelse(Ålder == 100, Värde, as.numeric(Värde))) %>%
  select(-Värde2)

riket_invånare <- riket_invånare |> select(Ålder, Kön, Flyttrisk)

riket_prognosinvånare <- left_join(riket_prognosinvånare, riket_invånare, by = c("Kön", "Ålder"))

riket_prognosinvånare$Flyttare <- riket_prognosinvånare$Värde*riket_prognosinvånare$Flyttrisk



## Måste relatera till den gamla historien och sedan i relation till hur många som flyttar i Sverige i den åldersgruppen osv.

############################################## Utflyttningskvoter

# Inrikes utflyttningskvoter ----------------------------------------------


# Beräkna genomsnittfödslar för alla kommuner
utflyttningssrisker_snitt <- kommun_lista[[8]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

# Beräkna genomsnittsfolkmängd för kvinnor i barnafödande ålder
folkmangd_genomsnitt <- kommun_lista[[1]] %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

utflyttningsrisker_prognosklar <-  utflyttningssrisker_snitt %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / folkmangd_genomsnitt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / folkmangd_genomsnitt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / folkmangd_genomsnitt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / folkmangd_genomsnitt$Nioårsgenomsnitt
  )





################ BERÄKNINGAR #########################

# Basår -------------------------------------------------------------------


### År 1

# Kom ihåg att köra 50/50 på födda och att de ska tillföras 0-åringar

# Anta att 'basår' och 'födelserisker_prognosklar' är dina dataframes
basår <- kommun_lista[[9]] # Ersätt med din 'basår' data
basår <- basår |> filter(År == last(År))

födelserisker_prognosklar <- födelserisker_prognosklar

# Hitta unika regioner och år för att iterera över
unika_regioner <- unique(basår$Region)
unika_kon <- unique(basår$Kön)

# Ta bort "Riket" från vektorn
unika_regioner <- unika_regioner[unika_regioner != "Riket"]

unika_ar <- unique(basår$År)

unika_ar <- as.numeric(unika_ar)


# Skapa en tom lista för att lagra resultaten
födda_lista <- list()
beräkningsgrund_lista <- list()
döda_lista <- list()
invandring_lista <- list()
utvandring_lista <- list()
inrikesinflytt <- list()
inrikesutflytt <- list()
prognos_lista <- list()

# Loop över varje unik region och år
for(region in unika_regioner) {
  for(ar in unika_ar) {
    
    ################################### FÖDDA ################################### 
    
    # Filtrera basår-data för aktuell region och år
    basår_kommun <- basår |> filter(Region == region, År == ar)
    basår_kommun$År <- as.numeric(basår_kommun$År)
    
    # Expandera till fler rader (ålder)
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(basår_kommun$Kön), Region = unique(basår_kommun$Region))
    basår_kommun <- left_join(nya_aldersintervall, basår_kommun, by = c("Region", "Ålder", "Kön"))
    basår_kommun$Värde[is.na(basår_kommun$Värde)] <- 0
    
    # Gruppera och fyll i data
    basår_kommun <- basår_kommun %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    # Flytta fram ett år
    basår_kommun <- basår_kommun %>%
      group_by(Region, Kön) %>%
      mutate(Värde = lag(Värde, n = 1, default = 0))
    basår_kommun$År <- basår_kommun$År + 1
    
    # Summera värden för 101-åringar för varje kombination av Region och Kön
    # Summera värden för 101-åringar för varje kombination av Region och Kön
    värde_för_101 <- basår_kommun %>%
      filter(Ålder == 101) %>%
      group_by(Region, Kön) %>%
      summarise(Värde_101 = sum(Värde), .groups = 'drop')
    
    # Lägg till värdet för 101-åringar till 100-åringar och nollställ värden för åldrar över 100
    basår_kommun <- basår_kommun %>%
      left_join(värde_för_101, by = c("Region", "Kön")) %>%
      mutate(Värde = ifelse(Ålder == 100, Värde + Värde_101, ifelse(Ålder > 100, 0, Värde))) %>%
      select(-Värde_101)
    
    
    # Beräkna mödrar
    basår_kommun_modrar <- basår_kommun |> filter(Ålder >= 15 & Ålder <= 49, Kön == "kvinnor")
    
    
    # Hantera födelserisker
    födelserisker_kommun <- födelserisker_prognosklar |> filter(Region == region, År == ar+1)
    födelserisker_kommun <- födelserisker_kommun %>%
      mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt), as.numeric))
    
    # Beräkna födda
    födda_kommun <-  födelserisker_kommun %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * basår_kommun_modrar$Värde,
        Treårsgenomsnitt = Treårsgenomsnitt * basår_kommun_modrar$Värde,
        Femårsgenomsnitt = Femårsgenomsnitt * basår_kommun_modrar$Värde,
        Nioårsgenomsnitt = Nioårsgenomsnitt * basår_kommun_modrar$Värde
      )
    
    # Beräkna summor
    summor <- födda_kommun %>%
      summarise(
        SummaEttår = sum(Ettårsgenomsnitt) / 2,
        SummaTreår = sum(Treårsgenomsnitt) / 2,
        SummaFemår = sum(Femårsgenomsnitt) / 2,
        SummaNioår = sum(Nioårsgenomsnitt) / 2
      )
    
    # Uppdatera basår_kommun för 0-åringar
    födda_kommun <- basår_kommun %>%
      mutate(
        Ettårsgenomsnitt = ifelse(Ålder == 0, summor$SummaEttår, 0),
        Treårsgenomsnitt = ifelse(Ålder == 0, summor$SummaTreår, 0),
        Femårsgenomsnitt = ifelse(Ålder == 0, summor$SummaFemår, 0),
        Nioårsgenomsnitt = ifelse(Ålder == 0, summor$SummaNioår, 0)
      )
    
    födda_kommun$Variabel <- "Födda"
    
    # Ersätt NA med 0 i specifika kolumner
    födda_kommun <- födda_kommun %>%
      mutate(
        Ettårsgenomsnitt = ifelse(is.na(Ettårsgenomsnitt), 0, Ettårsgenomsnitt),
        Treårsgenomsnitt = ifelse(is.na(Treårsgenomsnitt), 0, Treårsgenomsnitt),
        Femårsgenomsnitt = ifelse(is.na(Femårsgenomsnitt), 0, Femårsgenomsnitt),
        Nioårsgenomsnitt = ifelse(is.na(Nioårsgenomsnitt), 0, Nioårsgenomsnitt),
        Variabel = "Födda"
      )
    
    # Skapa en ny tabell där alla utom 0-åringar får 'Värde' i genomsnittskolumnerna
    beräkningsgrund_kommun <- födda_kommun %>%
      mutate(
        Ettårsgenomsnitt = ifelse(Ålder == 0, Ettårsgenomsnitt, Värde),
        Treårsgenomsnitt = ifelse(Ålder == 0, Treårsgenomsnitt, Värde),
        Femårsgenomsnitt = ifelse(Ålder == 0, Femårsgenomsnitt, Värde),
        Nioårsgenomsnitt = ifelse(Ålder == 0, Nioårsgenomsnitt, Värde),
        
      )
    
    beräkningsgrund_kommun$Variabel <- "Totalt"
    
    # Lagra resultatet i listan
    födda_lista[[paste(region, ar+1, sep="_")]] <- födda_kommun
    beräkningsgrund_lista[[paste(region, ar+1, sep="_")]] <- beräkningsgrund_kommun
    
    ################################### DÖDA ###################################
    
    # Hämta aktuell beräkningsgrund och dödsrisker
    aktuell_beräkningsgrund <- beräkningsgrund_lista[[paste(region, ar+1, sep="_")]]
    aktuella_dödsrisker <- dödsrisker_prognosklar |> filter(Region == region, År == ar+1)
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(aktuella_dödsrisker$Kön), Region = unique(aktuella_dödsrisker$Region))
    
    aktuella_dödsrisker <- left_join(nya_aldersintervall, aktuella_dödsrisker, by = c("Region", "Ålder", "Kön"))
    
    aktuella_dödsrisker<- aktuella_dödsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      arrange(Region, Kön, Ålder)
    
    aktuella_dödsrisker$År <- as.numeric(aktuella_dödsrisker$År)
    
    
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt), as.numeric))
    
    
    aktuella_dödsrisker_kvinnor <- aktuella_dödsrisker |> filter(Kön == "kvinnor")
    
    döda_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_dödsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_dödsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_dödsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_dödsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Döda"
      ) 
    
    aktuella_dödsrisker_män <- aktuella_dödsrisker |> filter(Kön == "män")
    
    döda_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_dödsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_dödsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_dödsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_dödsrisker_män$Nioårsgenomsnitt,
        Variabel = "Döda"
      ) 
    
    döda_kommun <- bind_rows(döda_kommun_kvinnor, döda_kommun_män)
    
    # Lagra resultatet i listorna
    döda_lista[[paste(region, ar + 1, sep = "_")]] <- döda_kommun
    
    ######################################## Invandring ################################### 
    
    prognos_riket_invandring_aktuelltår <- invandringsrisker_riksprognos |> filter(År == ar+1)
    
    prognos_riket_invandring_aktuelltår_kvinnor <- prognos_riket_invandring_aktuelltår |> filter(Kön == "kvinnor")
    prognos_riket_invandring_aktuelltår_män <- prognos_riket_invandring_aktuelltår |> filter(Kön == "män")
    
    invandringsrisker_män <- invandringsrisker_prognosklar |> filter(Region == region)
    invandringsrisker_män <- invandringsrisker_män |> filter(Kön == "män")
    invandringsrisker_kvinnor <- invandringsrisker_prognosklar |> filter(Region == region)
    invandringsrisker_kvinnor <- invandringsrisker_kvinnor |> filter(Kön == "kvinnor")
    
    invandringsrisker_män <- invandringsrisker_män %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandringsrisker_kvinnor <- invandringsrisker_kvinnor %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandring_kommun_män <- invandringsrisker_män %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Treårsgenomsnitt = Treårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Femårsgenomsnitt = Femårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Nioårsgenomsnitt = Nioårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Variabel = "Invandringar"
      ) 
    
    invandring_kommun_kvinnor <- invandringsrisker_kvinnor %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Treårsgenomsnitt = Treårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Femårsgenomsnitt = Femårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Nioårsgenomsnitt = Nioårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Variabel = "Invandringar"
      ) 
    
    invandring_kommun <- bind_rows(invandring_kommun_män, invandring_kommun_kvinnor)
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(invandring_kommun$Kön), Region = unique(invandring_kommun$Region))
    
    invandring_kommun <- left_join(nya_aldersintervall, invandring_kommun, by = c("Region", "Ålder", "Kön"))
    
    invandring_kommun <- invandring_kommun %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandring_kommun <- as_tibble(invandring_kommun)
    invandring_kommun$År <- ar+1 
    
    # Lagra resultatet i listorna
    invandring_lista[[paste(region, ar + 1, sep = "_")]] <- invandring_kommun
    
    ######################################## Utvandring ################################### 
    
    # Hämta aktuell beräkningsgrund och dödsrisker
    aktuell_beräkningsgrund <- beräkningsgrund_lista[[paste(region, ar+1, sep="_")]]
    aktuella_utvandringsrisker <- utvandringsrisker_prognosklar |> filter(Region == region, År == ar)
    
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(aktuella_utvandringsrisker$Kön), Region = unique(aktuella_dödsrisker$Region))
    
    aktuella_utvandringsrisker <- left_join(nya_aldersintervall, aktuella_utvandringsrisker, by = c("Region", "Ålder", "Kön"))
    
    aktuella_utvandringsrisker<- aktuella_utvandringsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    aktuella_utvandringsrisker <- aktuella_utvandringsrisker %>%
      arrange(Region, Kön, Ålder)
    
    aktuella_utvandringsrisker$År <- as.numeric(aktuella_utvandringsrisker$År)
    
    aktuella_utvandringsrisker <- aktuella_utvandringsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    aktuella_utvandringsrisker_kvinnor <- aktuella_utvandringsrisker |> filter(Kön == "kvinnor")
    
    utvandrade_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Utvandringar"
      ) 
    
    aktuella_utvandringsrisker_män <-  aktuella_utvandringsrisker |> filter(Kön == "män")
    
    utvandrade_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_utvandringsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_utvandringsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_utvandringsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_utvandringsrisker_män$Nioårsgenomsnitt,
        Variabel = "Utvandringar"
      ) 
    
    utvandrade_kommun <- bind_rows(utvandrade_kommun_kvinnor, utvandrade_kommun_män)
    
    # Lagra resultatet i listorna
    utvandring_lista[[paste(region, ar + 1, sep = "_")]] <- utvandrade_kommun
    
    ######################################### Inrikes inflyttning
    
    inflyttningsrisker <- inflyttningsrisker_prognosklar |> filter(Region == region)
    riket_prognosinvånare_filter <- riket_prognosinvånare |> filter(År == ar+1)
    riket_prognosinvånare_män <- riket_prognosinvånare_filter |> filter(Kön == "män")
    riket_prognosinvånare_kvinnor <- riket_prognosinvånare_filter |> filter(Kön == "kvinnor")
    
    inflyttade_kommun_män <- inflyttningsrisker %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Treårsgenomsnitt = Treårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Femårsgenomsnitt = Femårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Nioårsgenomsnitt = Nioårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        År = ar+1,
        Variabel = "Inrikes inflyttningar"
      ) 
    
    inflyttade_kommun_kvinnor <- inflyttningsrisker %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Treårsgenomsnitt = Treårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Femårsgenomsnitt = Femårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Nioårsgenomsnitt = Nioårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        År = ar+1,
        Variabel = "Inrikes inflyttningar"
      ) 
    
    inflyttade_kommun <- bind_rows(inflyttade_kommun_kvinnor, inflyttade_kommun_män)
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(inflyttade_kommun$Kön), Region = unique(inflyttade_kommun$Region))
    
    inflyttade_kommun <- left_join(nya_aldersintervall, inflyttade_kommun, by = c("Region", "Ålder", "Kön"))
    
    inflyttade_kommun <- inflyttade_kommun %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    inflyttade_kommun <- as_tibble(inflyttade_kommun)
    
    inrikesinflytt[[paste(region, ar + 1, sep = "_")]] <- inflyttade_kommun
    
    ######################################### Inrikes utflyttning
    
    utflyttningsrisker <- utflyttningsrisker_prognosklar
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(utflyttningsrisker$Kön), Region = unique(utflyttningsrisker$Region))
    
    utflyttningsrisker <- left_join(nya_aldersintervall, utflyttningsrisker, by = c("Region", "Ålder", "Kön"))
    
    
    utflyttningsrisker<- utflyttningsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    utflyttningsrisker <-  utflyttningsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    utflyttningsrisker <- utflyttningsrisker %>%
      arrange(Region, Kön, Ålder)
    
    
    utflyttningsrisker <- utflyttningsrisker |> filter(Region == region)
    utflyttningsrisker_kvinnor <- utflyttningsrisker |> filter(Kön == "kvinnor")
    utflyttningsrisker_män <- utflyttningsrisker |> filter(Kön == "män")
    
    utflyttade_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * utflyttningsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * utflyttningsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * utflyttningsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * utflyttningsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Inrikes utflyttningar"
      ) 
    
    utflyttade_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * utflyttningsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * utflyttningsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * utflyttningsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * utflyttningsrisker_män$Nioårsgenomsnitt,
        Variabel = "Inrikes utflyttningar"
      ) 
    
    utflyttade_kommun <- bind_rows(utflyttade_kommun_kvinnor, utflyttade_kommun_män)
    
    # Lagra resultatet i listorna
    inrikesutflytt[[paste(region, ar + 1, sep = "_")]] <-  utflyttade_kommun
    
  }
}


## Konvertera alla till år

# Funktion som konverterar 'År' till numeriskt format
konvertera_ar_till_numerisk <- function(df) {
  df %>%
    mutate(År = as.numeric(as.character(År)))
}

# Uppdatera varje lista individuellt
beräkningsgrund_lista <- map(beräkningsgrund_lista, konvertera_ar_till_numerisk)
födda_lista <- map(födda_lista, konvertera_ar_till_numerisk)
döda_lista <- map(döda_lista, konvertera_ar_till_numerisk)
invandring_lista <- map(invandring_lista, konvertera_ar_till_numerisk)
utvandring_lista <- map(utvandring_lista, konvertera_ar_till_numerisk)
inrikesinflytt <- map(inrikesinflytt, konvertera_ar_till_numerisk)
inrikesutflytt <- map(inrikesutflytt, konvertera_ar_till_numerisk)

### Beräkningar


# Funktion för att addera genomsnittsvärden från en lista till prognos_listan
addera_genomsnitt <- function(prognos_df, tillagg_df) {
  prognos_df %>%
    mutate(
      Ettårsgenomsnitt = Ettårsgenomsnitt + tillagg_df$Ettårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
      Treårsgenomsnitt = Treårsgenomsnitt + tillagg_df$Treårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
      Femårsgenomsnitt = Femårsgenomsnitt + tillagg_df$Femårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
      Nioårsgenomsnitt = Nioårsgenomsnitt + tillagg_df$Nioårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))]
    )
}

# Funktion för att subtrahera genomsnittsvärden från en lista från prognos_listan
subtrahera_genomsnitt <- function(prognos_df, avdrag_df) {
  prognos_df %>%
    mutate(
      Ettårsgenomsnitt = Ettårsgenomsnitt - avdrag_df$Ettårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
      Treårsgenomsnitt = Treårsgenomsnitt - avdrag_df$Treårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
      Femårsgenomsnitt = Femårsgenomsnitt - avdrag_df$Femårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
      Nioårsgenomsnitt = Nioårsgenomsnitt - avdrag_df$Nioårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))]
    )
}

# Födda har redan adderats i "beräkningsgrunder"

# Börja med att subtrahera döda från beräkningsgrund_listan
prognos_lista <- map2(beräkningsgrund_lista, döda_lista, subtrahera_genomsnitt)

# Addera invandring
prognos_lista <- map2(prognos_lista, invandring_lista, addera_genomsnitt)

# Subtrahera utvandring
prognos_lista <- map2(prognos_lista, utvandring_lista, subtrahera_genomsnitt)

# Addera inrikes inflyttningar
prognos_lista <- map2(prognos_lista, inrikesinflytt, addera_genomsnitt)

# Subtrahera inrikes utflyttningar
prognos_lista <- map2(prognos_lista, inrikesutflytt, subtrahera_genomsnitt)

# Funktion för att ersätta NA med 0 och sätta negativa värden till 0
ersatt_na_och_negativa <- function(df) {
  df %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
    mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt),
                  ~ ifelse(. < 0, 0, .)))
}

# Använd funktionen på prognos_listan
prognos_lista <- map(prognos_lista, ersatt_na_och_negativa)


# Övriga år ---------------------------------------------------------------


unika_ar <- unique(dodstal$År)
unika_ar <- as_tibble(unika_ar)
unika_ar <- as.numeric(unika_ar$value)

# Ta också bort första prognosåret då vi redan behandlat det
unika_ar <- unika_ar[unika_ar != unika_ar[1:1]] 

### START LOOP

for(region in unika_regioner) {
  for(ar in unika_ar) {
    
    # Starttiden för varje iteration
    iteration_start_tid <- Sys.time()
    
    
    # Välj ut basår från vår förstaårsprognos
    nyckel <- paste(region, ar-1, sep = "_")
    
    basår_kommun <- prognos_lista[[nyckel]]
    
    # Flytta fram ett år
    basår_kommun <- basår_kommun %>%
      group_by(Region, Kön) %>%
      mutate(Ettårsgenomsnitt = lag(Ettårsgenomsnitt, n = 1, default = 0),
             Treårsgenomsnitt = lag(Treårsgenomsnitt, n = 1, default = 0),
             Femårsgenomsnitt = lag(Femårsgenomsnitt, n = 1, default = 0),
             Nioårsgenomsnitt = lag(Nioårsgenomsnitt, n = 1, default = 0))
    
    # Summera värden för 101-åringar för varje kombination av Region och Kön
    värden_för_101 <- basår_kommun %>%
      filter(Ålder == 101) %>%
      group_by(Region, Kön) %>%
      summarise(
        Värde_101 = sum(Värde),
        Ettårsgenomsnitt_101 = sum(Ettårsgenomsnitt),
        Treårsgenomsnitt_101 = sum(Treårsgenomsnitt),
        Femårsgenomsnitt_101 = sum(Femårsgenomsnitt),
        Nioårsgenomsnitt_101 = sum(Nioårsgenomsnitt),
        .groups = 'drop'
      )
    
    # Lägg till värdena för 101-åringar till 100-åringar och nollställ värden för åldrar över 100
    basår_kommun <- basår_kommun %>%
      left_join(värden_för_101, by = c("Region", "Kön")) %>%
      mutate(
        Värde = ifelse(Ålder == 100, Värde + Värde_101, ifelse(Ålder > 100, 0, Värde)),
        Ettårsgenomsnitt = ifelse(Ålder == 100, Ettårsgenomsnitt + Ettårsgenomsnitt_101, ifelse(Ålder > 100, 0, Ettårsgenomsnitt)),
        Treårsgenomsnitt = ifelse(Ålder == 100, Treårsgenomsnitt + Treårsgenomsnitt_101, ifelse(Ålder > 100, 0, Treårsgenomsnitt)),
        Femårsgenomsnitt = ifelse(Ålder == 100, Femårsgenomsnitt + Femårsgenomsnitt_101, ifelse(Ålder > 100, 0, Femårsgenomsnitt)),
        Nioårsgenomsnitt = ifelse(Ålder == 100, Nioårsgenomsnitt + Nioårsgenomsnitt_101, ifelse(Ålder > 100, 0, Nioårsgenomsnitt))
      ) %>%
      select(-ends_with("_101"))
    
    # Nu har vi vår beräkningstabell
    basår_kommun$År <- ar
    
    ###################### FÖDDA
    
    # Beräkna mödrar
    basår_kommun_modrar <- basår_kommun |> filter(Ålder >= 15 & Ålder <= 49, Kön == "kvinnor")
    
    födelserisker_kommun <- födelserisker_prognosklar |> filter(Region == region, År == ar)
    
    födelserisker_kommun <- födelserisker_kommun %>%
      mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt), as.numeric))
    
    # Beräkna födda
    födda_kommun <-  födelserisker_kommun %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * basår_kommun_modrar$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * basår_kommun_modrar$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * basår_kommun_modrar$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * basår_kommun_modrar$Nioårsgenomsnitt
      )
    
    # Beräkna summor
    summor <- födda_kommun %>%
      summarise(
        SummaEttår = sum(Ettårsgenomsnitt) / 2,
        SummaTreår = sum(Treårsgenomsnitt) / 2,
        SummaFemår = sum(Femårsgenomsnitt) / 2,
        SummaNioår = sum(Nioårsgenomsnitt) / 2
      )
    
    # Uppdatera basår_kommun för 0-åringar
    födda_kommun <- basår_kommun %>%
      mutate(
        Ettårsgenomsnitt = ifelse(Ålder == 0, summor$SummaEttår, 0),
        Treårsgenomsnitt = ifelse(Ålder == 0, summor$SummaTreår, 0),
        Femårsgenomsnitt = ifelse(Ålder == 0, summor$SummaFemår, 0),
        Nioårsgenomsnitt = ifelse(Ålder == 0, summor$SummaNioår, 0)
      )
    
    födda_kommun$Variabel <- "Födda"
    
    # Skapa en ny tabell där alla utom 0-åringar får 'Värde' i genomsnittskolumnerna ## Vi måste plocka data från annat än värde här.... måste ta från vår nya tabell ettårsgenomsnitt
    # Extrahera 0-åringarna från födda_kommun
    födda_0_aringar <- filter(födda_kommun, Ålder == 0)
    
    # Extrahera alla åldersgrupper utom 0-åringar från basår_kommun
    basår_utom_0_aringar <- filter(basår_kommun, Ålder != 0)
    
    # Kombinera de två datamängderna
    beräkningsgrund_kommun <- rbindlist(list(födda_0_aringar, basår_utom_0_aringar))
    
    
    beräkningsgrund_kommun$Variabel <- "Totalt"
    
    # Lagra resultatet i listan
    födda_lista[[paste(region, ar, sep="_")]] <- födda_kommun
    beräkningsgrund_lista[[paste(region, ar, sep="_")]] <- beräkningsgrund_kommun
    
    ################### DÖDA
    
    # Hämta aktuell beräkningsgrund och dödsrisker
    aktuell_beräkningsgrund <- beräkningsgrund_lista[[paste(region, ar, sep="_")]]
    aktuella_dödsrisker <- dödsrisker_prognosklar |> filter(Region == region, År == ar)
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(aktuella_dödsrisker$Kön), Region = unique(aktuella_dödsrisker$Region))
    
    aktuella_dödsrisker <- left_join(nya_aldersintervall, aktuella_dödsrisker, by = c("Region", "Ålder", "Kön"))
    
    aktuella_dödsrisker<- aktuella_dödsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      arrange(Region, Kön, Ålder)
    
    aktuella_dödsrisker$År <- as.numeric(aktuella_dödsrisker$År)
    
    aktuella_dödsrisker <- aktuella_dödsrisker %>%
      mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt), as.numeric))
    
    aktuella_dödsrisker_kvinnor <- aktuella_dödsrisker |> filter(Kön == "kvinnor")
    
    döda_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_dödsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_dödsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_dödsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_dödsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Döda"
      ) 
    
    aktuella_dödsrisker_män <- aktuella_dödsrisker |> filter(Kön == "män")
    
    döda_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_dödsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_dödsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_dödsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_dödsrisker_män$Nioårsgenomsnitt,
        Variabel = "Döda"
      ) 
    
    döda_kommun <- rbindlist(list(döda_kommun_kvinnor, döda_kommun_män))
    
    # Lagra resultatet i listorna
    döda_lista[[paste(region, ar, sep = "_")]] <- döda_kommun
    
    
    ######################################## Invandring ################################### 
    
    # Hämta prognosticerad invandring för det aktuella året
    prognos_riket_invandring_aktuelltår <- invandringsrisker_riksprognos |> filter(År == ar)
    
    # Separera könen för nationell prognos
    prognos_riket_invandring_aktuelltår_kvinnor <- prognos_riket_invandring_aktuelltår |> filter(Kön == "kvinnor")
    prognos_riket_invandring_aktuelltår_män <- prognos_riket_invandring_aktuelltår |> filter(Kön == "män")
    
    # Hämta invandringsrisker för den aktuella regionen
    invandringsrisker_män <- invandringsrisker_prognosklar |> filter(Region == region, Kön == "män")
    invandringsrisker_kvinnor <- invandringsrisker_prognosklar |> filter(Region == region, Kön == "kvinnor")
    
    invandringsrisker_män <- invandringsrisker_män %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandringsrisker_kvinnor <- invandringsrisker_kvinnor %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandring_kommun_män <- invandringsrisker_män %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Treårsgenomsnitt = Treårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Femårsgenomsnitt = Femårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Nioårsgenomsnitt = Nioårsgenomsnitt * prognos_riket_invandring_aktuelltår_män$Värde,
        Variabel = "Invandringar"
      ) 
    
    invandring_kommun_kvinnor <- invandringsrisker_kvinnor %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Treårsgenomsnitt = Treårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Femårsgenomsnitt = Femårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Nioårsgenomsnitt = Nioårsgenomsnitt * prognos_riket_invandring_aktuelltår_kvinnor$Värde,
        Variabel = "Invandringar"
      ) 
    
    
    invandring_kommun <- rbindlist(list(invandring_kommun_män, invandring_kommun_kvinnor))
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(invandring_kommun$Kön), Region = unique(invandring_kommun$Region))
    
    invandring_kommun <- left_join(nya_aldersintervall, invandring_kommun, by = c("Region", "Ålder", "Kön"))
    
    invandring_kommun <- invandring_kommun %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    invandring_kommun <- as_tibble(invandring_kommun)
    invandring_kommun$År <- ar 
    
    # Lagra resultatet i listorna
    invandring_lista[[paste(region, ar, sep = "_")]] <- invandring_kommun
    
    ######################################## Utvandring ################################### 
    
    aktuell_beräkningsgrund <- beräkningsgrund_lista[[paste(region, ar, sep="_")]]
    aktuella_utvandringsrisker <- utvandringsrisker_prognosklar |> filter(Region == region, År == last(År))
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(aktuella_utvandringsrisker$Kön), Region = unique(aktuella_dödsrisker$Region))
    
    aktuella_utvandringsrisker <- left_join(nya_aldersintervall, aktuella_utvandringsrisker, by = c("Region", "Ålder", "Kön"))
    
    aktuella_utvandringsrisker<- aktuella_utvandringsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    aktuella_utvandringsrisker <- aktuella_utvandringsrisker %>%
      arrange(Region, Kön, Ålder)
    
    aktuella_utvandringsrisker$År <- as.numeric(aktuella_utvandringsrisker$År)
    
    aktuella_utvandringsrisker <- aktuella_utvandringsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    aktuella_utvandringsrisker_kvinnor <- aktuella_utvandringsrisker |> filter(Kön == "kvinnor")
    
    utvandrade_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_utvandringsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Utvandringar"
      ) 
    
    aktuella_utvandringsrisker_män <-  aktuella_utvandringsrisker |> filter(Kön == "män")
    
    utvandrade_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * aktuella_utvandringsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * aktuella_utvandringsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * aktuella_utvandringsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * aktuella_utvandringsrisker_män$Nioårsgenomsnitt,
        Variabel = "Utvandringar"
      ) 
    
    
    utvandrade_kommun <- rbindlist(list(utvandrade_kommun_kvinnor, utvandrade_kommun_män))
    
    # Lagra resultatet i listorna
    utvandring_lista[[paste(region, ar, sep = "_")]] <- utvandrade_kommun
    
    ######################################### Inrikes inflyttning
    
    inflyttningsrisker <- inflyttningsrisker_prognosklar |> filter(Region == region)
    riket_prognosinvånare_filter <- riket_prognosinvånare |> filter(År == ar)
    riket_prognosinvånare_män <- riket_prognosinvånare_filter |> filter(Kön == "män")
    riket_prognosinvånare_kvinnor <- riket_prognosinvånare_filter |> filter(Kön == "kvinnor")
    
    inflyttade_kommun_män <- inflyttningsrisker %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Treårsgenomsnitt = Treårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Femårsgenomsnitt = Femårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        Nioårsgenomsnitt = Nioårsgenomsnitt * riket_prognosinvånare_män$Flyttare,
        År = ar,
        Variabel = "Inrikes inflyttningar"
      ) 
    
    inflyttade_kommun_kvinnor <- inflyttningsrisker %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Treårsgenomsnitt = Treårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Femårsgenomsnitt = Femårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        Nioårsgenomsnitt = Nioårsgenomsnitt * riket_prognosinvånare_kvinnor$Flyttare,
        År = ar,
        Variabel = "Inrikes inflyttningar"
      ) 
    
    inflyttade_kommun <- rbindlist(list(inflyttade_kommun_kvinnor, inflyttade_kommun_män))
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(inflyttade_kommun$Kön), Region = unique(inflyttade_kommun$Region))
    
    inflyttade_kommun <- left_join(nya_aldersintervall, inflyttade_kommun, by = c("Region", "Ålder", "Kön"))
    
    inflyttade_kommun <- inflyttade_kommun %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    inflyttade_kommun <- as_tibble(inflyttade_kommun)
    
    inrikesinflytt[[paste(region, ar, sep = "_")]] <- inflyttade_kommun
    
    ######################################### Inrikes utflyttning
    
    utflyttningsrisker <- utflyttningsrisker_prognosklar
    
    nya_aldersintervall <- expand.grid(Ålder = 0:150, Kön = unique(utflyttningsrisker$Kön), Region = unique(utflyttningsrisker$Region))
    
    utflyttningsrisker <- left_join(nya_aldersintervall, utflyttningsrisker, by = c("Region", "Ålder", "Kön"))
    
    
    utflyttningsrisker<- utflyttningsrisker %>% 
      group_by(Region, Kön) %>% 
      fill(År, Variabel, .direction = "downup")
    
    utflyttningsrisker <-  utflyttningsrisker %>%
      mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
    
    # Sortera 'aktuell_beräkningsgrund'
    aktuell_beräkningsgrund <- aktuell_beräkningsgrund %>%
      arrange(Region, Kön, Ålder)
    
    # Sortera 'aktuella_dödsrisker'
    utflyttningsrisker <- utflyttningsrisker %>%
      arrange(Region, Kön, Ålder)
    
    
    utflyttningsrisker <- utflyttningsrisker |> filter(Region == region)
    utflyttningsrisker_kvinnor <- utflyttningsrisker |> filter(Kön == "kvinnor")
    utflyttningsrisker_män <- utflyttningsrisker |> filter(Kön == "män")
    
    utflyttade_kommun_kvinnor <- aktuell_beräkningsgrund %>% 
      filter(Kön == "kvinnor") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * utflyttningsrisker_kvinnor$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * utflyttningsrisker_kvinnor$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * utflyttningsrisker_kvinnor$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * utflyttningsrisker_kvinnor$Nioårsgenomsnitt,
        Variabel = "Inrikes utflyttningar"
      ) 
    
    utflyttade_kommun_män <- aktuell_beräkningsgrund %>% 
      filter(Kön == "män") %>%
      mutate(
        Ettårsgenomsnitt = Ettårsgenomsnitt * utflyttningsrisker_män$Ettårsgenomsnitt,
        Treårsgenomsnitt = Treårsgenomsnitt * utflyttningsrisker_män$Treårsgenomsnitt,
        Femårsgenomsnitt = Femårsgenomsnitt * utflyttningsrisker_män$Femårsgenomsnitt,
        Nioårsgenomsnitt = Nioårsgenomsnitt * utflyttningsrisker_män$Nioårsgenomsnitt,
        Variabel = "Inrikes utflyttningar"
      ) 
    
    utflyttade_kommun <- rbindlist(list(utflyttade_kommun_kvinnor, utflyttade_kommun_män))
    
    # Lagra resultatet i listorna
    inrikesutflytt[[paste(region, ar, sep = "_")]] <-  utflyttade_kommun
    
    ############################# SAMMANSLAGNINGAR
    
    # Funktion som konverterar 'År' till numeriskt format
    konvertera_ar_till_numerisk <- function(df) {
      df %>%
        mutate(År = as.numeric(as.character(År)))
    }
    
    # Uppdatera varje lista individuellt
    beräkningsgrund_lista <- map(beräkningsgrund_lista, konvertera_ar_till_numerisk)
    födda_lista <- map(födda_lista, konvertera_ar_till_numerisk)
    döda_lista <- map(döda_lista, konvertera_ar_till_numerisk)
    invandring_lista <- map(invandring_lista, konvertera_ar_till_numerisk)
    utvandring_lista <- map(utvandring_lista, konvertera_ar_till_numerisk)
    inrikesinflytt <- map(inrikesinflytt, konvertera_ar_till_numerisk)
    inrikesutflytt <- map(inrikesutflytt, konvertera_ar_till_numerisk)
    
    ### Beräkningar
    
    
    # Funktion för att addera genomsnittsvärden från en lista till prognos_listan
    addera_genomsnitt <- function(prognos_df, tillagg_df) {
      prognos_df %>%
        mutate(
          Ettårsgenomsnitt = Ettårsgenomsnitt + tillagg_df$Ettårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
          Treårsgenomsnitt = Treårsgenomsnitt + tillagg_df$Treårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
          Femårsgenomsnitt = Femårsgenomsnitt + tillagg_df$Femårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))],
          Nioårsgenomsnitt = Nioårsgenomsnitt + tillagg_df$Nioårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(tillagg_df$Region, tillagg_df$År, tillagg_df$Kön, tillagg_df$Ålder))]
        )
    }
    
    # Funktion för att subtrahera genomsnittsvärden från en lista från prognos_listan
    subtrahera_genomsnitt <- function(prognos_df, avdrag_df) {
      prognos_df %>%
        mutate(
          Ettårsgenomsnitt = Ettårsgenomsnitt - avdrag_df$Ettårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
          Treårsgenomsnitt = Treårsgenomsnitt - avdrag_df$Treårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
          Femårsgenomsnitt = Femårsgenomsnitt - avdrag_df$Femårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))],
          Nioårsgenomsnitt = Nioårsgenomsnitt - avdrag_df$Nioårsgenomsnitt[match(paste(Region, År, Kön, Ålder), paste(avdrag_df$Region, avdrag_df$År, avdrag_df$Kön, avdrag_df$Ålder))]
        )
    }
    
    # Födda har redan adderats i "beräkningsgrunder"
    
    # Börja med att subtrahera döda från beräkningsgrund_listan
    prognos_lista <- map2(beräkningsgrund_lista, döda_lista, subtrahera_genomsnitt)
    
    # Addera invandring
    prognos_lista <- map2(prognos_lista, invandring_lista, addera_genomsnitt)
    
    # Subtrahera utvandring
    prognos_lista <- map2(prognos_lista, utvandring_lista, subtrahera_genomsnitt)
    
    # Addera inrikes inflyttningar
    prognos_lista <- map2(prognos_lista, inrikesinflytt, addera_genomsnitt)
    
    # Subtrahera inrikes utflyttningar
    prognos_lista <- map2(prognos_lista, inrikesutflytt, subtrahera_genomsnitt)
    
    # Funktion för att ersätta NA med 0 och sätta negativa värden till 0
    ersatt_na_och_negativa <- function(df) {
      df %>%
        mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
        mutate(across(c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt),
                      ~ ifelse(. < 0, 0, .)))
    }
    
    # Använd funktionen på prognos_listan
    prognos_lista <- map(prognos_lista, ersatt_na_och_negativa)
    
    # Avsluta tiden för varje iteration
    iteration_slut_tid <- Sys.time()
    
    # Utskrift för att visa framsteg och hur lång tid iterationen tog
    cat(sprintf("Region: %s, År: %s, Iterationstid: %s sekunder\n", 
                region, ar, difftime(iteration_slut_tid, iteration_start_tid, units = "secs")))
    
    
  }
}
######## STOP LOOP

sammanslagen_lista <- c(födda_lista,
                        döda_lista,
                        invandring_lista,
                        utvandring_lista,
                        inrikesinflytt,
                        inrikesutflytt,
                        prognos_lista)

sammanslagen_tabell <- map_dfr(sammanslagen_lista, bind_rows)
sammanslagen_tabell <- sammanslagen_tabell %>% select(-Värde)

sammanslagen_tabell <- sammanslagen_tabell |> filter(Variabel != "0")

sammanslagen_tabell <- pivot_longer(
  sammanslagen_tabell, 
  cols = c(Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt),
  names_to = "Genomsnitt", 
  values_to = "Värde"
)


saveRDS(sammanslagen_tabell, "befolkningsprognos.rds")

# Länsberäkningar (OBS! UPPDATERA ÅR) ---------------------------------------------------------


pxweb_query_list <- 
  list("Region"=c(Kommuner),
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("1","2"),
       "Fodelseregion"=c("09","11"),
       "ContentsCode"=c("000001E7","000001E8","000001EA","000001E9"),
       "Tid"=c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022", "2023"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/FlyttFodReg",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

flyttningar <- as_tibble(px_data_frame)

inflyttningar <- flyttningar |>select(region, ålder, kön, födelseregion, år, "Inflyttningar från kommuner inom länet", "Inflyttningar från övriga län")
utflyttningar <- flyttningar |>select(region, ålder, kön, födelseregion, år, "Utflyttningar till kommuner inom länet", "Utflyttningar till övriga län")

inflyttningar <- inflyttningar |> filter(region != "Riket")
utflyttningar <- utflyttningar |> filter(region != "Riket")

########### Inflyttningar
inflyttningar <- inflyttningar %>%
  group_by(region, ålder, kön, år) %>% # inkludera alla kolumner utom 'födelseregion'
  summarize(Inflyttningar_kommuner = sum(`Inflyttningar från kommuner inom länet`),
            Inflyttningar_län = sum(`Inflyttningar från övriga län`),
            .groups = 'drop') # För att undvika grupperingsvarning

inflyttningar$Totalt <- inflyttningar$Inflyttningar_kommuner+inflyttningar$Inflyttningar_län
inflyttningar_totalt <- inflyttningar |> select(region, ålder, kön, år, Totalt)

inflyttningar_totalt <- namnge_och_rensa_kolumner(inflyttningar_totalt)


inflyttningar_totalt <- inflyttningar_totalt %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

inflyttningar_lan <- inflyttningar |> select(region, ålder, kön, år, Inflyttningar_län)

inflyttningar_lan <- namnge_och_rensa_kolumner(inflyttningar_lan)


inflyttningar_lan <- inflyttningar_lan %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

inflyttning_lan_risk <-  inflyttningar_lan %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / inflyttningar_totalt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / inflyttningar_totalt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / inflyttningar_totalt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / inflyttningar_totalt$Nioårsgenomsnitt
  )

# Ersätt NA med 0 i specifika kolumner
inflyttning_lan_risk <- inflyttning_lan_risk %>%
  mutate(
    Ettårsgenomsnitt = ifelse(is.na(Ettårsgenomsnitt), 0, Ettårsgenomsnitt),
    Treårsgenomsnitt = ifelse(is.na(Treårsgenomsnitt), 0, Treårsgenomsnitt),
    Femårsgenomsnitt = ifelse(is.na(Femårsgenomsnitt), 0, Femårsgenomsnitt),
    Nioårsgenomsnitt = ifelse(is.na(Nioårsgenomsnitt), 0, Nioårsgenomsnitt)
  )

########## Utflyttningar

utflyttningar <- utflyttningar %>%
  group_by(region, ålder, kön, år) %>% # inkludera alla kolumner utom 'födelseregion'
  summarize(Utflyttningar_kommuner = sum(`Utflyttningar till kommuner inom länet`),
            Utflyttningar_län = sum(`Utflyttningar till övriga län`),
            .groups = 'drop') # För att undvika grupperingsvarning

utflyttningar$Totalt <- utflyttningar$Utflyttningar_kommuner+utflyttningar$Utflyttningar_län
utflyttningar_totalt <- utflyttningar |> select(region, ålder, kön, år, Totalt)

utflyttningar_totalt <- namnge_och_rensa_kolumner(utflyttningar_totalt)


utflyttningar_totalt <- utflyttningar_totalt %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

utflyttningar_lan <- utflyttningar |> select(region, ålder, kön, år, Utflyttningar_län)

utflyttningar_lan <- namnge_och_rensa_kolumner(utflyttningar_lan)


utflyttningar_lan <- utflyttningar_lan %>%
  group_by(Ålder) %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = rollapply(Värde, width = 1, FUN = mean, fill = NA, align = 'right'),
    Treårsgenomsnitt = rollapply(Värde, width = 3, FUN = mean, fill = NA, align = 'right'),
    Femårsgenomsnitt = rollapply(Värde, width = 5, FUN = mean, fill = NA, align = 'right'),
    Nioårsgenomsnitt = rollapply(Värde, width = 9, FUN = mean, fill = NA, align = 'right')
  ) %>%
  ungroup() |> 
  filter(År == last(År))

utflyttning_lan_risk <-  utflyttningar_lan %>%
  arrange(Kön, Region, Ålder, År) %>%
  mutate(
    Ettårsgenomsnitt = Ettårsgenomsnitt / utflyttningar_totalt$Ettårsgenomsnitt,
    Treårsgenomsnitt = Treårsgenomsnitt / utflyttningar_totalt$Treårsgenomsnitt,
    Femårsgenomsnitt = Femårsgenomsnitt / utflyttningar_totalt$Femårsgenomsnitt,
    Nioårsgenomsnitt = Nioårsgenomsnitt / utflyttningar_totalt$Nioårsgenomsnitt
  )

# Ersätt NA med 0 i specifika kolumner
utflyttning_lan_risk <- utflyttning_lan_risk %>%
  mutate(
    Ettårsgenomsnitt = ifelse(is.na(Ettårsgenomsnitt), 0, Ettårsgenomsnitt),
    Treårsgenomsnitt = ifelse(is.na(Treårsgenomsnitt), 0, Treårsgenomsnitt),
    Femårsgenomsnitt = ifelse(is.na(Femårsgenomsnitt), 0, Femårsgenomsnitt),
    Nioårsgenomsnitt = ifelse(is.na(Nioårsgenomsnitt), 0, Nioårsgenomsnitt)
  )

######################### Beräkna antal in/utflyttare utomläns baserat på kommuner

inflyttningar_prognos <- sammanslagen_tabell |>  filter(Variabel == "Inrikes inflyttningar")
utflyttningar_prognos <- sammanslagen_tabell |> filter(Variabel == "Inrikes utflyttningar")

inflyttningar_prognos$År <- as.numeric(inflyttningar_prognos$År)
utflyttningar_prognos$År <- as.numeric(utflyttningar_prognos$År)
inflyttning_lan_risk$År <- as.numeric(inflyttning_lan_risk$År)
utflyttning_lan_risk$År <- as.numeric(utflyttning_lan_risk$År)

inflyttningar_prognos <- inflyttningar_prognos %>%
  pivot_wider(names_from = Genomsnitt, values_from = Värde)

utflyttningar_prognos <- utflyttningar_prognos %>%
  pivot_wider(names_from = Genomsnitt, values_from = Värde)
############ Beräkna inflyttningar
# Förbered inflyttning_lan_risk
inflyttning_lan_risk <- inflyttning_lan_risk |> 
  select(Region, Ålder, Kön, År, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt) |> 
  arrange(Ålder, Kön, Region)

# Funktion för att utföra multiplikationen för ett specifikt år
calculate_for_year <- function(year) {
  inflyttningar_prognos_filter <- inflyttningar_prognos |>
    filter(År == year) |>
    select(Region, Ålder, Kön, År, Variabel, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt) |>
    arrange(Ålder, Kön, Region) |>
    ungroup() |>
    mutate(
      Ettårsgenomsnitt = Ettårsgenomsnitt * inflyttning_lan_risk$Ettårsgenomsnitt,
      Treårsgenomsnitt = Treårsgenomsnitt * inflyttning_lan_risk$Treårsgenomsnitt,
      Femårsgenomsnitt = Femårsgenomsnitt * inflyttning_lan_risk$Femårsgenomsnitt,
      Nioårsgenomsnitt = Nioårsgenomsnitt * inflyttning_lan_risk$Nioårsgenomsnitt
    )
  return(inflyttningar_prognos_filter)
}

# Hämta unika år
unique_years <- unique(inflyttningar_prognos$År)

# Applicera funktionen för varje år och sammanfoga resultaten
inrikes_inflyttningar_klar <- lapply(unique_years, calculate_for_year) |> bind_rows()

inrikes_inflyttningar_klar <- pivot_longer(inrikes_inflyttningar_klar, cols = 6:9, names_to = "Genomsnitt", values_to = "Värde")

inrikes_inflyttningar_klar <- inrikes_inflyttningar_klar |> group_by(Ålder, Kön, År, Variabel, Genomsnitt) |>
  summarize(Värde = sum(Värde))


###################### Beräkna utflyttningar
utflyttningar_prognos <- utflyttningar_prognos |> filter(Ålder <=100)

# Förbered inflyttning_lan_risk
utflyttning_lan_risk <- utflyttning_lan_risk |> 
  select(Region, Ålder, Kön, År, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt) |> 
  arrange(Ålder, Kön, Region)

# Funktion för att utföra multiplikationen för ett specifikt år
calculate_for_year <- function(year) {
  utflyttningar_prognos_filter <- utflyttningar_prognos |>
    filter(År == year) |>
    select(Region, Ålder, Kön, År, Variabel, Ettårsgenomsnitt, Treårsgenomsnitt, Femårsgenomsnitt, Nioårsgenomsnitt) |>
    arrange(Ålder, Kön, Region) |>
    ungroup() |>
    mutate(
      Ettårsgenomsnitt = Ettårsgenomsnitt * utflyttning_lan_risk$Ettårsgenomsnitt,
      Treårsgenomsnitt = Treårsgenomsnitt * utflyttning_lan_risk$Treårsgenomsnitt,
      Femårsgenomsnitt = Femårsgenomsnitt * utflyttning_lan_risk$Femårsgenomsnitt,
      Nioårsgenomsnitt = Nioårsgenomsnitt * utflyttning_lan_risk$Nioårsgenomsnitt
    )
  return(utflyttningar_prognos_filter)
}

# Hämta unika år
unique_years <- unique(utflyttningar_prognos$År)

# Applicera funktionen för varje år och sammanfoga resultaten
inrikes_utflyttningar_klar <- lapply(unique_years, calculate_for_year) |> bind_rows()

inrikes_utflyttningar_klar <- pivot_longer(inrikes_utflyttningar_klar, cols = 6:9, names_to = "Genomsnitt", values_to = "Värde")

inrikes_utflyttningar_klar <- inrikes_utflyttningar_klar |> group_by(Ålder, Kön, År, Variabel, Genomsnitt) |>
  summarize(Värde = sum(Värde))

############# Summera för region

region <- sammanslagen_tabell |> group_by(Ålder, Kön, År, Variabel, Genomsnitt) |> 
  summarize(Värde = sum(Värde))

region <- region |> filter(Variabel != "Inrikes inflyttningar" & Variabel != "Inrikes utflyttningar")

region <- rbind(region, inrikes_inflyttningar_klar)
region <- rbind(region, inrikes_utflyttningar_klar)

region$Region <- "Hallands län"

region <- region |> select(Ålder, Kön, Region, År, Variabel, Genomsnitt, Värde)

sammanslagen_tabell <- rbind(sammanslagen_tabell, region)


# Historisk data och sammanslagning (OBS! UPPPDATERA LÄN; KOMMUN; ÅR) ---------------------------------------


År <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023")
Kommuner <- c("13","1315","1380","1381","1382","1383","1384")



# Totalbefolkning senaste året

pxweb_query_list <- 
  list("Region"= Kommuner,
       "Alder"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100+"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101N1"),
       "Tid"= År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy",
            query = pxweb_query_list)

totfolkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

totfolkmangd <- namnge_och_rensa_kolumner(totfolkmangd)

totfolkmangd$Variabel <- "Totalt"

# Födda

pxweb_query_list <- 
  list("Region"=Kommuner,
       "AlderModer"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101E2"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101H/FoddaK",
            query = pxweb_query_list)

fodda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

fodda <- namnge_och_rensa_kolumner(fodda)

fodda <- drop_na(fodda)

fodda <- fodda |> group_by(Region, År, Kön) |> 
  summarize(Värde = sum(Värde))

fodda$Ålder <- 0

fodda$Variabel <- "Födda"



fodda <- fodda |> select(Region, Ålder, Kön, År, Värde, Variabel)

## Döda

# Döda

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101D8"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101I/DodaFodelsearK",
            query = pxweb_query_list)

doda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

doda <- namnge_och_rensa_kolumner(doda)

doda <- drop_na(doda)

doda$Variabel <- "Döda"



# Invandring

pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AX"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

invandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

invandring <- namnge_och_rensa_kolumner(invandring)

invandring$Variabel <- "Invandringar"

## Utvandring

# Utvandring 
pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101AY"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

utvandring <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

utvandring <- namnge_och_rensa_kolumner(utvandring)

utvandring$Variabel <- "Utvandringar"

# Inrikes inflyttade 


pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A2"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_inflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

inrikes_inflyttade <- namnge_och_rensa_kolumner(inrikes_inflyttade)

inrikes_inflyttade$Variabel <- "Inrikes inflyttningar"

# Inrikes utflyttade


pxweb_query_list <- 
  list("Region"=Kommuner,
       "Alder"=c("*"),
       "Kon"=c("*"),
       "ContentsCode"=c("BE0101A3"),
       "Tid"=År)

px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/Flyttningar97",
            query = pxweb_query_list)

inrikes_utflyttade <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

inrikes_utflyttade <- namnge_och_rensa_kolumner(inrikes_utflyttade)

inrikes_utflyttade$Variabel <- "Inrikes utflyttningar"



totfolkmangd <- as_tibble(totfolkmangd)
fodda <- as_tibble(fodda)
doda <- as_tibble(doda)
invandring <- as_tibble(invandring)
utvandring <- as_tibble(utvandring)
inrikes_inflyttade <- as_tibble(inrikes_inflyttade)
inrikes_utflyttade <- as_tibble(inrikes_utflyttade)


sammanslagen_historik <- rbind(totfolkmangd, fodda, doda, invandring, utvandring, inrikes_inflyttade, inrikes_utflyttade)
sammanslagen_historik$Genomsnitt <- "Faktiskt utfall"
sammanslagen_historik <- sammanslagen_historik |> select(Ålder, Kön, Region, År, Variabel, Genomsnitt, Värde)
sammanslagen_historik$År <- as.numeric(sammanslagen_historik$År)


sammanslagen_master <- rbind(sammanslagen_tabell, sammanslagen_historik)
sammanslagen_master <- sammanslagen_master |> select(Region, Ålder, Kön, År, Variabel, Genomsnitt, Värde)
sammanslagen_master <- sammanslagen_master |> filter(Variabel != "0")
sammanslagen_master <- drop_na(sammanslagen_master)

sammanslagen_master_sum <- sammanslagen_master |> 
  group_by(Region, År, Variabel, Genomsnitt) |> 
  summarize(Värde = sum(Värde))

brett_dataset <- sammanslagen_master_sum %>%
  pivot_wider(
    names_from = Variabel,
    values_from = Värde
  )

födelsenetto <- brett_dataset$Födda-brett_dataset$Döda
inrikesflyttnetto <- brett_dataset$`Inrikes inflyttningar`-brett_dataset$`Inrikes utflyttningar`
utrikesflyttnetto <- brett_dataset$Invandringar-brett_dataset$Utvandringar

brett_dataset$Födelsenetto <- födelsenetto
brett_dataset$`Inrikes flyttnetto` <- inrikesflyttnetto
brett_dataset$`Utrikes flyttnetto` <- utrikesflyttnetto

# Omvandla tillbaka till långt format
sammanslagen_master_sum <- brett_dataset %>%
  pivot_longer(
    cols = c(Döda, Födda, `Inrikes inflyttningar`, `Inrikes utflyttningar`, Invandringar, Totalt, Utvandringar, Födelsenetto, `Inrikes flyttnetto`, `Utrikes flyttnetto`),
    names_to = "Variabel",
    values_to = "Värde"
  )

sammanslagen_master_sum <- sammanslagen_master_sum |>  group_by(Region, År, Genomsnitt, Variabel) |> 
  summarize(Värde = sum(Värde))

totalt <- sammanslagen_master_sum |> filter(Variabel == "Totalt")



# Spara det senaste året för 'Faktiskt utfall' i en variabel
sista_faktiska_ar <- max(totalt$År[totalt$Genomsnitt == "Faktiskt utfall"])


# Skapa en ny tibble som innehåller alla genomsnittsnamn
genomsnittsnamn <- tibble(Genomsnitt = c("Faktiskt utfall", "Ettårsgenomsnitt", 
                                         "Treårsgenomsnitt", "Femårsgenomsnitt", 
                                         "Nioårsgenomsnitt"))



# Expandera den befintliga datan för att inkludera de nya genomsnittsnamnen
totalt_kalk <- totalt %>%
  filter(Genomsnitt == "Faktiskt utfall", År <= sista_faktiska_ar) %>%
  select(Region, År, Värde) %>% # Välj bara de kolumner som behövs för expansionen
  cross_join(genomsnittsnamn) %>% # Skapa alla kombinationer av rader och genomsnittsnamn
  mutate(Variabel = "Totalt") # Sätt Variabel till "Totalt" för alla nya rader

totalt_kalk <- totalt_kalk |> select(Region, År, Värde, Genomsnitt.y, Variabel)

totalt_kalk <- drop_na(totalt_kalk)

totalt_kalk <- rename(totalt_kalk, Genomsnitt = Genomsnitt.y)

totalt_kalk <- totalt_kalk |> select(Region, År, Genomsnitt, Variabel, Värde)

totalt <- rbind(totalt, totalt_kalk)

totalt <- totalt |> filter(Genomsnitt != "Faktiskt utfall")

totalt$Variabel <- "Förändring folkmängd"

# Beräkna den årliga förändringen för varje Region och Genomsnitt
totalt <- totalt %>%
  arrange(Region, Genomsnitt, År) %>% # Se till att datan är ordnad
  group_by(Region, Genomsnitt) %>%
  mutate(Förändring = Värde - lag(Värde)) %>%
  ungroup() # Ta bort grupperingen för vidare användning av datan

totalt <- drop_na(totalt)

totalt <- totalt |> select(Region, År, Genomsnitt, Variabel, Förändring)

totalt <- rename(totalt, Värde = "Förändring")

# Lägg till en ny kolumn 'Kategori' baserad på året i jämförelse med 'sista_faktiska_ar'

totalt <- totalt |> select(Region, År, Genomsnitt, Variabel, Värde)


sammanslagen_master_sum <- rbind(sammanslagen_master_sum, totalt)

# Expandera den befintliga datan för att inkludera de nya genomsnittsnamnen
historik <- sammanslagen_master_sum %>%
  filter(Genomsnitt == "Faktiskt utfall", År <= sista_faktiska_ar) %>%
  select(Region, År, Variabel, Värde) %>% # Välj bara de kolumner som behövs för expansionen
  cross_join(genomsnittsnamn)

historik  <- rename(historik , Genomsnitt = Genomsnitt.y)

historik  <- historik  |> select(Region, År, Genomsnitt, Variabel, Värde)

sammanslagen_master_sum <- rbind(sammanslagen_master_sum, historik)

sammanslagen_master_sum <- sammanslagen_master_sum |> filter(Genomsnitt != "Faktiskt utfall")

sammanslagen_master_sum <- sammanslagen_master_sum %>%
  mutate(Kategori = if_else(År <= sista_faktiska_ar, "Faktiskt utfall", "Prognos"))

sammanslagen_master_sum <- sammanslagen_master_sum  %>%
  mutate(Variabel = case_when(
    Variabel == "Totalt" ~ "Folkmängd",
    TRUE ~ Variabel
  ))

saveRDS(sammanslagen_master, "befolkningsprognos_ettar_2024_t.rds")
saveRDS(sammanslagen_master_sum, "befolkningsprognos_sum_2024_t.rds")
write_xlsx(sammanslagen_master_sum, "befolkningsprognos_sum_2024.xlsx")
 # OBS! Skriv in kommuner och år
