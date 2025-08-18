############################################################
#           REGIONAL BEFOLKNINGSPROGNOS MED AVSTÄMNING     #
############################################################
#                                                          #
#    Genomför prognos för ett län och dess kommuner        #
#    med avstämning så att kommunsummor = länstotaler      #
#                                                          #
#    Avstämningsmetod:                                     #
#    - Länets totaler anses mest tillförlitliga            #
#    - Kommunernas värden justeras proportionellt          #
#    - Särskild hantering av länsgränsflyttningar          #
#                                                          #
#    Förväntar följande variabler från huvudskriptet:      #
#    - SCENARIO_ATT_KORA: "standard" eller "alternativ"   #
#    - lan_namn: namn på länet                             #
#    - kommuner: vektor med kommunnamn                     #
#                                                          #
############################################################

library(tidyverse)

set.seed(2025) # För reproducerbarhet så håller vi rbinom() konsekvent, dvs. utfallet blir slumpmässigt men samma vid varje körning

# ===========================================================
# LÄSA IN DATA
# ===========================================================

message("\n📊 LÄSER IN DATA FÖR REGIONAL PROGNOS...")

kommun_lista <- read_rds("Data_underlag/kommun_lista.rds")
riket_lista <- read_rds("Data_underlag/riket_lista.rds")

# Välj riskmapp baserat på scenario
if (SCENARIO_ATT_KORA == "alternativ") {
  risk_path <- "Data_riskmatt/scenario_alternativ"
  message(sprintf("  Läser alternativa risktal från: %s", risk_path))
} else {
  risk_path <- "Data_riskmatt"
  message(sprintf("  Läser standardrisktal från: %s", risk_path))
}

## Läs in risktal ##
fodelserisker <- read_rds(file.path(risk_path, "fodelserisker.rds"))
dodsrisker <- read_rds(file.path(risk_path, "dodsrisker.rds"))
inflyttningsrisker <- read_rds(file.path(risk_path, "inflyttningsrisker.rds"))
utflyttningsrisker <- read_rds(file.path(risk_path, "utflyttningsrisker.rds"))
utvandringsrisker <- read_rds(file.path(risk_path, "utvandringsrisker.rds"))
invandringsrisker <- read_rds(file.path(risk_path, "invandringsrisker.rds"))
inflyttningsandelar_lansgrans <- read_rds(file.path(risk_path, "inflyttningsandelar_lansgrans.rds"))
utflyttningsandelar_lansgrans <- read_rds(file.path(risk_path, "utflyttningsandelar_lansgrans.rds"))

# Samla risktal i lista
risktal <- list(
  fodelserisker = fodelserisker,
  dodsrisker = dodsrisker,
  inflyttningsrisker = inflyttningsrisker,
  utflyttningsrisker = utflyttningsrisker,
  invandringsrisker = invandringsrisker,
  utvandringsrisker = utvandringsrisker,
  inflyttningsandelar_lansgrans = inflyttningsandelar_lansgrans,
  utflyttningsandelar_lansgrans = utflyttningsandelar_lansgrans
)

risktal <- risktal %>% 
  map(as_tibble)

# ===========================================================
# IDENTIFIERA OCH VALIDERA GEOGRAFIER
# ===========================================================

# Hämta basbefolkning från senaste året
basbefolkning_kommun <- kommun_lista$totfolkmangd %>% 
  filter(År == max(År)) %>%
  mutate(Ålder = as.numeric(Ålder))

# Validera att län och kommuner finns
if (!lan_namn %in% unique(basbefolkning_kommun$Region)) {
  stop(paste("❌ Länet", lan_namn, "finns inte i data!"))
}

saknade_kommuner <- kommuner[!kommuner %in% unique(basbefolkning_kommun$Region)]
if (length(saknade_kommuner) > 0) {
  stop(paste("❌ Följande kommuner finns inte i data:", paste(saknade_kommuner, collapse = ", ")))
}

# Filtrera basbefolkning för valda geografier
basbefolkning_kommun <- basbefolkning_kommun %>%
  filter(Region %in% c(lan_namn, kommuner))

message(sprintf("  ✅ Data inläst för: %s", lan_namn))
message(sprintf("  ✅ Antal kommuner: %d", length(kommuner)))

# Förbered riksprognosdata
riksbefolkning_prognos <- riket_lista$riket_prognosinvånare_grund
invandring_till_riket_prognos <- riket_lista$invandring_riket

# Konvertera år till character för konsistent hantering
if(!is.character(riksbefolkning_prognos$År)) {
  riksbefolkning_prognos <- riksbefolkning_prognos %>% 
    mutate(År = as.character(År))
}

if(!is.character(invandring_till_riket_prognos$År)) {
  invandring_till_riket_prognos <- invandring_till_riket_prognos %>% 
    mutate(År = as.character(År))
}

# ===========================================================
# HJÄLPFUNKTIONER (delade med enskild prognos)
# ===========================================================

# Funktion för att åldra befolkningen ett år
aldra_befolkning <- function(befolkning) {
  # Öka åldern med 1
  aldrad_befolkning <- befolkning %>%
    mutate(Ålder = Ålder + 1) %>%
    filter(Ålder <= 100)
  
  # Hantera 100+ åringar
  aldrad_100_plus <- aldrad_befolkning %>%
    filter(Ålder >= 100) %>%
    group_by(Region, Kön, År, Variabel) %>%
    summarise(Värde = sum(Värde), .groups = "drop") %>%
    mutate(Ålder = 100)
  
  # Kombinera
  aldrad_befolkning <- aldrad_befolkning %>%
    filter(Ålder < 100) %>%
    bind_rows(aldrad_100_plus)
  
  return(aldrad_befolkning)
}

# Funktion för att beräkna antal födda
berakna_fodda <- function(befolkning, fodelserisker_data, prognos_ar) {
  # Filtrera kvinnor i fertil ålder
  kvinnor_fertil <- befolkning %>%
    filter(Kön == "kvinnor", Ålder >= 15, Ålder <= 49)
  
  # Hämta födelserisker för aktuellt år
  fodelserisker_ar <- fodelserisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(fodelserisker_ar) == 0) {
    stop(paste("Inga födelserisker finns för år", prognos_ar))
  }
  
  # Beräkna antal födda
  fodda_per_alder <- kvinnor_fertil %>%
    left_join(
      fodelserisker_ar %>% select(Region, Ålder, Fodelserisk = Värde),
      by = c("Region", "Ålder")
    ) %>%
    mutate(
      Antal_fodda = Värde * Fodelserisk,
      Antal_fodda = replace_na(Antal_fodda, 0)
    ) %>%
    group_by(Region) %>%
    summarise(Antal_fodda = sum(Antal_fodda, na.rm = TRUE), .groups = "drop")
  
  # Fördela på kön (51.2% pojkar, 48.8% flickor)
  fodda_pojkar <- fodda_per_alder %>%
    mutate(
      Kön = "män",
      Ålder = 0,
      År = prognos_ar,
      Värde = round(Antal_fodda * 0.512, 0),
      Variabel = "Total folkmängd"
    ) %>%
    select(Region, Ålder, Kön, År, Värde, Variabel)
  
  fodda_flickor <- fodda_per_alder %>%
    mutate(
      Kön = "kvinnor",
      Ålder = 0,
      År = prognos_ar,
      Värde = round(Antal_fodda * 0.488, 0),
      Variabel = "Total folkmängd"
    ) %>%
    select(Region, Ålder, Kön, År, Värde, Variabel)
  
  fodda <- bind_rows(fodda_pojkar, fodda_flickor)
  
  fodda_rapport <- fodda %>%
    mutate(Variabel = "Födda")
  
  return(list(fodda = fodda, fodda_rapport = fodda_rapport))
}

# Funktion för att beräkna döda
berakna_doda <- function(befolkning, dodsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta dödsrisker för aktuellt år
  dodsrisker_ar <- dodsrisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(dodsrisker_ar) == 0) {
    stop(paste("Inga dödsrisker finns för år", prognos_ar))
  }
  
  # Beräkna antal döda - det finns tre olika metoder: "stokastisk", "heltal", "ingen".
  if (AVRUNDNING == "stokastisk") {
    doda <- befolkning %>%
      left_join(
        dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Dodsrisk = case_when(
          is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
            dplyr::pull(Värde) %>% first(),
          TRUE ~ Dodsrisk
        ),
        Antal_doda_raw = map2_dbl(Värde, Dodsrisk, ~ {
          if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1) {
            NA_real_
          } else {
            tryCatch(rbinom(1, size = .x, prob = .y), warning = function(w) NA_real_, error = function(e) NA_real_)
          }
        }),
        Antal_doda = replace_na(Antal_doda_raw, 0),
        Värde = Antal_doda,
        Variabel = "Döda"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  } else if (AVRUNDNING == "heltal") {
    
    doda <- befolkning %>%
      left_join(
        dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd dödsrisken för 100-åringar
        Dodsrisk = case_when(
          is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Dodsrisk
        ),
        Antal_doda = round(Värde * Dodsrisk, 0)
      ) %>%
      mutate(
        Värde = Antal_doda,
        Variabel = "Döda"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  } else if (AVRUNDNING == "ingen") {
    
    doda <- befolkning %>%
      left_join(
        dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd dödsrisken för 100-åringar
        Dodsrisk = case_when(
          is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Dodsrisk
        ),
        Antal_doda = Värde * Dodsrisk
      ) %>%
      mutate(
        Värde = Antal_doda,
        Variabel = "Döda"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  }
  return(doda)
}

# Funktion för att beräkna inrikes inflyttningar
berakna_inrikes_inflyttningar <- function(inflyttningsrisker_data, riksbefolkning, prognos_ar) {
  # Hämta riksbefolkning för aktuellt år
  riksbef_ar <- riksbefolkning %>%
    filter(År == prognos_ar)
  
  # Hämta inflyttningsrisker för aktuellt år
  inflyttningsrisker_ar <- inflyttningsrisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(inflyttningsrisker_ar) == 0) {
    stop(paste("Inga inflyttningsrisker finns för år", prognos_ar))
  }
  
  # Beräkna inflyttningar - det finns tre olika metoder: "stokastisk", "heltal", "ingen"
  if (AVRUNDNING == "stokastisk") {
    inflyttningar <- expand_grid(
      Region = unique(inflyttningsrisker_ar$Region),
      Kön = unique(riksbef_ar$Kön),
      Ålder = unique(riksbef_ar$Ålder)
    ) %>%
      left_join(
        riksbef_ar %>% select(Kön, Ålder, Riksbefolkning = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        inflyttningsrisker_ar %>% select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Antal_inflyttningar_raw = map2_dbl(
          Riksbefolkning, Inflyttningsrisk,
          ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1) {
            rbinom(1, size = .x, prob = .y)
          } else {
            NA_real_
          }
        ),
        Antal_inflyttningar = replace_na(Antal_inflyttningar_raw, 0),
        År = prognos_ar,
        Värde = Antal_inflyttningar,
        Variabel = "Inrikes inflyttning"
      ) %>%
      select(-Antal_inflyttningar_raw)
  } else if (AVRUNDNING == "heltal") {
    
    inflyttningar <- expand_grid(
      Region = unique(inflyttningsrisker_ar$Region),
      Kön = unique(riksbef_ar$Kön),
      Ålder = unique(riksbef_ar$Ålder)
    ) %>%
      left_join(
        riksbef_ar %>% select(Kön, Ålder, Riksbefolkning = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        inflyttningsrisker_ar %>% select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Antal_inflyttningar = round(Riksbefolkning * Inflyttningsrisk, 0),
        Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
        År = prognos_ar,
        Värde = Antal_inflyttningar,
        Variabel = "Inrikes inflyttning"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  } else if (AVRUNDNING == "ingen") {
    
    inflyttningar <- expand_grid(
      Region = unique(inflyttningsrisker_ar$Region),
      Kön = unique(riksbef_ar$Kön),
      Ålder = unique(riksbef_ar$Ålder)
    ) %>%
      left_join(
        riksbef_ar %>% select(Kön, Ålder, Riksbefolkning = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        inflyttningsrisker_ar %>% select(Region, Kön, Ålder, Inflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Antal_inflyttningar = Riksbefolkning * Inflyttningsrisk,
        Antal_inflyttningar = replace_na(Antal_inflyttningar, 0),
        År = prognos_ar,
        Värde = Antal_inflyttningar,
        Variabel = "Inrikes inflyttning"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  }
  
  return(inflyttningar)
}

# Funktion för att beräkna inrikes utflyttningar
berakna_inrikes_utflyttningar <- function(befolkning, utflyttningsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta utflyttningsrisker för aktuellt år
  utflyttningsrisker_ar <- utflyttningsrisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(utflyttningsrisker_ar) == 0) {
    stop(paste("Inga utflyttningsrisker finns för år", prognos_ar))
  }
  
  # Beräkna utflyttningar - det finns tre olika metoder: "stokastisk", "heltal", "ingen"
  if (AVRUNDNING == "stokastisk") {
    utflyttningar <- befolkning %>%
      left_join(
        utflyttningsrisker_ar %>% select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Utflyttningsrisk = case_when(
          is.na(Utflyttningsrisk) & Ålder > 100 ~ utflyttningsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
            dplyr::pull(Värde) %>% first(),
          TRUE ~ Utflyttningsrisk
        ),
        Antal_utflyttningar_raw = map2_dbl(Värde, Utflyttningsrisk, ~ {
          if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1) {
            NA_real_
          } else {
            tryCatch(rbinom(1, size = .x, prob = .y), warning = function(w) NA_real_, error = function(e) NA_real_)
          }
        }),
        Antal_utflyttningar = replace_na(Antal_utflyttningar_raw, 0),
        Värde = Antal_utflyttningar,
        Variabel = "Inrikes utflyttning"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  } else if (AVRUNDNING == "heltal") {
    
    utflyttningar <- befolkning %>%
      left_join(
        utflyttningsrisker_ar %>% select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd risken för 100-åringar
        Utflyttningsrisk = case_when(
          is.na(Utflyttningsrisk) & Ålder > 100 ~ utflyttningsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Utflyttningsrisk
        ),
        Utflyttningsrisk = replace_na(Utflyttningsrisk, 0),
        Antal_utflyttningar = round(Värde * Utflyttningsrisk, 0),
        Värde = Antal_utflyttningar,
        Variabel = "Inrikes utflyttning"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  } else if (AVRUNDNING == "ingen") {
    
    utflyttningar <- befolkning %>%
      left_join(
        utflyttningsrisker_ar %>% select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd risken för 100-åringar
        Utflyttningsrisk = case_when(
          is.na(Utflyttningsrisk) & Ålder > 100 ~ utflyttningsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Utflyttningsrisk
        ),
        Utflyttningsrisk = replace_na(Utflyttningsrisk, 0),
        Antal_utflyttningar = Värde * Utflyttningsrisk,
        Värde = Antal_utflyttningar,
        Variabel = "Inrikes utflyttning"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  }
  
  return(utflyttningar)
}

# Funktion för att beräkna invandring
berakna_invandring <- function(invandringsrisker_data, invandring_riket, prognos_ar) {
  # Hämta invandring till riket för aktuellt år
  invandring_ar <- invandring_riket %>%
    filter(År == prognos_ar)
  
  # Hämta invandringsrisker för aktuellt år
  invandringsrisker_ar <- invandringsrisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(invandringsrisker_ar) == 0) {
    stop(paste("Inga invandringsrisker finns för år", prognos_ar))
  }
  
  # Beräkna invandring per kommun - det finns tre olika metoder: "stokastisk", "heltal", "ingen"
  if (AVRUNDNING == "stokastisk") {
      
    invandring <- expand_grid(
      Region = unique(invandringsrisker_ar$Region),
      Kön = unique(invandring_ar$Kön),
      Ålder = unique(invandring_ar$Ålder)
    ) %>%
      left_join(
        invandring_ar %>% select(Kön, Ålder, Riksinvandring = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        invandringsrisker_ar %>% select(Region, Kön, Ålder, Invandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
    mutate(
      Antal_invandrare_raw = map2_dbl(
        Riksinvandring, Invandringsrisk,
        ~ if (!is.na(.x) && !is.na(.y) && .x >= 0 && .y >= 0 && .y <= 1) {
          rbinom(1, size = .x, prob = .y)
        } else {
          NA_real_
        }
      ),
      Antal_invandrare = replace_na(Antal_invandrare_raw, 0),
      År = prognos_ar,
      Värde = Antal_invandrare,
      Variabel = "Invandring"
    ) %>%
      select(-Antal_invandrare_raw)
  } else if (AVRUNDNING == "heltal") {
   
    invandring <- expand_grid(
      Region = unique(invandringsrisker_ar$Region),
      Kön = unique(invandring_ar$Kön),
      Ålder = unique(invandring_ar$Ålder)
    ) %>%
      left_join(
        invandring_ar %>% select(Kön, Ålder, Riksinvandring = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        invandringsrisker_ar %>% select(Region, Kön, Ålder, Invandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Antal_invandrare = round(Riksinvandring * Invandringsrisk, 0),
        Antal_invandrare = replace_na(Antal_invandrare, 0),
        År = prognos_ar,
        Värde = Antal_invandrare,
        Variabel = "Invandring"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
     
  } else if (AVRUNDNING == "ingen") {
    
    invandring <- expand_grid(
      Region = unique(invandringsrisker_ar$Region),
      Kön = unique(invandring_ar$Kön),
      Ålder = unique(invandring_ar$Ålder)
    ) %>%
      left_join(
        invandring_ar %>% select(Kön, Ålder, Riksinvandring = Värde),
        by = c("Kön", "Ålder")
      ) %>%
      left_join(
        invandringsrisker_ar %>% select(Region, Kön, Ålder, Invandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Antal_invandrare = Riksinvandring * Invandringsrisk,
        Antal_invandrare = replace_na(Antal_invandrare, 0),
        År = prognos_ar,
        Värde = Antal_invandrare,
        Variabel = "Invandring"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  }
  
  return(invandring)
}

# Funktion för att beräkna utvandring
berakna_utvandring <- function(befolkning, utvandringsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta utvandringsrisker för aktuellt år
  utvandringsrisker_ar <- utvandringsrisker_data %>%
    filter(År == prognos_ar)
  
  if(nrow(utvandringsrisker_ar) == 0) {
    stop(paste("Inga utvandringsrisker finns för år", prognos_ar))
  }
  
  # Beräkna utvandring - det finns tre olika metoder: "stokastisk", "heltal", "ingen"
  if (AVRUNDNING == "stokastisk") {
    
    utvandring <- befolkning %>%
      left_join(
        utvandringsrisker_ar %>% select(Region, Kön, Ålder, Utvandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Utvandringsrisk = case_when(
          is.na(Utvandringsrisk) & Ålder > 100 ~ utvandringsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>%
            dplyr::pull(Värde) %>% first(),
          TRUE ~ Utvandringsrisk
        ),
        Antal_utvandrare_raw = map2_dbl(Värde, Utvandringsrisk, ~ {
          if (is.na(.x) || is.na(.y) || .x < 0 || .y < 0 || .y > 1) {
            NA_real_
          } else {
            tryCatch(rbinom(1, size = .x, prob = .y), warning = function(w) NA_real_, error = function(e) NA_real_)
          }
        }),
        Antal_utvandrare = replace_na(Antal_utvandrare_raw, 0),
        Värde = Antal_utvandrare,
        Variabel = "Utvandring"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  } else if (AVRUNDNING == "heltal") {
    
    utvandring <- befolkning %>%
      left_join(
        utvandringsrisker_ar %>% select(Region, Kön, Ålder, Utvandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd risken för 100-åringar
        Utvandringsrisk = case_when(
          is.na(Utvandringsrisk) & Ålder > 100 ~ utvandringsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Utvandringsrisk
        ),
        Utvandringsrisk = replace_na(Utvandringsrisk, 0),
        Antal_utvandrare = round(Värde * Utvandringsrisk, 0),
        Värde = Antal_utvandrare,
        Variabel = "Utvandring"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
  } else if (AVRUNDNING == "ingen") {
    
    utvandring <- befolkning %>%
      left_join(
        utvandringsrisker_ar %>% select(Region, Kön, Ålder, Utvandringsrisk = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        # För åldrar över 100, använd risken för 100-åringar
        Utvandringsrisk = case_when(
          is.na(Utvandringsrisk) & Ålder > 100 ~ utvandringsrisker_ar %>% 
            filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
            dplyr::pull(Värde) %>% 
            first(),
          TRUE ~ Utvandringsrisk
        ),
        Utvandringsrisk = replace_na(Utvandringsrisk, 0),
        Antal_utvandrare = Värde * Utvandringsrisk,
        Värde = Antal_utvandrare,
        Variabel = "Utvandring"
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
  }
  return(utvandring)
}

# ===========================================================
# AVSTÄMNINGSFUNKTION: Flyttningar över länsgräns
# ===========================================================

avstam_flyttningar_lansgrans <- function(kommun_flyttningar, lansgransandelar, lan_flyttningar, 
                                        lan_namn, kommuner, ar, flyttningstyp = "inflyttning") {
  
  if (AVRUNDNING == "heltal") {
    # 1. Beräkna kommunernas flyttningar över länsgräns
    kommun_lansgrans <- kommun_flyttningar %>%
      filter(Region %in% kommuner) %>%
      left_join(
        lansgransandelar %>%
          filter(År == ar) %>%
          select(Region, Kön, Ålder, Andel_lansgrans = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Flyttningar_lansgrans = round(Värde * Andel_lansgrans, 0)
      ) %>%
      group_by(Kön, Ålder, År, Variabel) %>%
      summarise(
        Kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 2. Hämta länets flyttningar (endast över länsgräns)
    lan_total <- lan_flyttningar %>%
      filter(Region == lan_namn) %>%
      select(Kön, Ålder, År, Variabel, Lan_total = Värde)
    
    # 3. Beräkna justeringsfaktor
    justering <- lan_total %>%
      left_join(kommun_lansgrans, by = c("Kön", "Ålder", "År", "Variabel")) %>%
      mutate(
        Kommun_lansgrans_total = replace_na(Kommun_lansgrans_total, 0),
        Justeringsfaktor = case_when(
          Kommun_lansgrans_total > 0 ~ Lan_total / Kommun_lansgrans_total,
          Lan_total > 0 & Kommun_lansgrans_total == 0 ~ NA_real_,
          TRUE ~ 1
        )
      )
    
    # 4. Applicera justering på länsgränsdelen
    kommun_justerad <- kommun_flyttningar %>%
      filter(Region %in% kommuner) %>%
      left_join(
        lansgransandelar %>%
          filter(År == ar) %>%
          select(Region, Kön, Ålder, Andel_lansgrans = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      left_join(
        justering %>% select(Kön, Ålder, År, Variabel, Justeringsfaktor, Lan_total),
        by = c("Kön", "Ålder", "År", "Variabel")
      ) %>%
      mutate(
        # Separera flyttningar inom och över länsgräns
        Flyttningar_inom_lan = round(Värde * (1 - Andel_lansgrans), 0),
        Flyttningar_lansgrans_original = round(Värde * Andel_lansgrans, 0),
        
        # Justera endast länsgränsdelen
        Flyttningar_lansgrans_justerad = case_when(
          !is.na(Justeringsfaktor) ~ round(Flyttningar_lansgrans_original * Justeringsfaktor, 0),
          is.na(Justeringsfaktor) & !is.na(Lan_total) & Lan_total > 0 ~ 
            round(Lan_total / n_distinct(Region[Region %in% kommuner]), 0),  # Fördela jämnt
          TRUE ~ Flyttningar_lansgrans_original
        ),
        
        # Ny total = inom län + justerad länsgräns
        Värde = Flyttningar_inom_lan + Flyttningar_lansgrans_justerad
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
    # Hantera avrundningsfel
    slutlig_kontroll <- kommun_justerad %>%
      left_join(
        lansgransandelar %>%
          filter(År == ar) %>%
          select(Region, Kön, Ålder, Andel_lansgrans = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(Flyttningar_lansgrans = round(Värde * Andel_lansgrans, 0)) %>%
      group_by(Kön, Ålder, År, Variabel) %>%
      summarise(Ny_kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE), .groups = "drop") %>%
      left_join(lan_total, by = c("Kön", "Ålder", "År", "Variabel")) %>%
      mutate(Slutlig_diff = Lan_total - Ny_kommun_lansgrans_total)
    
    # Justera största kommunen för avrundningsfel
    if (any(abs(slutlig_kontroll$Slutlig_diff) > 0)) {
      for (i in 1:nrow(slutlig_kontroll)) {
        if (abs(slutlig_kontroll$Slutlig_diff[i]) > 0) {
          # Hitta största kommunen
          storsta_kommun <- kommun_justerad %>%
            filter(
              Kön == slutlig_kontroll$Kön[i],
              Ålder == slutlig_kontroll$Ålder[i],
              År == slutlig_kontroll$År[i],
              Variabel == slutlig_kontroll$Variabel[i]
            ) %>%
            arrange(desc(Värde)) %>%
            slice(1)
          
          if (nrow(storsta_kommun) > 0) {
            kommun_justerad <- kommun_justerad %>%
              mutate(
                Värde = case_when(
                  Region == storsta_kommun$Region[1] &
                    Kön == storsta_kommun$Kön[1] &
                    Ålder == storsta_kommun$Ålder[1] &
                    År == storsta_kommun$År[1] &
                    Variabel == storsta_kommun$Variabel[1] ~ 
                    Värde + slutlig_kontroll$Slutlig_diff[i],
                  TRUE ~ Värde
                )
              )
          }
        }
      }
    }
    
    # Logga information om stora justeringar
    stora_justeringar <- justering %>%
      filter(abs(Justeringsfaktor - 1) > 0.1 | is.na(Justeringsfaktor)) %>%
      nrow()
    
    if (stora_justeringar > 0) {
      cat(paste0("  Avstämning ", flyttningstyp, " över länsgräns: ", 
                 stora_justeringar, " celler med justering > 10%\n"))
    }
    
    return(kommun_justerad)
    
  } else if (AVRUNDNING == "ingen") {
    
    # 1. Kommunernas flyttningar över länsgräns utan avrundning
    kommun_lansgrans <- kommun_flyttningar %>%
      filter(Region %in% kommuner) %>%
      left_join(
        lansgransandelar %>%
          filter(År == ar) %>%
          select(Region, Kön, Ålder, Andel_lansgrans = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      mutate(
        Flyttningar_lansgrans = Värde * Andel_lansgrans
      ) %>%
      group_by(Kön, Ålder, År, Variabel) %>%
      summarise(
        Kommun_lansgrans_total = sum(Flyttningar_lansgrans, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 2. Länets flyttningar
    lan_total <- lan_flyttningar %>%
      filter(Region == lan_namn) %>%
      select(Kön, Ålder, År, Variabel, Lan_total = Värde)
    
    # 3. Justeringsfaktor
    justering <- lan_total %>%
      left_join(kommun_lansgrans, by = c("Kön", "Ålder", "År", "Variabel")) %>%
      mutate(
        Kommun_lansgrans_total = replace_na(Kommun_lansgrans_total, 0),
        Justeringsfaktor = case_when(
          Kommun_lansgrans_total > 0 ~ Lan_total / Kommun_lansgrans_total,
          Lan_total > 0 & Kommun_lansgrans_total == 0 ~ NA_real_,
          TRUE ~ 1
        )
      )
    
    # 4. Applicera justering utan avrundning
    kommun_justerad <- kommun_flyttningar %>%
      filter(Region %in% kommuner) %>%
      left_join(
        lansgransandelar %>%
          filter(År == ar) %>%
          select(Region, Kön, Ålder, Andel_lansgrans = Värde),
        by = c("Region", "Kön", "Ålder")
      ) %>%
      left_join(
        justering %>% select(Kön, Ålder, År, Variabel, Justeringsfaktor, Lan_total),
        by = c("Kön", "Ålder", "År", "Variabel")
      ) %>%
      mutate(
        Flyttningar_inom_lan = Värde * (1 - Andel_lansgrans),
        Flyttningar_lansgrans_original = Värde * Andel_lansgrans,
        Flyttningar_lansgrans_justerad = case_when(
          !is.na(Justeringsfaktor) ~ Flyttningar_lansgrans_original * Justeringsfaktor,
          is.na(Justeringsfaktor) & !is.na(Lan_total) & Lan_total > 0 ~ 
            Lan_total / n_distinct(Region[Region %in% kommuner]),
          TRUE ~ Flyttningar_lansgrans_original
        ),
        Värde = Flyttningar_inom_lan + Flyttningar_lansgrans_justerad
      ) %>%
      select(Region, Kön, Ålder, År, Värde, Variabel)
    
    # 5. Robust avrundning sist (balanced rounding, NA-säker)
    kommun_final <- kommun_justerad %>%
      dplyr::group_by(Kön, Ålder, År, Variabel) %>%
      dplyr::group_modify(~{
        x <- .x
        # gör numeriskt och ersätt NA med 0
        x$Värde <- as.numeric(x$Värde)
        x$Värde[is.na(x$Värde)] <- 0
        
        # Largest Remainder-metoden med målsumma = round(sum(x))
        base  <- floor(x$Värde)
        resid <- x$Värde - base
        target <- as.integer(round(sum(x$Värde, na.rm = TRUE)))
        need   <- target - sum(base)
        
        if (!is.na(need) && need != 0 && nrow(x) > 0) {
          k <- min(abs(need), nrow(x))
          idx <- if (need > 0) order(resid, decreasing = TRUE) else order(resid, decreasing = FALSE)
          idx <- idx[seq_len(k)]
          base[idx] <- base[idx] + ifelse(need > 0, 1L, -1L)
        }
        
        x$Värde <- base
        x
      }) %>%
      dplyr::ungroup()
    
    return(kommun_final)
    
  }
  
}

# ===========================================================
# GENERELL AVSTÄMNINGSFUNKTION (för övriga komponenter)
# ===========================================================

# avstam_komponent_mot_lan <- function(komponent_data, lan_namn, kommuner, komponent_namn = "") {
#   # Separera län- och kommundata
#   lan_data <- komponent_data %>% filter(Region == lan_namn)
#   kommun_data <- komponent_data %>% filter(Region %in% kommuner)
#   
#   # Om ingen länsdata finns, returnera kommundata
#   if (nrow(lan_data) == 0) {
#     return(kommun_data)
#   }
#   
#   # Summera kommundata per kön/ålder
#   kommun_summa <- kommun_data %>%
#     group_by(Kön, Ålder, År, Variabel) %>%
#     summarise(Kommun_total = sum(Värde, na.rm = TRUE), .groups = "drop")
#   
#   # Jämför med länsdata
#   avstamning <- lan_data %>%
#     select(Kön, Ålder, År, Variabel, Lan_total = Värde) %>%
#     left_join(kommun_summa, by = c("Kön", "Ålder", "År", "Variabel")) %>%
#     mutate(
#       Kommun_total = replace_na(Kommun_total, 0),
#       Differens = Lan_total - Kommun_total,
#       Justeringsfaktor = case_when(
#         Kommun_total > 0 ~ Lan_total / Kommun_total,
#         Lan_total > 0 & Kommun_total == 0 ~ NA_real_,
#         TRUE ~ 1
#       )
#     )
#   
#   # Logga stora avvikelser
#   stora_avvikelser <- avstamning %>%
#     filter(abs(Differens) > 5) %>%
#     arrange(desc(abs(Differens)))
#   
#   if (nrow(stora_avvikelser) > 0 && komponent_namn != "") {
#     cat(paste0("  Avstämning ", komponent_namn, ": Justerar ", 
#                nrow(stora_avvikelser), " celler med differens > 5\n"))
#   }
#   
#   # För celler där län har värde men kommuner saknar
#   nya_rader <- avstamning %>%
#     filter(is.na(Justeringsfaktor) & Lan_total > 0) %>%
#     crossing(Region = kommuner) %>%
#     mutate(
#       Värde = round(Lan_total / length(kommuner), 0)
#     ) %>%
#     select(Region, Kön, Ålder, År, Variabel, Värde)
#   
#   # nytt tillägg till nya_rader
#   nya_rader_justerade <- nya_rader %>%
#     group_by(Kön, Ålder, År, Variabel) %>%
#     mutate(
#       diff = first(Värde * length(kommuner)) - sum(Värde),
#       index_max = which.max(Värde),
#       Värde = if_else(row_number() == index_max, Värde + diff, Värde)
#     ) %>%
#     ungroup()
#   
#   
#   # Justera befintlig kommundata
#   # kommun_justerad <- kommun_data %>%
#   #   left_join(
#   #     avstamning %>% select(Kön, Ålder, År, Variabel, Justeringsfaktor),
#   #     by = c("Kön", "Ålder", "År", "Variabel")
#   #   ) %>%
#   #   mutate(
#   #     Värde = case_when(
#   #       !is.na(Justeringsfaktor) ~ round(Värde * Justeringsfaktor, 0),
#   #       TRUE ~ Värde
#   #     )
#   #   ) %>%
#   #   select(-Justeringsfaktor)
#   
#   kommun_justerad <- kommun_data %>%
#     left_join(
#       avstamning %>% select(Kön, Ålder, År, Variabel, Lan_total, Justeringsfaktor),
#       by = c("Kön", "Ålder", "År", "Variabel")
#     ) %>%
#     mutate(
#       Värde = case_when(
#         !is.na(Justeringsfaktor) ~ round(Värde * Justeringsfaktor, 0),
#         TRUE ~ Värde
#       )
#     ) %>%
#     group_by(Kön, Ålder, År, Variabel) %>%
#     mutate(
#       diff = first(Lan_total) - sum(Värde),
#       index_max = which.max(Värde),
#       Värde = if_else(row_number() == index_max, Värde + diff, Värde)
#     ) %>%
#     ungroup() %>%
#     select(-Justeringsfaktor, -Lan_total, -diff, -index_max)
#   
#   
#   # Kombinera med nya rader
#   befintliga_kombinationer <- kommun_justerad %>%
#     select(Region, Kön, Ålder, År, Variabel) %>%
#     distinct()
#   
#   # nya_rader_unika <- nya_rader %>%
#   #   anti_join(befintliga_kombinationer, by = c("Region", "Kön", "Ålder", "År", "Variabel"))
#   nya_rader_unika <- nya_rader_justerade %>%
#     anti_join(befintliga_kombinationer, by = c("Region", "Kön", "Ålder", "År", "Variabel"))
#   
#   
#   kommun_final <- bind_rows(kommun_justerad, nya_rader_unika)
#   
#   # Hantera avrundningsfel
#   slutlig_kontroll <- kommun_final %>%
#     group_by(Kön, Ålder, År, Variabel) %>%
#     summarise(Ny_kommun_total = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
#     left_join(
#       lan_data %>% select(Kön, Ålder, År, Variabel, Lan_total = Värde),
#       by = c("Kön", "Ålder", "År", "Variabel")
#     ) %>%
#     mutate(Slutlig_diff = Lan_total - Ny_kommun_total)
#   
#   # Justera största kommunen för avrundningsfel
#   if (any(abs(slutlig_kontroll$Slutlig_diff) > 0)) {
#     for (i in 1:nrow(slutlig_kontroll)) {
#       if (abs(slutlig_kontroll$Slutlig_diff[i]) > 0) {
#         storsta_kommun <- kommun_final %>%
#           filter(
#             Kön == slutlig_kontroll$Kön[i],
#             Ålder == slutlig_kontroll$Ålder[i],
#             År == slutlig_kontroll$År[i],
#             Variabel == slutlig_kontroll$Variabel[i]
#           ) %>%
#           arrange(desc(Värde)) %>%
#           slice(1)
#         
#         if (nrow(storsta_kommun) > 0) {
#           kommun_final <- kommun_final %>%
#             mutate(
#               Värde = case_when(
#                 Region == storsta_kommun$Region[1] &
#                   Kön == storsta_kommun$Kön[1] &
#                   Ålder == storsta_kommun$Ålder[1] &
#                   År == storsta_kommun$År[1] &
#                   Variabel == storsta_kommun$Variabel[1] ~ 
#                   Värde + slutlig_kontroll$Slutlig_diff[i],
#                 TRUE ~ Värde
#               )
#             )
#         }
#       }
#     }
#   }
#   
#   return(kommun_final)
# }

avstam_komponent_mot_lan <- function(komponent_data, lan_namn, kommuner, komponent_namn = "",
                                     metod = "minsta_kvadrat",                # raking, proportional, minsta_kvadrat eller antal_baserad_proportionell
                                     max_iter = 50, tol = 1e-6        # inställningar för raking-metoden
) {
  lan_data <- komponent_data %>% filter(Region == lan_namn)
  kommun_data <- komponent_data %>% filter(Region %in% kommuner)
  
  if (nrow(lan_data) == 0) return(kommun_data)
  
  kommun_mat <- kommun_data %>%
    mutate(id = paste(Region, Kön, Ålder, sep = "_")) %>%
    select(id, Region, Kön, Ålder, År, Variabel, Värde)
  
  lan_mal <- lan_data %>%
    group_by(Kön, Ålder, År, Variabel) %>%
    summarise(Lan_total = sum(Värde), .groups = "drop")
  
  kommun_summa <- kommun_mat %>%
    group_by(Kön, Ålder, År, Variabel) %>%
    summarise(Kommun_total = sum(Värde), .groups = "drop")
  
  justeringstabell <- lan_mal %>%
    left_join(kommun_summa, by = c("Kön", "Ålder", "År", "Variabel")) %>%
    mutate(Justeringsfaktor = if_else(Kommun_total > 0, Lan_total / Kommun_total, NA_real_))
  
  nya_rader <- justeringstabell %>%
    filter(is.na(Kommun_total) & Lan_total > 0) %>%
    crossing(Region = kommuner) %>%
    mutate(Värde = round(Lan_total / length(kommuner), 0)) %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  if (metod == "raking") {
    
    df <- kommun_mat %>%
      left_join(justeringstabell %>% select(Kön, Ålder, År, Variabel, Lan_total), by = c("Kön", "Ålder", "År", "Variabel")) %>%
      group_by(Region, Kön, Ålder, År, Variabel) %>%
      summarise(Värde = sum(Värde), Lan_total = first(Lan_total), .groups = "drop")
    
    iter <- 0
    konvergerat <- FALSE
    
    while (iter < max_iter && !konvergerat) {
      iter <- iter + 1
      
      # Justera per kön
      df <- df %>%
        group_by(Kön, År, Variabel) %>%
        mutate(faktor_kon = if_else(sum(Värde) > 0, Lan_total[1] / sum(Värde), 1),
               Värde = Värde * faktor_kon) %>%
        ungroup()
      
      # Justera per ålder
      df <- df %>%
        group_by(Ålder, År, Variabel) %>%
        mutate(faktor_alder = if_else(sum(Värde) > 0, Lan_total[1] / sum(Värde), 1),
               Värde = Värde * faktor_alder) %>%
        ungroup()
      
      # Kontrollera konvergens
      totalskillnad <- df %>%
        group_by(Kön, Ålder, År, Variabel) %>%
        summarise(Summa = sum(Värde), .groups = "drop") %>%
        left_join(lan_mal, by = c("Kön", "Ålder", "År", "Variabel")) %>%
        summarise(diff = sum(abs(Summa - Lan_total), na.rm = TRUE)) %>%
        dplyr::pull(diff)
      
      konvergerat <- totalskillnad < tol
    }
    
    befintliga_kombinationer <- df %>%
      select(Region, Kön, Ålder, År, Variabel) %>%
      distinct()
    
    nya_rader_unika <- nya_rader %>%
      anti_join(befintliga_kombinationer, by = c("Region", "Kön", "Ålder", "År", "Variabel"))
    
    kommun_final <- bind_rows(df, nya_rader_unika)
    
  } else if (metod == "antal_baserad_proportionell") {
    
    # Heuristik: om Värde ser ut som risk (<~1.5) behandla som risk, annars antal
    is_risk <- max(komponent_data$Värde, na.rm = TRUE) <= 1.5
    
    # --- Bygg mix: länets ålders/könsfördelning per år ---
    # 1) Försök ta den från kommunernas prognosbefolkning (summa över kommunerna)
    mix <- tryCatch({
      befolkningsprognos %>%                             # din prognosbefolkning per År/Ålder/Kön/Region
        dplyr::filter(Region %in% kommuner) %>%
        dplyr::group_by(År, Ålder, Kön) %>%
        dplyr::summarise(n_mix = sum(Värde, na.rm = TRUE), .groups = "drop")
    }, error = function(e) {
      # 2) Fallback: rikets mix (utan Region-kolumn)
      riket_lista$riket_prognosinvånare_grund %>%
        dplyr::select(År, Ålder, Kön, n_mix = Värde)
    })
    
    
    # Antal = risk * mix (om risk); annars är Värde redan antal
    to_antal <- function(df) {
      df %>%
        dplyr::left_join(mix, by = c("År","Ålder","Kön")) %>%
        dplyr::mutate(antal = if (is_risk) Värde * n_mix else Värde)
    }
    
    lan_ant <- to_antal(lan_data)
    kom_ant <- to_antal(kommun_data)
    
    # Länets mål: totalsumma per år (och variabel)
    lan_mal <- lan_ant %>%
      dplyr::group_by(År, Variabel) %>%
      dplyr::summarise(Lan_total = sum(antal, na.rm = TRUE), .groups = "drop")
    
    # Kommunernas totalsumma per år
    kom_sum <- kom_ant %>%
      dplyr::group_by(Region, År, Variabel) %>%
      dplyr::summarise(Kommun_total = sum(antal, na.rm = TRUE), .groups = "drop")
    
    # Skalfaktor per år: mål / faktisk summa
    faktor <- kom_sum %>%
      dplyr::group_by(År, Variabel) %>%
      dplyr::summarise(sum_kom = sum(Kommun_total, na.rm = TRUE), .groups = "drop") %>%
      dplyr::left_join(lan_mal, by = c("År","Variabel")) %>%
      dplyr::mutate(faktor = dplyr::if_else(sum_kom > 0, Lan_total / sum_kom, 1)) %>%
      dplyr::select(År, Variabel, faktor)
    
    # Skala kommunernas antal proportionellt
    kom_ant_adj <- kom_ant %>%
      dplyr::left_join(faktor, by = c("År","Variabel")) %>%
      dplyr::mutate(antal_adj = antal * dplyr::coalesce(faktor, 1))
    
    # Tillbaka till risk om det var risk från början
    kommun_final <- kom_ant_adj %>%
      dplyr::mutate(
        Värde = if (is_risk) dplyr::if_else(n_mix > 0, antal_adj / n_mix, 0) else antal_adj
      ) %>%
      dplyr::select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (metod == "proportional") {
    
    # Proportionell justering (standard)
    kommun_justerad <- kommun_data %>%
      left_join(justeringstabell %>% select(Kön, Ålder, År, Variabel, Justeringsfaktor),
                by = c("Kön", "Ålder", "År", "Variabel")) %>%
      mutate(Värde = if_else(!is.na(Justeringsfaktor), round(Värde * Justeringsfaktor), Värde)) %>%
      select(-Justeringsfaktor)
    
    befintliga_kombinationer <- kommun_justerad %>%
      select(Region, Kön, Ålder, År, Variabel) %>%
      distinct()
    
    nya_rader_unika <- nya_rader %>%
      anti_join(befintliga_kombinationer, by = c("Region", "Kön", "Ålder", "År", "Variabel"))
    
    kommun_final <- bind_rows(kommun_justerad, nya_rader_unika)
    
  } else if (metod == "minsta_kvadrat") {
    
    kommun_justerad <- kommun_data %>%
      left_join(lan_mal, by = c("Kön", "Ålder", "År", "Variabel")) %>%
      group_by(Kön, Ålder, År, Variabel) %>%
      group_modify(~ {
        komm <- .x
        
        if (nrow(komm) == 0) return(komm)
        
        lan_tot <- first(komm$Lan_total)
        
        if (!is.na(lan_tot) && sum(komm$Värde) > 0) {
          n <- nrow(komm)
          Dmat <- diag(n)
          dvec <- komm$Värde
          Amat <- matrix(1, nrow = n)
          bvec <- lan_tot
          
          res <- try(quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq = 1), silent = TRUE)
          if (!inherits(res, "try-error")) {
            komm$Värde <- round(res$solution)
          }
        }
        
        komm
      }) %>%
      ungroup()
    
    befintliga_kombinationer <- kommun_justerad %>%
      select(Region, Kön, Ålder, År, Variabel) %>%
      distinct()
    
    nya_rader_unika <- nya_rader %>%
      anti_join(befintliga_kombinationer, by = c("Region", "Kön", "Ålder", "År", "Variabel"))
    
    kommun_final <- bind_rows(kommun_justerad, nya_rader_unika)
    
  }
  
  return(kommun_final)
}



# ===========================================================
# HUVUDFUNKTION MED REGIONAL AVSTÄMNING
# ===========================================================

gora_regional_befolkningsprognos <- function(
    basbefolkning,
    risktal,
    riksbefolkning_prognos,
    invandring_till_riket_prognos,
    lan_namn,
    kommuner,
    startår,
    slutår
) {
  
  # Initiera resultatstruktur
  alla_resultat <- list()
  
  # Informationsmeddelande
  cat("Startar regional befolkningsprognos...\n")
  cat(paste("Period:", startår, "-", slutår, "\n"))
  cat(paste("Län:", lan_namn, "\n"))
  cat(paste("Antal kommuner:", length(kommuner), "\n\n"))
  
  # För varje år i prognosperioden
  for (ar in as.character(startår:slutår)) {
    
    cat(paste0("Beräknar prognos för år ", ar, "...\n"))
    
    # 1. Åldra befolkningen ett år
    if (ar == startår) {
      aktuell_befolkning <- basbefolkning %>%
        mutate(År = as.character(as.numeric(År) + 1))
      aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
    } else {
      aktuell_befolkning <- alla_resultat[[as.character(as.numeric(ar) - 1)]]$befolkning %>%
        mutate(År = ar)
      aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
    }
    
    # 2. Beräkna antal födda
    fodda_resultat <- berakna_fodda(
      aktuell_befolkning, 
      risktal$fodelserisker, 
      ar
    )
    
    # Avstäm födda
    fodda_kommun_justerad <- avstam_komponent_mot_lan(
      fodda_resultat$fodda_rapport,
      lan_namn,
      kommuner,
      "födda", 
      metod = metod_avstamning_regional
    )
    
    fodda_rapport_final <- bind_rows(
      fodda_resultat$fodda_rapport %>% filter(Region == lan_namn),
      fodda_kommun_justerad
    )
    
    # Lägg till 0-åringar
    fodda_0_aringar <- fodda_rapport_final %>%
      mutate(Ålder = 0, Variabel = "Total folkmängd") %>%
      select(Region, Ålder, Kön, År, Värde, Variabel)
    
    aktuell_befolkning <- bind_rows(aktuell_befolkning, fodda_0_aringar)
    
    # 3. Beräkna antal döda
    doda <- berakna_doda(aktuell_befolkning, risktal$dodsrisker)
    
    # Avstäm döda
    doda_kommun_justerad <- avstam_komponent_mot_lan(doda, lan_namn, kommuner, "döda", metod = metod_avstamning_regional)
    doda_final <- bind_rows(
      doda %>% filter(Region == lan_namn),
      doda_kommun_justerad
    )
    
    # 4. Beräkna inrikes flyttningar med särskild länsgränshantering
    
    # Beräkna alla flyttningar
    inrikes_inflyttningar <- berakna_inrikes_inflyttningar(
      risktal$inflyttningsrisker,
      riksbefolkning_prognos,
      ar
    )
    
    inrikes_utflyttningar <- berakna_inrikes_utflyttningar(
      aktuell_befolkning,
      risktal$utflyttningsrisker
    )
    
    # Avstäm inflyttningar över länsgräns
    inflyttningar_kommun_justerad <- avstam_flyttningar_lansgrans(
      inrikes_inflyttningar,
      risktal$inflyttningsandelar_lansgrans,
      inrikes_inflyttningar,
      lan_namn,
      kommuner,
      ar,
      "inflyttning"
    )
    
    inrikes_inflyttningar_final <- bind_rows(
      inrikes_inflyttningar %>% filter(Region == lan_namn),
      inflyttningar_kommun_justerad
    )
    
    # Avstäm utflyttningar över länsgräns
    utflyttningar_kommun_justerad <- avstam_flyttningar_lansgrans(
      inrikes_utflyttningar,
      risktal$utflyttningsandelar_lansgrans,
      inrikes_utflyttningar,
      lan_namn,
      kommuner,
      ar,
      "utflyttning"
    )
    
    inrikes_utflyttningar_final <- bind_rows(
      inrikes_utflyttningar %>% filter(Region == lan_namn),
      utflyttningar_kommun_justerad
    )
    
    # 5. Beräkna invandring
    invandring <- berakna_invandring(
      risktal$invandringsrisker,
      invandring_till_riket_prognos,
      ar
    )
    
    # Avstäm invandring
    invandring_kommun_justerad <- avstam_komponent_mot_lan(invandring, lan_namn, kommuner, "invandring", metod = metod_avstamning_regional)
    invandring_final <- bind_rows(
      invandring %>% filter(Region == lan_namn),
      invandring_kommun_justerad
    )
    
    # 6. Beräkna utvandring
    utvandring <- berakna_utvandring(
      aktuell_befolkning,
      risktal$utvandringsrisker
    )
    
    # Avstäm utvandring
    utvandring_kommun_justerad <- avstam_komponent_mot_lan(utvandring, lan_namn, kommuner, "utvandring", metod = metod_avstamning_regional)
    utvandring_final <- bind_rows(
      utvandring %>% filter(Region == lan_namn),
      utvandring_kommun_justerad
    )
    
    # 7. Sammanställ alla komponenter
    befolkning_komponenter <- bind_rows(
      fodda_rapport_final,
      doda_final,
      inrikes_inflyttningar_final,
      inrikes_utflyttningar_final,
      invandring_final,
      utvandring_final
    )
    
    # 8. Beräkna nettoförändringar
    befolkning_forandringar <- befolkning_komponenter %>%
      mutate(
        Förändring = case_when(
          Variabel == "Födda" ~ 0,  # Födda har redan lagts till
          Variabel == "Döda" ~ -Värde,
          Variabel == "Inrikes inflyttning" ~ Värde,
          Variabel == "Inrikes utflyttning" ~ -Värde,
          Variabel == "Invandring" ~ Värde,
          Variabel == "Utvandring" ~ -Värde,
          TRUE ~ 0
        )
      ) %>%
      group_by(Region, Kön, Ålder, År) %>%
      summarise(Nettoförändring = sum(Förändring), .groups = "drop")
    
    # 9. Beräkna ny befolkning
    ny_befolkning <- aktuell_befolkning %>%
      select(Region, Kön, Ålder, År, Värde) %>%
      left_join(
        befolkning_forandringar,
        by = c("Region", "Kön", "Ålder", "År")
      ) %>%
      mutate(
        Nettoförändring = replace_na(Nettoförändring, 0),
        Ny_befolkning = Värde + Nettoförändring,
        Ny_befolkning = round(Ny_befolkning, 0),
        Ny_befolkning = pmax(0, Ny_befolkning)
      ) %>%
      select(Region, Kön, Ålder, År, Värde = Ny_befolkning) %>%
      mutate(Variabel = "Total folkmängd")
    
    # 10. Spara resultat
    alla_resultat[[ar]] <- list(
      befolkning = ny_befolkning,
      komponenter = list(
        födda = fodda_rapport_final,
        döda = doda_final,
        inrikes_inflyttning = inrikes_inflyttningar_final,
        inrikes_utflyttning = inrikes_utflyttningar_final,
        invandring = invandring_final,
        utvandring = utvandring_final
      )
    )
    
    # Visa progress
    total_bef <- sum(ny_befolkning$Värde)
    forandring <- sum(befolkning_forandringar$Nettoförändring) + 
      sum(fodda_rapport_final$Värde)
    cat(paste0("  - Total befolkning: ", format(total_bef, big.mark = " "), 
               " (förändring: ", ifelse(forandring >= 0, "+", ""), 
               format(forandring, big.mark = " "), ")\n"))
  }
  
  # 11. Sammanställ resultat per region
  cat("\nSammanställer resultat per region...\n")
  
  region_resultat <- list()
  alla_regioner <- c(lan_namn, kommuner)
  
  for (region in alla_regioner) {
    region_befolkning <- tibble()
    region_komponenter <- list()
    
    for (ar in as.character(startår:slutår)) {
      region_befolkning <- bind_rows(
        region_befolkning,
        alla_resultat[[ar]]$befolkning %>% filter(Region == region)
      )
      
      region_komponenter[[ar]] <- lapply(alla_resultat[[ar]]$komponenter, function(komp) {
        komp %>% filter(Region == region)
      })
    }
    
    region_resultat[[region]] <- list(
      totalbefolkning = region_befolkning,
      komponenter = region_komponenter
    )
  }
  
  cat("Regional prognos klar!\n\n")
  
  return(region_resultat)
}

# ===========================================================
# KÖR PROGNOSEN
# ===========================================================

# Genomför prognosen
regional_befolkningsprognos <- gora_regional_befolkningsprognos(
  basbefolkning = basbefolkning_kommun,
  risktal = risktal,
  riksbefolkning_prognos = riksbefolkning_prognos,
  invandring_till_riket_prognos = invandring_till_riket_prognos,
  lan_namn = lan_namn,
  kommuner = kommuner,
  startår = PROGNOS_START,
  slutår = PROGNOS_SLUT
)

# Spara resultatet
if (SCENARIO_ATT_KORA == "alternativ") {
  output_filename <- paste0("Data_resultat/befolkningsprognos_regional_", PROGNOS_START, "-", PROGNOS_SLUT, "_alternativ.rds")
} else {
  output_filename <- paste0("Data_resultat/befolkningsprognos_regional_", PROGNOS_START, "-", PROGNOS_SLUT, ".rds")
}

saveRDS(regional_befolkningsprognos, output_filename)

# ===========================================================
# AVSTÄMNINGSSUMMERING
# ===========================================================

cat("\n=== SAMMANFATTNING ===\n")
cat(paste("Scenario:", SCENARIO_ATT_KORA, "\n"))
cat(paste("Antal regioner:", length(regional_befolkningsprognos), "\n"))

cat("\n=== AVSTÄMNING: LÄNSTOTALER VS KOMMUNSUMMOR ===\n")

# Skapa avstämningstabell
avstamning_tabell <- tibble()

# skapa en vektor med vart femte år av alla prognosår
prognos_ar_vektor <- seq(PROGNOS_START, PROGNOS_SLUT, by = 5)

for (ar in prognos_ar_vektor) {
  # Länstotal
  lan_total <- regional_befolkningsprognos[[lan_namn]]$totalbefolkning %>%
    filter(År == ar) %>%
    summarise(Lan_total = sum(Värde, na.rm = TRUE)) %>%
    dplyr::pull(Lan_total)
  
  # Summa av kommuner
  kommun_summa <- map_dbl(kommuner, function(kommun) {
    regional_befolkningsprognos[[kommun]]$totalbefolkning %>%
      filter(År == ar) %>%
      summarise(tot = sum(Värde, na.rm = TRUE)) %>%
      dplyr::pull(tot)
  }) %>% sum()
  
  avstamning_tabell <- bind_rows(
    avstamning_tabell,
    tibble(
      År = ar,
      Lan_total = lan_total,
      Kommun_summa = kommun_summa,
      Differens = lan_total - kommun_summa,
      Procent_avvikelse = round((lan_total - kommun_summa) / lan_total * 100, 3)
    )
  )
}

cat("\nTotalbefolkning - avstämning:\n")
print(avstamning_tabell)

# vi väljer komponentavstämningsår som det 5:e året efter prognosstartåret
komponentavstamningsar <- as.character(as.numeric(PROGNOS_START) +5)

# Komponentavstämning för ett år
cat(paste0("\n=== KOMPONENTAVSTÄMNING FÖR ÅR ", komponentavstamningsar, " ===\n"))

komponenter <- c("födda", "döda", "inrikes_inflyttning", "inrikes_utflyttning", 
                 "invandring", "utvandring")

komponent_avstamning <- tibble()

for (komp in komponenter) {
  # Länstotal
  lan_komp <- regional_befolkningsprognos[[lan_namn]]$komponenter[[komponentavstamningsar]][[komp]] %>%
    summarise(Lan_total = sum(Värde, na.rm = TRUE)) %>%
    dplyr::pull(Lan_total)
  
  # Kommunsumma
  kommun_komp_summa <- map_dbl(kommuner, function(kommun) {
    regional_befolkningsprognos[[kommun]]$komponenter[[komponentavstamningsar]][[komp]] %>%
      summarise(tot = sum(Värde, na.rm = TRUE)) %>%
      dplyr::pull(tot)
  }) %>% sum()
  
  komponent_avstamning <- bind_rows(
    komponent_avstamning,
    tibble(
      Komponent = komp,
      Lan_total = lan_komp,
      Kommun_summa = kommun_komp_summa,
      Differens = lan_komp - kommun_komp_summa
    )
  )
}

cat(paste0("\nKomponenter år ", komponentavstamningsar, ":\n"))
print(komponent_avstamning)

# Kontroll av avstämning
if (all(abs(avstamning_tabell$Differens) <= 1) && 
    all(abs(komponent_avstamning$Differens) <= 1)) {
  cat("\n✓ Avstämning OK - alla differenser inom avrundningsfel (±1 person)\n")
} else {
  cat("\nVARNING: Det finns avstämningsfel större än avrundningsfel!\n")
}

cat(paste0("\nPrognos sparad i: ", output_filename, "\n"))