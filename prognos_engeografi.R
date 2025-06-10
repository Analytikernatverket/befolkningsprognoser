############################################################
#          BEFOLKNINGSPROGNOS FÖR ENSKILD GEOGRAFI        #
############################################################
#                                                          #
#    Genomför kohort-komponent-prognos för en geografi     #
#                                                          #
#    Prognosmetod:                                         #
#    - Åldrar befolkningen årligen                         #
#    - Beräknar demografiska komponenter                   #
#    - Uppdaterar befolkningen baserat på nettoeffekter    #
#                                                          #
#    Förväntar följande variabler från huvudskriptet:      #
#    - GEOGRAFI_ATT_KORA: namn på vald geografi           #
#    - SCENARIO_ATT_KORA: "standard" eller "alternativ"   #
#                                                          #
############################################################

library(tidyverse)

# ===========================================================
# LÄSA IN DATA
# ===========================================================

message("\n📊 LÄSER IN DATA FÖR ENSKILD PROGNOS...")

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

# Samla risktal i lista för enklare hantering
risktal <- list(
  fodelserisker = fodelserisker,
  dodsrisker = dodsrisker,
  inflyttningsrisker = inflyttningsrisker,
  utflyttningsrisker = utflyttningsrisker,
  invandringsrisker = invandringsrisker,
  utvandringsrisker = utvandringsrisker
)

risktal <- risktal %>% 
  map(as_tibble)

## Förbered basbefolkning ##

# Hämta senaste årets befolkning för vald geografi
basbefolkning_geografi <- kommun_lista$totfolkmangd %>% 
  filter(Region == GEOGRAFI_ATT_KORA, År == max(År)) %>%
  mutate(Ålder = as.numeric(Ålder))

# Validera att geografin finns
if (nrow(basbefolkning_geografi) == 0) {
  stop(paste("❌ Geografin", GEOGRAFI_ATT_KORA, "finns inte i data!"))
}

message(paste("  ✅ Data inläst för:", GEOGRAFI_ATT_KORA))

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
# HJÄLPFUNKTIONER
# ===========================================================

# Funktion för att åldra befolkningen ett år
aldra_befolkning <- function(befolkning) {
  # Öka åldern med 1 för alla individer
  aldrad_befolkning <- befolkning %>%
    mutate(Ålder = Ålder + 1) %>%
    filter(Ålder <= 100)
  
  # Hantera åldersgruppen 100+ (samla alla över 100)
  aldrad_100_plus <- aldrad_befolkning %>%
    filter(Ålder >= 100) %>%
    group_by(Region, Kön, År, Variabel) %>%
    summarise(Värde = sum(Värde), .groups = "drop") %>%
    mutate(Ålder = 100)
  
  # Kombinera åldrad befolkning
  aldrad_befolkning <- aldrad_befolkning %>%
    filter(Ålder < 100) %>%
    bind_rows(aldrad_100_plus)
  
  return(aldrad_befolkning)
}

# Funktion för att beräkna antal födda
berakna_fodda <- function(befolkning, fodelserisker_data, prognos_ar) {
  # Identifiera kvinnor i fertil ålder
  kvinnor_fertil <- befolkning %>%
    filter(Kön == "kvinnor", Ålder >= 15, Ålder <= 49)
  
  # Hämta årets födelserisker
  fodelserisker_ar <- fodelserisker_data %>%
    filter(År == prognos_ar, Region == unique(befolkning$Region))
  
  if(nrow(fodelserisker_ar) == 0) {
    stop(paste("Inga födelserisker finns för år", prognos_ar, "och region", unique(befolkning$Region)))
  }
  
  # Beräkna antal födda per åldersgrupp
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
  
  # Fördela födda på kön enligt historisk fördelning
  fodda_pojkar <- fodda_per_alder %>%
    mutate(
      Kön = "män",
      Ålder = 0,
      År = prognos_ar,
      Värde = round(Antal_fodda * 0.512, 0),  # 51.2% pojkar
      Variabel = "Total folkmängd"
    ) %>%
    select(Region, Ålder, Kön, År, Värde, Variabel)
  
  fodda_flickor <- fodda_per_alder %>%
    mutate(
      Kön = "kvinnor",
      Ålder = 0,
      År = prognos_ar,
      Värde = round(Antal_fodda * 0.488, 0),  # 48.8% flickor
      Variabel = "Total folkmängd"
    ) %>%
    select(Region, Ålder, Kön, År, Värde, Variabel)
  
  fodda <- bind_rows(fodda_pojkar, fodda_flickor)
  
  # Skapa rapportversion för komponenter
  fodda_rapport <- fodda %>%
    mutate(Variabel = "Födda")
  
  return(list(fodda = fodda, fodda_rapport = fodda_rapport))
}

# Funktion för att beräkna döda
berakna_doda <- function(befolkning, dodsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta årets dödsrisker
  dodsrisker_ar <- dodsrisker_data %>%
    filter(År == prognos_ar, Region == unique(befolkning$Region))
  
  if(nrow(dodsrisker_ar) == 0) {
    stop(paste("Inga dödsrisker finns för år", prognos_ar, "och region", unique(befolkning$Region)))
  }
  
  # Beräkna antal döda
  doda <- befolkning %>%
    left_join(
      dodsrisker_ar %>% select(Region, Kön, Ålder, Dodsrisk = Värde),
      by = c("Region", "Kön", "Ålder")
    ) %>%
    mutate(
      # För höga åldrar, använd risk för 100-åringar
      Dodsrisk = case_when(
        is.na(Dodsrisk) & Ålder > 100 ~ dodsrisker_ar %>% 
          filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
          pull(Värde) %>% 
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
  
  return(doda)
}

# Funktion för att beräkna inrikes inflyttningar
berakna_inrikes_inflyttningar <- function(inflyttningsrisker_data, riksbefolkning, prognos_ar, geografi) {
  # Hämta riksbefolkning för året
  riksbef_ar <- riksbefolkning %>%
    filter(År == prognos_ar)
  
  # Hämta inflyttningsrisker
  inflyttningsrisker_ar <- inflyttningsrisker_data %>%
    filter(År == prognos_ar, Region == geografi)
  
  if(nrow(inflyttningsrisker_ar) == 0) {
    stop(paste("Inga inflyttningsrisker finns för år", prognos_ar, "och region", geografi))
  }
  
  # Beräkna inflyttning baserat på riksbefolkning
  inflyttningar <- riksbef_ar %>%
    select(Kön, Ålder, Riksbefolkning = Värde) %>%
    crossing(Region = geografi) %>%
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
  
  return(inflyttningar)
}

# Funktion för att beräkna inrikes utflyttningar
berakna_inrikes_utflyttningar <- function(befolkning, utflyttningsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta utflyttningsrisker
  utflyttningsrisker_ar <- utflyttningsrisker_data %>%
    filter(År == prognos_ar, Region == unique(befolkning$Region))
  
  if(nrow(utflyttningsrisker_ar) == 0) {
    stop(paste("Inga utflyttningsrisker finns för år", prognos_ar, "och region", unique(befolkning$Region)))
  }
  
  # Beräkna utflyttning baserat på egen befolkning
  utflyttningar <- befolkning %>%
    left_join(
      utflyttningsrisker_ar %>% select(Region, Kön, Ålder, Utflyttningsrisk = Värde),
      by = c("Region", "Kön", "Ålder")
    ) %>%
    mutate(
      # För höga åldrar, använd risk för 100-åringar
      Utflyttningsrisk = case_when(
        is.na(Utflyttningsrisk) & Ålder > 100 ~ utflyttningsrisker_ar %>% 
          filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
          pull(Värde) %>% 
          first(),
        TRUE ~ Utflyttningsrisk
      ),
      Utflyttningsrisk = replace_na(Utflyttningsrisk, 0),
      Antal_utflyttningar = round(Värde * Utflyttningsrisk, 0),
      Värde = Antal_utflyttningar,
      Variabel = "Inrikes utflyttning"
    ) %>%
    select(Region, Kön, Ålder, År, Värde, Variabel)
  
  return(utflyttningar)
}

# Funktion för att beräkna invandring
berakna_invandring <- function(invandringsrisker_data, invandring_riket, prognos_ar, geografi) {
  # Hämta rikets invandring för året
  invandring_ar <- invandring_riket %>%
    filter(År == prognos_ar)
  
  # Hämta invandringsrisker (andelar)
  invandringsrisker_ar <- invandringsrisker_data %>%
    filter(År == prognos_ar, Region == geografi)
  
  if(nrow(invandringsrisker_ar) == 0) {
    stop(paste("Inga invandringsrisker finns för år", prognos_ar, "och region", geografi))
  }
  
  # Beräkna invandring som andel av rikets
  invandring <- invandring_ar %>%
    select(Kön, Ålder, Riksinvandring = Värde) %>%
    crossing(Region = geografi) %>%
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
  
  return(invandring)
}

# Funktion för att beräkna utvandring
berakna_utvandring <- function(befolkning, utvandringsrisker_data) {
  prognos_ar <- unique(befolkning$År)
  
  # Hämta utvandringsrisker
  utvandringsrisker_ar <- utvandringsrisker_data %>%
    filter(År == prognos_ar, Region == unique(befolkning$Region))
  
  if(nrow(utvandringsrisker_ar) == 0) {
    stop(paste("Inga utvandringsrisker finns för år", prognos_ar, "och region", unique(befolkning$Region)))
  }
  
  # Beräkna utvandring baserat på egen befolkning
  utvandring <- befolkning %>%
    left_join(
      utvandringsrisker_ar %>% select(Region, Kön, Ålder, Utvandringsrisk = Värde),
      by = c("Region", "Kön", "Ålder")
    ) %>%
    mutate(
      # För höga åldrar, använd risk för 100-åringar
      Utvandringsrisk = case_when(
        is.na(Utvandringsrisk) & Ålder > 100 ~ utvandringsrisker_ar %>% 
          filter(Region == .data$Region, Kön == .data$Kön, Ålder == 100) %>% 
          pull(Värde) %>% 
          first(),
        TRUE ~ Utvandringsrisk
      ),
      Utvandringsrisk = replace_na(Utvandringsrisk, 0),
      Antal_utvandrare = round(Värde * Utvandringsrisk, 0),
      Värde = Antal_utvandrare,
      Variabel = "Utvandring"
    ) %>%
    select(Region, Kön, Ålder, År, Värde, Variabel)
  
  return(utvandring)
}

# ===========================================================
# HUVUDFUNKTION FÖR PROGNOS
# ===========================================================

gora_befolkningsprognos_single <- function(
    basbefolkning,
    risktal,
    riksbefolkning_prognos,
    invandring_till_riket_prognos,
    geografi,
    startår = "2025",
    slutår = "2040"
) {
  
  # Initiera resultatstruktur
  alla_resultat <- list()
  
  # Informationsmeddelande
  cat("Startar befolkningsprognos för en geografi...\n")
  cat(paste("Geografi:", geografi, "\n"))
  cat(paste("Period:", startår, "-", slutår, "\n\n"))
  
  # Genomför prognos för varje år
  for (ar in as.character(startår:slutår)) {
    
    cat(paste0("Beräknar prognos för år ", ar, "...\n"))
    
    # 1. Åldra befolkningen ett år
    if (ar == startår) {
      # Första året - utgå från basbefolkningen
      aktuell_befolkning <- basbefolkning %>%
        mutate(År = as.character(as.numeric(År) + 1))
      aktuell_befolkning <- aldra_befolkning(aktuell_befolkning)
    } else {
      # Efterföljande år - utgå från föregående års resultat
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
    
    # Lägg till nyfödda i befolkningen
    aktuell_befolkning <- bind_rows(
      aktuell_befolkning,
      fodda_resultat$fodda
    )
    
    # 3. Beräkna antal döda
    doda <- berakna_doda(aktuell_befolkning, risktal$dodsrisker)
    
    # 4. Beräkna inrikes inflyttningar
    inrikes_inflyttningar <- berakna_inrikes_inflyttningar(
      risktal$inflyttningsrisker,
      riksbefolkning_prognos,
      ar,
      geografi
    )
    
    # 5. Beräkna inrikes utflyttningar
    inrikes_utflyttningar <- berakna_inrikes_utflyttningar(
      aktuell_befolkning,
      risktal$utflyttningsrisker
    )
    
    # 6. Beräkna invandring
    invandring <- berakna_invandring(
      risktal$invandringsrisker,
      invandring_till_riket_prognos,
      ar,
      geografi
    )
    
    # 7. Beräkna utvandring
    utvandring <- berakna_utvandring(
      aktuell_befolkning,
      risktal$utvandringsrisker
    )
    
    # 8. Sammanställ alla komponenter
    befolkning_komponenter <- bind_rows(
      fodda_resultat$fodda_rapport,
      doda,
      inrikes_inflyttningar,
      inrikes_utflyttningar,
      invandring,
      utvandring
    )
    
    # 9. Beräkna nettoförändringar
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
    
    # 10. Beräkna ny befolkning
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
        Ny_befolkning = pmax(0, Ny_befolkning)  # Ingen negativ befolkning
      ) %>%
      select(Region, Kön, Ålder, År, Värde = Ny_befolkning) %>%
      mutate(Variabel = "Total folkmängd")
    
    # 11. Spara resultat för detta år
    alla_resultat[[ar]] <- list(
      befolkning = ny_befolkning,
      komponenter = list(
        födda = fodda_resultat$fodda_rapport,
        döda = doda,
        inrikes_inflyttning = inrikes_inflyttningar,
        inrikes_utflyttning = inrikes_utflyttningar,
        invandring = invandring,
        utvandring = utvandring
      )
    )
    
    # Visa progress
    total_bef <- sum(ny_befolkning$Värde)
    forandring <- sum(befolkning_forandringar$Nettoförändring) + sum(fodda_resultat$fodda$Värde)
    cat(paste0("  - Total befolkning: ", format(total_bef, big.mark = " "), 
               " (förändring: ", ifelse(forandring >= 0, "+", ""), 
               format(forandring, big.mark = " "), ")\n"))
  }
  
  # 12. Sammanställ resultat
  cat("\nSammanställer resultat...\n")
  
  # Samla all befolkningsdata
  totalbefolkning <- tibble()
  komponenter <- list()
  
  for (ar in as.character(startår:slutår)) {
    # Befolkningsdata
    totalbefolkning <- bind_rows(
      totalbefolkning,
      alla_resultat[[ar]]$befolkning
    )
    
    # Komponentdata
    komponenter[[ar]] <- alla_resultat[[ar]]$komponenter
  }
  
  # Skapa resultatstruktur
  resultat <- list(
    geografi = geografi,
    totalbefolkning = totalbefolkning,
    komponenter = komponenter,
    sammanfattning = totalbefolkning %>%
      group_by(År) %>%
      summarise(
        Total_befolkning = sum(Värde),
        Kvinnor = sum(Värde[Kön == "kvinnor"]),
        Män = sum(Värde[Kön == "män"]),
        .groups = "drop"
      )
  )
  
  cat("Prognos klar!\n\n")
  
  return(resultat)
}

# ===========================================================
# KÖR PROGNOSEN
# ===========================================================

# Genomför prognos
befolkningsprognos_single <- gora_befolkningsprognos_single(
  basbefolkning = basbefolkning_geografi,
  risktal = risktal,
  riksbefolkning_prognos = riksbefolkning_prognos,
  invandring_till_riket_prognos = invandring_till_riket_prognos,
  geografi = GEOGRAFI_ATT_KORA,
  startår = "2025",
  slutår = "2040"
)

# Skapa filnamn baserat på geografi och scenario
geografi_filnamn <- gsub(" ", "_", GEOGRAFI_ATT_KORA)

if (SCENARIO_ATT_KORA == "alternativ") {
  output_filename <- paste0("Data_resultat/befolkningsprognos_", 
                            geografi_filnamn, 
                            "_2025_2040_alternativ.rds")
} else {
  output_filename <- paste0("Data_resultat/befolkningsprognos_", 
                            geografi_filnamn, 
                            "_2025_2040.rds")
}

saveRDS(befolkningsprognos_single, output_filename)

# Sammanfattning
cat("=== SAMMANFATTNING ===\n")
cat(paste("Geografi:", GEOGRAFI_ATT_KORA, "\n"))
cat(paste("Scenario:", SCENARIO_ATT_KORA, "\n\n"))

# Visa befolkningsförändring
sammanfattning <- befolkningsprognos_single$sammanfattning

start_bef <- sammanfattning %>%
  filter(År == "2025") %>%
  pull(Total_befolkning)

slut_bef <- sammanfattning %>%
  filter(År == "2040") %>%
  pull(Total_befolkning)

forandring <- slut_bef - start_bef
procent <- (forandring / start_bef) * 100

cat("Befolkningsutveckling:\n")
cat(paste0("  2025: ", format(start_bef, big.mark = " "), "\n"))
cat(paste0("  2040: ", format(slut_bef, big.mark = " "), "\n"))
cat(paste0("  Förändring: ", ifelse(forandring >= 0, "+", ""), 
           format(forandring, big.mark = " "), 
           " (", sprintf("%+.1f", procent), "%)\n\n"))

# Visa utvalda mellanår
cat("Utveckling över tid:\n")
print(sammanfattning %>% 
        filter(År %in% c("2025", "2030", "2035", "2040")) %>%
        mutate(across(c(Total_befolkning, Kvinnor, Män), 
                      ~format(., big.mark = " "))))

cat(paste0("\nPrognos sparad i: ", output_filename, "\n"))