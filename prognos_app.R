############################################################
#            VISUALISERINGSAPP FÖR BEFOLKNINGSPROGNOS      #
############################################################
#                                                          #
#    Interaktiv Shiny-app för att visualisera och          #
#    analysera resultat från befolkningsprognos            #
#                                                          #
#    Funktioner:                                           #
#    - Visa prognosmetod och inställningar                 #
#    - Visualisera demografiska komponenter                #
#    - Analysera komponenter per åldersklass               #
#    - Granska använda risktal                             #
#                                                          #
#    Appen läser automatiskt in senaste prognosresultat    #
#    och anpassar sig efter prognostyp och scenario        #
#                                                          #
############################################################

# Nödvändiga paket
library(shiny)
library(bslib)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(stringr)
library(plotly)
library(viridis)

# ===========================================================
# FLEXIBEL INLÄSNING AV DATA
# ===========================================================

message("\n📊 STARTAR VISUALISERINGSAPP...")

# Funktion för att hantera olika dataformat
konvertera_till_enhetlig_struktur <- function(prognos_data) {
  # Kontrollera om data är i enskild-format
  if ("geografi" %in% names(prognos_data) && 
      "totalbefolkning" %in% names(prognos_data) && 
      "komponenter" %in% names(prognos_data)) {
    
    # Konvertera från enskild-struktur till multi-struktur
    geografi_namn <- prognos_data$geografi
    ny_struktur <- list()
    ny_struktur[[geografi_namn]] <- list(
      totalbefolkning = prognos_data$totalbefolkning,
      komponenter = prognos_data$komponenter
    )
    
    message(paste("  Konverterade data för enskild geografi:", geografi_namn))
    return(ny_struktur)
    
  } else {
    # Data är redan i multi-struktur format
    message(paste("  Data redan i multi-struktur format med", 
                  length(prognos_data), "geografier"))
    return(prognos_data)
  }
}

# Lista tillgängliga prognosfiler
prognos_filer <- list.files("Data_resultat", 
                            pattern = "^befolkningsprognos_.*\\.rds$", 
                            full.names = TRUE)

if (length(prognos_filer) == 0) {
  stop("❌ Ingen prognosfil hittades i Data_resultat!")
}

# Visa tillgängliga filer
message("\n=== TILLGÄNGLIGA PROGNOSFILER ===")
for (i in seq_along(prognos_filer)) {
  message(paste("  ", i, ":", basename(prognos_filer[i])))
}

# Välj första filen automatiskt
vald_fil <- prognos_filer[1]
message(paste("\n  Laddar:", basename(vald_fil)))

# Ladda och konvertera prognosdata
befolkningsprognos_raa <- readRDS(vald_fil)
befolkningsprognos <- konvertera_till_enhetlig_struktur(befolkningsprognos_raa)

# Kontrollera att data finns
if (length(befolkningsprognos) == 0) {
  stop("❌ Ingen prognosdata hittades!")
}

# Ladda övrig data
kommun_lista <- readRDS("Data_underlag/kommun_lista.rds")

# Identifiera scenario baserat på filnamn
if (grepl("alternativ", vald_fil)) {
  risk_path <- "Data_riskmatt/scenario_alternativ"
  message("  Laddar alternativa risktal")
  scenario_typ <- "alternativ"
} else {
  risk_path <- "Data_riskmatt"
  message("  Laddar standardrisktal")
  scenario_typ <- "standard"
}

# Ladda riskdata
fodelserisker <- readRDS(file.path(risk_path, "fodelserisker.rds"))
dodsrisker <- readRDS(file.path(risk_path, "dodsrisker.rds"))
inflyttningsrisker <- readRDS(file.path(risk_path, "inflyttningsrisker.rds"))
utflyttningsrisker <- readRDS(file.path(risk_path, "utflyttningsrisker.rds"))
invandringsrisker <- readRDS(file.path(risk_path, "invandringsrisker.rds"))
utvandringsrisker <- readRDS(file.path(risk_path, "utvandringsrisker.rds"))

# Ladda parametrar om de finns
parametrar <- tryCatch({
  readRDS("Data_underlag/senaste_parametrar.rds")
}, error = function(e) {
  message("  Kunde inte ladda parametrar - använder standardvärden")
  NULL
})

# Ladda alternativjusteringar om scenario är alternativ
alternativ_justeringar <- NULL
if (scenario_typ == "alternativ") {
  justeringar_fil <- "Data_underlag/senaste_justeringar.rds"
  if (file.exists(justeringar_fil)) {
    alternativ_justeringar <- readRDS(justeringar_fil)
    message("  Laddade alternativjusteringar från senaste körningen")
  } else {
    message("  Ingen justeringsfil hittades - alternativjusteringar visas inte")
  }
}

message("\n✅ All data inläst - startar app...")

# ===========================================================
# HJÄLPFUNKTIONER
# ===========================================================

# Funktion för att beräkna historiska risker från originaldata
berakna_historiska_risker <- function(kommun_lista, risk_typ, kommun_namn) {
  
  # Definiera de 10 senaste åren
  if (risk_typ == "Födelserisker") {
    senaste_10_ar <- kommun_lista$fodda %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    # Beräkna fruktsamhetskvoter för historiska år
    historisk_risk <- kommun_lista$fodda %>%
      filter(Region == kommun_namn, År %in% senaste_10_ar, Ålder >= 15, Ålder <= 49) %>%
      inner_join(
        kommun_lista$medelfolkmangd_modrar %>%
          filter(Region == kommun_namn, År %in% senaste_10_ar),
        by = c("Region", "År", "Ålder")
      ) %>%
      mutate(
        Värde = Värde.x / Värde.y,
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Kön = "kvinnor",
        Variabel = "Födelserisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Dödsrisker") {
    senaste_10_ar <- kommun_lista$doda %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    # Beräkna dödsrisker per år
    historisk_risk <- kommun_lista$doda %>%
      filter(Region == kommun_namn, År %in% senaste_10_ar) %>%
      inner_join(
        kommun_lista$totfolkmangd %>%
          filter(Region == kommun_namn, År %in% senaste_10_ar),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = Värde.x / Värde.y,
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Variabel = "Dödsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Inflyttningsrisker") {
    inflyttade <- kommun_lista$inrikes_inflyttade %>% 
      filter(Region == kommun_namn)
    
    senaste_10_ar <- inflyttade %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    # Hämta riksbefolkning för historiska år
    riket_befolkning <- kommun_lista$medelfolkmangd %>%
      filter(Region == "Riket", År %in% senaste_10_ar)
    
    historisk_risk <- inflyttade %>%
      filter(År %in% senaste_10_ar) %>%
      inner_join(
        riket_befolkning %>%
          select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = Värde / antal_riket,
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Variabel = "Inflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Utflyttningsrisker") {
    utflyttade <- kommun_lista$inrikes_utflyttade %>%
      filter(Region == kommun_namn)
    
    senaste_10_ar <- utflyttade %>%
      mutate(År = as.character(År)) %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    historisk_risk <- utflyttade %>%
      mutate(År = as.character(År)) %>%
      filter(År %in% senaste_10_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% senaste_10_ar) %>%
          select(Region, År, Ålder, Kön, antal_befolkning = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = Värde / antal_befolkning,
        Värde = replace_na(Värde, 0),
        Värde = pmin(Värde, 0.5),
        Variabel = "Utflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Invandringsrisker") {
    invandrade <- kommun_lista$invandring %>%
      filter(Region == kommun_namn)
    
    senaste_10_ar <- invandrade %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    # Hämta riksinvandring
    riket_invandrade <- kommun_lista$invandring %>% 
      filter(Region == "Riket", År %in% senaste_10_ar)
    
    historisk_risk <- invandrade %>%
      filter(År %in% senaste_10_ar) %>%
      inner_join(
        riket_invandrade %>%
          select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = Värde / antal_riket,
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 1.0),
        Variabel = "Invandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Utvandringsrisker") {
    utvandrade <- kommun_lista$utvandring %>%
      filter(Region == kommun_namn)
    
    senaste_10_ar <- utvandrade %>%
      mutate(År = as.character(År)) %>%
      pull(År) %>%
      unique() %>%
      sort() %>%
      tail(10)
    
    historisk_risk <- utvandrade %>%
      mutate(År = as.character(År)) %>%
      filter(År %in% senaste_10_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% senaste_10_ar) %>%
          select(Region, År, Ålder, Kön, antal_befolkning = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = Värde / antal_befolkning,
        Värde = replace_na(Värde, 0),
        Värde = pmin(Värde, 0.5),
        Variabel = "Utvandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
  }
  
  return(historisk_risk)
}

# Funktion för att skapa ettårsklass-data
skapa_ettarsklass_data <- function(prognos, kommun_namn, komponent_typ, valda_ar, kommun_lista = NULL, antal_historiska_ar = 10) {
  
  if (!kommun_namn %in% names(prognos)) {
    return(NULL)
  }
  
  # Skapa prognosdata för valda år
  prognos_data <- tibble()
  
  # Filtrera valda år som finns i prognosen
  prognos_ar_i_data <- names(prognos[[kommun_namn]]$komponenter)
  valda_prognos_ar <- valda_ar[valda_ar %in% prognos_ar_i_data]
  
  for (ar in valda_prognos_ar) {
    ar_komponenter <- prognos[[kommun_namn]]$komponenter[[ar]]
    
    # Beräkna värde baserat på komponenttyp
    if (komponent_typ == "Födda") {
      ar_data <- ar_komponenter$födda %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Födda efter moderns ålder") {
      # Beräkna födda efter moderns ålder
      kvinnor_fertil <- prognos[[kommun_namn]]$totalbefolkning %>%
        filter(År == ar, Kön == "kvinnor", Ålder >= 15, Ålder <= 49) %>%
        select(Ålder, Antal_kvinnor = Värde)
      
      # Hämta födelserisker
      fodelse_risk_ar <- fodelserisker %>%
        filter(Region == kommun_namn, År == ar) %>%
        select(Ålder, Fodelserisk = Värde)
      
      # Beräkna antal födda per moderns ålder
      ar_data <- kvinnor_fertil %>%
        left_join(fodelse_risk_ar, by = "Ålder") %>%
        mutate(
          Värde = Antal_kvinnor * Fodelserisk,
          Värde = replace_na(Värde, 0),
          År = ar,
          Komponent = komponent_typ,
          Dataserie = "Prognos"
        ) %>%
        select(Ålder, År, Värde, Komponent, Dataserie)
    } else if (komponent_typ == "Döda") {
      ar_data <- ar_komponenter$döda %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Inrikes inflyttade") {
      ar_data <- ar_komponenter$inrikes_inflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Inrikes utflyttade") {
      ar_data <- ar_komponenter$inrikes_utflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Inrikes flyttnetto") {
      inflyttning <- ar_komponenter$inrikes_inflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Inflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      utflyttning <- ar_komponenter$inrikes_utflyttning %>%
        group_by(Ålder, År) %>%
        summarise(Utflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      ar_data <- inflyttning %>%
        left_join(utflyttning, by = c("Ålder", "År")) %>%
        mutate(Värde = Inflyttning - Utflyttning,
               Komponent = komponent_typ,
               Dataserie = "Prognos") %>%
        select(Ålder, År, Värde, Komponent, Dataserie)
    } else if (komponent_typ == "Invandrade") {
      ar_data <- ar_komponenter$invandring %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Utvandrade") {
      ar_data <- ar_komponenter$utvandring %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else if (komponent_typ == "Utrikes flyttnetto") {
      invandring <- ar_komponenter$invandring %>%
        group_by(Ålder, År) %>%
        summarise(Invandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      utvandring <- ar_komponenter$utvandring %>%
        group_by(Ålder, År) %>%
        summarise(Utvandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      ar_data <- invandring %>%
        left_join(utvandring, by = c("Ålder", "År")) %>%
        mutate(Värde = Invandring - Utvandring,
               Komponent = komponent_typ,
               Dataserie = "Prognos") %>%
        select(Ålder, År, Värde, Komponent, Dataserie)
    } else if (komponent_typ == "Total befolkning") {
      ar_data <- prognos[[kommun_namn]]$totalbefolkning %>%
        filter(År == ar) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Prognos")
    } else {
      ar_data <- tibble()
    }
    
    prognos_data <- bind_rows(prognos_data, ar_data)
  }
  
  # Säkerställ att År är numerisk
  if (nrow(prognos_data) > 0) {
    prognos_data <- prognos_data %>%
      mutate(År = as.numeric(År))
  }
  
  # Lägg till historiska data om tillgängliga
  historisk_data <- tibble()
  
  if (!is.null(kommun_lista)) {
    # Hämta historiska data baserat på komponenttyp
    if (komponent_typ == "Födda" && "fodda" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$fodda %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Födda efter moderns ålder") {
      # Approximativ historisk fördelning
      if ("fodda" %in% names(kommun_lista) && "totfolkmangd" %in% names(kommun_lista)) {
        # Hämta totalt antal födda per år
        fodda_totalt <- kommun_lista$fodda %>%
          filter(Region == kommun_namn) %>%
          group_by(År) %>%
          summarise(Totalt_fodda = sum(Värde, na.rm = TRUE), .groups = "drop")
        
        # Hämta kvinnor i fertil ålder
        kvinnor_fertil <- kommun_lista$totfolkmangd %>%
          filter(Region == kommun_namn, Kön == "kvinnor", Ålder >= 15, Ålder <= 49) %>%
          group_by(År, Ålder) %>%
          summarise(Antal_kvinnor = sum(Värde, na.rm = TRUE), .groups = "drop")
        
        # Använd normalfördelning för åldersfördelning
        alder_fordelning <- tibble(
          Ålder = 15:49,
          Vikt = dnorm(15:49, mean = 30, sd = 5)
        ) %>%
          mutate(Vikt = Vikt / sum(Vikt))
        
        # Fördela födda
        historisk_data <- kvinnor_fertil %>%
          inner_join(fodda_totalt, by = "År") %>%
          left_join(alder_fordelning, by = "Ålder") %>%
          group_by(År) %>%
          mutate(
            Total_vikt = sum(Antal_kvinnor * Vikt, na.rm = TRUE),
            Andel = (Antal_kvinnor * Vikt) / Total_vikt,
            Värde = Totalt_fodda * Andel,
            Komponent = komponent_typ,
            Dataserie = "Historisk"
          ) %>%
          ungroup() %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      }
      
    } else if (komponent_typ == "Döda" && "doda" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$doda %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Inrikes inflyttade" && "inrikes_inflyttade" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$inrikes_inflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Inrikes utflyttade" && "inrikes_utflyttade" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$inrikes_utflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Invandrade" && "invandring" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$invandring %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Utvandrade" && "utvandring" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$utvandring %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Total befolkning" && "totfolkmangd" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$totfolkmangd %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
    }
    
    # Beräkna nettovärden för historiska data
    if (komponent_typ == "Inrikes flyttnetto" && "inrikes_inflyttade" %in% names(kommun_lista) && "inrikes_utflyttade" %in% names(kommun_lista)) {
      inflyttning_hist <- kommun_lista$inrikes_inflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Inflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      utflyttning_hist <- kommun_lista$inrikes_utflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Utflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      if (nrow(inflyttning_hist) > 0 && nrow(utflyttning_hist) > 0) {
        historisk_data <- inflyttning_hist %>%
          left_join(utflyttning_hist, by = c("Ålder", "År")) %>%
          mutate(Värde = Inflyttning - Utflyttning,
                 Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      }
    } else if (komponent_typ == "Utrikes flyttnetto" && "invandring" %in% names(kommun_lista) && "utvandring" %in% names(kommun_lista)) {
      invandring_hist <- kommun_lista$invandring %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Invandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      utvandring_hist <- kommun_lista$utvandring %>%
        filter(Region == kommun_namn) %>%
        group_by(Ålder, År) %>%
        summarise(Utvandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      if (nrow(invandring_hist) > 0 && nrow(utvandring_hist) > 0) {
        historisk_data <- invandring_hist %>%
          left_join(utvandring_hist, by = c("Ålder", "År")) %>%
          mutate(Värde = Invandring - Utvandring,
                 Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(Ålder, År, Värde, Komponent, Dataserie)
      }
    }
    
    # Begränsa historiska data till valda år
    if (nrow(historisk_data) > 0) {
      historisk_data <- historisk_data %>%
        mutate(År = as.numeric(År)) %>%
        filter(År %in% as.numeric(valda_ar))
    }
  }
  
  # Kombinera data
  alla_data <- bind_rows(historisk_data, prognos_data) %>%
    filter(!is.na(Värde)) %>%
    mutate(År = as.numeric(År),
           Ålder = as.numeric(Ålder))
  
  return(alla_data)
}

# Funktion för att skapa ettårsklass-plot
skapa_ettarsklass_plot <- function(data, titel) {
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + 
             labs(title = titel, subtitle = "Ingen data tillgänglig") +
             theme_minimal())
  }
  
  # Separera historisk och prognosdata
  historisk_data <- data %>% filter(Dataserie == "Historisk")
  prognos_data <- data %>% filter(Dataserie == "Prognos")
  
  # Definiera färgpaletter
  n_hist <- length(unique(historisk_data$År))
  n_prog <- length(unique(prognos_data$År))
  
  # Historiska år: gråskala
  hist_colors <- if(n_hist > 0) {
    colorRampPalette(c("#B0B0B0", "#606060"))(n_hist)
  } else {
    character(0)
  }
  
  # Prognosår: blå skala
  prog_colors <- if(n_prog > 0) {
    colorRampPalette(c("#4A90E2", "#1E5BA8"))(n_prog)
  } else {
    character(0)
  }
  
  # Kombinera färger
  all_years <- c(sort(unique(historisk_data$År)), sort(unique(prognos_data$År)))
  all_colors <- c(hist_colors, prog_colors)
  names(all_colors) <- as.character(all_years)
  
  # Skapa plot
  p <- ggplot() +
    # Historiska data - tunnare och genomskinliga
    {if(nrow(historisk_data) > 0) {
      geom_line(data = historisk_data, 
                aes(x = Ålder, y = Värde, group = År, color = as.character(År)),
                linewidth = 0.8, alpha = 0.5)
    }} +
    # Prognosdata - tjockare linjer
    {if(nrow(prognos_data) > 0) {
      geom_line(data = prognos_data,
                aes(x = Ålder, y = Värde, group = År, color = as.character(År)),
                linewidth = 1.8, alpha = 0.7)
    }} +
    scale_color_manual(values = all_colors, name = "År") +
    labs(title = titel,
         subtitle = ifelse(grepl("moderns ålder", titel, ignore.case = TRUE), 
                           "Antal födda fördelat på moderns ålder", 
                           ""),
         x = "Ålder",
         y = "Antal") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray50"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))
  
  # Justera x-axeln baserat på komponenttyp
  if (grepl("moderns ålder", titel, ignore.case = TRUE)) {
    p <- p + scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15, 49))
  } else {
    p <- p + scale_x_continuous(breaks = seq(0, 100, by = 10))
  }
  
  # Lägg till noll-linje för netto-komponenter
  if (grepl("netto|förändring", titel, ignore.case = TRUE)) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", alpha = 0.7)
  }
  
  return(p)
}

# Funktion för att skapa risk_data med flera valda år
skapa_risk_data_multi <- function(risk_data, kommun_namn, valda_ar, kommun_lista = NULL, risk_typ = NULL) {
  if (!kommun_namn %in% risk_data$Region) {
    return(NULL)
  }
  
  # Konvertera till character för konsistens
  valda_ar <- as.character(valda_ar)
  
  # Säkerställ att År är character
  risk_data <- risk_data %>%
    mutate(År = as.character(År))
  
  # Hämta prognosår från risk_data
  prognos_ar_tillgangliga <- unique(risk_data$År)
  
  # Dela upp valda år
  valda_prognos_ar <- valda_ar[valda_ar %in% prognos_ar_tillgangliga]
  valda_historiska_ar <- valda_ar[!valda_ar %in% prognos_ar_tillgangliga]
  
  # Hämta prognosdata
  prognos_data <- risk_data %>%
    filter(Region == kommun_namn, År %in% valda_prognos_ar) %>%
    mutate(Typ = "Prognos") %>%
    arrange(År, Ålder)
  
  # Hämta historiska data
  historisk_data <- tibble()
  
  if (length(valda_historiska_ar) > 0 && !is.null(kommun_lista) && !is.null(risk_typ)) {
    # Beräkna historiska risker
    historisk_data <- tryCatch({
      berakna_historiska_risker_for_ar(kommun_lista, risk_typ, kommun_namn, valda_historiska_ar) %>%
        mutate(
          Typ = "Historisk",
          År = as.character(År)
        ) %>%
        arrange(År, Ålder)
    }, error = function(e) {
      warning(paste("Kunde inte beräkna historiska risker:", e$message))
      tibble()
    })
  }
  
  # Kombinera all data
  all_data <- bind_rows(historisk_data, prognos_data) %>%
    mutate(År = as.character(År)) %>%
    filter(!is.na(Värde), !is.na(Ålder), !is.na(År)) %>%
    arrange(År, Ålder)
  
  return(all_data)
}

# Funktion för att beräkna historiska risker för specifika år
berakna_historiska_risker_for_ar <- function(kommun_lista, risk_typ, kommun_namn, valda_ar) {
  
  # Konvertera till character
  valda_ar <- as.character(valda_ar)
  
  if (risk_typ == "Födelserisker") {
    # Beräkna fruktsamhetskvoter
    historisk_risk <- kommun_lista$fodda %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar, Ålder >= 15, Ålder <= 49) %>%
      inner_join(
        kommun_lista$medelfolkmangd_modrar %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% valda_ar),
        by = c("Region", "År", "Ålder")
      ) %>%
      mutate(
        Värde = ifelse(Värde.y > 0, Värde.x / Värde.y, 0),
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Kön = "kvinnor",
        Variabel = "Födelserisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Dödsrisker") {
    # Beräkna dödsrisker
    historisk_risk <- kommun_lista$doda %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$totfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% valda_ar),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = ifelse(Värde.y > 0, Värde.x / Värde.y, 0),
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Variabel = "Dödsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Inflyttningsrisker") {
    # Hämta riksbefolkning
    riket_befolkning <- kommun_lista$medelfolkmangd %>%
      mutate(År = as.character(År)) %>%
      filter(Region == "Riket", År %in% valda_ar)
    
    historisk_risk <- kommun_lista$inrikes_inflyttade %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar) %>%
      inner_join(
        riket_befolkning %>%
          select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = ifelse(antal_riket > 0, Värde / antal_riket, 0),
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 0.5),
        Variabel = "Inflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Utflyttningsrisker") {
    historisk_risk <- kommun_lista$inrikes_utflyttade %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% valda_ar) %>%
          select(Region, År, Ålder, Kön, antal_befolkning = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = ifelse(antal_befolkning > 0, Värde / antal_befolkning, 0),
        Värde = replace_na(Värde, 0),
        Värde = pmin(Värde, 0.5),
        Variabel = "Utflyttningsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Invandringsrisker") {
    # Hämta riksinvandring
    riket_invandrade <- kommun_lista$invandring %>% 
      mutate(År = as.character(År)) %>%
      filter(Region == "Riket", År %in% valda_ar)
    
    historisk_risk <- kommun_lista$invandring %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar) %>%
      inner_join(
        riket_invandrade %>%
          select(År, Ålder, Kön, antal_riket = Värde),
        by = c("År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = ifelse(antal_riket > 0, Värde / antal_riket, 0),
        Värde = ifelse(is.infinite(Värde) | is.nan(Värde), 0, Värde),
        Värde = pmin(Värde, 1.0),
        Variabel = "Invandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
    
  } else if (risk_typ == "Utvandringsrisker") {
    historisk_risk <- kommun_lista$utvandring %>%
      mutate(År = as.character(År)) %>%
      filter(Region == kommun_namn, År %in% valda_ar) %>%
      inner_join(
        kommun_lista$medelfolkmangd %>%
          mutate(År = as.character(År)) %>%
          filter(Region == kommun_namn, År %in% valda_ar) %>%
          select(Region, År, Ålder, Kön, antal_befolkning = Värde),
        by = c("Region", "År", "Ålder", "Kön")
      ) %>%
      mutate(
        Värde = ifelse(antal_befolkning > 0, Värde / antal_befolkning, 0),
        Värde = replace_na(Värde, 0),
        Värde = pmin(Värde, 0.5),
        Variabel = "Utvandringsrisker"
      ) %>%
      select(Region, Kön, Ålder, År, Variabel, Värde)
  } else {
    historisk_risk <- tibble()
  }
  
  # Ta bort NA-värden
  if (nrow(historisk_risk) > 0) {
    historisk_risk <- historisk_risk %>%
      filter(!is.na(Värde), !is.na(Ålder), !is.na(År))
  }
  
  return(historisk_risk)
}

# Funktion för att skapa riskplot med flera år
skapa_risk_plot_multi <- function(data, titel, y_label = "Risk", valt_kon = "Båda") {
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot() + 
             labs(title = titel, subtitle = "Ingen data tillgänglig") +
             theme_minimal())
  }
  
  # Säkerställ character för År
  data <- data %>%
    mutate(År = as.character(År))
  
  # Filtrera på kön om relevant
  if (valt_kon != "Båda" && "Kön" %in% names(data)) {
    data <- data %>% filter(Kön == valt_kon)
  }
  
  # Ta bort NA-värden
  data <- data %>%
    filter(!is.na(Värde), !is.na(Ålder), !is.na(År))
  
  # För födelserisker finns bara kvinnor
  if (any(grepl("Födelserisker", data$Variabel, ignore.case = TRUE))) {
    # Separera historisk och prognosdata
    historisk_data <- data %>% filter(Typ == "Historisk")
    prognos_data <- data %>% filter(Typ == "Prognos")
    
    # Definiera färgpaletter
    n_hist <- length(unique(historisk_data$År))
    n_prog <- length(unique(prognos_data$År))
    
    # Historiska år: gråskala
    hist_colors <- if(n_hist > 0) {
      colorRampPalette(c("#B0B0B0", "#606060"))(n_hist)
    } else {
      character(0)
    }
    
    # Prognosår: blå skala
    prog_colors <- if(n_prog > 0) {
      colorRampPalette(c("#4A90E2", "#1E5BA8"))(n_prog)
    } else {
      character(0)
    }
    
    # Kombinera färger
    all_years <- c(
      if(n_hist > 0) sort(unique(historisk_data$År)) else character(0),
      if(n_prog > 0) sort(unique(prognos_data$År)) else character(0)
    )
    all_colors <- c(hist_colors, prog_colors)
    names(all_colors) <- all_years
    
    # Skapa plot
    p <- ggplot(data = data, aes(x = Ålder, y = Värde))
    
    # Lägg till historiska data
    if(nrow(historisk_data) > 0) {
      p <- p + geom_line(
        data = historisk_data,
        aes(group = År, color = År),
        linewidth = 0.8, 
        alpha = 0.5
      )
    }
    
    # Lägg till prognosdata
    if(nrow(prognos_data) > 0) {
      p <- p + geom_line(
        data = prognos_data,
        aes(group = År, color = År),
        linewidth = 1.8, 
        alpha = 0.7
      )
    }
    
    # Lägg till resten
    p <- p +
      scale_color_manual(values = all_colors, name = "År") +
      labs(title = titel,
           subtitle = paste("Endast kvinnor 15-49 år | Kön:", valt_kon),
           x = "Ålder",
           y = y_label) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray50"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA)
      ) +
      scale_x_continuous(breaks = seq(15, 50, by = 5), limits = c(15, 49)) +
      guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))
    
  } else {
    # För andra risktyper med könsuppdelning
    if ("Kön" %in% names(data) && valt_kon == "Båda") {
      # Om båda könen ska visas men vi har könsuppdelning
      p <- ggplot() + 
        labs(title = titel, 
             subtitle = "Välj ett specifikt kön för att se data (Kvinnor eller Män)") +
        theme_minimal() +
        theme(plot.subtitle = element_text(color = "red", face = "bold"))
    } else {
      # Separera historisk och prognosdata
      historisk_data <- data %>% filter(Typ == "Historisk")
      prognos_data <- data %>% filter(Typ == "Prognos")
      
      # Definiera färgpaletter
      n_hist <- length(unique(historisk_data$År))
      n_prog <- length(unique(prognos_data$År))
      
      # Historiska år: gråskala
      hist_colors <- if(n_hist > 0) {
        colorRampPalette(c("#B0B0B0", "#606060"))(n_hist)
      } else {
        character(0)
      }
      
      # Prognosår: blå skala
      prog_colors <- if(n_prog > 0) {
        colorRampPalette(c("#4A90E2", "#1E5BA8"))(n_prog)
      } else {
        character(0)
      }
      
      # Kombinera färger
      all_years <- c(
        if(n_hist > 0) sort(unique(historisk_data$År)) else character(0),
        if(n_prog > 0) sort(unique(prognos_data$År)) else character(0)
      )
      all_colors <- c(hist_colors, prog_colors)
      names(all_colors) <- all_years
      
      # Skapa plot
      p <- ggplot(data = data, aes(x = Ålder, y = Värde))
      
      # Lägg till historiska data
      if(nrow(historisk_data) > 0) {
        p <- p + geom_line(
          data = historisk_data,
          aes(group = År, color = År),
          linewidth = 0.8, 
          alpha = 0.5
        )
      }
      
      # Lägg till prognosdata
      if(nrow(prognos_data) > 0) {
        p <- p + geom_line(
          data = prognos_data,
          aes(group = År, color = År),
          linewidth = 1.8, 
          alpha = 0.7
        )
      }
      
      # Lägg till resten
      p <- p +
        scale_color_manual(values = all_colors, name = "År") +
        labs(title = titel,
             subtitle = paste("Kön:", valt_kon),
             x = "Ålder",
             y = y_label) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray50"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90"),
          legend.position = "right",
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA)
        ) +
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1)))
    }
  }
  
  # Formatera y-axel för procent om det är risker
  if (grepl("risk", y_label, ignore.case = TRUE)) {
    p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  }
  
  return(p)
}

# Funktion för att skapa komponentdata över tid
skapa_komponent_data <- function(prognos, kommun_namn, komponent_typ, kommun_lista = NULL, antal_historiska_ar = 10) {
  
  if (!kommun_namn %in% names(prognos)) {
    return(NULL)
  }
  
  # Hämta prognosdata
  alla_prognos_ar <- sort(unique(prognos[[kommun_namn]]$totalbefolkning$År))
  
  # Skapa prognosdata
  prognos_data <- tibble()
  
  for (ar in alla_prognos_ar) {
    ar_komponenter <- prognos[[kommun_namn]]$komponenter[[ar]]
    
    # Beräkna värde baserat på komponenttyp
    if (komponent_typ == "Födda") {
      varde <- sum(ar_komponenter$födda$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Döda") {
      varde <- sum(ar_komponenter$döda$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Födelsenetto") {
      fodda <- sum(ar_komponenter$födda$Värde, na.rm = TRUE)
      doda <- sum(ar_komponenter$döda$Värde, na.rm = TRUE)
      varde <- fodda - doda
    } else if (komponent_typ == "Inrikes inflyttade") {
      varde <- sum(ar_komponenter$inrikes_inflyttning$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Inrikes utflyttade") {
      varde <- sum(ar_komponenter$inrikes_utflyttning$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Inrikes flyttnetto") {
      inflyttning <- sum(ar_komponenter$inrikes_inflyttning$Värde, na.rm = TRUE)
      utflyttning <- sum(ar_komponenter$inrikes_utflyttning$Värde, na.rm = TRUE)
      varde <- inflyttning - utflyttning
    } else if (komponent_typ == "Invandrade") {
      varde <- sum(ar_komponenter$invandring$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Utvandrade") {
      varde <- sum(ar_komponenter$utvandring$Värde, na.rm = TRUE)
    } else if (komponent_typ == "Utrikes flyttnetto") {
      invandring <- sum(ar_komponenter$invandring$Värde, na.rm = TRUE)
      utvandring <- sum(ar_komponenter$utvandring$Värde, na.rm = TRUE)
      varde <- invandring - utvandring
    } else if (komponent_typ == "Total befolkning") {
      varde <- sum(prognos[[kommun_namn]]$totalbefolkning$Värde[prognos[[kommun_namn]]$totalbefolkning$År == ar], na.rm = TRUE)
    } else if (komponent_typ == "Total befolkningsförändring") {
      # Beräkna förändring från föregående år
      if (ar == min(alla_prognos_ar)) {
        # För första prognosåret, jämför med sista historiska året
        if (!is.null(kommun_lista) && "totfolkmangd" %in% names(kommun_lista)) {
          historisk_befolkning_2024 <- kommun_lista$totfolkmangd %>%
            filter(Region == kommun_namn, År == "2024") %>%
            summarise(Folkmangd_2024 = sum(Värde, na.rm = TRUE)) %>%
            pull(Folkmangd_2024)
          
          if (length(historisk_befolkning_2024) > 0 && !is.na(historisk_befolkning_2024)) {
            aktuell_bef <- sum(prognos[[kommun_namn]]$totalbefolkning$Värde[
              prognos[[kommun_namn]]$totalbefolkning$År == ar], na.rm = TRUE)
            
            varde <- aktuell_bef - historisk_befolkning_2024
          } else {
            varde <- NA
          }
        } else {
          varde <- NA
        }
      } else {
        # För övriga år
        foregaende_ar <- as.character(as.numeric(ar) - 1)
        if (foregaende_ar %in% alla_prognos_ar) {
          aktuell_bef <- sum(prognos[[kommun_namn]]$totalbefolkning$Värde[
            prognos[[kommun_namn]]$totalbefolkning$År == ar], na.rm = TRUE)
          foregaende_bef <- sum(prognos[[kommun_namn]]$totalbefolkning$Värde[
            prognos[[kommun_namn]]$totalbefolkning$År == foregaende_ar], na.rm = TRUE)
          varde <- aktuell_bef - foregaende_bef
        } else {
          varde <- NA
        }
      }
    }
    
    ar_data <- tibble(
      År = as.numeric(ar),
      Värde = varde,
      Komponent = komponent_typ,
      Dataserie = "Prognos"
    )
    
    prognos_data <- bind_rows(prognos_data, ar_data)
  }
  
  # Lägg till historiska data om tillgängliga
  historisk_data <- tibble()
  
  if (!is.null(kommun_lista)) {
    # Hämta historiska data baserat på komponenttyp
    if (komponent_typ == "Födda" && "fodda" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$fodda %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Döda" && "doda" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$doda %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Inrikes inflyttade" && "inrikes_inflyttade" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$inrikes_inflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Inrikes utflyttade" && "inrikes_utflyttade" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$inrikes_utflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Invandrade" && "invandring" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$invandring %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Utvandrade" && "utvandring" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$utvandring %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
      
    } else if (komponent_typ == "Total befolkning" && "totfolkmangd" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$totfolkmangd %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Värde = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        mutate(Komponent = komponent_typ, Dataserie = "Historisk")
    }
    
    # Beräkna nettovärden för historiska data
    if (komponent_typ == "Födelsenetto" && "fodda" %in% names(kommun_lista) && "doda" %in% names(kommun_lista)) {
      fodda_hist <- kommun_lista$fodda %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Fodda = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      doda_hist <- kommun_lista$doda %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Doda = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      if (nrow(fodda_hist) > 0 && nrow(doda_hist) > 0) {
        historisk_data <- fodda_hist %>%
          left_join(doda_hist, by = "År") %>%
          mutate(Värde = Fodda - Doda,
                 Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      }
    } else if (komponent_typ == "Inrikes flyttnetto" && "inrikes_inflyttade" %in% names(kommun_lista) && "inrikes_utflyttade" %in% names(kommun_lista)) {
      inflyttning_hist <- kommun_lista$inrikes_inflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Inflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      utflyttning_hist <- kommun_lista$inrikes_utflyttade %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Utflyttning = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      if (nrow(inflyttning_hist) > 0 && nrow(utflyttning_hist) > 0) {
        historisk_data <- inflyttning_hist %>%
          left_join(utflyttning_hist, by = "År") %>%
          mutate(Värde = Inflyttning - Utflyttning,
                 Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      }
    } else if (komponent_typ == "Utrikes flyttnetto" && "invandring" %in% names(kommun_lista) && "utvandring" %in% names(kommun_lista)) {
      invandring_hist <- kommun_lista$invandring %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Invandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      utvandring_hist <- kommun_lista$utvandring %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Utvandring = sum(Värde, na.rm = TRUE), .groups = "drop")
      
      if (nrow(invandring_hist) > 0 && nrow(utvandring_hist) > 0) {
        historisk_data <- invandring_hist %>%
          left_join(utvandring_hist, by = "År") %>%
          mutate(Värde = Invandring - Utvandring,
                 Komponent = komponent_typ,
                 Dataserie = "Historisk") %>%
          select(År, Värde, Komponent, Dataserie)
      }
    } else if (komponent_typ == "Total befolkningsförändring" && "totfolkmangd" %in% names(kommun_lista)) {
      historisk_data <- kommun_lista$totfolkmangd %>%
        filter(Region == kommun_namn) %>%
        group_by(År) %>%
        summarise(Folkmangd = sum(Värde, na.rm = TRUE), .groups = "drop") %>%
        arrange(År) %>%
        mutate(Forandring = Folkmangd - lag(Folkmangd)) %>%
        filter(!is.na(Forandring)) %>%
        mutate(Värde = Forandring,
               Komponent = komponent_typ,
               Dataserie = "Historisk") %>%
        select(År, Värde, Komponent, Dataserie)
    }
    
    # Begränsa historiska data
    if (nrow(historisk_data) > 0) {
      max_hist_ar <- max(as.numeric(historisk_data$År))
      min_hist_ar <- max_hist_ar - antal_historiska_ar + 1
      historisk_data <- historisk_data %>%
        filter(as.numeric(År) >= min_hist_ar) %>%
        mutate(År = as.numeric(År))
    }
  }
  
  # Kombinera data
  alla_data <- bind_rows(historisk_data, prognos_data) %>%
    filter(!is.na(Värde))
  
  return(alla_data)
}

# Funktion för att skapa plot över tid
skapa_komponent_plot <- function(data, titel) {
  if (is.null(data) || nrow(data) == 0) {
    return(girafe(ggobj = ggplot() + 
             labs(title = titel, subtitle = "Ingen data tillgänglig") +
             theme_minimal()))
  }
  
  # Hitta brytpunkt mellan historisk och prognosdata
  prognos_ar <- data %>% filter(Dataserie == "Prognos") %>% pull(År)
  brytpunkt <- ifelse(length(prognos_ar) > 0, min(prognos_ar) - 0.5, NA)
  
  p <- ggplot(data, aes(x = År, y = Värde, color = Dataserie, group = Dataserie)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(aes(tooltip = paste0("År: ", År, "\nVärde: ", Värde)), size = 2) +
    scale_color_manual(values = c("Historisk" = "black", "Prognos" = "blue")) +
    labs(title = titel,
         x = "År",
         y = "Antal",
         color = "Dataserie") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9)
    )
  
  # Lägg till brytlinje
  if (!is.na(brytpunkt)) {
    p <- p + geom_vline(xintercept = brytpunkt, linetype = "dashed", color = "darkgray", alpha = 0.7)
  }
  
  
  return(girafe(ggobj = p,
                width_svg = if(str_detect(titel, "Total")) 7.9 else 5.0,
                height_svg = 3.5,
                options = list(
                  opts_toolbar(saveaspng = FALSE),
                  opts_sizing(rescale = FALSE)
                  )))
}

# Hjälpfunktioner för Analysresultat-fliken
formatera_vikter <- function(antal_ar, viktningstyp, alpha = 0.5) {
  ar_index <- seq(1, antal_ar)
  
  if (viktningstyp == 1) {
    vikter <- rep(1/antal_ar, antal_ar)
    metod <- "Jämn viktning"
  } else if (viktningstyp == 2) {
    vikter_raw <- ar_index
    vikter <- vikter_raw / sum(vikter_raw)
    metod <- "Linjär viktning"
  } else if (viktningstyp == 3) {
    # EWMA - Exponentially Weighted Moving Average
    vikter_raw <- alpha * (1 - alpha)^(antal_ar - ar_index)
    vikter <- vikter_raw / sum(vikter_raw)
    metod <- paste0("EWMA (alpha = ", alpha, ")")
  }
  
  # Skapa vektor med år bakåt från nu
  ar_nu <- as.numeric(format(Sys.Date(), "%Y"))
  ar_vektor <- seq(ar_nu - antal_ar, ar_nu - 1)
  
  # Formatera vikterna
  vikt_text <- paste(ar_vektor, sprintf("%.1f%%", vikter * 100), sep = ": ", collapse = ", ")
  
  return(list(metod = metod, vikter = vikt_text))
}

formatera_justeringar <- function(justeringar) {
  if (is.null(justeringar) || length(justeringar$perioder) == 0) {
    return("Inga justeringar")
  }
  
  text_delar <- c()
  for (period in justeringar$perioder) {
    procent <- (period$multiplikator - 1) * 100
    text <- sprintf("%d-%d: %+.0f%%", period$från_år, period$till_år, procent)
    text_delar <- c(text_delar, text)
  }
  
  return(paste(text_delar, collapse = ", "))
}

# ===========================================================
# UI
# ===========================================================

ui <- page_navbar(
  title = "Befolkningsprognos",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel(
    title = "Metod och inställningar",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Information",
        p("Denna sida innehåller detaljerad information om prognosmetoden, 
          inställningar och tolkningshjälp för varje komponent."),
        hr(),
        selectInput(
          "kommun_analys",
          "Välj geografi:",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        h6("Scenariotyp"),
        uiOutput("scenario_info"),
        hr(),
        p(em("Tips: Klicka på rubrikerna nedan för att läsa mer om varje komponent."))
      ),
      
      # Huvudinnehåll
      div(
        # Övergripande beskrivning
        card(
          card_header(class = "bg-primary text-white", 
                      h4("Om kohort-komponent-metoden")),
          card_body(
            HTML("
            <p><strong>Kohort-komponent-metoden</strong> är den vanligaste metoden för befolkningsprognoser och används av de flesta statistikmyndigheter världen över, inklusive SCB. Metoden följer befolkningen uppdelad i kohorter (födelseårsgrupper) och beräknar hur varje kohort förändras över tiden genom demografiska komponenter.</p>
            
            <h5>Prognosens huvudsteg:</h5>
            <ol>
              <li><strong>Startbefolkning:</strong> Utgår från den senaste kända befolkningen (2024) uppdelad efter kön och 1-årsklasser (0-100+ år).</li>
              
              <li><strong>Åldring:</strong> För varje prognosår åldras befolkningen ett år. Personer som är 0 år blir 1 år, 1-åringar blir 2 år, och så vidare.</li>
              
              <li><strong>Demografiska händelser:</strong> För varje år beräknas:
                <ul>
                  <li>Antal födda baserat på antalet kvinnor i fertil ålder och åldersspecifika fruktsamhetskvoter</li>
                  <li>Antal döda baserat på åldersspecifika dödsrisker för kvinnor och män</li>
                  <li>Inrikes in- och utflyttningar baserat på åldersspecifika flyttrisker</li>
                  <li>In- och utvandring baserat på åldersspecifika migrationsrisker</li>
                </ul>
              </li>
              
              <li><strong>Ny befolkning:</strong> Den nya befolkningen = Gammal befolkning + Födda - Döda + Inflyttade - Utflyttade + Invandrade - Utvandrade</li>
              
              <li><strong>Upprepning:</strong> Processen upprepas för varje prognosår fram till 2040.</li>
            </ol>
            
            <h5>Risktal och antaganden:</h5>
            <p>Centralt för prognosen är de <em>risktal</em> som används för att beräkna antalet demografiska händelser. 
            Dessa baseras på historiska mönster och antaganden om framtiden. Modellen använder:</p>
            <ul>
              <li><strong>Lokala risktal</strong> för kommunen/regionen baserat på historiska data</li>
              <li><strong>Relationer till riksprognosen</strong> för att fånga nationella trender</li>
              <li><strong>Viktning av historiska år</strong> för att balansera stabilitet och aktualitet</li>
              <li><strong>Utjämning (splines)</strong> för att hantera slumpmässiga variationer</li>
            </ul>
            
            <p class='text-muted'><em>Nedan följer detaljerad information om varje demografisk komponent.</em></p>
            ")
          )
        ),
        
        # Regional vs Enskild prognos
        card(
          card_header(class = "bg-info text-white", 
                      h4("Regional och enskild prognos - skillnader och avstämning")),
          card_body(
            HTML("
            <h5>Två typer av prognoser</h5>
            <p>Modellen kan köra två typer av prognoser:</p>
            
            <ol>
              <li><strong>Enskild prognos:</strong> Beräknar befolkningsutvecklingen för EN geografi (kommun eller region) 
              utan hänsyn till andra geografier. Enklare och snabbare, men saknar regional konsistens.</li>
              
              <li><strong>Regional prognos:</strong> Beräknar samtidigt för ett län och alla dess kommuner med 
              avstämning så att kommunernas summor matchar länstotalerna. Mer komplex men säkerställer konsistens.</li>
            </ol>
            
            <h5>Varför behövs avstämning?</h5>
            <p>När prognoser beräknas separat för län och kommuner uppstår ofta skillnader:</p>
            <ul>
              <li>Summan av kommunernas födda ≠ Länets födda</li>
              <li>Summan av kommunernas inflyttning över länsgräns ≠ Länets inflyttning</li>
              <li>Och så vidare för alla komponenter...</li>
            </ul>
            
            <p>Detta beror på att risktal beräknas separat för varje geografi baserat på deras historiska data. 
            Små slumpmässiga variationer och avrundningar gör att totalerna inte stämmer.</p>
            
            <h5>Avstämningsprocessen</h5>
            <p><strong>Grundprincipen:</strong> Länets totaler anses mer tillförlitliga eftersom de baseras på större 
            befolkningsunderlag. Därför justeras kommunernas värden så att summan matchar länets.</p>
            
            <h6>1. Komponenter som alltid avstäms (födda, döda, invandring, utvandring):</h6>
            <p>För dessa komponenter är processen relativt enkel:</p>
            <ol>
              <li>Beräkna komponenten för varje kommun baserat på deras risktal</li>
              <li>Summera alla kommuners värden per kön/ålder/år</li>
              <li>Jämför med länets motsvarande värde</li>
              <li>Beräkna justeringsfaktor: <code>Länstotal / Kommunsumma</code></li>
              <li>Multiplicera varje kommuns värde med justeringsfaktorn</li>
            </ol>
            
            <p><em>Exempel:</em> Om kommunerna tillsammans beräknas få 1050 födda pojkar i åldern 0, 
            men länet ska ha 1000, blir justeringsfaktorn 1000/1050 = 0,952. 
            Varje kommuns antal födda pojkar multipliceras med 0,952.</p>
            
            <h6>2. Speciell hantering av inrikes flyttningar:</h6>
            <p>Inrikes flyttningar är mer komplexa eftersom vi måste skilja på:</p>
            <ul>
              <li><strong>Flyttningar inom länet</strong> (mellan länets kommuner)</li>
              <li><strong>Flyttningar över länsgräns</strong> (till/från andra län)</li>
            </ul>
            
            <p><strong>Länets flyttningar = ENDAST flyttningar över länsgräns</strong><br>
            <strong>Kommunernas flyttningar = BÅDE inom länet OCH över länsgräns</strong></p>
            
            <p>Avstämningsprocessen för flyttningar:</p>
            <ol>
              <li>För varje kommun: Separera flyttningar i två delar baserat på historiska andelar
                <ul>
                  <li>Del som går inom länet (påverkas INTE av avstämning)</li>
                  <li>Del som går över länsgräns (denna del justeras)</li>
                </ul>
              </li>
              <li>Summera kommunernas länsgränsflyttningar</li>
              <li>Jämför med länets total (som bara innehåller länsgränsflyttningar)</li>
              <li>Justera ENDAST länsgränsdelen: <code>Ny länsgränsdel = Original × (Länstotal / Kommunsumma)</code></li>
              <li>Kommunens nya total = Inom länet + Justerad länsgränsdel</li>
            </ol>
            
            <p><em>Exempel:</em> Halmstad har 1000 inflyttare varav historiskt 70% kommer från andra kommuner 
            i Halland (700 personer) och 30% från andra län (300 personer). Om avstämningen visar att 
            länsgränsinflyttningen ska justeras med faktor 0,9, blir det: 700 + (300 × 0,9) = 970 inflyttare totalt.</p>
            
            <h6>3. Hantering av saknade värden:</h6>
            <p>Om länet har värden för en viss kön/ålder-kombination men ingen kommun har det:</p>
            <ul>
              <li>Fördela länets värde jämnt mellan kommunerna</li>
              <li>Varje kommun får: <code>Länsvärde / Antal kommuner</code></li>
            </ul>
            
            <h6>4. Avrundningskorrigering:</h6>
            <p>Efter justering kan avrundningsfel göra att summan fortfarande inte stämmer exakt. Då:</p>
            <ul>
              <li>Beräkna återstående differens</li>
              <li>Lägg hela differensen på den största kommunen (som tål avrundningsfelet bäst)</li>
            </ul>
            
            <h5>Fördelar med regional avstämning</h5>
            <ul>
              <li><strong>Konsistens:</strong> Summan av kommunerna = Länstotalen</li>
              <li><strong>Robusthet:</strong> Länets större befolkning ger stabilare skattningar</li>
              <li><strong>Realism:</strong> Fångar regionala samband och begränsningar</li>
              <li><strong>Jämförbarhet:</strong> Kommuner kan jämföras rättvist inom regionen</li>
            </ul>
            
            <h5>När bör man välja vad?</h5>
            <ul>
              <li><strong>Välj enskild prognos när:</strong> Du bara behöver prognos för en kommun/region, 
              vill ha snabbt resultat, eller när regional konsistens inte är kritisk.</li>
              
              <li><strong>Välj regional prognos när:</strong> Du behöver prognoser för flera kommuner, 
              vill säkerställa att totalerna stämmer, eller när du ska jämföra kommuner inom länet.</li>
            </ul>
            
            <p class='text-info'><strong>Tips:</strong> I visualiseringsappen kan du se avstämningseffekten 
            genom att jämföra en kommuns värden när den körts som del av regional prognos versus enskild prognos.</p>
            ")
          )
        ),
        
        # Standard vs Alternativ prognos
        card(
          card_header(class = "bg-warning text-white", 
                      h4("Standard- och alternativprognos - scenarioanalys")),
          card_body(
            HTML("
    <h5>Två prognosscenarier</h5>
    <p>Modellen kan köra två olika scenarier för att hantera osäkerhet om framtiden:</p>
    
    <ol>
      <li><strong>Standardprognos:</strong> Utgår från att historiska mönster fortsätter oförändrade. 
      De risktal som beräknats från historiska data appliceras rakt av på hela prognosperioden.</li>
      
      <li><strong>Alternativprognos:</strong> Tillåter justeringar av risktalen för specifika perioder 
      för att simulera förändrade förutsättningar eller politiska beslut.</li>
    </ol>
    
    <h5>Hur fungerar periodiseringar?</h5>
    <p>Periodiseringar är justeringar av risktal som gäller för specifika tidsperioder. 
    De fungerar som multiplikatorer på de historiskt beräknade risktalen:</p>
    
    <div class='alert alert-info'>
      <strong>Exempel:</strong> Om inflyttningsrisken för 25-åringar historiskt varit 0,01 (1%), 
      och vi tillämpar en multiplikator på 1,10 för perioden 2025-2029, blir den justerade risken 
      0,011 (1,1%) under dessa år.
    </div>
    
    <h5>Kopplingen till historiska beräkningar</h5>
    <p><strong>Viktigt att förstå:</strong> Alternativscenarier bygger fortfarande på historiska data som bas. 
    De historiska mönstren är utgångspunkten som sedan justeras.</p>
    
    <p>Processen ser ut så här:</p>
    <ol>
      <li><strong>Historisk analys:</strong> Risktal beräknas baserat på faktiska data från de senaste 7-10 åren 
      (beroende på komponent). Detta ger grundmönstret.</li>
      
      <li><strong>Viktning av historik:</strong> Olika viktningsmetoder (jämn, linjär eller EWMA) används 
      för att balansera mellan stabilitet och aktualitet i de historiska mönstren.</li>
      
      <li><strong>Utjämning:</strong> Risktalen utjämnas med splines för att ta bort slumpmässiga variationer 
      och få mjuka åldersprofiler.</li>
      
      <li><strong>Scenariojustering:</strong> FÖRST EFTER alla dessa steg appliceras eventuella 
      scenariojusteringar som multiplikatorer på de färdiga risktalen.</li>
    </ol>
    
    <h5>Varför behövs alternativscenarier?</h5>
    <p>Historiska data fångar inte framtida strukturella förändringar:</p>
    <ul>
      <li><strong>Politiska beslut:</strong> Nya bostadsområden, infrastruktursatsningar, företagsetableringar</li>
      <li><strong>Demografiska trender:</strong> Förändrade barnafödandemönster, pensionsålder</li>
      <li><strong>Externa chocker:</strong> Pandemier, ekonomiska kriser, migration</li>
      <li><strong>Lokala satsningar:</strong> Universitetsexpansion, vårdcentraler, äldreboenden</li>
    </ul>
    
    <h5>Exempel på periodiseringar</h5>
    <table class='table table-sm'>
      <thead>
        <tr>
          <th>Komponent</th>
          <th>Period</th>
          <th>Justering</th>
          <th>Motivering</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>Inflyttning</td>
          <td>2025-2029</td>
          <td>+10%</td>
          <td>Nytt bostadsområde med 500 lägenheter</td>
        </tr>
        <tr>
          <td>Födelser</td>
          <td>2030-2040</td>
          <td>+5%</td>
          <td>Familjevänlig politik och barnomsorgsgaranti</td>
        </tr>
        <tr>
          <td>Dödsrisker</td>
          <td>2027-2040</td>
          <td>-2%</td>
          <td>Förbättrad äldreomsorg och vårdcentral</td>
        </tr>
        <tr>
          <td>Utvandring</td>
          <td>2025-2030</td>
          <td>+20%</td>
          <td>Osäker arbetsmarknad för nyanlända</td>
        </tr>
      </tbody>
    </table>
    
    <h5>Teknisk implementation</h5>
    <p>I koden definieras alternativscenarier i <code>prognos_konfiguration.R</code> under 
    <code>ALTERNATIV_JUSTERINGAR</code>. Varje komponent kan ha flera periodiseringar:</p>
    
    <pre><code>inflyttningsrisker = list(
  perioder = list(
    list(från_år = 2025, till_år = 2029, multiplikator = 1.10),
    list(från_år = 2030, till_år = 2040, multiplikator = 1.00)
  )
)</code></pre>
    
    <p>Detta exempel ger 10% högre inflyttning 2025-2029, sedan återgång till normalnivå.</p>
    
    <h5>Tolkning av resultat</h5>
    <p>När du jämför standard- och alternativprognos:</p>
    <ul>
      <li><strong>Standardprognos</strong> = \"Business as usual\" - vad händer om inget förändras</li>
      <li><strong>Alternativprognos</strong> = \"What if\" - vad händer vid specifika förändringar</li>
      <li><strong>Skillnaden</strong> = Effekten av de antagna förändringarna</li>
    </ul>
    
    <div class='alert alert-warning'>
      <strong>Varning:</strong> Alternativscenarier är inte prognoser av vad som kommer hända - 
      de är simuleringar av vad som skulle kunna hända under givna antaganden. 
      Använd dem för att förstå känslighet och planera för olika utfall.
    </div>
    
    <h5>Best practice för scenarioanalys</h5>
    <ol>
      <li><strong>Dokumentera antaganden:</strong> Varje justering bör ha en tydlig motivering</li>
      <li><strong>Var realistisk:</strong> Extrema justeringar (±50%) är sällan trovärdiga</li>
      <li><strong>Tänk systemiskt:</strong> Om inflyttning ökar, påverkas även utflyttning och födelser</li>
      <li><strong>Använd flera scenarier:</strong> Kör optimistiskt, realistiskt och pessimistiskt</li>
      <li><strong>Följ upp:</strong> Jämför prognoser mot utfall och justera metoden</li>
    </ol>
    
    <p class='text-muted'><em>Tips: Du kan köra samma prognos med olika alternativscenarier 
    för att skapa ett osäkerhetsintervall kring standardprognosen.</em></p>
    ")
          )
        ),
        
        # Komponenter
        div(id = "komponenter",
            # Födda
            card(
              card_header(
                class = "bg-light",
                h4("📊 Födda", 
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#fodda-content" 
                       aria-expanded="false" aria-controls="fodda-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "fodda-content",
                uiOutput("fodda_info")
              )
            ),
            
            # Döda
            card(
              card_header(
                class = "bg-light",
                h4("📊 Döda",
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#doda-content" 
                       aria-expanded="false" aria-controls="doda-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "doda-content",
                uiOutput("doda_info")
              )
            ),
            
            # Inrikes inflyttningar
            card(
              card_header(
                class = "bg-light",
                h4("📊 Inrikes inflyttningar",
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#inflyttning-content" 
                       aria-expanded="false" aria-controls="inflyttning-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "inflyttning-content",
                uiOutput("inflyttning_info")
              )
            ),
            
            # Inrikes utflyttningar
            card(
              card_header(
                class = "bg-light",
                h4("📊 Inrikes utflyttningar",
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#utflyttning-content" 
                       aria-expanded="false" aria-controls="utflyttning-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "utflyttning-content",
                uiOutput("utflyttning_info")
              )
            ),
            
            # Invandring
            card(
              card_header(
                class = "bg-light",
                h4("📊 Invandring",
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#invandring-content" 
                       aria-expanded="false" aria-controls="invandring-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "invandring-content",
                uiOutput("invandring_info")
              )
            ),
            
            # Utvandring
            card(
              card_header(
                class = "bg-light",
                h4("📊 Utvandring",
                   HTML('<button class="btn btn-sm btn-outline-secondary float-end" type="button" 
                       data-bs-toggle="collapse" data-bs-target="#utvandring-content" 
                       aria-expanded="false" aria-controls="utvandring-content">
                       Visa/Dölj
                       </button>'))
              ),
              card_body(
                class = "collapse",
                id = "utvandring-content",
                uiOutput("utvandring_info")
              )
            )
        )
      )
    )
  ),
  
  nav_panel(
    title = "Demografiska komponenter",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Inställningar",
        selectInput(
          "kommun",
          "Välj geografi:",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        h6("Information"),
        p("Visar demografiska komponenter för vald geografi med både historiska data (svart) och prognosdata (blå)."),
        p("Den streckade linjen markerar övergången mellan historisk data och prognos.")
      ),
      
      # Huvudinnehåll med kort i rutnät
      div(
        # Rad 4: Total befolkning
        fluidRow(
          class = "mb-3",
          column(6, card(
            card_header("Total befolkning"),
            card_body(girafeOutput("plot_total_befolkning", height = "300px"))
          )),
          column(6, card(
            card_header("Total befolkningsförändring"),
            card_body(girafeOutput("plot_total_forandring", height = "300px"))
          ))
        ),
        
        # Rad 1: Födelse-komponenter
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Födda"),
            card_body(girafeOutput("plot_fodda"))
          )),
          column(4, card(
            card_header("Döda"), 
            card_body(girafeOutput("plot_doda"))
          )),
          column(4, card(
            card_header("Födelsenetto"),
            card_body(girafeOutput("plot_fodelsenetto"))
          ))
        ),
        
        # Rad 2: Inrikes flyttningar
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Inrikes inflyttade"),
            card_body(girafeOutput("plot_inrikes_inflyttade", width = "90%", height = "100%"))
          )),
          column(4, card(
            card_header("Inrikes utflyttade"),
            card_body(girafeOutput("plot_inrikes_utflyttade", width = "90%", height = "100%"))
          )),
          column(4, card(
            card_header("Inrikes flyttnetto"),
            card_body(girafeOutput("plot_inrikes_netto", width = "90%", height = "100%"))
          ))
        ),
        
        # Rad 3: Utrikes flyttningar  
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Invandrade"),
            card_body(girafeOutput("plot_invandrade", height = "300px"))
          )),
          column(4, card(
            card_header("Utvandrade"),
            card_body(girafeOutput("plot_utvandrade", height = "300px"))
          )),
          column(4, card(
            card_header("Utrikes flyttnetto"),
            card_body(girafeOutput("plot_utrikes_netto", height = "300px"))
          ))
        )
        
      )
    )
  ),
  
  # Flik för 1-årsklasser
  nav_panel(
    title = "1-årsklasser",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Inställningar",
        selectInput(
          "kommun_ettar",
          "Välj geografi:",
          choices = NULL,
          selected = NULL
        ),
        checkboxGroupInput(
          "ar_ettar",
          "Välj år att visa:",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        h6("Information"),
        p("Visar demografiska komponenter fördelade på 1-årsklasser (summerat över könen)."),
        p("'Födda efter moderns ålder' visar antal födda fördelat på moderns ålder (15-49 år)."),
        p("Endast de år du väljer visas i graferna."),
        p("Du kan välja flera år samtidigt för att jämföra både historiska data och prognosdata.")
      ),
      
      # Huvudinnehåll med kort i rutnät
      div(
        # Rad 1: Födelse- och dödskomponenter
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Födda efter moderns ålder"),
            card_body(plotOutput("plot_fodda_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Döda per åldersklass"), 
            card_body(plotOutput("plot_doda_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Total befolkning per åldersklass"),
            card_body(plotOutput("plot_total_befolkning_ettar", height = "300px"))
          ))
        ),
        
        # Rad 2: Inrikes flyttningar
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Inrikes inflyttade per åldersklass"),
            card_body(plotOutput("plot_inrikes_inflyttade_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Inrikes utflyttade per åldersklass"),
            card_body(plotOutput("plot_inrikes_utflyttade_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Inrikes flyttnetto per åldersklass"),
            card_body(plotOutput("plot_inrikes_netto_ettar", height = "300px"))
          ))
        ),
        
        # Rad 3: Utrikes flyttningar  
        fluidRow(
          class = "mb-3",
          column(4, card(
            card_header("Invandrade per åldersklass"),
            card_body(plotOutput("plot_invandrade_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Utvandrade per åldersklass"),
            card_body(plotOutput("plot_utvandrade_ettar", height = "300px"))
          )),
          column(4, card(
            card_header("Utrikes flyttnetto per åldersklass"),
            card_body(plotOutput("plot_utrikes_netto_ettar", height = "300px"))
          ))
        )
      )
    )
  ),
  
  nav_panel(
    title = "Risktal",
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Inställningar",
        selectInput(
          "kommun_risk",
          "Välj geografi:",
          choices = NULL,
          selected = NULL
        ),
        checkboxGroupInput(
          "ar_risk_multi",
          "Välj år att visa:",
          choices = NULL,
          selected = NULL
        ),
        selectInput(
          "kon_risk",
          "Välj kön:",
          choices = c("Båda" = "Båda", "Kvinnor" = "kvinnor", "Män" = "män"),
          selected = "kvinnor"
        ),
        hr(),
        h6("Information"),
        p("Visar risktalen som används i befolkningsprognosen för vald geografi."),
        p(strong("Historiska år"), " (före 2025) visas med tunnare linjer och är något genomskinliga."),
        p(strong("Prognosår"), " (2025 och framåt) visas med tjockare linjer."),
        p("Födelserisker visas endast för kvinnor i fertil ålder (15-49 år)."),
        p("För övriga risktal rekommenderas att välja antingen kvinnor eller män för tydligare visualisering.")
      ),
      
      # Huvudinnehåll med kort för risktal
      div(
        # Rad 1: Födelserisker och Dödsrisker
        fluidRow(
          class = "mb-3",
          column(6, card(
            card_header("Födelserisker"),
            card_body(plotOutput("plot_fodelserisker", height = "300px"))
          )),
          column(6, card(
            card_header("Dödsrisker"), 
            card_body(plotOutput("plot_dodsrisker", height = "300px"))
          ))
        ),
        
        # Rad 2: Inrikes risker
        fluidRow(
          class = "mb-3",
          column(6, card(
            card_header("Inflyttningsrisker"),
            card_body(plotOutput("plot_inflyttningsrisker", height = "300px"))
          )),
          column(6, card(
            card_header("Utflyttningsrisker"),
            card_body(plotOutput("plot_utflyttningsrisker", height = "300px"))
          ))
        ),
        
        # Rad 3: Utrikes risker
        fluidRow(
          class = "mb-3",
          column(6, card(
            card_header("Invandringsrisker"),
            card_body(plotOutput("plot_invandringsrisker", height = "300px"))
          )),
          column(6, card(
            card_header("Utvandringsrisker"),
            card_body(plotOutput("plot_utvandringsrisker", height = "300px"))
          ))
        )
      )
    )
  ),
  nav_panel(
    title = "Riksprognos",
    h3("Kommande: Riksprognos"),
    p("Denna flik kommer att innehålla riksprognosen och jämförelser med lokala prognoser.")
  ),
  
  nav_panel(
    title = "Analyser över tid", 
    h3("Kommande: Analyser över tid"),
    p("Denna flik kommer att innehålla möjlighet att dekomponera demografiska effekter över tid.")
  )
)

# ===========================================================
# SERVER
# ===========================================================

server <- function(input, output, session) {
  
  # Initialisera kommun-val och årtal för alla flikar
  observe({
    kommun_val <- names(befolkningsprognos)
    
    # Uppdatera alla dropdown-menyer
    updateSelectInput(session, "kommun",
                      choices = kommun_val,
                      selected = kommun_val[1])
    
    updateSelectInput(session, "kommun_risk",
                      choices = kommun_val,
                      selected = kommun_val[1])
    
    updateSelectInput(session, "kommun_analys",
                      choices = kommun_val,
                      selected = kommun_val[1])
    
    updateSelectInput(session, "kommun_ettar",
                      choices = kommun_val,
                      selected = kommun_val[1])
    
    # Sätt kvinnor som default för könsvalet
    updateSelectInput(session, "kon_risk",
                      selected = "kvinnor")
    
    # Hämta tillgängliga år dynamiskt för 1-årsklasser
    if (length(kommun_val) > 0) {
      # Hämta prognosår
      prognos_ar <- unique(befolkningsprognos[[kommun_val[1]]]$totalbefolkning$År)
      första_prognos_ar <- min(as.numeric(prognos_ar))
      
      # Hämta historiska år
      historiska_ar <- c()
      if ("totfolkmangd" %in% names(kommun_lista)) {
        historiska_ar <- unique(kommun_lista$totfolkmangd$År)
        historiska_ar <- historiska_ar[as.numeric(historiska_ar) < första_prognos_ar]
      }
      
      # Kombinera alla tillgängliga år
      alla_ar <- sort(as.numeric(c(historiska_ar, prognos_ar)))
      
      updateCheckboxGroupInput(session, "ar_ettar",
                               choices = as.character(alla_ar),
                               selected = as.character(första_prognos_ar))
    }
    
    # Hämta tillgängliga år för risktal
    if (length(kommun_val) > 0) {
      # Hämta tillgängliga prognosår
      prognos_ar <- unique(befolkningsprognos[[kommun_val[1]]]$totalbefolkning$År)
      
      # Hämta historiska år
      historiska_ar <- c()
      if ("totfolkmangd" %in% names(kommun_lista)) {
        historiska_ar <- unique(kommun_lista$totfolkmangd$År)
        # Begränsa till de 10 senaste historiska åren
        historiska_ar <- sort(as.numeric(historiska_ar))
        if (length(historiska_ar) > 10) {
          historiska_ar <- tail(historiska_ar, 10)
        }
        historiska_ar <- as.character(historiska_ar)
      }
      
      # Kombinera alla år
      alla_ar <- sort(c(historiska_ar, prognos_ar))
      
      # Uppdatera checkboxGroupInput
      updateCheckboxGroupInput(session, "ar_risk_multi",
                               choices = alla_ar,
                               selected = c("2023", "2024", "2025"))
    }
  })
  
  # Scenario info i Analysresultat
  output$scenario_info <- renderUI({
    if (scenario_typ == "alternativ") {
      tagList(
        tags$span(class = "badge bg-warning text-dark", "Alternativscenario"),
        br(), br(),
        p("Prognosen använder justerade risktal enligt alternativscenariot.")
      )
    } else {
      tagList(
        tags$span(class = "badge bg-success", "Standardscenario"),
        br(), br(),
        p("Prognosen använder standardrisktal utan justeringar.")
      )
    }
  })
  
  # Renderera komponentinformation för Analysresultat
  output$fodda_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$fodelserisker else list(antal_ar = 7, viktningstyp = 2)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.5)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$fodelserisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>Den valda viktningsmetoden innebär att vi lägger ", 
                ifelse(params$viktningstyp == 1, "lika stor vikt vid alla historiska år, vilket ger en stabil prognos som tar hänsyn till långsiktiga trender",
                       ifelse(params$viktningstyp == 2, "gradvis ökande vikt mot senare år, vilket gör att prognosen följer den senaste utvecklingen samtidigt som äldre mönster fortfarande påverkar",
                              paste0("exponentiellt avtagande vikt bakåt i tiden (EWMA med alpha = ", 
                                     ifelse(!is.null(params$alpha), params$alpha, "0.5"), 
                                     "), vilket betyder att det senaste året får störst vikt och tidigare år får exponentiellt minskande betydelse"))),
                ".</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Födelserisker beräknas genom följande stegvisa process:</p>
    
    <ol>
      <li><strong>Datainsamling:</strong> Vi hämtar antal födda barn för varje modersålder (15-49 år) samt medelfolkmängden av kvinnor i motsvarande åldrar för de senaste ", params$antal_ar, " åren.</li>
      
      <li><strong>Beräkning av fruktsamhetskvoter:</strong> För varje år och ålder beräknas den råa fruktsamhetskvoten som:
        <br><code>Fruktsamhetskvot = Antal födda / Antal kvinnor</code>
        <br>Detta ger sannolikheten att en kvinna i en viss ålder föder barn under ett år.</li>
      
      <li><strong>Poolad estimering med viktning:</strong> Data från alla historiska år kombineras med den valda viktningsmetoden. 
        Detta innebär att vi beräknar: <br>
        <code>Viktat antal födda = Σ(Antal födda år i × Vikt år i)</code><br>
        <code>Viktat antal kvinnor = Σ(Antal kvinnor år i × Vikt år i)</code><br>
        <code>Poolad fruktsamhetskvot = Viktat antal födda / Viktat antal kvinnor</code></li>
      
      <li><strong>Utjämning med splines:</strong> De poolade kvoterna utjämnas över åldrarna med en automatisk spline-funktion (smooth.spline med cross-validation). 
        Detta tar bort slumpmässiga variationer och ger en mjuk kurva som bevarar den totala fruktsamheten.</li>
      
      <li><strong>Relativ justering mot riket:</strong> Vi beräknar kvoten mellan kommunens/regionens utjämnade fruktsamhetskvoter och rikets:
        <br><code>Relativ fruktsamhet = Lokal fruktsamhet / Rikets fruktsamhet</code>
        <br>Denna kvot utjämnas också med splines för stabilitet.</li>
      
      <li><strong>Framskrivning:</strong> De relativa kvoterna appliceras på SCB:s riksprognos för fruktsamhet. 
        Detta säkerställer att lokala särdrag bevaras samtidigt som vi följer nationella trender för framtida fruktsamhetsutveckling.</li>
    </ol>
    
    <p class='text-info'><strong>Viktigt:</strong> Fruktsamhetstalen från riksprognosen uppdateras för varje prognosår, vilket innebär att födelseriskerna kan förändras över prognosperioden enligt SCB:s antaganden om framtida fruktsamhetsutveckling.</p>
    
    <h5>Tolkning</h5>
    <p>.</p>
    "))
  })
  
  output$doda_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$dodsrisker else list(antal_ar = 7, viktningstyp = 1)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.5)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$dodsrisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> Jämn viktning - alla år väger lika</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>Jämn viktning används för dödsrisker eftersom dödligheten följer långsiktiga trender och kortvariga variationer 
    (som pandemiår) bör inte få för stort genomslag i prognosen. Detta ger en mer stabil och tillförlitlig prognos.</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Dödsrisker beräknas genom en process som skiljer sig från övriga komponenter genom användning av åldersgrupper:</p>
    
    <ol>
      <li><strong>Åldersgruppering:</strong> För att hantera små tal grupperas åldrarna:
        <ul>
          <li>0 år (spädbarnsdödlighet behandlas separat)</li>
          <li>1-4 år</li>
          <li>5-9, 10-14, ..., 85-89 år (5-årsgrupper)</li>
          <li>90+ år (äldre behandlas som en grupp)</li>
        </ul>
      </li>
      
      <li><strong>Poolad estimering:</strong> För varje åldersgrupp och kön summeras antal döda och folkmängd över ", params$antal_ar, " år:
        <br><code>Dödsrisk = Totalt antal döda / Total folkmängd</code>
        <br>Detta ger robusta skattningar även för små kommuner.</li>
      
      <li><strong>Kontroll för små tal:</strong> Om antalet döda i en åldersgrupp är mindre än 50 över hela perioden, 
        används rikets dödsrisker istället. Detta säkerställer statistisk tillförlitlighet.</li>
      
      <li><strong>Relativa dödsrisker:</strong> För varje åldersgrupp beräknas kvoten mot riket:
        <br><code>Relativ dödsrisk = Lokal dödsrisk / Rikets dödsrisk</code>
        <br>Kvoten begränsas till intervallet 0.7-1.3 för att undvika extrema värden.</li>
      
      <li><strong>Expansion till 1-årsklasser:</strong> De relativa riskerna expanderas från åldersgrupper till 1-årsklasser. 
        Alla åldrar inom en grupp får samma relativa risk.</li>
      
      <li><strong>Framskrivning:</strong> De relativa dödskvoterna appliceras på SCB:s riksprognos för dödstal. 
        Detta innebär att lokala dödlighetsmönster bevaras medan den allmänna trenden följer nationella antaganden om framtida livslängdsutveckling.</li>
    </ol>
    
    <p class='text-info'><strong>Viktigt:</strong> Dödstalen från riksprognosen uppdateras för varje prognosår enligt SCB:s antaganden om fortsatt ökande livslängd.</p>
    
    <h5>Tolkning</h5>
    <p></p>
    "))
  })
  
  output$inflyttning_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$inflyttningsrisker else list(antal_ar = 7, viktningstyp = 2)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.5)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$inflyttningsrisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>", ifelse(params$viktningstyp == 2, 
                 "Linjär viktning används vilket innebär att senare års flyttmönster får gradvis större betydelse. Detta fångar upp trender som ändrade pendlingsmönster eller bostadsmarknadens utveckling.",
                 ifelse(params$viktningstyp == 3,
                        paste0("EWMA med alpha = ", ifelse(!is.null(params$alpha), params$alpha, "0.5"), 
                               " ger exponentiellt ökande vikt åt senare år, vilket gör prognosen känslig för aktuella förändringar i arbetsmarknad och bostadsutbud."),
                        "Jämn viktning ger en stabil prognos baserad på långsiktiga flyttmönster.")), "</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Inflyttningsrisker beskriver sannolikheten att personer från övriga Sverige flyttar till kommunen/regionen:</p>
    
    <ol>
      <li><strong>Datainsamling:</strong> Vi hämtar antal inflyttade från andra kommuner samt riksbefolkningen 
        för varje kön och ålder under de senaste ", params$antal_ar, " åren.</li>
      
      <li><strong>Riskberäkning mot riksbefolkning:</strong> Inflyttningsrisken beräknas som:
        <br><code>Inflyttningsrisk = Antal inflyttade / Riksbefolkning</code>
        <br>Detta ger sannolikheten att en person i riket flyttar till just denna kommun.</li>
      
      <li><strong>Poolad estimering:</strong> Data från alla historiska år kombineras med viktning:
        <br><code>Poolad risk = Σ(Inflyttade × Vikt) / Σ(Riksbefolkning × Vikt)</code></li>
      
      <li><strong>Utjämning med splines:</strong> Riskerna utjämnas över åldrarna för varje kön separat. 
        Detta tar bort slumpmässiga variationer samtidigt som typiska mönster bevaras (t.ex. högre flyttbenägenhet i 20-30-årsåldern).</li>
      
      <li><strong>Framskrivning:</strong> De beräknade inflyttningsriskerna appliceras på SCB:s riksbefolkningsprognos:
        <br><code>Inflyttade år t = Riksbefolkning år t × Inflyttningsrisk</code>
        <br>Detta innebär att om riket växer, ökar också det potentiella antalet inflyttare.</li>
      
      <li><strong>Regional avstämning:</strong> För regionala prognoser sker avstämning så att kommunernas 
        sammanlagda inflyttning över länsgräns matchar länets totala inflyttning.</li>
    </ol>
    
    <p class='text-info'><strong>OBS:</strong> Inflyttningsriskerna hålls konstanta över prognosperioden 
    (om inte alternativscenario används), men antalet inflyttade kan ändå variera beroende på riksbefolkningens utveckling.</p>
    
    <h5>Tolkning</h5>
    <p>.</p>
    "))
  })
  
  output$utflyttning_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$utflyttningsrisker else list(antal_ar = 7, viktningstyp = 3, alpha = 0.5)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.5)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$utflyttningsrisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>EWMA (Exponentially Weighted Moving Average) används ofta för utflyttning eftersom utflyttningsmönster kan förändras snabbt 
    med lokala förhållanden som bostadsbrist, arbetsmarknad eller förändrad infrastruktur. Med alpha = ", 
                ifelse(!is.null(params$alpha), params$alpha, "0.5"), 
                " får det senaste året ", 
                ifelse(!is.null(params$alpha), sprintf("%.0f%%", params$alpha * 100), "50%"), 
                " av vikten.</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Utflyttningsrisker beskriver sannolikheten att befintlig befolkning flyttar till andra delar av Sverige:</p>
    
    <ol>
      <li><strong>Datainsamling:</strong> Antal utflyttade och medelfolkmängd hämtas för varje kön och ålder 
        under de senaste ", params$antal_ar, " åren.</li>
      
      <li><strong>Riskberäkning mot egen befolkning:</strong> Till skillnad från inflyttning baseras utflyttningsrisken på egen befolkning:
        <br><code>Utflyttningsrisk = Antal utflyttade / Befolkning i kommunen</code></li>
      
      <li><strong>Poolad estimering med viktning:</strong> Data kombineras över åren med vald viktningsmetod för att 
        få robusta skattningar som samtidigt fångar aktuella trender.</li>
      
      <li><strong>Spline-utjämning:</strong> Riskerna utjämnas över åldrarna. För riket görs ingen utjämning 
        eftersom datamängden är tillräckligt stor.</li>
      
      <li><strong>Framskrivning:</strong> Utflyttningsriskerna appliceras på prognosbefolkningen:
        <br><code>Utflyttade år t = Befolkning år t × Utflyttningsrisk</code>
        <br>Detta innebär att utflyttningen är proportionell mot befolkningens storlek.</li>
      
      <li><strong>Regional avstämning:</strong> För regionala prognoser justeras utflyttningen så att 
        länsgränsflyttningar stämmer mellan kommun- och länsnivå.</li>
    </ol>
    
    <p class='text-info'><strong>Viktigt:</strong> Utflyttningsrisker är konstanta men appliceras på en befolkning 
    som förändras, vilket gör att antalet utflyttade varierar över tid.</p>
    
    <h5>Tolkning</h5>
    <p></p>
    "))
  })
  
  output$invandring_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$invandringsrisker else list(antal_ar = 7, viktningstyp = 3, alpha = 0.3)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.3)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$invandringsrisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>EWMA används eftersom invandring påverkas starkt av aktuell migrationspolitik, 
    internationella händelser och kommunens mottagningskapacitet. Med alpha = ", 
                ifelse(!is.null(params$alpha), params$alpha, "0.3"), 
                " får det senaste året ", 
                ifelse(!is.null(params$alpha), sprintf("%.0f%%", params$alpha * 100), "30%"), 
                " av vikten, vilket ger en balans mellan aktualitet och stabilitet.</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Invandringsrisker beskriver kommunens andel av rikets totala invandring:</p>
    
    <ol>
      <li><strong>Datainsamling:</strong> Antal invandrade till kommunen och riket hämtas för varje kön och ålder 
        under de senaste ", params$antal_ar, " åren.</li>
      
      <li><strong>Andelsberäkning:</strong> Invandringsrisken beräknas som kommunens andel av rikets invandring:
        <br><code>Invandringsrisk = Invandrade till kommunen / Invandrade till riket</code>
        <br>Detta ger kommunens 'marknadsandel' av invandringen.</li>
      
      <li><strong>Poolad estimering:</strong> Data viktas över åren enligt vald metod. 
        EWMA gör att de senaste årens fördelning får exponentiellt ökande vikt.</li>
      
      <li><strong>Utjämning:</strong> Andelarna utjämnas över åldrarna. Riket behöver ingen utjämning 
        då summan av alla kommuners andelar alltid blir 1.</li>
      
      <li><strong>Framskrivning:</strong> Kommunens invandring beräknas genom att applicera andelarna på riksprognosen:
        <br><code>Invandring = Riksprognos invandring × Kommunens andel</code>
        <br>Detta kopplar kommunens invandring till nationella antaganden om framtida migration.</li>
    </ol>
    
    <p class='text-info'><strong>OBS:</strong> SCB:s riksprognos för invandring varierar mellan åren baserat på 
    antaganden om internationell migration, vilket direkt påverkar kommunens invandring.</p>
    
    <h5>Tolkning</h5>
    <p></p>
    "))
  })
  
  output$utvandring_info <- renderUI({
    req(input$kommun_analys)
    
    # Hämta parametrar
    params <- if (!is.null(parametrar)) parametrar$utvandringsrisker else list(antal_ar = 7, viktningstyp = 3, alpha = 0.3)
    vikt_info <- formatera_vikter(params$antal_ar, params$viktningstyp, 
                                  if(params$viktningstyp == 3 && !is.null(params$alpha)) params$alpha else 0.3)
    
    # Hämta justeringar om alternativscenario
    justeringar <- "Inga justeringar (standardscenario)"
    if (scenario_typ == "alternativ" && !is.null(alternativ_justeringar)) {
      justeringar <- formatera_justeringar(alternativ_justeringar$utvandringsrisker)
    }
    
    HTML(paste0("
    <h5>Inställningar</h5>
    <ul>
      <li><strong>Antal historiska år:</strong> ", params$antal_ar, " år</li>
      <li><strong>Viktningsmetod:</strong> ", vikt_info$metod, "</li>
      <li><strong>Vikter per år:</strong> ", vikt_info$vikter, "</li>
      <li><strong>Scenariojusteringar:</strong> ", justeringar, "</li>
    </ul>
    
    <p>EWMA fångar upp aktuella trender i utvandring som kan påverkas av arbetsmarknadsläge, 
    bostadssituation eller hur väl integrerade olika invandrargrupper blivit. Med alpha = ", 
                ifelse(!is.null(params$alpha), params$alpha, "0.3"), 
                " får det senaste året ", 
                ifelse(!is.null(params$alpha), sprintf("%.0f%%", params$alpha * 100), "30%"), 
                " av vikten.</p>
    
    <h5>Beräkningar och metoder</h5>
    <p>Utvandringsrisker beskriver sannolikheten att befolkningen utvandrar:</p>
    
    <ol>
      <li><strong>Datainsamling:</strong> Antal utvandrade och medelfolkmängd för kommunen samlas in 
        för varje kön och ålder under ", params$antal_ar, " år.</li>
      
      <li><strong>Riskberäkning:</strong> Utvandringsrisken beräknas mot egen befolkning:
        <br><code>Utvandringsrisk = Antal utvandrade / Befolkning</code>
        <br>Detta ger sannolikheten att en person utvandrar under ett år.</li>
      
      <li><strong>Poolad estimering:</strong> Data kombineras med viktning där senare år får exponentiellt större betydelse 
        vid EWMA-viktning.</li>
      
      <li><strong>Utjämning:</strong> Riskerna utjämnas över åldrarna för att hantera slumpmässig variation, 
        särskilt viktigt för mindre kommuner.</li>
      
      <li><strong>Framskrivning:</strong> Utvandringsriskerna appliceras på prognosbefolkningen:
        <br><code>Utvandrade = Befolkning × Utvandringsrisk</code>
        <br>Riskerna hålls konstanta men appliceras på en föränderlig befolkning.</li>
    </ol>
    
    <p class='text-info'><strong>Notera:</strong> Till skillnad från invandring är utvandring inte kopplad till 
    riksprognosen utan baseras helt på lokala mönster.</p>
    
    <h5>Tolkning</h5>
    <p></p>
    "))
  })
  
  # Reaktiva data för varje komponent
  data_fodda <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Födda", kommun_lista)
  })
  
  data_doda <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Döda", kommun_lista)
  })
  
  data_fodelsenetto <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Födelsenetto", kommun_lista)
  })
  
  data_inrikes_inflyttade <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Inrikes inflyttade", kommun_lista)
  })
  
  data_inrikes_utflyttade <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Inrikes utflyttade", kommun_lista)
  })
  
  data_inrikes_netto <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Inrikes flyttnetto", kommun_lista)
  })
  
  data_invandrade <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Invandrade", kommun_lista)
  })
  
  data_utvandrade <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Utvandrade", kommun_lista)
  })
  
  data_utrikes_netto <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Utrikes flyttnetto", kommun_lista)
  })
  
  data_total_befolkning <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Total befolkning", kommun_lista)
  })
  
  data_total_forandring <- reactive({
    req(input$kommun)
    skapa_komponent_data(befolkningsprognos, input$kommun, "Total befolkningsförändring", kommun_lista)
  })
  
  # Reaktiva data för risktal
  data_fodelserisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(fodelserisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Födelserisker")
  })
  
  data_dodsrisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(dodsrisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Dödsrisker")
  })
  
  data_inflyttningsrisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(inflyttningsrisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Inflyttningsrisker")
  })
  
  data_utflyttningsrisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(utflyttningsrisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Utflyttningsrisker")
  })
  
  data_invandringsrisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(invandringsrisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Invandringsrisker")
  })
  
  data_utvandringsrisker <- reactive({
    req(input$kommun_risk, input$ar_risk_multi)
    skapa_risk_data_multi(utvandringsrisker, input$kommun_risk, input$ar_risk_multi, 
                          kommun_lista = kommun_lista, 
                          risk_typ = "Utvandringsrisker")
  })
  
  # Reaktiva data för 1-årsklasser
  data_fodda_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Födda efter moderns ålder", input$ar_ettar, kommun_lista)
  })
  
  data_doda_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Döda", input$ar_ettar, kommun_lista)
  })
  
  data_inrikes_inflyttade_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Inrikes inflyttade", input$ar_ettar, kommun_lista)
  })
  
  data_inrikes_utflyttade_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Inrikes utflyttade", input$ar_ettar, kommun_lista)
  })
  
  data_inrikes_netto_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Inrikes flyttnetto", input$ar_ettar, kommun_lista)
  })
  
  data_invandrade_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Invandrade", input$ar_ettar, kommun_lista)
  })
  
  data_utvandrade_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Utvandrade", input$ar_ettar, kommun_lista)
  })
  
  data_utrikes_netto_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Utrikes flyttnetto", input$ar_ettar, kommun_lista)
  })
  
  data_total_befolkning_ettar <- reactive({
    req(input$kommun_ettar, input$ar_ettar)
    skapa_ettarsklass_data(befolkningsprognos, input$kommun_ettar, "Total befolkning", input$ar_ettar, kommun_lista)
  })
  
  # Skapa plottar för 1-årsklasser
  output$plot_fodda_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_fodda_ettar(), "Födda efter moderns ålder")
  })
  
  output$plot_doda_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_doda_ettar(), "Döda per åldersklass")
  })
  
  output$plot_inrikes_inflyttade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_inflyttade_ettar(), "Inrikes inflyttade per åldersklass")
  })
  
  output$plot_inrikes_utflyttade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_utflyttade_ettar(), "Inrikes utflyttade per åldersklass")
  })
  
  output$plot_inrikes_netto_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_inrikes_netto_ettar(), "Inrikes flyttnetto per åldersklass")
  })
  
  output$plot_invandrade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_invandrade_ettar(), "Invandrade per åldersklass")
  })
  
  output$plot_utvandrade_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_utvandrade_ettar(), "Utvandrade per åldersklass")
  })
  
  output$plot_utrikes_netto_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_utrikes_netto_ettar(), "Utrikes flyttnetto per åldersklass")
  })
  
  output$plot_total_befolkning_ettar <- renderPlot({
    skapa_ettarsklass_plot(data_total_befolkning_ettar(), "Total befolkning per åldersklass")
  })
  
  # Risktal-plottar
  output$plot_fodelserisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_fodelserisker(), "Födelserisker", "Fruktsamhetskvot", input$kon_risk)
  })
  
  output$plot_dodsrisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_dodsrisker(), "Dödsrisker", "Dödsrisk", input$kon_risk)
  })
  
  output$plot_inflyttningsrisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_inflyttningsrisker(), "Inflyttningsrisker", "Inflyttningsrisk", input$kon_risk)
  })
  
  output$plot_utflyttningsrisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_utflyttningsrisker(), "Utflyttningsrisker", "Utflyttningsrisk", input$kon_risk)
  })
  
  output$plot_invandringsrisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_invandringsrisker(), "Invandringsrisker", "Invandringsrisk", input$kon_risk)
  })
  
  output$plot_utvandringsrisker <- renderPlot({
    req(input$kon_risk)
    skapa_risk_plot_multi(data_utvandringsrisker(), "Utvandringsrisker", "Utvandringsrisk", input$kon_risk)
  })
  
  # Komponent-plottar
  output$plot_fodda <- renderGirafe({
    skapa_komponent_plot(data_fodda(), "Födda")
  })
  
  output$plot_doda <- renderGirafe({
    skapa_komponent_plot(data_doda(), "Döda")
  })
  
  output$plot_fodelsenetto <- renderGirafe({
    skapa_komponent_plot(data_fodelsenetto(), "Födelsenetto")
  })
  
  output$plot_inrikes_inflyttade <- renderGirafe({
    skapa_komponent_plot(data_inrikes_inflyttade(), "Inrikes inflyttade")
  })
  
  output$plot_inrikes_utflyttade <- renderGirafe({
    skapa_komponent_plot(data_inrikes_utflyttade(), "Inrikes utflyttade")
  })
  
  output$plot_inrikes_netto <- renderGirafe({
    skapa_komponent_plot(data_inrikes_netto(), "Inrikes flyttnetto")
  })
  
  output$plot_invandrade <- renderGirafe({
    skapa_komponent_plot(data_invandrade(), "Invandrade")
  })
  
  output$plot_utvandrade <- renderGirafe({
    skapa_komponent_plot(data_utvandrade(), "Utvandrade")
  })
  
  output$plot_utrikes_netto <- renderGirafe({
    skapa_komponent_plot(data_utrikes_netto(), "Utrikes flyttnetto")
  })
  
  output$plot_total_befolkning <- renderGirafe({
    skapa_komponent_plot(data_total_befolkning(), "Total befolkning")
  })
  
  output$plot_total_forandring <- renderGirafe({
    skapa_komponent_plot(data_total_forandring(), "Total befolkningsförändring")
  })
}

# Kör appen
shinyApp(ui = ui, server = server)