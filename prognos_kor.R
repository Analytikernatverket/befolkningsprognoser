############################################################
#              BEFOLKNINGSPROGNOS - KÖRNING                #
############################################################
#                                                          #
#    Huvudkörningsfil som orkestrerar prognosprocessen     #
#                                                          #
#    Processsteg:                                          #
#    1. Validering av konfiguration                        #
#    2. Datainhämtning från SCB                            #
#    3. Beräkning av demografiska risktal                  #
#    4. Genomförande av prognos                            #
#    5. Start av visualiseringsapp                         #
#                                                          #
############################################################

# Aktivera detaljerade varningar
options(warn = 1)

# Loggningsfunktion för spårbarhet
track_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, msg))
  
  # Spara till loggfil
  log_file <- "tracking_log.txt"
  cat(sprintf("[%s] %s: %s\n", timestamp, level, msg), 
      file = log_file, append = TRUE)
}

track_message("=== STARTAR PROGNOSKÖRNING ===", "START")

# Rensa workspace (behåll konfigurationsvariabler)
track_message("Rensar workspace...")
rm(list = setdiff(ls(), c("PROJEKT_NAMN", "PROJEKT_BESKRIVNING", "PROJEKT_ANSVARIG",
                          "PROGNOSTYP", "ENSKILD_GEOGRAFI", "REGIONAL_INSTÄLLNINGAR",
                          "HISTORISKA_ÅR", "PROGNOS_START", "PROGNOS_SLUT", "AVRUNDNING", 
                          "bevara_summa_auto_spline_utvandring", "bevara_summa_auto_spline_inflyttning_lansgrans", 
                          "bevara_summa_auto_spline_inflyttning", "bevara_niva_per_ar_inflyttning", "metod_avstamning_regional",
                          "SCENARIO", "ALTERNATIV_JUSTERINGAR", "RISKBERÄKNINGS_PARAMETRAR",
                          "KÖR_DATAINHÄMTNING", "KÖR_RISKBERÄKNINGAR", "KÖR_PROGNOS",
                          "ÖPPNA_APP", "RENSA_GAMLA_RESULTAT", "VISA_DETALJERAD_OUTPUT",
                          "SPARA_MELLANRESULTAT", "track_message")))

# Sätt automatisk tidsstämpel
KÖRNING_DATUM <- Sys.Date()

# Visa aktiva konfigurationsvariabler
track_message("Aktiva konfigurationsvariabler:")
track_message(paste("  SCENARIO:", SCENARIO))
track_message(paste("  PROGNOSTYP:", PROGNOSTYP))
track_message(paste("  PROGNOS_START:", PROGNOS_START))
track_message(paste("  PROGNOS_SLUT:", PROGNOS_SLUT))

# Visa alternativjusteringar om definierade
if (SCENARIO == "alternativ" && exists("ALTERNATIV_JUSTERINGAR")) {
  track_message("Alternativjusteringar definierade:")
  for (komp in names(ALTERNATIV_JUSTERINGAR)) {
    track_message(paste("  -", komp, "har", 
                        length(ALTERNATIV_JUSTERINGAR[[komp]]$perioder), 
                        "perioder"))
  }
}

cat("
╔══════════════════════════════════════════════════════════╗
║          BEFOLKNINGSPROGNOS - KONTROLLPANEL              ║
╚══════════════════════════════════════════════════════════╝
")

# ===========================================================
# FÖRBEREDELSER OCH VALIDERING
# ===========================================================

track_message("Påbörjar förberedelser och validering...")

# Generera prognosår automatiskt
track_message("Genererar prognosår...")
PROGNOS_ÅR <- as.character(seq(as.numeric(PROGNOS_START), 
                               as.numeric(PROGNOS_SLUT) + 10))  # Extra år för riksdata
track_message(paste("  Genererade", length(PROGNOS_ÅR), "prognosår"))

# Bestäm geografier baserat på prognostyp
track_message(paste("Bestämmer geografier för prognostyp:", PROGNOSTYP))

if (PROGNOSTYP == "enskild") {
  track_message("  Konfigurerar för enskild prognos...")
  # För enskild prognos: vald geografi + riket
  ALLA_GEOGRAFIER <- c(
    "00",                        # Riket (krävs för beräkningar)
    ENSKILD_GEOGRAFI$kod         # Den valda geografin
  )
  
  # Sätt geografinamn för senare användning
  GEOGRAFI_NAMN <- ENSKILD_GEOGRAFI$namn
  track_message(paste("  Vald geografi:", GEOGRAFI_NAMN, "kod:", ENSKILD_GEOGRAFI$kod))
  
} else if (PROGNOSTYP == "regional") {
  track_message("  Konfigurerar för regional prognos...")
  # För regional prognos: län + alla kommuner + riket
  ALLA_GEOGRAFIER <- c(
    "00",                                    # Riket
    REGIONAL_INSTÄLLNINGAR$län_kod,         # Länet
    REGIONAL_INSTÄLLNINGAR$kommun_koder     # Kommunerna
  )
  
  # Sätt geografinamn för senare användning
  GEOGRAFI_NAMN <- REGIONAL_INSTÄLLNINGAR$län
  track_message(paste("  Län:", REGIONAL_INSTÄLLNINGAR$län))
  track_message(paste("  Antal kommuner:", length(REGIONAL_INSTÄLLNINGAR$kommuner)))
}

track_message(paste("Totalt antal geografier att hämta:", length(ALLA_GEOGRAFIER)))

# Visa vilka geografier som kommer hämtas
cat("\n📍 GEOGRAFIER SOM KOMMER HÄMTAS:\n")
if (PROGNOSTYP == "enskild") {
  cat(paste("  - Riket (00) - för beräkningar\n"))
  cat(paste("  -", ENSKILD_GEOGRAFI$namn, "(", ENSKILD_GEOGRAFI$kod, ")\n"))
} else {
  cat(paste("  - Riket (00) - för beräkningar\n"))
  cat(paste("  -", REGIONAL_INSTÄLLNINGAR$län, "(", REGIONAL_INSTÄLLNINGAR$län_kod, ")\n"))
  cat(paste("  -", length(REGIONAL_INSTÄLLNINGAR$kommuner), "kommuner\n"))
}

# ===========================================================
# VALIDERING
# ===========================================================

cat("\n📋 VALIDERAR INSTÄLLNINGAR...\n")
track_message("Startar validering av inställningar...")

# Kontrollera nödvändiga paket
track_message("Kontrollerar installerade paket...")
required_packages <- c("tidyverse", "pxweb", "writexl", "readxl", "zoo", 
                       "openxlsx", "shiny", "bslib", "ggplot2", "plotly", "viridis", "ggiraph")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages) > 0) {
  track_message(paste("VARNING: Saknade paket:", paste(missing_packages, collapse = ", ")), "ERROR")
  cat("❌ Följande paket saknas och måste installeras:\n")
  cat(paste("  -", missing_packages, "\n"))
  stop("Installera saknade paket med: install.packages(c('", 
       paste(missing_packages, collapse = "', '"), "'))")
} else {
  track_message("  Alla nödvändiga paket är installerade")
}

# Validera prognostyp
track_message("Validerar prognostyp...")
if(!PROGNOSTYP %in% c("enskild", "regional")) {
  track_message("FEL: Ogiltig prognostyp", "ERROR")
  stop("❌ PROGNOSTYP måste vara 'enskild' eller 'regional'")
}
track_message(paste("  Prognostyp OK:", PROGNOSTYP))

# Validera scenario
track_message("Validerar scenario...")
if(!SCENARIO %in% c("standard", "alternativ")) {
  track_message("FEL: Ogiltigt scenario", "ERROR")
  stop("❌ SCENARIO måste vara 'standard' eller 'alternativ'")
}
track_message(paste("  Scenario OK:", SCENARIO))

# Kontrollera alternativjusteringar om alternativscenario
if (SCENARIO == "alternativ") {
  track_message("Kontrollerar alternativjusteringar...")
  if (!exists("ALTERNATIV_JUSTERINGAR")) {
    track_message("VARNING: ALTERNATIV_JUSTERINGAR saknas!", "WARNING")
  } else {
    track_message(paste("  Alternativjusteringar finns med", 
                        length(ALTERNATIV_JUSTERINGAR), "komponenter"))
  }
}

# Validera geografiska inställningar
track_message("Validerar geografiska inställningar...")
if(PROGNOSTYP == "enskild") {
  # Kontrollera ENSKILD_GEOGRAFI
  if(!is.list(ENSKILD_GEOGRAFI)) {
    track_message("FEL: ENSKILD_GEOGRAFI är inte en lista", "ERROR")
    stop("❌ ENSKILD_GEOGRAFI måste vara en lista med 'namn' och 'kod'")
  }
  if(is.null(ENSKILD_GEOGRAFI$namn) || is.null(ENSKILD_GEOGRAFI$kod)) {
    track_message("FEL: ENSKILD_GEOGRAFI saknar namn eller kod", "ERROR")
    stop("❌ ENSKILD_GEOGRAFI måste innehålla både 'namn' och 'kod'")
  }
  if(nchar(ENSKILD_GEOGRAFI$kod) != 2 && nchar(ENSKILD_GEOGRAFI$kod) != 4) {
    track_message("FEL: Ogiltig geografikod", "ERROR")
    stop("❌ ENSKILD_GEOGRAFI$kod måste vara 2 siffror (län) eller 4 siffror (kommun)")
  }
  track_message(paste("✅ Enskild geografi validerad:", ENSKILD_GEOGRAFI$namn))
  cat(paste("✅ Enskild geografi validerad:", ENSKILD_GEOGRAFI$namn, 
            "(kod:", ENSKILD_GEOGRAFI$kod, ")\n"))
} else {
  # Validera regional prognos
  if(!is.list(REGIONAL_INSTÄLLNINGAR)) {
    track_message("FEL: REGIONAL_INSTÄLLNINGAR är inte en lista", "ERROR")
    stop("❌ REGIONAL_INSTÄLLNINGAR måste vara en lista")
  }
  if(length(REGIONAL_INSTÄLLNINGAR$kommuner) != length(REGIONAL_INSTÄLLNINGAR$kommun_koder)) {
    track_message("FEL: Antal kommunnamn och koder matchar inte", "ERROR")
    stop("❌ Antal kommunnamn och kommunkoder måste vara lika")
  }
  if(nchar(REGIONAL_INSTÄLLNINGAR$län_kod) != 2) {
    track_message("FEL: Ogiltig länskod", "ERROR")
    stop("❌ Länskod måste vara 2 siffror")
  }
  track_message(paste("✅ Regional prognos validerad:", REGIONAL_INSTÄLLNINGAR$län))
  cat(paste("✅ Regional prognos validerad:", REGIONAL_INSTÄLLNINGAR$län, 
            "med", length(REGIONAL_INSTÄLLNINGAR$kommuner), "kommuner\n"))
}

cat("✅ Alla inställningar validerade!\n")
track_message("Validering klar - alla kontroller godkända")

# ===========================================================
# SKAPA MAPPSTRUKTUR OCH RENSA GAMLA RESULTAT
# ===========================================================

cat("\n📁 SKAPAR MAPPSTRUKTUR...\n")
track_message("Skapar mappstruktur...")

mappar <- c("Data_underlag", "Data_riskmatt", "Data_resultat", "Loggar")

if(SCENARIO == "alternativ") {
  track_message("  Lägger till mapp för alternativscenario")
  mappar <- c(mappar, "Data_riskmatt/scenario_alternativ")
}

for(mapp in mappar) {
  track_message(paste("  Kontrollerar mapp:", mapp))
  if(!dir.exists(mapp)) {
    dir.create(mapp, recursive = TRUE)
    track_message(paste("    Skapade mapp:", mapp))
    cat(paste("  ✅ Skapade mapp:", mapp, "\n"))
  } else {
    cat(paste("  ℹ️  Mapp finns redan:", mapp, "\n"))
  }
}

# Rensa gamla resultatfiler om konfigurerat
if(KÖR_PROGNOS && RENSA_GAMLA_RESULTAT) {
  track_message("Rensar gamla resultatfiler...")
  gamla_filer <- list.files("Data_resultat", pattern = "befolkningsprognos_.*\\.rds$", full.names = TRUE)
  if(length(gamla_filer) > 0) {
    cat("\n🗑️  RENSAR GAMLA RESULTATFILER...\n")
    for(fil in gamla_filer) {
      file.remove(fil)
      track_message(paste("  Raderade:", basename(fil)))
      cat(paste("  ❌ Raderade:", basename(fil), "\n"))
    }
  }
}

# ===========================================================
# STARTA LOGGNING
# ===========================================================

track_message("Startar loggning...")
log_fil <- file.path("Loggar", paste0("prognos_log_", 
                                      format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                      ".txt"))

sink(log_fil, split = TRUE)  # Logga både till fil och konsol
track_message(paste("Loggfil skapad:", log_fil))

cat("\n════════════════════════════════════════════════════════════\n")
cat("BEFOLKNINGSPROGNOS - KÖRNINGSLOGG\n")
cat("════════════════════════════════════════════════════════════\n")
cat("Projekt:", PROJEKT_NAMN, "\n")
cat("Beskrivning:", PROJEKT_BESKRIVNING, "\n")
if(exists("PROJEKT_ANSVARIG") && nchar(PROJEKT_ANSVARIG) > 0) {
  cat("Ansvarig:", PROJEKT_ANSVARIG, "\n")
}
cat("Körning startad:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Prognostyp:", PROGNOSTYP, "\n")
cat("Scenario:", SCENARIO, "\n")
cat("════════════════════════════════════════════════════════════\n\n")

# ===========================================================
# KONTROLLERA KÄLLFILER
# ===========================================================

track_message("Kontrollerar källfiler...")

# Lista över nödvändiga källfiler
source_files <- c(
  "prognos_datainhamtning.R",
  "prognos_riskberakningar.R",
  "prognos_engeografi.R",
  "prognos_regional.R"
)

# Kontrollera att alla filer finns
for(fil in source_files) {
  track_message(paste("  Kontrollerar:", fil))
  if(!file.exists(fil)) {
    track_message(paste("FEL: Kan inte hitta fil:", fil), "ERROR")
    stop(paste("❌ Kan inte hitta fil:", fil))
  }
}
track_message("  Alla källfiler finns")

# ===========================================================
# KÖR PROGNOSEN STEG FÖR STEG
# ===========================================================

total_start <- Sys.time()
track_message("BÖRJAR HUVUDKÖRNING")

# STEG 1: DATAINHÄMTNING
if(KÖR_DATAINHÄMTNING) {
  track_message("STEG 1: DATAINHÄMTNING", "STEP")
  cat("\n🔄 STEG 1: DATAINHÄMTNING\n")
  cat("════════════════════════════════════════════\n")
  
  # Sätt globala variabler för datainhämtning
  track_message("  Sätter globala variabler för datainhämtning...")
  år_historisk <- HISTORISKA_ÅR
  Kommuner <- ALLA_GEOGRAFIER
  Län <- REGIONAL_INSTÄLLNINGAR$län
  år_prognos <- PROGNOS_ÅR
  
  track_message(paste("  Historiska år:", length(år_historisk)))
  track_message(paste("  Kommuner att hämta:", length(Kommuner)))
  track_message(paste("  Prognosår:", length(år_prognos)))
  
  # Kör datainhämtning
  tryCatch({
    track_message("  Kör prognos_datainhamtning.R...")
    source("prognos_datainhamtning.R", local = FALSE)
    track_message("✅ Datainhämtning klar!")
    cat("\n✅ Datainhämtning klar!\n")
  }, error = function(e) {
    track_message(paste("FEL vid datainhämtning:", e$message), "ERROR")
    cat("\n❌ Fel vid datainhämtning:\n")
    print(e)
    stop("Avbryter körning")
  })
} else {
  track_message("Hoppar över datainhämtning (KÖR_DATAINHÄMTNING = FALSE)")
  cat("\n⏭️  Hoppar över datainhämtning (KÖR_DATAINHÄMTNING = FALSE)\n")
}

# STEG 2: RISKBERÄKNINGAR
if(KÖR_RISKBERÄKNINGAR) {
  track_message("STEG 2: RISKBERÄKNINGAR", "STEP")
  cat("\n🔄 STEG 2: RISKBERÄKNINGAR\n")
  cat("════════════════════════════════════════════\n")
  
  # Sätt globala variabler för riskberäkningar
  track_message("  Sätter globala variabler för riskberäkningar...")
  AKTIVT_SCENARIO <- SCENARIO
  PARAMETRAR <- RISKBERÄKNINGS_PARAMETRAR
  ALTERNATIV_FRAMSKRIVNING <- ALTERNATIV_JUSTERINGAR
  
  track_message(paste("  AKTIVT_SCENARIO:", AKTIVT_SCENARIO))
  track_message(paste("  PARAMETRAR definierade:", !is.null(PARAMETRAR)))
  track_message(paste("  ALTERNATIV_FRAMSKRIVNING definierad:", !is.null(ALTERNATIV_FRAMSKRIVNING)))
  
  # Visa vilket scenario som används
  if(VISA_DETALJERAD_OUTPUT) {
    cat(paste("\nKör riskberäkningar med scenario:", AKTIVT_SCENARIO, "\n"))
  }
  
  # Kör riskberäkningar
  tryCatch({
    track_message("  Kör prognos_riskberakningar.R...")
    track_message("  Beräkningar kan ta några minuter...")
    
    # Logga alternativjusteringar om de används
    if (AKTIVT_SCENARIO == "alternativ") {
      track_message("  Kör med alternativscenario - kontrollerar justeringar...")
      if (exists("ALTERNATIV_FRAMSKRIVNING") && !is.null(ALTERNATIV_FRAMSKRIVNING)) {
        for (komp in names(ALTERNATIV_FRAMSKRIVNING)) {
          if (!is.null(ALTERNATIV_FRAMSKRIVNING[[komp]]$perioder)) {
            track_message(paste("    -", komp, "har justeringar"))
          }
        }
      }
    }
    
    source("prognos_riskberakningar.R", local = FALSE)
    if (!exists("bevara_summa_auto_spline_utvandring")) bevara_summa_auto_spline_utvandring <- FALSE
    if (!exists("bevara_summa_auto_spline_inflyttning_lansgrans")) bevara_summa_auto_spline_inflyttning_lansgrans <- FALSE
    if (!exists("bevara_summa_auto_spline_inflyttning")) bevara_summa_auto_spline_inflyttning <- TRUE
    if (!exists("bevara_niva_per_ar_inflyttning")) bevara_niva_per_ar_inflyttning <- FALSE
    cat(paste0("\nauto_spline_utvandring: ", bevara_summa_auto_spline_utvandring))
    cat(paste0("\nauto_spline_inflyttning_lansgrans: ", bevara_summa_auto_spline_inflyttning_lansgrans))
    cat(paste0("\nauto_spline_inflyttning: ", bevara_summa_auto_spline_inflyttning))
    cat(paste0("\nbevara_niva_per_ar_inflyttning: ", bevara_niva_per_ar_inflyttning))
    
    kor_riskberakningar(bevara_summa_auto_spline_utvandring = bevara_summa_auto_spline_utvandring,
                        bevara_summa_auto_spline_inflyttning_lansgrans = bevara_summa_auto_spline_inflyttning_lansgrans,
                        bevara_summa_auto_spline_inflyttning = bevara_summa_auto_spline_inflyttning,
                        bevara_niva_per_ar_inflyttning = bevara_niva_per_ar_inflyttning)
    track_message("✅ Riskberäkningar klara!")
    cat("\n✅ Riskberäkningar klara!\n")
    
    # Spara parametrar för visualiseringsapp
    saveRDS(RISKBERÄKNINGS_PARAMETRAR, "Data_underlag/senaste_parametrar.rds")
    track_message("  Parametrar sparade för visualiseringsapp")
    cat("  ✅ Parametrar sparade för visualiseringsapp\n")
    
  }, error = function(e) {
    track_message(paste("FEL vid riskberäkningar:", e$message), "ERROR")
    cat("\n❌ Fel vid riskberäkningar:\n")
    print(e)
    stop("Avbryter körning")
  })
} else {
  track_message("Hoppar över riskberäkningar (KÖR_RISKBERÄKNINGAR = FALSE)")
  cat("\n⏭️  Hoppar över riskberäkningar (KÖR_RISKBERÄKNINGAR = FALSE)\n")
}

# Spara parametrar för visualiseringsappen
saveRDS(RISKBERÄKNINGS_PARAMETRAR, "Data_underlag/senaste_parametrar.rds")

# Spara alternativjusteringar om de används
if (SCENARIO == "alternativ" && exists("ALTERNATIV_JUSTERINGAR")) {
  saveRDS(ALTERNATIV_JUSTERINGAR, "Data_underlag/senaste_justeringar.rds")
  cat("  ✅ Alternativjusteringar sparade för visualiseringsapp\n")
}

# STEG 3: PROGNOS
if(KÖR_PROGNOS) {
  track_message("STEG 3: BEFOLKNINGSPROGNOS", "STEP")
  cat("\n🔄 STEG 3: BEFOLKNINGSPROGNOS\n")
  cat("════════════════════════════════════════════\n")
  
  if(PROGNOSTYP == "enskild") {
    track_message("Kör enskild prognos...")
    # Kör prognos för en geografi
    GEOGRAFI_ATT_KORA <- ENSKILD_GEOGRAFI$namn
    SCENARIO_ATT_KORA <- SCENARIO
    
    track_message(paste("  GEOGRAFI_ATT_KORA:", GEOGRAFI_ATT_KORA))
    track_message(paste("  SCENARIO_ATT_KORA:", SCENARIO_ATT_KORA))
    
    # Visa detaljer om konfigurerat
    if(VISA_DETALJERAD_OUTPUT) {
      cat(paste("\nKör enskild prognos med scenario:", SCENARIO_ATT_KORA, "\n"))
      cat(paste("Geografi:", GEOGRAFI_ATT_KORA, "(kod:", ENSKILD_GEOGRAFI$kod, ")\n"))
    }
    
    tryCatch({
      track_message("  Kör prognos_engeografi.R...")
      source("prognos_engeografi.R", local = FALSE)
      track_message(paste("✅ Prognos klar för", GEOGRAFI_ATT_KORA))
      cat("\n✅ Prognos klar för", GEOGRAFI_ATT_KORA, "!\n")
    }, error = function(e) {
      track_message(paste("FEL vid prognoskörning:", e$message), "ERROR")
      cat("\n❌ Fel vid prognoskörning:\n")
      print(e)
      stop("Avbryter körning")
    })
    
  } else {
    track_message("Kör regional prognos...")
    # Kör regional prognos
    SCENARIO_ATT_KORA <- SCENARIO
    lan_namn <- REGIONAL_INSTÄLLNINGAR$län
    kommuner <- REGIONAL_INSTÄLLNINGAR$kommuner
    
    track_message(paste("  SCENARIO_ATT_KORA:", SCENARIO_ATT_KORA))
    track_message(paste("  lan_namn:", lan_namn))
    track_message(paste("  Antal kommuner:", length(kommuner)))
    
    # Visa detaljer om konfigurerat
    if(VISA_DETALJERAD_OUTPUT) {
      cat(paste("\nKör regional prognos med scenario:", SCENARIO_ATT_KORA, "\n"))
      cat(paste("Län:", lan_namn, "\n"))
      cat(paste("Kommuner som ska köras:", paste(kommuner, collapse = ", "), "\n"))
    }
    
    tryCatch({
      track_message("  Kör prognos_regional.R...")
      track_message("  Regional prognos kan ta längre tid...")
      source("prognos_regional.R", local = FALSE)
      track_message("✅ Regional prognos klar!")
      cat("\n✅ Regional prognos klar!\n")
    }, error = function(e) {
      track_message(paste("FEL vid regional prognos:", e$message), "ERROR")
      cat("\n❌ Fel vid regional prognos:\n")
      print(e)
      stop("Avbryter körning")
    })
  }
} else {
  track_message("Hoppar över prognos (KÖR_PROGNOS = FALSE)")
  cat("\n⏭️  Hoppar över prognos (KÖR_PROGNOS = FALSE)\n")
}

# ===========================================================
# AVSLUTNING
# ===========================================================

total_tid <- difftime(Sys.time(), total_start, units = "mins")

cat("\n\n════════════════════════════════════════════════════════════\n")
cat("KÖRNING AVSLUTAD\n")
cat("════════════════════════════════════════════════════════════\n")
cat("Total tid:", round(total_tid, 1), "minuter\n")
cat("Loggfil sparad:", log_fil, "\n")

# Lista resultatfiler
resultat_filer <- list.files("Data_resultat", pattern = "befolkningsprognos_.*\\.rds$", full.names = TRUE)
if(length(resultat_filer) > 0) {
  cat("\nResultatfiler:\n")
  for(fil in resultat_filer) {
    # Visa scenario baserat på filnamn
    if(grepl("alternativ", fil)) {
      cat("  ✅", basename(fil), "(ALTERNATIVSCENARIO)\n")
    } else {
      cat("  ✅", basename(fil), "(STANDARDSCENARIO)\n")
    }
  }
}

cat("\n🎉 PROGNOS KLAR! 🎉\n")

# Stäng loggning
sink()

# ===========================================================
# ÖPPNA VISUALISERINGSAPP
# ===========================================================

if(ÖPPNA_APP) {
  # Pausa kort för att säkerställa filskrivning
  Sys.sleep(1)
  
  # Visa tillgängliga resultatfiler
  resultat_filer <- list.files("Data_resultat", pattern = "befolkningsprognos_.*\\.rds$", full.names = FALSE)
  
  if(length(resultat_filer) > 0) {
    cat("\n")
    cat("╔══════════════════════════════════════════════════════════╗\n")
    cat("║           VISUALISERINGSAPPEN STARTAR                    ║\n")
    cat("║                                                          ║\n")
    cat("║  • Appen öppnas i din webbläsare                        ║\n")
    cat("║  • Stäng webbläsarfliken när du är klar                 ║\n")
    cat("║  • Tryck sedan Ctrl+C här för att avsluta               ║\n")
    cat("╚══════════════════════════════════════════════════════════╝\n\n")
    
    # Kontrollera att app-filen finns
    if(!file.exists("prognos_app.R")) {
      cat("❌ Kan inte hitta prognos_app.R\n")
      stop("App-fil saknas")
    } else {
      # Säkerställ att appen inte redan körs
      flagg_fil <- "Data_underlag/.app_running"
      
      if(!file.exists(flagg_fil)) {
        # Skapa flaggfil för att indikera körning
        writeLines("App startar", flagg_fil)
        
        tryCatch({
          # Starta appen
          shiny::runApp("prognos_app.R", launch.browser = TRUE)
        }, finally = {
          # Ta bort flaggfilen när appen stängs
          if(file.exists(flagg_fil)) {
            file.remove(flagg_fil)
          }
        })
      } else {
        cat("\n⚠️  Visualiseringsappen verkar redan köra.\n")
        cat("Om detta är felaktigt, ta bort filen 'Data_underlag/.app_running' och försök igen.\n")
      }
    }
  } else {
    cat("\n❌ Inga prognosfiler hittades i Data_resultat/\n")
    cat("Kör en prognos först innan du öppnar visualiseringsappen.\n")
  }
}

# Avsluta skriptet
cat("\n✅ Alla processer avslutade.\n")