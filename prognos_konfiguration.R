############################################################
#              BEFOLKNINGSPROGNOS - KONFIGURATION          #
############################################################
#                                                          #
#    Huvudkonfigurationsfil för befolkningsprognos         #
#                                                          #
#    Innehåll:                                             #
#    1. Projektinformation                                 #
#    2. Geografiska inställningar                          #
#    3. Tidsperioder                                       #
#    4. Scenarioval och justeringar                        #
#    5. Metodinställningar för riskberäkningar             #
#    6. Körningsval                                        #
#                                                          #
#    Instruktioner:                                        #
#    1. Anpassa inställningarna enligt projektbehov        #
#    2. Spara filen (Ctrl+S)                               #
#    3. Kör hela skriptet (Ctrl+Shift+Enter)               #
#                                                          #
############################################################

# Rensa konsolen för överskådlighet
cat("\014")


# ╔════════════════════════════════════════════════════════╗
# ║  📋 PROJEKTINFORMATION                                 ║
# ╚════════════════════════════════════════════════════════╝

PROJEKT_NAMN <- "Halland 2025"
PROJEKT_BESKRIVNING <- "Regional befolkningsprognos för Hallands län"

# ╔════════════════════════════════════════════════════════╗
# ║  🌍 GEOGRAFISKA VAL                                    ║
# ╚════════════════════════════════════════════════════════╝

# ┌─────────────────────────────────────────────────────────
# │ Prognostyp bestämmer omfattning:
# │ 
# │ • enskild  = En kommun eller ett län
# │ • regional = Ett län med alla dess kommuner
# └─────────────────────────────────────────────────────────

PROGNOSTYP <- "regional"  


# ┌─────────────────────────────────────────────────────────
# │ För ENSKILD prognos - specificera geografi
# └─────────────────────────────────────────────────────────

ENSKILD_GEOGRAFI <- list(
  namn = "Hallands län",     # Namn enligt SCB:s nomenklatur
  kod = "13"                 # SCB-kod: 2 siffror för län, 4 för kommun
)

# ┌─────────────────────────────────────────────────────────
# │ För REGIONAL prognos - specificera län och kommuner
# │ 
# │ Används endast om PROGNOSTYP = "regional"
# └─────────────────────────────────────────────────────────

REGIONAL_INSTÄLLNINGAR <- list(
  län = "Hallands län",
  län_kod = "13",
  
  kommuner = c("Halmstad", "Laholm", "Falkenberg", 
               "Varberg", "Kungsbacka", "Hylte"),
  
  kommun_koder = c("1380", "1381", "1382", 
                   "1383", "1384", "1315")
)


# ╔════════════════════════════════════════════════════════╗
# ║  📅 TIDSPERIODER                                       ║
# ╚════════════════════════════════════════════════════════╝

# Historiska år används för datainhämtning och trendanalys
# Rekommendation: minst 7 år för stabil statistik
HISTORISKA_ÅR <- c("2014", "2015", "2016", "2017", "2018", 
                   "2019", "2020", "2021", "2022", "2023", "2024")

# Prognosperiod
PROGNOS_START <- "2025"
PROGNOS_SLUT <- "2040"


# ╔════════════════════════════════════════════════════════╗
# ║  🎯 SCENARIOVAL                                        ║
# ╚════════════════════════════════════════════════════════╝

# ┌─────────────────────────────────────────────────────────
# │ Scenariotyp styr prognosantaganden:
# │ 
# │ • standard   = Historiska trender fortsätter oförändrade
# │ • alternativ = Justerade antaganden enligt nedan
# └─────────────────────────────────────────────────────────

SCENARIO <- "standard"

# ┌─────────────────────────────────────────────────────────
# │ ALTERNATIVSCENARIO - Justeringar av demografiska trender
# │ 
# │ Multiplikatorer appliceras på beräknade risktal:
# │ • 1.00 = ingen förändring
# │ • 1.10 = 10% ökning
# │ • 0.90 = 10% minskning
# └─────────────────────────────────────────────────────────

# ╔════════════════════════════════════════════════════════╗
# ║  ⚙️ METODINSTÄLLNINGAR                                 ║
# ╚════════════════════════════════════════════════════════╝

# ┌─────────────────────────────────────────────────────────
# │ Viktningsmetoder för historiska data:
# │ 
# │ 1 = Jämn viktning     (alla år väger lika)
# │ 2 = Linjär viktning   (senare år väger mer)
# │ 3 = EWMA-viktning     (exponentiell viktning)
# │ 
# │ För EWMA (typ 3), välj alpha mellan 0.1-0.9:
# │ • 0.1-0.3 = långsam anpassning (stabilitet)
# │ • 0.4-0.6 = balanserad anpassning
# │ • 0.7-0.9 = snabb anpassning (känslighet)
# └─────────────────────────────────────────────────────────

RISKBERÄKNINGS_PARAMETRAR <- list(
  
  fodelserisker = list(
    antal_ar = 7,        # Antal historiska år i beräkningen
    viktningstyp = 3,    # EWMA för att fånga aktuella trender
    alpha = 0.33         # Balanserad viktning
  ),
  
  dodsrisker = list(
    antal_ar = 7,
    viktningstyp = 1     # Jämn viktning för stabilitet
  ),
  
  inflyttningsrisker = list(
    antal_ar = 7,
    viktningstyp = 3,
    alpha = 0.33 
  ),
  
  utflyttningsrisker = list(
    antal_ar = 7,
    viktningstyp = 3,
    alpha = 0.33
  ),
  
  invandringsrisker = list(
    antal_ar = 7,
    viktningstyp = 3,
    alpha = 0.33
  ),
  
  utvandringsrisker = list(
    antal_ar = 7,
    viktningstyp = 3,
    alpha = 0.33
  )
)

# ╔════════════════════════════════════════════════════════╗
# ║  📊 ALTERNATIVSCENARIO - DETALJERADE JUSTERINGAR       ║
# ╚════════════════════════════════════════════════════════╝

# Används endast när SCENARIO = "alternativ"
# Definiera periodvisa justeringar för varje komponent

ALTERNATIV_JUSTERINGAR <- list(
  
  # Födelserisker
  fodelserisker = list(
    perioder = list(
      list(från_år = 2030, till_år = 2040, multiplikator = 1.05)  # +5%
    )
  ),
  
  # Dödsrisker  
  dodsrisker = list(
    perioder = list(
      list(från_år = 2027, till_år = 2040, multiplikator = 0.98)  # -2%
    )
  ),
  
  # Inflyttningsrisker
  inflyttningsrisker = list(
    perioder = list(
      list(från_år = 2025, till_år = 2029, multiplikator = 1.10), # +10%
      list(från_år = 2030, till_år = 2040, multiplikator = 1.00)  # Normal
    )
  ),
  
  # Utflyttningsrisker
  utflyttningsrisker = list(
    perioder = list(
      list(från_år = 2025, till_år = 2040, multiplikator = 1.00)  # Oförändrad
    )
  ),
  
  # Invandringsrisker
  invandringsrisker = list(
    perioder = list(
      list(från_år = 2025, till_år = 2040, multiplikator = 1.00)  # Oförändrad
    )
  ),
  
  # Utvandringsrisker
  utvandringsrisker = list(
    perioder = list(
      list(från_år = 2025, till_år = 2040, multiplikator = 1.00)  # Oförändrad
    )
  )
)


# ╔════════════════════════════════════════════════════════╗
# ║  🚀 KÖRNINGSVAL                                        ║
# ╚════════════════════════════════════════════════════════╝

# ┌─────────────────────────────────────────────────────────
# │ Välj vilka processteg som ska köras
# │ 
# │ TRUE  = Kör detta steg
# │ FALSE = Hoppa över (använd befintlig data)
# └─────────────────────────────────────────────────────────

KÖR_DATAINHÄMTNING <- FALSE     # Hämta data från SCB
KÖR_RISKBERÄKNINGAR <- TRUE    # Beräkna demografiska risktal
KÖR_PROGNOS <- TRUE            # Genomför befolkningsprognos
ÖPPNA_APP <- TRUE              # Starta visualiseringsapp


# ┌─────────────────────────────────────────────────────────
# │ Avancerade inställningar
# └─────────────────────────────────────────────────────────

RENSA_GAMLA_RESULTAT <- TRUE     # Ta bort tidigare resultatfiler
VISA_DETALJERAD_OUTPUT <- FALSE  # Utökad loggning för felsökning
SPARA_MELLANRESULTAT <- TRUE     # Spara data efter varje steg


# ╔════════════════════════════════════════════════════════╗
# ║  💾 KÖRNING                                            ║
# ╚════════════════════════════════════════════════════════╝
#                                                          
#  Konfigurationen är klar. Skriptet startar automatiskt   
#  prognoskörningen via source-kommandot nedan.            
#                                                          
############################################################


# ════════════════════════════════════════════════════════════
# AUTOMATISK KÖRNING
# ════════════════════════════════════════════════════════════
source("prognos_kor.R")