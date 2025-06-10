############################################################
#              RISKBERÄKNINGAR FÖR BEFOLKNINGSPROGNOS      #
############################################################

# Detta skript beräknar demografiska risktal baserat på historiska data
# som används som input för befolkningsprognoser. Skriptet körs från 
# huvudskriptet och förväntar sig att följande variabler är definierade:
# - AKTIVT_SCENARIO: "standard" eller "alternativ"
# - PARAMETRAR: lista med viktningstyp och antal_ar för varje komponent
# - ALTERNATIV_FRAMSKRIVNING: justeringar om AKTIVT_SCENARIO = "alternativ"

library(tidyverse)
library(zoo)

# ===========================================================
# LÄSA IN DATA
# ===========================================================

message("\n📊 LÄSER IN UNDERLAGSDATA FÖR RISKBERÄKNINGAR...")

kommun_lista <- read_rds("Data_underlag/kommun_lista.rds")
riket_lista <- read_rds("Data_underlag/riket_lista.rds")

message("  ✅ Data inläst")

# ===========================================================
# HJÄLPFUNKTIONER
# ===========================================================

# Beräkna vikter för historiska år baserat på vald viktningsmetod
berakna_tidsvikter <- function(antal_ar, viktningstyp, alpha = 0.5) {
  ar_index <- seq(1, antal_ar)
  
  if (viktningstyp == 1) {
    # Jämn viktning - alla år väger lika
    vikter <- rep(1/antal_ar, antal_ar)
  } else if (viktningstyp == 2) {
    # Linjär viktning - senare år får gradvis högre vikt
    vikter_raw <- ar_index
    vikter <- vikter_raw / sum(vikter_raw)
  } else if (viktningstyp == 3) {
    # EWMA (Exponentially Weighted Moving Average)
    # Exponentiellt ökande vikt för senare år
    if (is.null(alpha) || is.na(alpha) || !is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
      warning(paste("Ogiltig alpha-parameter:", alpha, "- använder standardvärde 0.5"))
      alpha <- 0.5
    }
    
    # Beräkna exponentiella vikter
    vikter_raw <- alpha * (1 - alpha)^(antal_ar - ar_index)
    
    # Kontrollera att beräkningen lyckades
    if (any(is.na(vikter_raw)) || any(is.infinite(vikter_raw)) || sum(vikter_raw) == 0) {
      warning("Problem med EWMA-beräkning, använder jämn viktning istället")
      vikter <- rep(1/antal_ar, antal_ar)
    } else {
      vikter <- vikter_raw / sum(vikter_raw)
    }
  } else {
    stop("Ogiltig viktningstyp. Välj 1 (jämn), 2 (linjär) eller 3 (EWMA).")
  }
  
  # Slutlig kontroll av vikternas giltighet
  if (any(is.na(vikter)) || abs(sum(vikter) - 1) > 0.001) {
    warning("Problem med viktningsberäkning, använder jämn viktning")
    vikter <- rep(1/antal_ar, antal_ar)
  }
  
  return(vikter)
}

# Automatisk spline-utjämning med möjlighet att bevara totalsumma
spline_auto <- function(alder_vektor, varde_vektor, bevara_summa = TRUE) {
  # För korta dataserier returneras originaldata utan utjämning
  if (length(alder_vektor) < 10) {
    return(varde_vektor)
  }
  
  # Ta bort NA-värden och nollor för robusthets skull
  valid_idx <- !is.na(varde_vektor) & varde_vektor > 0
  if (sum(valid_idx) < 10) {
    return(varde_vektor)
  }
  
  original_summa <- sum(varde_vektor, na.rm = TRUE)
  
  tryCatch({
    # Använd cross-validation för optimal utjämningsgrad
    spline_fit <- smooth.spline(
      alder_vektor[valid_idx], 
      varde_vektor[valid_idx], 
      cv = TRUE,           # Cross-validation för optimal smoothing
      all.knots = TRUE     # Använd alla datapunkter som knutpunkter
    )
    
    # Prediktera utjämnade värden för alla åldrar
    pred_varden <- predict(spline_fit, alder_vektor)$y
    pred_varden <- pmax(pred_varden, 0)  # Säkerställ inga negativa värden
    
    # Bevara ursprungssumman om så önskas (viktigt för t.ex. fruktsamhet)
    if (bevara_summa && original_summa > 0) {
      ny_summa <- sum(pred_varden, na.rm = TRUE)
      if (ny_summa > 0) {
        pred_varden <- pred_varden * (original_summa / ny_summa)
      }
    }
    
    return(pred_varden)
    
  }, error = function(e) {
    # Vid fel returneras originaldata
    warning(paste("Spline-utjämning misslyckades:", e$message))
    return(varde_vektor)
  })
}

# Applicera scenariojusteringar på beräknade risktal
applicera_scenariojustering <- function(data, komponent_namn) {
  # För standardscenario returneras data oförändrad
  if (AKTIVT_SCENARIO != "alternativ") {
    return(data)
  }
  
  # Hämta definierade justeringar för aktuell komponent
  justeringar <- ALTERNATIV_FRAMSKRIVNING[[komponent_namn]]
  
  # Om inga justeringar är definierade returneras data oförändrad
  if (is.null(justeringar) || is.null(justeringar$perioder)) {
    return(data)
  }
  
  # Applicera multiplikatorer för varje definierad tidsperiod
  justerad_data <- data
  for (period in justeringar$perioder) {
    justerad_data <- justerad_data %>%
      mutate(
        Värde = ifelse(
          as.numeric(År) >= period$från_år & as.numeric(År) <= period$till_år,
          Värde * period$multiplikator,
          Värde
        )
      )
  }
  
  # Logga att justeringar har applicerats
  if (length(justeringar$perioder) > 0) {
    message(sprintf("  - Applicerade scenariojustering för %s", komponent_namn))
  }
  
  return(justerad_data)
}

# ===========================================================
# VISUALISERING AV VIKTNINGSMETODER
# ===========================================================

# Skapa översikt och visualisering av valda viktningsmetoder
visualisera_viktningsmetoder <- function() {
  
  # Sammanställ alla komponenters inställningar i en tabell
  komponent_info <- tibble(
    Komponent = c("Födelserisker", "Dödsrisker", "Inflyttningsrisker", 
                  "Utflyttningsrisker", "Invandringsrisker", "Utvandringsrisker"),
    `Antal år` = c(PARAMETRAR$fodelserisker$antal_ar,
                   PARAMETRAR$dodsrisker$antal_ar,
                   PARAMETRAR$inflyttningsrisker$antal_ar,
                   PARAMETRAR$utflyttningsrisker$antal_ar,
                   PARAMETRAR$invandringsrisker$antal_ar,
                   PARAMETRAR$utvandringsrisker$antal_ar),
    Viktningstyp = c(PARAMETRAR$fodelserisker$viktningstyp,
                     PARAMETRAR$dodsrisker$viktningstyp,
                     PARAMETRAR$inflyttningsrisker$viktningstyp,
                     PARAMETRAR$utflyttningsrisker$viktningstyp,
                     PARAMETRAR$invandringsrisker$viktningstyp,
                     PARAMETRAR$utvandringsrisker$viktningstyp),
    Viktningsmetod = case_when(
      Viktningstyp == 1 ~ "Jämn viktning",
      Viktningstyp == 2 ~ "Linjär viktning",
      Viktningstyp == 3 ~ "EWMA",
      TRUE ~ "Okänd"
    )
  )
  
  # Lägg till alpha-värden för EWMA-metoden
  komponent_info <- komponent_info %>%
    mutate(
      Alpha = case_when(
        Komponent == "Födelserisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$fodelserisker$alpha), PARAMETRAR$fodelserisker$alpha, 0.5),
        Komponent == "Dödsrisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$dodsrisker$alpha), PARAMETRAR$dodsrisker$alpha, 0.5),
        Komponent == "Inflyttningsrisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$inflyttningsrisker$alpha), PARAMETRAR$inflyttningsrisker$alpha, 0.5),
        Komponent == "Utflyttningsrisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$utflyttningsrisker$alpha), PARAMETRAR$utflyttningsrisker$alpha, 0.5),
        Komponent == "Invandringsrisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$invandringsrisker$alpha), PARAMETRAR$invandringsrisker$alpha, 0.5),
        Komponent == "Utvandringsrisker" & Viktningstyp == 3 ~ 
          ifelse(!is.null(PARAMETRAR$utvandringsrisker$alpha), PARAMETRAR$utvandringsrisker$alpha, 0.5),
        TRUE ~ NA_real_
      )
    )
  
  # Visa sammanfattning av inställningar
  cat("\n=== INSTÄLLNINGAR PER KOMPONENT ===\n")
  print(komponent_info, n = Inf)
  
  # Visa information om aktivt scenario
  cat("\n=== SCENARIOINSTÄLLNINGAR ===\n")
  cat(sprintf("Aktivt scenario: %s\n", AKTIVT_SCENARIO))
  if (AKTIVT_SCENARIO == "alternativ") {
    cat("\nJusteringar för alternativ framskrivning:\n")
    for (komp in names(ALTERNATIV_FRAMSKRIVNING)) {
      justeringar <- ALTERNATIV_FRAMSKRIVNING[[komp]]
      if (!is.null(justeringar$perioder) && length(justeringar$perioder) > 0) {
        cat(sprintf("\n%s:\n", komp))
        for (period in justeringar$perioder) {
          cat(sprintf("  %d-%d: multiplikator %.2f (%+.0f%%)\n", 
                      period$från_år, period$till_år, period$multiplikator, 
                      (period$multiplikator - 1) * 100))
        }
      }
    }
  }
  
  # Identifiera unika kombinationer av viktningsmetoder för visualisering
  unika_kombinationer <- komponent_info %>%
    distinct(`Antal år`, Viktningstyp, Alpha) %>%
    arrange(`Antal år`, Viktningstyp)
  
  # Beräkna vikter för alla kombinationer
  viktdata <- tibble()
  
  for (i in 1:nrow(unika_kombinationer)) {
    antal_ar <- unika_kombinationer$`Antal år`[i]
    viktningstyp <- unika_kombinationer$Viktningstyp[i]
    alpha <- unika_kombinationer$Alpha[i]
    
    ar_index <- seq(1, antal_ar)
    
    # Beräkna vikter med korrekt alpha-parameter
    if (viktningstyp == 3 && !is.na(alpha)) {
      vikter <- berakna_tidsvikter(antal_ar, viktningstyp, alpha)
      metod <- paste0("EWMA (α=", sprintf("%.2f", alpha), ")")
    } else {
      vikter <- berakna_tidsvikter(antal_ar, viktningstyp)
      metod <- case_when(
        viktningstyp == 1 ~ "Jämn viktning",
        viktningstyp == 2 ~ "Linjär viktning",
        viktningstyp == 3 ~ "EWMA (α=0.5)",
        TRUE ~ "Okänd"
      )
    }
    
    # Identifiera vilka komponenter som använder denna viktningskombination
    komponenter <- komponent_info %>%
      filter(`Antal år` == antal_ar, 
             Viktningstyp == viktningstyp,
             (is.na(Alpha) & is.na(alpha)) | Alpha == alpha) %>%
      pull(Komponent)
    
    kombination_namn <- paste0(antal_ar, " år, ", metod, "\n", 
                               paste(komponenter, collapse = ", "))
    
    viktdata <- bind_rows(
      viktdata,
      tibble(
        År = ar_index,
        Vikt = vikter,
        Kombination = kombination_namn,
        `Antal år` = antal_ar
      )
    )
  }
  
  # Skapa visualisering av viktningsmetoder
  p <- ggplot(viktdata, aes(x = År, y = Vikt, fill = as.factor(`Antal år`))) +
    geom_col(width = 0.7) +
    facet_wrap(~ Kombination, ncol = 2, scales = "free_x") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis_d(name = "Antal år") +
    labs(
      title = "Viktningsmetoder per komponent",
      subtitle = "Olika komponenter kan använda olika antal år och viktningsmetoder",
      x = "Historiska år (1 = äldst)",
      y = "Vikt i procent"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  print(p)
  
  # Detaljerad textsammanfattning av vikter
  cat("\n=== DETALJERAD SAMMANFATTNING AV VIKTER ===\n")
  for (i in 1:nrow(unika_kombinationer)) {
    antal_ar <- unika_kombinationer$`Antal år`[i]
    viktningstyp <- unika_kombinationer$Viktningstyp[i]
    alpha <- unika_kombinationer$Alpha[i]
    
    komponenter <- komponent_info %>%
      filter(`Antal år` == antal_ar, 
             Viktningstyp == viktningstyp,
             (is.na(Alpha) & is.na(alpha)) | Alpha == alpha) %>%
      pull(Komponent)
    
    if (viktningstyp == 3 && !is.na(alpha)) {
      vikter <- berakna_tidsvikter(antal_ar, viktningstyp, alpha)
    } else {
      vikter <- berakna_tidsvikter(antal_ar, viktningstyp)
    }
    
    cat("\n", paste(komponenter, collapse = ", "), "\n", sep = "")
    cat("  Antal år: ", antal_ar, ", Viktningstyp: ", viktningstyp, sep = "")
    if (viktningstyp == 3 && !is.na(alpha)) {
      cat(", Alpha: ", sprintf("%.2f", alpha), sep = "")
    }
    cat("\n")
    cat("  Vikter: ", paste0(sprintf("%.1f%%", vikter * 100), collapse = ", "), "\n", sep = "")
  }
}

# ===========================================================
# 1. FÖDELSERISKER
# ===========================================================

# Beräkna åldersspecifika fruktsamhetskvoter baserat på historiska data
berakna_fodelserisker <- function() {
  
  params <- PARAMETRAR$fodelserisker
  antal_ar <- params$antal_ar
  
  message("Beräknar födelserisker...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5  # Standardvärde
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Definiera de senaste åren som ska användas i beräkningen
  senaste_ar <- kommun_lista$fodda %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa fruktsamhetskvoter för kvinnor i fertil ålder (15-49 år)
  fruktsamhet_raa <- kommun_lista$fodda %>%
    filter(År %in% senaste_ar, Ålder >= 15, Ålder <= 49) %>%
    inner_join(
      kommun_lista$medelfolkmangd_modrar %>%
        filter(År %in% senaste_ar),
      by = c("Region", "År", "Ålder")
    ) %>%
    mutate(
      fruktsamhetskvot = Värde.x / Värde.y,
      fruktsamhetskvot = ifelse(is.infinite(fruktsamhetskvot) | is.nan(fruktsamhetskvot), 0, fruktsamhetskvot),
      fruktsamhetskvot = pmin(fruktsamhetskvot, 0.5)  # Begränsa till rimliga värden
    ) %>%
    select(Region, År, Ålder, fruktsamhetskvot, Antal_födda = Värde.x, Antal_kvinnor = Värde.y)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- senaste_ar
  
  # Poola data med viktning över flera år för stabilare skattningar
  poolad_data <- fruktsamhet_raa %>%
    group_by(Region, Ålder) %>%
    summarise(
      viktad_antal_fodda = sum(Antal_födda * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_antal_kvinnor = sum(Antal_kvinnor * tidsvikter[as.character(År)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      poolad_fruktsamhetskvot = viktad_antal_fodda / viktad_antal_kvinnor,
      poolad_fruktsamhetskvot = ifelse(is.nan(poolad_fruktsamhetskvot) | is.infinite(poolad_fruktsamhetskvot), 
                                       0, poolad_fruktsamhetskvot),
      poolad_fruktsamhetskvot = pmin(poolad_fruktsamhetskvot, 0.5)
    )
  
  # Utjämna med automatisk spline (bevarar total fruktsamhet)
  poolad_data_spline <- poolad_data %>%
    group_by(Region) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_fruktsamhetskvot_spline = if(first(Region) == "Riket") {
        poolad_fruktsamhetskvot  # Riket utjämnas inte
      } else {
        spline_auto(Ålder, poolad_fruktsamhetskvot, bevara_summa = TRUE)
      }
    ) %>%
    ungroup()
  
  # Beräkna kvot mot riket för relativ fruktsamhet
  riket_referens <- poolad_data_spline %>%
    filter(Region == "Riket") %>%
    select(Ålder, riket_fruktsamhetskvot = poolad_fruktsamhetskvot)
  
  viktad_kvot <- poolad_data_spline %>%
    left_join(riket_referens, by = "Ålder") %>%
    mutate(
      tidsviktad_kvot = poolad_fruktsamhetskvot / riket_fruktsamhetskvot,
      tidsviktad_kvot = ifelse(is.infinite(tidsviktad_kvot) | is.nan(tidsviktad_kvot), 1, tidsviktad_kvot),
      tidsviktad_kvot = pmin(pmax(tidsviktad_kvot, 0.1), 3)  # Begränsa extremvärden
    ) %>%
    group_by(Region) %>%
    mutate(
      tidsviktad_kvot_spline = if(first(Region) == "Riket") {
        1.0  # Riket har kvot 1 per definition
      } else {
        spline_auto(Ålder, tidsviktad_kvot, bevara_summa = FALSE)
      }
    ) %>%
    ungroup()
  
  # Applicera relativa kvoter på SCB:s riksprognos för fruktsamhet
  fodelsetal_riksprognos <- riket_lista$fodelsetal %>%
    filter(Ålder >= 15, Ålder <= 49)
  
  fruktsamhet_prognos <- expand_grid(
    Region = unique(viktad_kvot$Region),
    År = unique(fodelsetal_riksprognos$År)
  ) %>%
    left_join(
      fodelsetal_riksprognos %>% select(År, Ålder, Riksvärde = Värde),
      by = "År",
      relationship = "many-to-many"
    ) %>%
    left_join(
      viktad_kvot %>% select(Region, Ålder, tidsviktad_kvot_spline),
      by = c("Region", "Ålder")
    ) %>%
    mutate(
      Värde = if_else(Region == "Riket", Riksvärde, Riksvärde * tidsviktad_kvot_spline),
      Kön = "kvinnor",
      Variabel = "Födelserisker"
    ) %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  fruktsamhet_prognos <- applicera_scenariojustering(fruktsamhet_prognos, "fodelserisker")
  
  return(fruktsamhet_prognos)
}

# ===========================================================
# 2. DÖDSRISKER
# ===========================================================

# Beräkna åldersspecifika dödsrisker baserat på historiska data
berakna_dodsrisker <- function() {
  
  params <- PARAMETRAR$dodsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar dödsrisker...")
  message(sprintf("  Använder %d års poolad estimering med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter (vanligtvis används jämn viktning för dödsrisker)
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Funktion för att gruppera åldrar i åldersgrupper
  gruppera_alder <- function(alder) {
    case_when(
      alder == 0 ~ "0",
      alder >= 1 & alder <= 4 ~ "1-4",
      alder >= 5 & alder <= 89 ~ paste0(5 * floor((alder - 5) / 5) + 5, "-", 5 * floor((alder - 5) / 5) + 9),
      alder >= 90 ~ "90+"
    )
  }
  
  # Funktion för att beräkna flerårsgenomsnitt
  beräkna_summor <- function(värde, width) {
    rollapply(värde, width = width, FUN = sum, fill = NA, align = 'right')
  }
  
  # Skapa mappning mellan åldersgrupper och enskilda åldrar
  aldersmappning <- tibble(
    Åldersgrupp = c("0", "1-4", paste0(seq(5, 85, 5), "-", seq(9, 89, 5)), "90+"),
    Ålder = list(0, 1:4, 5:9, 10:14, 15:19, 20:24, 25:29, 30:34, 35:39, 40:44, 
                 45:49, 50:54, 55:59, 60:64, 65:69, 70:74, 75:79, 80:84, 85:89, 90:100)
  ) %>% 
    unnest(Ålder)
  
  # Beräkna dödsrisker per åldersgrupp för robustare estimering
  dödsrisker_summor <- kommun_lista$doda %>% 
    mutate(Åldersgrupp = gruppera_alder(Ålder)) %>%
    group_by(Region, Kön, Åldersgrupp, År) %>% 
    summarise(Värde = sum(Värde), .groups = "drop") %>%
    group_by(Region, Kön, Åldersgrupp) %>% 
    arrange(År) %>%
    mutate(Flerårsgenomsnitt = beräkna_summor(Värde, antal_ar)) %>%
    ungroup() %>%
    filter(År == max(År))
  
  # Beräkna motsvarande folkmängd per åldersgrupp
  folkmangd_summor <- kommun_lista$totfolkmangd %>% 
    mutate(Åldersgrupp = gruppera_alder(Ålder)) %>%
    group_by(Region, Kön, Åldersgrupp, År) %>% 
    summarise(Värde = sum(Värde), .groups = "drop") %>%
    group_by(Region, Kön, Åldersgrupp) %>% 
    arrange(År) %>%
    mutate(Flerårsgenomsnitt = beräkna_summor(Värde, antal_ar)) %>%
    ungroup() %>%
    filter(År == max(År))
  
  # Beräkna dödsrisker som kvot mellan döda och befolkning
  dödsrisker_prognosklar <- dödsrisker_summor %>% 
    inner_join(
      folkmangd_summor %>% select(Region, Kön, Åldersgrupp, År, Folkmangd_Flerars = Flerårsgenomsnitt),
      by = c("Region", "Kön", "Åldersgrupp", "År")
    ) %>%
    mutate(Flerårsgenomsnitt = Flerårsgenomsnitt / Folkmangd_Flerars) %>%
    select(-Folkmangd_Flerars)
  
  # Identifiera åldersgrupper med för få dödsfall för tillförlitlig estimering
  kontroll_tabell <- dödsrisker_summor %>% 
    mutate(Använd_länsdata = Flerårsgenomsnitt <= 50)
  
  # Använd rikets dödsrisker för små kommuner/åldersgrupper
  riket_risker <- dödsrisker_prognosklar %>%
    filter(Region == "Riket") %>%
    select(Kön, Åldersgrupp, År, Riket_risk = Flerårsgenomsnitt)
  
  dödsrisker_prognosklar <- dödsrisker_prognosklar %>%
    left_join(riket_risker, by = c("Kön", "Åldersgrupp", "År")) %>%
    left_join(
      kontroll_tabell %>% select(Region, Kön, Åldersgrupp, År, Använd_länsdata),
      by = c("Region", "Kön", "Åldersgrupp", "År")
    ) %>%
    mutate(
      Flerårsgenomsnitt = if_else(
        Region == "Riket" | !Använd_länsdata, 
        Flerårsgenomsnitt, 
        Riket_risk
      )
    ) %>%
    select(-Riket_risk, -Använd_länsdata)
  
  # Beräkna relativa dödsrisker mot riket
  dödsrisker_riket <- dödsrisker_prognosklar %>%
    filter(Region == "Riket") %>%
    select(Kön, Åldersgrupp, Dodsrisk_riket = Flerårsgenomsnitt)
  
  dödsrisker_relativ <- dödsrisker_prognosklar %>%
    left_join(dödsrisker_riket, by = c("Kön", "Åldersgrupp")) %>%
    mutate(
      Kvot = case_when(
        Region == "Riket" ~ 1,
        is.na(Flerårsgenomsnitt / Dodsrisk_riket) ~ 1,
        TRUE ~ Flerårsgenomsnitt / Dodsrisk_riket
      ),
      Kvot = pmax(0.7, pmin(1.3, Kvot))  # Begränsa extremvärden
    ) %>%
    select(Region, Kön, Åldersgrupp, Kvot)
  
  # Expandera från åldersgrupper till enskilda åldrar (1-årsklasser)
  dödsrisker_relativ_ettarsklasser <- dödsrisker_relativ %>%
    left_join(aldersmappning, by = "Åldersgrupp", relationship = "many-to-many") %>%
    select(Region, Kön, Ålder, Kvot) %>%
    arrange(Region, Kön, Ålder)
  
  # Justera SCB:s riksprognos för dödstal
  dödskvoter_riksprognos_justerad <- riket_lista$dodstal %>%
    mutate(Ålder = pmin(Ålder, 100)) %>%  # Begränsa till max 100 år
    group_by(År, Kön, Ålder) %>%
    summarise(
      Värde = mean(Värde), 
      Variabel = first(Variabel), 
      .groups = "drop"
    )
  
  # Applicera kommunspecifika kvoter på riksprognosen
  kommun_prognos <- expand_grid(
    Region = unique(dödsrisker_relativ_ettarsklasser$Region),
    År = unique(dödskvoter_riksprognos_justerad$År)
  ) %>%
    left_join(
      dödskvoter_riksprognos_justerad,
      by = "År",
      relationship = "many-to-many"
    ) %>%
    left_join(
      dödsrisker_relativ_ettarsklasser,
      by = c("Region", "Kön", "Ålder")
    ) %>%
    mutate(
      Kvot = if_else(is.na(Kvot), 1, Kvot),
      Värde = if_else(Region == "Riket", Värde, Värde * Kvot),
      Variabel = "Dödsrisker"
    ) %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  kommun_prognos <- applicera_scenariojustering(kommun_prognos, "dodsrisker")
  
  return(kommun_prognos)
}

# ===========================================================
# 3. INFLYTTNINGSRISKER
# ===========================================================

# Beräkna sannolikheten att personer från övriga Sverige flyttar till kommunen
berakna_inflyttningsrisker <- function() {
  
  params <- PARAMETRAR$inflyttningsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar inrikes inflyttningsrisker...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Läs in nödvändig data
  inflyttade <- kommun_lista$inrikes_inflyttade %>% 
    filter(Region != "Riket")
  riket_medelfolkmangd_historisk <- kommun_lista$medelfolkmangd %>% 
    filter(Region == "Riket")
  riket_medelfolkmangd_prognos <- riket_lista$riket_prognosinvånare_grund
  
  # Definiera senaste åren för beräkning
  senaste_ar <- inflyttade %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa inflyttningsrisker som andel av riksbefolkningen
  inflyttningsrisker <- inflyttade %>%
    filter(År %in% senaste_ar) %>%
    inner_join(
      riket_medelfolkmangd_historisk %>%
        filter(År %in% senaste_ar) %>%
        select(År, Ålder, Kön, antal_riket = Värde),
      by = c("År", "Ålder", "Kön")
    ) %>%
    mutate(
      antal_inflyttade = Värde,
      inflyttningsrisk = antal_inflyttade / antal_riket,
      inflyttningsrisk = ifelse(is.infinite(inflyttningsrisk) | is.nan(inflyttningsrisk), 0, inflyttningsrisk),
      inflyttningsrisk = pmin(inflyttningsrisk, 0.5)  # Begränsa till rimliga värden
    ) %>%
    select(Region, År, Ålder, Kön, inflyttningsrisk, antal_inflyttade, antal_riket)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- senaste_ar
  
  # Poola data med viktning över flera år
  poolad_data <- inflyttningsrisker %>%
    filter(!is.na(antal_inflyttade), !is.na(antal_riket), antal_riket > 0) %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_antal_inflyttade = sum(antal_inflyttade * tidsvikter[År], na.rm = TRUE),
      viktad_antal_riket = sum(antal_riket * tidsvikter[År], na.rm = TRUE),
      poolad_inflyttningsrisk = viktad_antal_inflyttade / viktad_antal_riket,
      .groups = "drop"
    ) %>%
    mutate(
      poolad_inflyttningsrisk = ifelse(
        is.nan(poolad_inflyttningsrisk) | is.infinite(poolad_inflyttningsrisk), 
        0, 
        poolad_inflyttningsrisk
      ),
      poolad_inflyttningsrisk = pmin(poolad_inflyttningsrisk, 0.5)
    )
  
  # Utjämna med automatisk spline för mjukare åldersprofiler
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_inflyttningsrisk_spline = spline_auto(Ålder, poolad_inflyttningsrisk, bevara_summa = TRUE)
    ) %>%
    ungroup()
  
  # Skapa prognosrisktal för alla geografier och år
  alla_regioner <- unique(poolad_data_spline$Region)
  prognos_ar <- unique(riket_medelfolkmangd_prognos$År)
  
  inflyttningsrisker_prognos <- expand_grid(
    Region = alla_regioner,
    År = prognos_ar
  ) %>%
    left_join(
      riket_medelfolkmangd_prognos %>%
        select(År, Ålder, Kön, Riksbefolkning = Värde),
      by = "År",
      relationship = "many-to-many"
    ) %>%
    left_join(
      poolad_data_spline %>%
        select(Region, Ålder, Kön, poolad_inflyttningsrisk_spline),
      by = c("Region", "Ålder", "Kön")
    ) %>%
    mutate(
      Värde = poolad_inflyttningsrisk_spline,
      Variabel = "Inflyttningsrisker"
    ) %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  inflyttningsrisker_prognos <- applicera_scenariojustering(inflyttningsrisker_prognos, "inflyttningsrisker")
  
  return(inflyttningsrisker_prognos)
}

# ===========================================================
# 4. UTFLYTTNINGSRISKER
# ===========================================================

# Beräkna sannolikheten att befintlig befolkning flyttar till andra delar av Sverige
berakna_utflyttningsrisker <- function() {
  
  params <- PARAMETRAR$utflyttningsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar inrikes utflyttningsrisker...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Läs in nödvändig data
  utflyttade <- kommun_lista$inrikes_utflyttade
  medelfolkmangd <- kommun_lista$medelfolkmangd
  
  # Definiera senaste åren för beräkning
  senaste_ar <- utflyttade %>%
    mutate(År = as.numeric(as.character(År))) %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa utflyttningsrisker som andel av egen befolkning
  utflyttningsrisker <- utflyttade %>%
    mutate(År = as.numeric(as.character(År))) %>%
    filter(År %in% senaste_ar) %>%
    inner_join(
      medelfolkmangd %>%
        mutate(År = as.numeric(as.character(År))) %>%
        filter(År %in% senaste_ar) %>%
        select(Region, År, Ålder, Kön, antal_befolkning = Värde),
      by = c("Region", "År", "Ålder", "Kön")
    ) %>%
    mutate(
      antal_utflyttade = Värde,
      utflyttningsrisk = antal_utflyttade / antal_befolkning,
      utflyttningsrisk = replace_na(utflyttningsrisk, 0),
      utflyttningsrisk = pmin(utflyttningsrisk, 0.5)  # Begränsa till rimliga värden
    ) %>%
    select(Region, År, Ålder, Kön, utflyttningsrisk, antal_utflyttade, antal_befolkning)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- as.character(senaste_ar)
  
  # Poola data med viktning över flera år
  poolad_data <- utflyttningsrisker %>%
    filter(!is.na(antal_utflyttade), !is.na(antal_befolkning), antal_befolkning > 0) %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_antal_utflyttade = sum(antal_utflyttade * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_antal_befolkning = sum(antal_befolkning * tidsvikter[as.character(År)], na.rm = TRUE),
      poolad_utflyttningsrisk = viktad_antal_utflyttade / viktad_antal_befolkning,
      .groups = "drop"
    ) %>%
    mutate(
      poolad_utflyttningsrisk = ifelse(
        is.nan(poolad_utflyttningsrisk) | is.infinite(poolad_utflyttningsrisk), 
        0, 
        poolad_utflyttningsrisk
      ),
      poolad_utflyttningsrisk = pmin(poolad_utflyttningsrisk, 0.5)
    )
  
  # Utjämna med automatisk spline (riket utjämnas inte)
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_utflyttningsrisk_spline = if(first(Region) == "Riket") {
        poolad_utflyttningsrisk
      } else {
        spline_auto(Ålder, poolad_utflyttningsrisk, bevara_summa = FALSE)
      }
    ) %>%
    ungroup()
  
  # Skapa prognosrisktal för alla år
  prognos_ar <- unique(riket_lista$riket_prognosinvånare_grund$År)
  
  utflyttningsrisker_prognos <- poolad_data_spline %>%
    select(Region, Kön, Ålder, Värde = poolad_utflyttningsrisk_spline) %>%
    crossing(År = prognos_ar) %>%
    mutate(Variabel = "Utflyttningsrisker") %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  utflyttningsrisker_prognos <- applicera_scenariojustering(utflyttningsrisker_prognos, "utflyttningsrisker")
  
  return(utflyttningsrisker_prognos)
}

# ===========================================================
# 5. INVANDRINGSRISKER
# ===========================================================

# Beräkna kommunens andel av rikets totala invandring
berakna_invandringsrisker <- function() {
  
  params <- PARAMETRAR$invandringsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar invandringsrisker...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Läs in nödvändig data
  invandrade <- kommun_lista$invandring
  riket_invandrade <- kommun_lista$invandring %>% 
    filter(Region == "Riket")
  riket_invandring_prognos <- riket_lista$invandring_riket %>%
    mutate(Region = "Riket") %>%
    select(Region, År, Ålder, Kön, Värde)
  
  # Definiera senaste åren för beräkning
  senaste_ar <- invandrade %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa invandringsrisker som andel av rikets invandring
  invandringsrisker <- invandrade %>%
    filter(År %in% senaste_ar) %>%
    inner_join(
      riket_invandrade %>%
        filter(År %in% senaste_ar) %>%
        select(År, Ålder, Kön, antal_riket = Värde),
      by = c("År", "Ålder", "Kön")
    ) %>%
    mutate(
      antal_invandrade = Värde,
      invandringsrisk = antal_invandrade / antal_riket,
      invandringsrisk = ifelse(is.infinite(invandringsrisk) | is.nan(invandringsrisk), 0, invandringsrisk),
      invandringsrisk = pmin(invandringsrisk, 1.0)  # Kan vara upp till 100% av rikets invandring
    ) %>%
    select(Region, År, Ålder, Kön, invandringsrisk, antal_invandrade, antal_riket)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- senaste_ar
  
  # Poola data med viktning över flera år
  poolad_data <- invandringsrisker %>%
    filter(!is.na(antal_invandrade), !is.na(antal_riket), antal_riket > 0) %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_antal_invandrade = sum(antal_invandrade * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_antal_riket = sum(antal_riket * tidsvikter[as.character(År)], na.rm = TRUE),
      poolad_invandringsrisk = viktad_antal_invandrade / viktad_antal_riket,
      .groups = "drop"
    ) %>%
    mutate(
      poolad_invandringsrisk = ifelse(
        is.nan(poolad_invandringsrisk) | is.infinite(poolad_invandringsrisk), 
        0, 
        poolad_invandringsrisk
      ),
      poolad_invandringsrisk = pmin(poolad_invandringsrisk, 1.0)
    )
  
  # Utjämna med automatisk spline (riket utjämnas inte)
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_invandringsrisk_spline = if(first(Region) == "Riket") {
        poolad_invandringsrisk
      } else {
        spline_auto(Ålder, poolad_invandringsrisk, bevara_summa = FALSE)
      }
    ) %>%
    ungroup()
  
  # Skapa prognosrisktal kopplade till SCB:s riksprognos för invandring
  alla_regioner <- unique(poolad_data_spline$Region)
  prognos_ar <- unique(riket_invandring_prognos$År)
  
  invandringsrisker_prognos <- expand_grid(
    Region = alla_regioner,
    År = prognos_ar
  ) %>%
    left_join(
      riket_invandring_prognos %>%
        select(År, Ålder, Kön, invandring_riket = Värde),
      by = "År",
      relationship = "many-to-many"
    ) %>%
    left_join(
      poolad_data_spline %>%
        select(Region, Ålder, Kön, poolad_invandringsrisk_spline),
      by = c("Region", "Ålder", "Kön")
    ) %>%
    mutate(
      Värde = poolad_invandringsrisk_spline,
      Variabel = "Invandringsrisker"
    ) %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  invandringsrisker_prognos <- applicera_scenariojustering(invandringsrisker_prognos, "invandringsrisker")
  
  return(invandringsrisker_prognos)
}

# ===========================================================
# 6. UTVANDRINGSRISKER
# ===========================================================

# Beräkna sannolikheten att befolkningen utvandrar från kommunen
berakna_utvandringsrisker <- function() {
  
  params <- PARAMETRAR$utvandringsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar utvandringsrisker...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Läs in nödvändig data
  utvandrade <- kommun_lista$utvandring
  medelfolkmangd <- kommun_lista$medelfolkmangd
  
  # Definiera senaste åren för beräkning
  senaste_ar <- utvandrade %>%
    mutate(År = as.numeric(as.character(År))) %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa utvandringsrisker som andel av egen befolkning
  utvandringsrisker <- utvandrade %>%
    mutate(År = as.numeric(as.character(År))) %>%
    filter(År %in% senaste_ar) %>%
    inner_join(
      medelfolkmangd %>%
        mutate(År = as.numeric(as.character(År))) %>%
        filter(År %in% senaste_ar) %>%
        select(Region, År, Ålder, Kön, antal_befolkning = Värde),
      by = c("Region", "År", "Ålder", "Kön")
    ) %>%
    mutate(
      antal_utvandrade = Värde,
      utvandringsrisk = antal_utvandrade / antal_befolkning,
      utvandringsrisk = replace_na(utvandringsrisk, 0),
      utvandringsrisk = pmin(utvandringsrisk, 0.5)  # Begränsa till rimliga värden
    ) %>%
    select(Region, År, Ålder, Kön, utvandringsrisk, antal_utvandrade, antal_befolkning)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- as.character(senaste_ar)
  
  # Poola data med viktning över flera år
  poolad_data <- utvandringsrisker %>%
    filter(!is.na(antal_utvandrade), !is.na(antal_befolkning), antal_befolkning > 0) %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_antal_utvandrade = sum(antal_utvandrade * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_antal_befolkning = sum(antal_befolkning * tidsvikter[as.character(År)], na.rm = TRUE),
      poolad_utvandringsrisk = viktad_antal_utvandrade / viktad_antal_befolkning,
      .groups = "drop"
    ) %>%
    mutate(
      poolad_utvandringsrisk = ifelse(
        is.nan(poolad_utvandringsrisk) | is.infinite(poolad_utvandringsrisk), 
        0, 
        poolad_utvandringsrisk
      ),
      poolad_utvandringsrisk = pmin(poolad_utvandringsrisk, 0.5)
    )
  
  # Utjämna med automatisk spline (riket utjämnas inte)
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_utvandringsrisk_spline = if(first(Region) == "Riket") {
        poolad_utvandringsrisk
      } else {
        spline_auto(Ålder, poolad_utvandringsrisk, bevara_summa = FALSE)
      }
    ) %>%
    ungroup()
  
  # Skapa prognosrisktal för alla år
  prognos_ar <- unique(riket_lista$riket_prognosinvånare_grund$År)
  
  utvandringsrisker_prognos <- poolad_data_spline %>%
    select(Region, Kön, Ålder, Värde = poolad_utvandringsrisk_spline) %>%
    crossing(År = prognos_ar) %>%
    mutate(Variabel = "Utvandringsrisker") %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  # Applicera eventuella scenariojusteringar
  utvandringsrisker_prognos <- applicera_scenariojustering(utvandringsrisker_prognos, "utvandringsrisker")
  
  return(utvandringsrisker_prognos)
}

# ===========================================================
# 7. INFLYTTNINGSANDELAR LÄNSGRÄNS
# ===========================================================

# Beräkna andel av inflyttningarna som sker över länsgräns
berakna_inflyttningsandelar_lansgrans <- function() {
  
  # Använd samma metodinställningar som inflyttningsrisker
  params <- PARAMETRAR$inflyttningsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar inflyttningsandelar över länsgräns...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Hämta rådata om flyttningar inom och över länsgräns
  inflyttningar_raw <- kommun_lista$inflyttningar_lansgrans_raw
  
  # Definiera senaste åren för beräkning
  senaste_ar <- inflyttningar_raw %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa andelar per år
  inflyttningsandelar_raa <- inflyttningar_raw %>%
    filter(År %in% senaste_ar) %>%
    mutate(
      andel_ovriga_lan = ifelse(Total > 0, Ovriga_lan / Total, 0),
      andel_ovriga_lan = pmin(pmax(andel_ovriga_lan, 0), 1)  # Begränsa mellan 0 och 1
    ) %>%
    select(Region, År, Ålder, Kön, andel_ovriga_lan, Ovriga_lan, Total)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- senaste_ar
  
  # Poola data med viktning över flera år
  poolad_data <- inflyttningsandelar_raa %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_ovriga_lan = sum(Ovriga_lan * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_total = sum(Total * tidsvikter[as.character(År)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      poolad_andel = ifelse(viktad_total > 0, viktad_ovriga_lan / viktad_total, 0),
      poolad_andel = pmin(pmax(poolad_andel, 0), 1)
    )
  
  # Utjämna med automatisk spline för mjukare åldersprofiler
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_andel_spline = spline_auto(Ålder, poolad_andel, bevara_summa = FALSE)
    ) %>%
    ungroup()
  
  # Skapa prognosandelar för alla år
  prognos_ar <- unique(riket_lista$riket_prognosinvånare_grund$År)
  
  inflyttningsandelar_prognos <- poolad_data_spline %>%
    select(Region, Kön, Ålder, Värde = poolad_andel_spline) %>%
    crossing(År = prognos_ar) %>%
    mutate(Variabel = "Inflyttningsandel_lansgrans") %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  return(inflyttningsandelar_prognos)
}

# ===========================================================
# 8. UTFLYTTNINGSANDELAR LÄNSGRÄNS
# ===========================================================

# Beräkna andel av utflyttningarna som sker över länsgräns
berakna_utflyttningsandelar_lansgrans <- function() {
  
  # Använd samma metodinställningar som utflyttningsrisker
  params <- PARAMETRAR$utflyttningsrisker
  antal_ar <- params$antal_ar
  
  message("Beräknar utflyttningsandelar över länsgräns...")
  message(sprintf("  Använder %d års data med viktningsmetod %d", antal_ar, params$viktningstyp))
  
  # Hantera EWMA alpha-parameter
  alpha_varde <- 0.5
  if (params$viktningstyp == 3 && !is.null(params$alpha)) {
    alpha_varde <- params$alpha
    message(sprintf("  EWMA alpha = %.2f", alpha_varde))
  }
  
  # Hämta rådata om flyttningar inom och över länsgräns
  utflyttningar_raw <- kommun_lista$utflyttningar_lansgrans_raw
  
  # Definiera senaste åren för beräkning
  senaste_ar <- utflyttningar_raw %>%
    mutate(År = as.numeric(as.character(År))) %>%
    pull(År) %>%
    unique() %>%
    sort() %>%
    tail(antal_ar)
  
  # Beräkna råa andelar per år
  utflyttningsandelar_raa <- utflyttningar_raw %>%
    mutate(År = as.numeric(as.character(År))) %>%
    filter(År %in% senaste_ar) %>%
    mutate(
      andel_ovriga_lan = ifelse(Total > 0, Ovriga_lan / Total, 0),
      andel_ovriga_lan = pmin(pmax(andel_ovriga_lan, 0), 1)  # Begränsa mellan 0 och 1
    ) %>%
    select(Region, År, Ålder, Kön, andel_ovriga_lan, Ovriga_lan, Total)
  
  # Beräkna tidsvikter för poolad estimering
  tidsvikter <- berakna_tidsvikter(antal_ar, params$viktningstyp, alpha_varde)
  names(tidsvikter) <- as.character(senaste_ar)
  
  # Poola data med viktning över flera år
  poolad_data <- utflyttningsandelar_raa %>%
    group_by(Region, Kön, Ålder) %>%
    summarise(
      viktad_ovriga_lan = sum(Ovriga_lan * tidsvikter[as.character(År)], na.rm = TRUE),
      viktad_total = sum(Total * tidsvikter[as.character(År)], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      poolad_andel = ifelse(viktad_total > 0, viktad_ovriga_lan / viktad_total, 0),
      poolad_andel = pmin(pmax(poolad_andel, 0), 1)
    )
  
  # Utjämna med automatisk spline för mjukare åldersprofiler
  poolad_data_spline <- poolad_data %>%
    group_by(Region, Kön) %>%
    arrange(Ålder) %>%
    mutate(
      poolad_andel_spline = spline_auto(Ålder, poolad_andel, bevara_summa = FALSE)
    ) %>%
    ungroup()
  
  # Skapa prognosandelar för alla år
  prognos_ar <- unique(riket_lista$riket_prognosinvånare_grund$År)
  
  utflyttningsandelar_prognos <- poolad_data_spline %>%
    select(Region, Kön, Ålder, Värde = poolad_andel_spline) %>%
    crossing(År = prognos_ar) %>%
    mutate(Variabel = "Utflyttningsandel_lansgrans") %>%
    select(Region, Kön, Ålder, År, Variabel, Värde)
  
  return(utflyttningsandelar_prognos)
}

# ===========================================================
# KÖR ALLA BERÄKNINGAR
# ===========================================================

message("\n==== STARTAR RISKBERÄKNINGAR ====\n")

# Bestäm sparväg baserat på aktivt scenario
base_path <- "Data_riskmatt"
if (AKTIVT_SCENARIO == "alternativ") {
  save_path <- file.path(base_path, "scenario_alternativ")
} else {
  save_path <- base_path
}

# Visa översikt av viktningsmetoder och inställningar
visualisera_viktningsmetoder()

# Kör alla riskberäkningar
start_tid <- Sys.time()

# 1. Födelserisker - fruktsamhetskvoter för kvinnor 15-49 år
fodelserisker <- berakna_fodelserisker()
fodelserisker_slutlig <- fodelserisker %>%
  filter(Region != "Riket") %>%
  arrange(Region, År, Ålder)
saveRDS(fodelserisker_slutlig, file.path(save_path, "fodelserisker.rds"))
message(paste("✓ Födelserisker klara:", n_distinct(fodelserisker_slutlig$Region), "geografier"))

# 2. Dödsrisker - åldersspecifika dödsrisker för kvinnor och män
dodsrisker <- berakna_dodsrisker()
dodsrisker_slutlig <- dodsrisker %>%
  filter(Region != "Riket") %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(dodsrisker_slutlig, file.path(save_path, "dodsrisker.rds"))
message(paste("✓ Dödsrisker klara:", n_distinct(dodsrisker_slutlig$Region), "geografier"))

# 3. Inflyttningsrisker - sannolikhet att personer från övriga Sverige flyttar hit
inflyttningsrisker <- berakna_inflyttningsrisker()
inflyttningsrisker_slutlig <- inflyttningsrisker %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(inflyttningsrisker_slutlig, file.path(save_path, "inflyttningsrisker.rds"))
message(paste("✓ Inflyttningsrisker klara:", n_distinct(inflyttningsrisker_slutlig$Region), "geografier"))

# 4. Utflyttningsrisker - sannolikhet att befintlig befolkning flyttar bort
utflyttningsrisker <- berakna_utflyttningsrisker()
utflyttningsrisker_slutlig <- utflyttningsrisker %>%
  filter(Region != "Riket") %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(utflyttningsrisker_slutlig, file.path(save_path, "utflyttningsrisker.rds"))
message(paste("✓ Utflyttningsrisker klara:", n_distinct(utflyttningsrisker_slutlig$Region), "geografier"))

# 5. Invandringsrisker - kommunens andel av rikets totala invandring
invandringsrisker <- berakna_invandringsrisker()
invandringsrisker_slutlig <- invandringsrisker %>%
  filter(Region != "Riket") %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(invandringsrisker_slutlig, file.path(save_path, "invandringsrisker.rds"))
message(paste("✓ Invandringsrisker klara:", n_distinct(invandringsrisker_slutlig$Region), "geografier"))

# 6. Utvandringsrisker - sannolikhet att befolkningen utvandrar
utvandringsrisker <- berakna_utvandringsrisker()
utvandringsrisker_slutlig <- utvandringsrisker %>%
  filter(Region != "Riket") %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(utvandringsrisker_slutlig, file.path(save_path, "utvandringsrisker.rds"))
message(paste("✓ Utvandringsrisker klara:", n_distinct(utvandringsrisker_slutlig$Region), "geografier"))

# 7. Inflyttningsandelar länsgräns - andel inflyttningar som sker över länsgräns
inflyttningsandelar_lansgrans <- berakna_inflyttningsandelar_lansgrans()
inflyttningsandelar_lansgrans_slutlig <- inflyttningsandelar_lansgrans %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(inflyttningsandelar_lansgrans_slutlig, file.path(save_path, "inflyttningsandelar_lansgrans.rds"))
message(paste("✓ Inflyttningsandelar länsgräns klara:", n_distinct(inflyttningsandelar_lansgrans_slutlig$Region), "geografier"))

# 8. Utflyttningsandelar länsgräns - andel utflyttningar som sker över länsgräns
utflyttningsandelar_lansgrans <- berakna_utflyttningsandelar_lansgrans()
utflyttningsandelar_lansgrans_slutlig <- utflyttningsandelar_lansgrans %>%
  arrange(Region, År, Kön, Ålder)
saveRDS(utflyttningsandelar_lansgrans_slutlig, file.path(save_path, "utflyttningsandelar_lansgrans.rds"))
message(paste("✓ Utflyttningsandelar länsgräns klara:", n_distinct(utflyttningsandelar_lansgrans_slutlig$Region), "geografier"))

# Beräkna total körningstid
slut_tid <- Sys.time()
tid_åtgång <- difftime(slut_tid, start_tid, units = "mins")

# Spara alternativjusteringar för visualiseringsappen om de används
if (AKTIVT_SCENARIO == "alternativ" && exists("ALTERNATIV_FRAMSKRIVNING")) {
  saveRDS(ALTERNATIV_FRAMSKRIVNING, "Data_underlag/senaste_justeringar.rds")
  message("  ✅ Alternativjusteringar sparade för visualiseringsapp")
}

# Slutrapport
message("\n==== RISKBERÄKNINGAR KLARA! ====")
message(sprintf("Total körningstid: %.1f minuter", tid_åtgång))
message(sprintf("Antal geografier: %d", n_distinct(dodsrisker_slutlig$Region)))
message(sprintf("Aktivt scenario: %s", AKTIVT_SCENARIO))
message(sprintf("Resultat sparade i: %s/", save_path))