# Här är tanken att vi sparar bra att ha-funktioner kopplat till Robin Rikardssons befolkningsprognosskript
#
# Fyll gärna på 


befolkningsprognos_lista_till_dataframe <- function(
    inlaslista,
    returnera_wide_dataset = TRUE,
    koppla_pa_regionkod = TRUE
  ) {
  
  # Denna funktion tar emot en lista som blir resultatet av en befolkningsprognos-körning sätter ihop listan till en 
  # dataframe där komponenterna ligger i wide-format om returnera_wide_dataset är satt till TRUE, och kopplar på 
  # regionkoder om koppla_pa_regionkod är satt till TRUE (regionkoder saknas i resultatlistan som genereras av skriptet idag)
  
  library(tidyverse)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  
  progn_list <- inlaslista
  
  
  if (any(names(progn_list) %in% c("totalbefolkning", "komponenter"))) {
    progn_list <- progn_list %>% 
      keep(names(.) %in% c("totalbefolkning", "komponenter"))  
    
    total_df <- progn_list$totalbefolkning
    
    komponent_df <- map_dfr(progn_list$komponenter, function(arslista) {
      list_of_dfs <- compact(arslista)  # om vissa komponenter är NULL
      bind_rows(list_of_dfs)
    })
    retur_df <- bind_rows(total_df, komponent_df) %>% 
      rename(variabel = Variabel,
             varde = Värde,
             region = Region)
      
    if (returnera_wide_dataset) {
      retur_df <- retur_df %>% 
        pivot_wider(
          names_from = "variabel",
          values_from = "varde"
        ) %>%
        rename_with(~ str_replace_all(., " ", "_") %>% svenska_tecken_byt_ut() %>% tolower())
    }
  
  } else { # slut test om det är en enskild prognos
  
    alla_df <- imap_dfr(progn_list, function(kommun_data, kommun_namn) {
      total_df <- kommun_data$totalbefolkning
      
      komponent_df <- map_dfr(kommun_data$komponenter, function(arslista) {
        list_of_dfs <- compact(arslista)  # om vissa komponenter är NULL
        bind_rows(list_of_dfs)
      })
      
      retur_df <- bind_rows(total_df, komponent_df) %>%
        rename(variabel = Variabel,
               varde = Värde,
               region = Region) %>% 
        select(År, region, Kön, Ålder, variabel, varde)
    }) 
    
    if (returnera_wide_dataset) {
      retur_df <- alla_df %>%  
      pivot_wider(
        names_from = "variabel",
        values_from = "varde"
      ) %>%
      rename_with(~ str_replace_all(., " ", "_") %>% svenska_tecken_byt_ut() %>% tolower())
    }
  } # slut test om det är en regional prognos
     
  # koppla på regionkoder om koppla_pa_regionkod är TRUE
  if (koppla_pa_regionkod) {
    regionnyckel <- hamtaregtab()
  
    retur_df <- retur_df %>% 
      left_join(regionnyckel, by = "region") %>%
      relocate(regionkod, .before = 1)
  }
  return(retur_df)
} # slut funktion

