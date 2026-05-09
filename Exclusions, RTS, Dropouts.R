library(dplyr)

# =========================================================
# 1) Ausgangsdatensatz prüfen
# =========================================================

nrow(sir)   # ursprünglicher Datensatz


# =========================================================
# 2) Exclusions entfernen
# =========================================================

sir_clean <- sir %>%
  filter(
    (is.na(consent) | consent != "2"),
    (is.na(exclusionyn___1) | exclusionyn___1 != "1")
  )

nrow(sir)        # vorher
nrow(sir_clean)  # nach Exclusions


# Quality Check: sollte 0 Zeilen ergeben
sir_clean %>%
  filter(consent == "2" | exclusionyn___1 == "1")


# =========================================================
# 3) RTS-Exclusions dokumentieren inkl. Missing RTS data
# =========================================================

rts_excluded <- sir_clean %>%
  mutate(
    sport = as.numeric(sport),
    sportrts = as.numeric(sportrts),
    sportmin = as.numeric(sportmin),
    
    rts_exclusion = case_when(
      is.na(sport) | is.na(sportrts) | is.na(sportmin) ~ "No wish to return to sport",
      sport == 2 ~ "No sports participation",
      sport == 1 & sportrts == 2 ~ "No return to sport",
      sport == 1 & sportrts == 1 & sportmin < 90 ~ "<90% return to sport level",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rts_exclusion)) %>%
  select(regid, sport, sportrts, sportmin, rts_exclusion) %>%
  arrange(rts_exclusion, regid)

rts_excluded

# Häufigkeiten der RTS-Exclusions
rts_excluded %>%
  count(rts_exclusion, sort = TRUE)


# =========================================================
# 4) RTS-Population erstellen
# =========================================================

RTS <- sir_clean %>%
  mutate(
    sport = as.numeric(sport),
    sportrts = as.numeric(sportrts),
    sportmin = as.numeric(sportmin)
  ) %>%
  filter(
    sport == 1,
    sportrts == 1,
    sportmin >= 90
  )

nrow(RTS)   # RTS-Population vor Dropout-Entfernung


# =========================================================
# 5) Dropouts innerhalb der RTS-Population identifizieren
# =========================================================

dropouts_RTS <- RTS %>%
  mutate(
    dropoutyn___1 = as.character(dropoutyn___1),
    dropoutreas = as.character(dropoutreas),
    dropoutdesc = as.character(dropoutdesc),
    
    dropout_label = case_when(
      dropoutyn___1 == "1" & dropoutreas == "1" ~ "Death",
      dropoutyn___1 == "1" & dropoutreas == "2" ~ "Patient withdrew",
      dropoutyn___1 == "1" & dropoutreas == "3" ~ "Illness",
      dropoutyn___1 == "1" & dropoutreas == "4" ~ "Abroad",
      dropoutyn___1 == "1" & dropoutreas == "5" ~ "Not reachable",
      dropoutyn___1 == "1" & dropoutreas == "6" ~ "Re-operation / revision",
      dropoutyn___1 == "1" & dropoutreas == "7" ~ "Switch to surgery",
      dropoutyn___1 == "1" & dropoutreas == "9" ~ paste0("Other: ", dropoutdesc),
      dropoutyn___1 == "1" ~ "Dropout reason missing/unknown",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(dropoutyn___1 == "1") %>%
  select(regid, dropoutreas, dropout_label, dropoutdesc) %>%
  arrange(dropout_label, regid)

dropouts_RTS

# Häufigkeiten der Dropout-Gründe
dropouts_RTS %>%
  count(dropout_label, sort = TRUE)

# Patient-IDs pro Dropout-Grund
dropouts_RTS %>%
  group_by(dropout_label) %>%
  summarise(
    n = n(),
    regid_list = paste(regid, collapse = ", "),
    .groups = "drop"
  )


# =========================================================
# 6) Dropouts entfernen = finale Analysepopulation
# =========================================================

RTS_final <- RTS %>%
  mutate(
    dropoutyn___1 = as.character(dropoutyn___1)
  ) %>%
  filter(
    is.na(dropoutyn___1) | dropoutyn___1 != "1"
  )

nrow(RTS)        # RTS-Population vor Dropout-Entfernung
nrow(RTS_final)  # finale Analysepopulation


# Quality Check: sollte 0 ergeben
sum(RTS_final$dropoutyn___1 == "1", na.rm = TRUE)


# =========================================================
# 7) Flow-Zusammenfassung
# =========================================================

flow_summary <- tibble(
  Step = c(
    "Original dataset",
    "After exclusions",
    "RTS population before dropout removal",
    "Final RTS analysis population"
  ),
  N = c(
    nrow(sir),
    nrow(sir_clean),
    nrow(RTS),
    nrow(RTS_final)
  )
)

flow_summary

