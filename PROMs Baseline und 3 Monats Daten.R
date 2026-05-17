# =========================================================
# CODE: Create_ALL_Scores_Baseline_and_3m_RTS_final
# Zweck: Berechnet BRS, SIRSI und WOSI für Baseline und 3 Monate
#        aus RTS_final inkl. Missing-Data-Checks und Boxplots
# =========================================================

library(dplyr)
library(ggplot2)

# ----------------------------
# Einstellungen
# ----------------------------
min_items_BRS   <- 4
min_items_SIRSI <- 10
min_items_WOSI  <- 17

# ----------------------------
# Item-Listen
# ----------------------------
brs_baseline_items <- c("brs10", "brs20", "brs30", "brs40", "brs50", "brs60")
brs_3m_items       <- c("brs13", "brs23", "brs33", "brs43", "brs53", "brs63")

sirsi_base_items <- paste0("sirsi", 1:12, "_0")
sirsi_3m_items   <- paste0("sirsi", 1:12, "_3")

wosi_base_items <- paste0("wosi", 1:21, "_0")
wosi_3m_items   <- paste0("wosi", 1:21, "_3")

expected_vars <- c(
  brs_baseline_items, brs_3m_items,
  sirsi_base_items, sirsi_3m_items,
  wosi_base_items, wosi_3m_items,
  "regid"
)

# ----------------------------
# Fehlende Variablen prüfen
# ----------------------------
missing_items <- setdiff(expected_vars, names(RTS_final))

if(length(missing_items) > 0){
  warning(
    "Fehlende Variablen im Datensatz RTS_final:\n",
    paste(missing_items, collapse = ", ")
  )
}

# ----------------------------
# Nur vorhandene Items verwenden
# ----------------------------
avail_brs_base   <- intersect(brs_baseline_items, names(RTS_final))
avail_brs_3m     <- intersect(brs_3m_items, names(RTS_final))
avail_sirsi_base <- intersect(sirsi_base_items, names(RTS_final))
avail_sirsi_3m   <- intersect(sirsi_3m_items, names(RTS_final))
avail_wosi_base  <- intersect(wosi_base_items, names(RTS_final))
avail_wosi_3m    <- intersect(wosi_3m_items, names(RTS_final))

# ----------------------------
# Scores berechnen
# ----------------------------
RTS_final <- RTS_final %>%
  mutate(
    n_brs_base   = rowSums(!is.na(across(all_of(avail_brs_base)))),
    n_brs_3m     = rowSums(!is.na(across(all_of(avail_brs_3m)))),
    n_sirsi_base = rowSums(!is.na(across(all_of(avail_sirsi_base)))),
    n_sirsi_3m   = rowSums(!is.na(across(all_of(avail_sirsi_3m)))),
    n_wosi_base  = rowSums(!is.na(across(all_of(avail_wosi_base)))),
    n_wosi_3m    = rowSums(!is.na(across(all_of(avail_wosi_3m))))
  ) %>%
  mutate(
    BRS_baseline_excluded_missing   = n_brs_base < min_items_BRS,
    BRS_3m_excluded_missing         = n_brs_3m < min_items_BRS,
    SIRSI_baseline_excluded_missing = n_sirsi_base < min_items_SIRSI,
    SIRSI_3m_excluded_missing       = n_sirsi_3m < min_items_SIRSI,
    WOSI_baseline_excluded_missing  = n_wosi_base < min_items_WOSI,
    WOSI_3m_excluded_missing        = n_wosi_3m < min_items_WOSI
  ) %>%
  mutate(
    BRS_baseline = if_else(
      !BRS_baseline_excluded_missing,
      rowMeans(across(all_of(avail_brs_base)), na.rm = TRUE),
      NA_real_
    ),
    
    BRS_3m = if_else(
      !BRS_3m_excluded_missing,
      rowMeans(across(all_of(avail_brs_3m)), na.rm = TRUE),
      NA_real_
    ),
    
    delta_BRS = if_else(
      !is.na(BRS_baseline) & !is.na(BRS_3m),
      BRS_3m - BRS_baseline,
      NA_real_
    ),
    
    SIRSI_baseline = if_else(
      !SIRSI_baseline_excluded_missing,
      (rowSums(across(all_of(avail_sirsi_base)), na.rm = TRUE) / 120) * 100,
      NA_real_
    ),
    
    SIRSI_3m = if_else(
      !SIRSI_3m_excluded_missing,
      (rowSums(across(all_of(avail_sirsi_3m)), na.rm = TRUE) / 120) * 100,
      NA_real_
    ),
    
    WOSI_baseline = if_else(
      !WOSI_baseline_excluded_missing,
      rowSums(across(all_of(avail_wosi_base)), na.rm = TRUE),
      NA_real_
    ),
    
    WOSI_3m = if_else(
      !WOSI_3m_excluded_missing,
      rowSums(across(all_of(avail_wosi_3m)), na.rm = TRUE),
      NA_real_
    )
  )

# ----------------------------
# Übersicht: verfügbare Scores
# ----------------------------
summary_df <- data.frame(
  N_total = nrow(RTS_final),
  BRS_baseline_available = sum(!is.na(RTS_final$BRS_baseline)),
  BRS_3m_available = sum(!is.na(RTS_final$BRS_3m)),
  delta_BRS_available = sum(!is.na(RTS_final$delta_BRS)),
  SIRSI_baseline_available = sum(!is.na(RTS_final$SIRSI_baseline)),
  SIRSI_3m_available = sum(!is.na(RTS_final$SIRSI_3m)),
  WOSI_baseline_available = sum(!is.na(RTS_final$WOSI_baseline)),
  WOSI_3m_available = sum(!is.na(RTS_final$WOSI_3m))
)

print(summary_df)

# ----------------------------
# Übersicht: wegen Missing Items nicht berechnete Scores
# ----------------------------
missing_score_summary <- RTS_final %>%
  summarise(
    BRS_baseline_missing   = sum(BRS_baseline_excluded_missing),
    BRS_3m_missing         = sum(BRS_3m_excluded_missing),
    SIRSI_baseline_missing = sum(SIRSI_baseline_excluded_missing),
    SIRSI_3m_missing       = sum(SIRSI_3m_excluded_missing),
    WOSI_baseline_missing  = sum(WOSI_baseline_excluded_missing),
    WOSI_3m_missing        = sum(WOSI_3m_excluded_missing)
  )

print(missing_score_summary)

# ----------------------------
# Betroffene Patienten anzeigen
# ----------------------------
missing_score_cases <- RTS_final %>%
  filter(
    BRS_baseline_excluded_missing |
      BRS_3m_excluded_missing |
      SIRSI_baseline_excluded_missing |
      SIRSI_3m_excluded_missing |
      WOSI_baseline_excluded_missing |
      WOSI_3m_excluded_missing
  ) %>%
  select(
    regid,
    n_brs_base,
    n_brs_3m,
    n_sirsi_base,
    n_sirsi_3m,
    n_wosi_base,
    n_wosi_3m,
    BRS_baseline_excluded_missing,
    BRS_3m_excluded_missing,
    SIRSI_baseline_excluded_missing,
    SIRSI_3m_excluded_missing,
    WOSI_baseline_excluded_missing,
    WOSI_3m_excluded_missing
  ) %>%
  arrange(regid)

missing_score_cases

# =========================================================
# Boxplots Baseline
# =========================================================

ggplot(RTS_final %>% filter(!is.na(BRS_baseline)), aes(y = BRS_baseline)) +
  geom_boxplot() +
  labs(
    title = "BRS Baseline",
    y = "BRS baseline score"
  ) +
  theme_minimal()

ggplot(RTS_final %>% filter(!is.na(SIRSI_baseline)), aes(y = SIRSI_baseline)) +
  geom_boxplot() +
  labs(
    title = "SIRSI Baseline",
    y = "SIRSI baseline score (0–100)"
  ) +
  theme_minimal()

ggplot(RTS_final %>% filter(!is.na(WOSI_baseline)), aes(y = WOSI_baseline)) +
  geom_boxplot() +
  labs(
    title = "WOSI Baseline",
    y = "WOSI baseline score"
  ) +
  theme_minimal()


library(tidyr)

# =========================================================
# BRS Long Format erstellen
# =========================================================

brs_long <- RTS_final %>%
  select(regid, BRS_baseline, BRS_3m) %>%
  pivot_longer(
    cols = c(BRS_baseline, BRS_3m),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  mutate(
    timepoint = dplyr::recode(
      timepoint,
      "BRS_baseline" = "Baseline",
      "BRS_3m" = "3 Months"
    )
  )

brs_long <- RTS_final %>%
  select(regid, BRS_baseline, BRS_3m) %>%
  pivot_longer(
    cols = c(BRS_baseline, BRS_3m),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  mutate(
    timepoint = dplyr::recode(
      timepoint,
      "BRS_baseline" = "Baseline",
      "BRS_3m" = "3 Months"
    ),
    timepoint = factor(
      timepoint,
      levels = c("Baseline", "3 Months")
    )
  )


ggplot(brs_long, aes(x = timepoint, y = score)) +
  geom_boxplot(width = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 2) +
  labs(
    title = "BRS Scores at Baseline and 3 Months",
    x = "",
    y = "BRS Score"
  ) +
  theme_classic(base_size = 14)

ggplot(brs_long, aes(x = timepoint, y = score, group = record_id)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.5, size = 1.8) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.5
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 3
  ) +
  labs(
    title = "BRS Scores Over Time",
    x = "",
    y = "BRS Score"
  ) +
  theme_classic(base_size = 14)


# =========================================================
# Deskriptive Werte für Baseline und 3 Monate
# =========================================================

descriptive_outcomes <- RTS_final %>%
  summarise(
    BRS_baseline_mean = mean(BRS_baseline, na.rm = TRUE),
    BRS_baseline_sd   = sd(BRS_baseline, na.rm = TRUE),
    BRS_baseline_min  = min(BRS_baseline, na.rm = TRUE),
    BRS_baseline_max  = max(BRS_baseline, na.rm = TRUE),
    BRS_baseline_n    = sum(!is.na(BRS_baseline)),
    
    BRS_3m_mean = mean(BRS_3m, na.rm = TRUE),
    BRS_3m_sd   = sd(BRS_3m, na.rm = TRUE),
    BRS_3m_min  = min(BRS_3m, na.rm = TRUE),
    BRS_3m_max  = max(BRS_3m, na.rm = TRUE),
    BRS_3m_n    = sum(!is.na(BRS_3m)),
    
    SIRSI_baseline_mean = mean(SIRSI_baseline, na.rm = TRUE),
    SIRSI_baseline_sd   = sd(SIRSI_baseline, na.rm = TRUE),
    SIRSI_baseline_min  = min(SIRSI_baseline, na.rm = TRUE),
    SIRSI_baseline_max  = max(SIRSI_baseline, na.rm = TRUE),
    SIRSI_baseline_n    = sum(!is.na(SIRSI_baseline)),
    
    SIRSI_3m_mean = mean(SIRSI_3m, na.rm = TRUE),
    SIRSI_3m_sd   = sd(SIRSI_3m, na.rm = TRUE),
    SIRSI_3m_min  = min(SIRSI_3m, na.rm = TRUE),
    SIRSI_3m_max  = max(SIRSI_3m, na.rm = TRUE),
    SIRSI_3m_n    = sum(!is.na(SIRSI_3m)),
    
    WOSI_baseline_mean = mean(WOSI_baseline, na.rm = TRUE),
    WOSI_baseline_sd   = sd(WOSI_baseline, na.rm = TRUE),
    WOSI_baseline_min  = min(WOSI_baseline, na.rm = TRUE),
    WOSI_baseline_max  = max(WOSI_baseline, na.rm = TRUE),
    WOSI_baseline_n    = sum(!is.na(WOSI_baseline)),
    
    WOSI_3m_mean = mean(WOSI_3m, na.rm = TRUE),
    WOSI_3m_sd   = sd(WOSI_3m, na.rm = TRUE),
    WOSI_3m_min  = min(WOSI_3m, na.rm = TRUE),
    WOSI_3m_max  = max(WOSI_3m, na.rm = TRUE),
    WOSI_3m_n    = sum(!is.na(WOSI_3m))
  )

print(descriptive_outcomes)


print(descriptive_outcomes, width = Inf)


# =========================================================
# AJSM-style baseline and delta plots
# =========================================================

library(ggplot2)
library(dplyr)

theme_ajsm <- theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# BRS baseline
p_brs_baseline <- ggplot(
  RTS_final %>% filter(!is.na(BRS_baseline)),
  aes(x = "", y = BRS_baseline)
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    color = "black",
    outlier.shape = 16,
    outlier.size = 1.8
  ) +
  labs(
    title = "BRS Baseline",
    x = NULL,
    y = "BRS score"
  ) +
  theme_ajsm

p_brs_baseline


# SIRSI baseline
p_sirsi_baseline <- ggplot(
  RTS_final %>% filter(!is.na(SIRSI_baseline)),
  aes(x = "", y = SIRSI_baseline)
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    color = "black",
    outlier.shape = 16,
    outlier.size = 1.8
  ) +
  labs(
    title = "SIRSI Baseline",
    x = NULL,
    y = "SIRSI score (%)"
  ) +
  theme_ajsm

p_sirsi_baseline


# WOSI baseline
p_wosi_baseline <- ggplot(
  RTS_final %>% filter(!is.na(WOSI_baseline)),
  aes(x = "", y = WOSI_baseline)
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    color = "black",
    outlier.shape = 16,
    outlier.size = 1.8
  ) +
  labs(
    title = "WOSI Baseline",
    x = NULL,
    y = "WOSI score"
  ) +
  theme_ajsm

p_wosi_baseline


# delta BRS
p_delta_brs <- ggplot(
  RTS_final %>% filter(!is.na(delta_BRS)),
  aes(x = "", y = delta_BRS)
) +
  geom_boxplot(
    width = 0.35,
    fill = "white",
    color = "black",
    outlier.shape = 16,
    outlier.size = 1.8
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "ΔBRS",
    x = NULL,
    y = expression(Delta*"BRS score")
  ) +
  theme_ajsm

p_delta_brs


ggsave("Figure_BRS_baseline.tiff", p_brs_baseline, width = 4, height = 4, dpi = 300)
ggsave("Figure_SIRSI_baseline.tiff", p_sirsi_baseline, width = 4, height = 4, dpi = 300)
ggsave("Figure_WOSI_baseline.tiff", p_wosi_baseline, width = 4, height = 4, dpi = 300)
ggsave("Figure_delta_BRS.tiff", p_delta_brs, width = 4, height = 4, dpi = 300)






# =========================================================
# SIRSI Plot
# =========================================================

sirsi_long <- RTS_final %>%
  select(regid, SIRSI_baseline, SIRSI_3m) %>%
  pivot_longer(
    cols = c(SIRSI_baseline, SIRSI_3m),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  mutate(
    timepoint = recode(
      timepoint,
      "SIRSI_baseline" = "Baseline",
      "SIRSI_3m" = "3 Months"
    ),
    timepoint = factor(timepoint, levels = c("Baseline", "3 Months"))
  )

ggplot(sirsi_long, aes(x = timepoint, y = score)) +
  geom_boxplot(width = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 2) +
  labs(
    title = "SIRSI Scores at Baseline and 3 Months",
    x = "",
    y = "SIRSI Score"
  ) +
  theme_classic(base_size = 14)



# =========================================================
# WOSI Plot
# =========================================================

wosi_long <- RTS_final %>%
  select(regid, WOSI_baseline, WOSI_3m) %>%
  pivot_longer(
    cols = c(WOSI_baseline, WOSI_3m),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  mutate(
    timepoint = recode(
      timepoint,
      "WOSI_baseline" = "Baseline",
      "WOSI_3m" = "3 Months"
    ),
    timepoint = factor(timepoint, levels = c("Baseline", "3 Months"))
  )

ggplot(wosi_long, aes(x = timepoint, y = score)) +
  geom_boxplot(width = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.08, alpha = 0.6, size = 2) +
  labs(
    title = "WOSI Scores at Baseline and 3 Months",
    x = "",
    y = "WOSI Score"
  ) +
  theme_classic(base_size = 14)


# =========================================================
# Spaghetti Plot BRS
# =========================================================

library(tidyr)
library(dplyr)
library(ggplot2)

# Long-Format
brs_long <- RTS_final %>%
  select(regid, BRS_baseline, BRS_3m) %>%
  pivot_longer(
    cols = c(BRS_baseline, BRS_3m),
    names_to = "time",
    values_to = "BRS"
  ) %>%
  mutate(
    time = factor(
      time,
      levels = c("BRS_baseline", "BRS_3m"),
      labels = c("Baseline", "3 Months")
    )
  )

# Plot
ggplot(brs_long, aes(x = time, y = BRS, group = regid)) +
  
  # individuelle Verläufe (dezent!)
  geom_line(color = "grey70", size = 0.5, alpha = 0.6) +
  
  # Punkte
  geom_point(color = "black", size = 1.5) +
  
  # Mittelwertlinie (wichtig!)
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    color = "black",
    size = 1.2
  ) +
  
  # Mittelwertpunkte
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    color = "black",
    size = 3
  ) +
  
  labs(
    title = "BRS from Baseline to 3 Months",
    x = NULL,
    y = "BRS score"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

geom_line(color = "grey80", size = 0.4)

ggsave("Figure_BRS_spaghetti.tiff", width = 5, height = 4, dpi = 300)



library(dplyr)
library(tidyr)
library(ggplot2)

brs_long <- RTS_final %>%
  select(regid, BRS_baseline, BRS_3m) %>%
  pivot_longer(
    cols = c(BRS_baseline, BRS_3m),
    names_to = "timepoint",
    values_to = "score"
  ) %>%
  mutate(
    timepoint = recode(
      timepoint,
      "BRS_baseline" = "Baseline",
      "BRS_3m" = "3 Months"
    ),
    timepoint = factor(timepoint, levels = c("Baseline", "3 Months"))
  )

ggplot(brs_long, aes(x = timepoint, y = score, group = regid)) +
  geom_line(alpha = 0.25, linewidth = 0.5) +
  geom_point(size = 2.2) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.3
  ) +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 3.2
  ) +
  labs(
    title = "BRS Scores at Baseline and 3 Months",
    x = "",
    y = "BRS Score"
  ) +
  theme_classic(base_size = 14) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "plain"),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

