library(dplyr)
library(flextable)
library(officer)

# =========================================================
# 1) Patient characteristics Datensatz
# =========================================================
RTS_pc <- RTS_final[, c(
  "age",
  "gender",
  "ansmoking",
  "alcohol",
  "bmi",
  "sportmin",
  "dominantside",
  "educ",
  "employstat",
  "fam",
  "instadauer",
  "sganrezidiv",
  "schulterbelastung"
)]

N <- nrow(RTS_pc)

# =========================================================
# 2) Labels für kategoriale Variablen
# =========================================================
RTS_labeled <- RTS_pc %>%
  mutate(
    gender_lab = case_when(
      !is.na(gender) & gender == 1 ~ "Male",
      !is.na(gender) & gender == 0 ~ "Female",
      TRUE ~ as.character(gender)
    ),
    ansmoking_lab = case_when(
      !is.na(ansmoking) & ansmoking == 1 ~ "Yes",
      !is.na(ansmoking) & ansmoking == 0 ~ "No",
      TRUE ~ as.character(ansmoking)
    ),
    dominantside_lab = case_when(
      !is.na(dominantside) & dominantside == 1 ~ "Right",
      !is.na(dominantside) & dominantside == 2 ~ "Left",
      !is.na(dominantside) & dominantside == 3 ~ "Ambidextrous",
      TRUE ~ as.character(dominantside)
    ),
    educ_lab = case_when(
      !is.na(educ) & educ == 1 ~ "Compulsory school",
      !is.na(educ) & educ == 2 ~ "Secondary level II",
      !is.na(educ) & educ == 3 ~ "Tertiary level",
      TRUE ~ as.character(educ)
    ),
    employstat_lab = case_when(
      !is.na(employstat) & employstat == 1 ~ "Employed full-time",
      !is.na(employstat) & employstat == 2 ~ "Employed part-time",
      !is.na(employstat) & employstat == 3 ~ "Homemaker",
      !is.na(employstat) & employstat == 4 ~ "Retired",
      !is.na(employstat) & employstat == 5 ~ "Student",
      !is.na(employstat) & employstat == 6 ~ "Unemployed",
      TRUE ~ as.character(employstat)
    ),
    fam_lab = case_when(
      !is.na(fam) & fam == 1 ~ "Married",
      !is.na(fam) & fam == 2 ~ "Single",
      !is.na(fam) & fam == 3 ~ "Divorced / separated",
      !is.na(fam) & fam == 4 ~ "Living with partner",
      !is.na(fam) & fam == 5 ~ "Widowed",
      TRUE ~ as.character(fam)
    ),
    instadauer_lab = case_when(
      !is.na(instadauer) & instadauer == 0 ~ "< 6 weeks",
      !is.na(instadauer) & instadauer == 1 ~ "< 3 months",
      !is.na(instadauer) & instadauer == 2 ~ "< 6 months",
      !is.na(instadauer) & instadauer == 3 ~ "< 1 year",
      !is.na(instadauer) & instadauer == 4 ~ "< 2 years",
      !is.na(instadauer) & instadauer == 5 ~ "< 5 years",
      !is.na(instadauer) & instadauer == 6 ~ "≥ 5 years",
      TRUE ~ as.character(instadauer)
    ),
    schulterbelastung_lab = case_when(
      !is.na(schulterbelastung) & schulterbelastung == 0 ~ "Regular contact sport with overhead activity",
      !is.na(schulterbelastung) & schulterbelastung == 1 ~ "Regular contact sport without overhead activity",
      !is.na(schulterbelastung) & schulterbelastung == 2 ~ "Regular sport with overhead activity",
      !is.na(schulterbelastung) & schulterbelastung == 3 ~ "Regular sport without overhead activity",
      !is.na(schulterbelastung) & schulterbelastung == 5 ~ "Regular sport with traction components",
      TRUE ~ as.character(schulterbelastung)
    ),
    sganrezidiv_lab = case_when(
      !is.na(sganrezidiv) & sganrezidiv == 1 ~ "No",
      !is.na(sganrezidiv) & sganrezidiv == 2 ~ "Yes",
      TRUE ~ as.character(sganrezidiv)
    )
  )

# =========================================================
# 3) Numerische Variablen
#    AJSM-kompakt: Statistik im Variablennamen
# =========================================================
num_summary_df <- bind_rows(
  
  # Age: mean ± SD
  {
    x <- RTS_labeled$age
    data.frame(
      Variable = "Age, mean ± SD",
      Level = "",
      Value = sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  },
  
  # BMI: mean ± SD
  {
    x <- RTS_labeled$bmi
    data.frame(
      Variable = "BMI, mean ± SD",
      Level = "",
      Value = sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  },
  
  # Alcohol: median (IQR)
  {
    x <- RTS_labeled$alcohol
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    data.frame(
      Variable = "Alcohol consumption, median (IQR)",
      Level = "",
      Value = sprintf("%.2f (%.2f–%.2f)", median(x, na.rm = TRUE), q[1], q[2]),
      stringsAsFactors = FALSE
    )
  },
  
  # Sport minutes: median (IQR)
  {
    x <- RTS_labeled$sportmin
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    data.frame(
      Variable = "Sport participation per week, min, median (IQR)",
      Level = "",
      Value = sprintf("%.2f (%.2f–%.2f)", median(x, na.rm = TRUE), q[1], q[2]),
      stringsAsFactors = FALSE
    )
  }
)

# =========================================================
# 4) Kategoriale Variablen
#    AJSM-kompakt: n (%) im Variablennamen
# =========================================================
cat_map <- list(
  gender = "gender_lab",
  ansmoking = "ansmoking_lab",
  dominantside = "dominantside_lab",
  educ = "educ_lab",
  employstat = "employstat_lab",
  fam = "fam_lab",
  instadauer = "instadauer_lab",
  sganrezidiv = "sganrezidiv_lab",
  schulterbelastung = "schulterbelastung_lab"
)

cat_var_labels <- c(
  gender = "Sex, n (%)",
  ansmoking = "Smoking status, n (%)",
  dominantside = "Dominant side, n (%)",
  educ = "Education, n (%)",
  employstat = "Employment status, n (%)",
  fam = "Family status, n (%)",
  instadauer = "Duration of symptoms, n (%)",
  sganrezidiv = "Recurrence, n (%)",
  schulterbelastung = "Shoulder load, n (%)"
)

cat_summary_list <- lapply(names(cat_map), function(var) {
  labvar <- cat_map[[var]]
  vec <- RTS_labeled[[labvar]]
  
  tab <- table(vec, useNA = "no")
  levels <- names(tab)
  cnts <- as.integer(tab)
  pct <- round(100 * cnts / N, 1)
  
  data.frame(
    Variable = cat_var_labels[[var]],
    Level = levels,
    Value = paste0(cnts, " (", sprintf("%0.1f", pct), "%)"),
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

# =========================================================
# 5) Gruppieren für schöne Darstellung
# =========================================================
cat_summary_grouped <- do.call(
  rbind,
  lapply(split(cat_summary_list, cat_summary_list$Variable), function(df) {
    df2 <- df
    if (nrow(df2) > 1) df2$Variable[-1] <- ""
    df2
  })
)

Table1_long <- bind_rows(
  num_summary_df,
  cat_summary_grouped
) %>%
  mutate(
    Level_display = ifelse(Level == "", "", paste0("    ", Level)),
    Variable_display = ifelse(Variable == "", "", Variable)
  )

# =========================================================
# 6) Flextable
# =========================================================
ft <- flextable(
  Table1_long %>%
    select(Variable_display, Level_display, Value)
)

ft <- set_header_labels(
  ft,
  Variable_display = "",
  Level_display = "",
  Value = ""
)

group_header_idx <- which(Table1_long$Variable_display != "")
if (length(group_header_idx) > 0) {
  ft <- bold(ft, i = group_header_idx, j = 1:3, bold = TRUE)
}

ft <- align(ft, j = 1:2, align = "left", part = "all")
ft <- align(ft, j = 3, align = "right", part = "all")
ft <- fontsize(ft, size = 10, part = "all")
ft <- padding(ft, padding.top = 4, padding.bottom = 4, part = "all")
ft <- theme_box(ft)
ft <- autofit(ft)

# =========================================================
# 7) Word export
# =========================================================
title_text <- paste0("Table 1. Patient Characteristics (N = ", N, ")")

doc <- read_docx()
doc <- body_add_par(doc, title_text, style = "heading 1")
doc <- body_add_par(doc, " ", style = "Normal")
doc <- body_add_flextable(doc, ft)
doc <- body_add_par(
  doc,
  "Values are presented as mean ± SD, median (IQR), or n (%), as appropriate.",
  style = "Normal"
)

out_file <- "Table1_RTS_AJSM_compact.docx"
print(doc, target = out_file)

message("Fertig: Word-Datei erstellt -> ", normalizePath(out_file))


RTS_final <- RTS_final %>%
  mutate(
    dominant_operated = case_when(
      dominantside == 1 & anshside == 1 ~ 1,
      dominantside == 2 & anshside == 2 ~ 1,
      dominantside %in% c(1,2) ~ 0,
      dominantside == 3 ~ NA_real_
    )
  )

table(RTS_final$dominant_operated, useNA = "ifany")

prop.table(table(RTS_final$dominant_operated))

RTS_final %>%
  count(dominant_operated) %>%
  mutate(percent = n / sum(n) * 100)
