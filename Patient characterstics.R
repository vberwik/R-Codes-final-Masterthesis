library(dplyr)
library(flextable)
library(officer)

# =========================================================
# 1) Patient characteristics Datensatz
# =========================================================

RTS_final <- RTS_final %>%
  mutate(
    dominant_operated = case_when(
      dominantside == 1 & anshside == 1 ~ 1,
      dominantside == 2 & anshside == 2 ~ 1,
      dominantside %in% c(1, 2) ~ 0,
      dominantside == 3 ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

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
  "schulterbelastung",
  "dominant_operated"
)]

N <- nrow(RTS_pc)

# =========================================================
# 2) Labels für kategoriale Variablen
# =========================================================

instadauer_levels <- c(
  "< 6 weeks",
  "< 3 months",
  "< 6 months",
  "< 1 year",
  "< 2 years",
  "< 5 years",
  "\u2265 5 years",
  "Missing"
)

RTS_labeled <- RTS_pc %>%
  mutate(
    gender_lab = case_when(
      gender == 1 ~ "Male",
      gender == 0 ~ "Female",
      is.na(gender) ~ "Missing",
      TRUE ~ as.character(gender)
    ),
    
    ansmoking_lab = case_when(
      ansmoking == 1 ~ "Yes",
      ansmoking == 0 ~ "No",
      is.na(ansmoking) ~ "Missing",
      TRUE ~ as.character(ansmoking)
    ),
    
    dominantside_lab = case_when(
      dominantside == 1 ~ "Right",
      dominantside == 2 ~ "Left",
      dominantside == 3 ~ "Ambidextrous",
      is.na(dominantside) ~ "Missing",
      TRUE ~ as.character(dominantside)
    ),
    
    dominant_operated_lab = case_when(
      dominant_operated == 1 ~ "Yes",
      dominant_operated == 0 ~ "No",
      is.na(dominant_operated) ~ "Missing",
      TRUE ~ as.character(dominant_operated)
    ),
    
    educ_lab = case_when(
      educ == 1 ~ "Compulsory school",
      educ == 2 ~ "Secondary level II",
      educ == 3 ~ "Tertiary level",
      is.na(educ) ~ "Missing",
      TRUE ~ as.character(educ)
    ),
    
    employstat_lab = case_when(
      employstat == 1 ~ "Employed full-time",
      employstat == 2 ~ "Employed part-time",
      employstat == 3 ~ "Homemaker",
      employstat == 4 ~ "Retired",
      employstat == 5 ~ "Student",
      employstat == 6 ~ "Unemployed",
      is.na(employstat) ~ "Missing",
      TRUE ~ as.character(employstat)
    ),
    
    fam_lab = case_when(
      fam == 1 ~ "Married",
      fam == 2 ~ "Single",
      fam == 3 ~ "Divorced / separated",
      fam == 4 ~ "Living with partner",
      fam == 5 ~ "Widowed",
      is.na(fam) ~ "Missing",
      TRUE ~ as.character(fam)
    ),
    
    instadauer_lab = case_when(
      instadauer == 0 ~ "< 6 weeks",
      instadauer == 1 ~ "< 3 months",
      instadauer == 2 ~ "< 6 months",
      instadauer == 3 ~ "< 1 year",
      instadauer == 4 ~ "< 2 years",
      instadauer == 5 ~ "< 5 years",
      instadauer == 6 ~ "\u2265 5 years",
      is.na(instadauer) ~ "Missing",
      TRUE ~ paste0("Unknown code: ", instadauer)
    ),
    
    instadauer_lab = factor(
      instadauer_lab,
      levels = instadauer_levels
    ),
    
    schulterbelastung_lab = case_when(
      schulterbelastung == 0 ~ "Regular contact sport with overhead activity",
      schulterbelastung == 1 ~ "Regular contact sport without overhead activity",
      schulterbelastung == 2 ~ "Regular sport with overhead activity",
      schulterbelastung == 3 ~ "Regular sport without overhead activity",
      schulterbelastung == 5 ~ "Regular sport with traction components",
      is.na(schulterbelastung) ~ "Missing",
      TRUE ~ as.character(schulterbelastung)
    ),
    
    sganrezidiv_lab = case_when(
      sganrezidiv == 1 ~ "No",
      sganrezidiv == 2 ~ "Yes",
      is.na(sganrezidiv) ~ "Missing",
      TRUE ~ as.character(sganrezidiv)
    )
  )

# Kontrolle
table(RTS_labeled$instadauer_lab, useNA = "ifany")

# =========================================================
# 3) Numerische Variablen
# =========================================================

num_summary_df <- bind_rows(
  {
    x <- RTS_labeled$age
    data.frame(
      Variable = "Age, mean \u00B1 SD",
      Level = "",
      Value = sprintf("%.2f \u00B1 %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  },
  {
    x <- RTS_labeled$bmi
    data.frame(
      Variable = "BMI, mean \u00B1 SD",
      Level = "",
      Value = sprintf("%.2f \u00B1 %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  },
  {
    x <- RTS_labeled$alcohol
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    data.frame(
      Variable = "Alcohol consumption, median (IQR)",
      Level = "",
      Value = sprintf("%.2f (%.2f\u2013%.2f)", median(x, na.rm = TRUE), q[1], q[2]),
      stringsAsFactors = FALSE
    )
  },
  {
    x <- RTS_labeled$sportmin
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    data.frame(
      Variable = "Sport participation per week, min, median (IQR)",
      Level = "",
      Value = sprintf("%.2f (%.2f\u2013%.2f)", median(x, na.rm = TRUE), q[1], q[2]),
      stringsAsFactors = FALSE
    )
  }
)

# =========================================================
# 4) Kategoriale Variablen
# =========================================================

cat_map <- list(
  gender = "gender_lab",
  ansmoking = "ansmoking_lab",
  dominantside = "dominantside_lab",
  dominant_operated = "dominant_operated_lab",
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
  dominant_operated = "Dominant side operated, n (%)",
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
}) %>%
  bind_rows()

# =========================================================
# 5) Gruppieren
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
  "Values are presented as mean \u00B1 SD, median (IQR), or n (%), as appropriate.",
  style = "Normal"
)

out_file <- "Table1_RTS_AJSM_compact.docx"
print(doc, target = out_file)

message("Fertig: Word-Datei erstellt -> ", normalizePath(out_file))

