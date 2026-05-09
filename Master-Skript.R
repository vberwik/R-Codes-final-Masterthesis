# =========================================================
# MASTER-SKRIPT THESIS
# Datensatz: RTS_final
# Zweck:
# 1) Quality Checks
# 2) Deskriptive Statistik
# 3) Wilcoxon-Test BRS
# 4) Regressionsmodelle SIRSI und WOSI
# 5) Modell-Diagnostik
# 6) Sensitivitätsanalysen
# 7) Word-Export zentraler Tabellen
# =========================================================

# =========================================================
# 0) Pakete laden
# =========================================================

library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(lmtest)
library(sandwich)
library(officer)
library(flextable)

# =========================================================
# 1) Basis-Checks
# =========================================================

cat("Anzahl Fälle in RTS_final:", nrow(RTS_final), "\n")

needed_vars <- c(
  "regid",
  "BRS_baseline",
  "BRS_3m",
  "delta_BRS",
  "SIRSI_baseline",
  "SIRSI_3m",
  "WOSI_baseline",
  "WOSI_3m",
  "schulter3"
)

missing_vars <- setdiff(needed_vars, names(RTS_final))

if(length(missing_vars) > 0){
  stop(
    "Folgende Variablen fehlen in RTS_final: ",
    paste(missing_vars, collapse = ", ")
  )
} else {
  cat("Alle benötigten Variablen sind vorhanden.\n")
}

# =========================================================
# 2) Missing Data / Score-Verfügbarkeit
# =========================================================

score_summary <- data.frame(
  Variable = c(
    "BRS_baseline",
    "BRS_3m",
    "delta_BRS",
    "SIRSI_baseline",
    "SIRSI_3m",
    "WOSI_baseline",
    "WOSI_3m"
  ),
  N_available = c(
    sum(!is.na(RTS_final$BRS_baseline)),
    sum(!is.na(RTS_final$BRS_3m)),
    sum(!is.na(RTS_final$delta_BRS)),
    sum(!is.na(RTS_final$SIRSI_baseline)),
    sum(!is.na(RTS_final$SIRSI_3m)),
    sum(!is.na(RTS_final$WOSI_baseline)),
    sum(!is.na(RTS_final$WOSI_3m))
  ),
  N_missing = c(
    sum(is.na(RTS_final$BRS_baseline)),
    sum(is.na(RTS_final$BRS_3m)),
    sum(is.na(RTS_final$delta_BRS)),
    sum(is.na(RTS_final$SIRSI_baseline)),
    sum(is.na(RTS_final$SIRSI_3m)),
    sum(is.na(RTS_final$WOSI_baseline)),
    sum(is.na(RTS_final$WOSI_3m))
  )
)

print(score_summary)

missing_score_cases <- RTS_final %>%
  filter(
    is.na(BRS_baseline) |
      is.na(BRS_3m) |
      is.na(delta_BRS) |
      is.na(SIRSI_baseline) |
      is.na(SIRSI_3m) |
      is.na(WOSI_baseline) |
      is.na(WOSI_3m)
  ) %>%
  select(
    regid,
    BRS_baseline,
    BRS_3m,
    delta_BRS,
    SIRSI_baseline,
    SIRSI_3m,
    WOSI_baseline,
    WOSI_3m
  )

print(missing_score_cases)

# =========================================================
# 3) Deskriptive Statistik
# =========================================================

descr_vars <- c(
  "BRS_baseline",
  "BRS_3m",
  "delta_BRS",
  "SIRSI_baseline",
  "SIRSI_3m",
  "WOSI_baseline",
  "WOSI_3m"
)

descr_table <- lapply(descr_vars, function(v){
  
  x <- RTS_final[[v]]
  
  tibble(
    Variable = v,
    N = sum(!is.na(x)),
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Max = round(max(x, na.rm = TRUE), 2),
    Summary = sprintf(
      "%0.2f ± %0.2f (%0.2f–%0.2f)",
      mean(x, na.rm = TRUE),
      sd(x, na.rm = TRUE),
      min(x, na.rm = TRUE),
      max(x, na.rm = TRUE)
    )
  )
}) %>%
  bind_rows()

print(descr_table)

# =========================================================
# 4) Wilcoxon-Test BRS Baseline vs. 3 Monate
# =========================================================

df_brs <- RTS_final %>%
  select(regid, BRS_baseline, BRS_3m) %>%
  filter(
    !is.na(BRS_baseline),
    !is.na(BRS_3m)
  )

n_paired <- nrow(df_brs)

wilcox_result <- wilcox.test(
  df_brs$BRS_3m,
  df_brs$BRS_baseline,
  paired = TRUE,
  exact = FALSE,
  correct = FALSE
)

V <- as.numeric(wilcox_result$statistic)
n <- n_paired

mu_V <- n * (n + 1) / 4
sigma_V <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

z_val <- (V - mu_V) / sigma_V
r_effect <- abs(z_val) / sqrt(n)

wilcox_table <- data.frame(
  Test = "Wilcoxon signed-rank test",
  N = n_paired,
  V = V,
  p_value = round(wilcox_result$p.value, 3),
  effect_size_r = round(r_effect, 3),
  interpretation = ifelse(
    r_effect < 0.1, "negligible",
    ifelse(
      r_effect < 0.3, "small",
      ifelse(r_effect < 0.5, "moderate", "large")
    )
  )
)

print(wilcox_table)

# =========================================================
# 5) Schulterbelastung: Referenzgruppe setzen
# =========================================================

RTS_final <- RTS_final %>%
  mutate(
    schulter3 = factor(
      schulter3,
      levels = c("Kein Überkopf / Zug", "Überkopf", "Kontakt")
    )
  )

table(RTS_final$schulter3, useNA = "ifany")

# =========================================================
# 6) Regressionsmodelle
# =========================================================

modelA3_df <- RTS_final %>%
  select(regid, SIRSI_3m, BRS_baseline, delta_BRS, schulter3) %>%
  filter(
    !is.na(SIRSI_3m),
    !is.na(BRS_baseline),
    !is.na(delta_BRS),
    !is.na(schulter3)
  )

modelB3_df <- RTS_final %>%
  select(regid, WOSI_3m, BRS_baseline, delta_BRS, schulter3) %>%
  filter(
    !is.na(WOSI_3m),
    !is.na(BRS_baseline),
    !is.na(delta_BRS),
    !is.na(schulter3)
  )

cat("Model A3 complete cases n =", nrow(modelA3_df), "\n")
cat("Model B3 complete cases n =", nrow(modelB3_df), "\n")

modelA3 <- lm(
  SIRSI_3m ~ BRS_baseline + delta_BRS + schulter3,
  data = modelA3_df
)

modelB3 <- lm(
  WOSI_3m ~ BRS_baseline + delta_BRS + schulter3,
  data = modelB3_df
)

summary(modelA3)
summary(modelB3)

Anova(modelA3, type = "II")
Anova(modelB3, type = "II")

coeftest(modelA3, vcov. = vcovHC(modelA3, type = "HC3"))
coeftest(modelB3, vcov. = vcovHC(modelB3, type = "HC3"))

# =========================================================
# 7) Modell-Diagnostik
# =========================================================

diagnose_lm <- function(model, data, modelname = "Modell") {
  
  cat("\n====================================\n")
  cat("Diagnostik für:", modelname, "\n")
  cat("====================================\n\n")
  
  res <- resid(model)
  fit <- fitted(model)
  
  # Normalität
  cat("Shapiro-Wilk-Test Residuen:\n")
  print(shapiro.test(res))
  
  # Homoskedastizität
  cat("\nBreusch-Pagan-Test:\n")
  print(bptest(model))
  
  # Multikollinearität
  cat("\nVIF:\n")
  print(vif(model))
  
  # Einflussreiche Fälle
  cooks <- cooks.distance(model)
  cutoff <- 4 / length(cooks)
  influential <- which(cooks > cutoff)
  
  cat("\nCook's Distance Cutoff:", round(cutoff, 3), "\n")
  cat("Einflussreiche Fälle Index:\n")
  print(influential)
  
  if(length(influential) > 0){
    cat("RegIDs einflussreicher Fälle:\n")
    print(data$regid[influential])
  }
  
  # Plots
  plot(
    fit, res,
    xlab = "Fitted values",
    ylab = "Residuals",
    main = paste(modelname, "- Residuals vs Fitted")
  )
  abline(h = 0, col = "red", lwd = 2)
  
  qqnorm(res, main = paste(modelname, "- QQ Plot"))
  qqline(res, col = "red", lwd = 2)
  
  plot(
    cooks,
    type = "h",
    main = paste(modelname, "- Cook's Distance"),
    ylab = "Cook's D"
  )
  abline(h = cutoff, col = "red", lwd = 2)
}

diagnose_lm(modelA3, modelA3_df, "Model A3: SIRSI")
diagnose_lm(modelB3, modelB3_df, "Model B3: WOSI")

# =========================================================
# 8) Sensitivitätsanalysen für einflussreiche Fälle
# =========================================================

# ---------- Model A3 ----------
cooks_A3 <- cooks.distance(modelA3)
cutoff_A3 <- 4 / length(cooks_A3)
infl_A3 <- which(cooks_A3 > cutoff_A3)

cat("\nEinflussreiche Fälle Model A3:\n")
print(modelA3_df$regid[infl_A3])

if(length(infl_A3) > 0){
  
  modelA3_sens <- lm(
    SIRSI_3m ~ BRS_baseline + delta_BRS + schulter3,
    data = modelA3_df[-infl_A3, ]
  )
  
  cat("\nOriginal Model A3:\n")
  print(coef(summary(modelA3)))
  
  cat("\nSensitivity Model A3:\n")
  print(coef(summary(modelA3_sens)))
}

# ---------- Model B3 ----------
cooks_B3 <- cooks.distance(modelB3)
cutoff_B3 <- 4 / length(cooks_B3)
infl_B3 <- which(cooks_B3 > cutoff_B3)

cat("\nEinflussreiche Fälle Model B3:\n")
print(modelB3_df$regid[infl_B3])

if(length(infl_B3) > 0){
  
  modelB3_sens <- lm(
    WOSI_3m ~ BRS_baseline + delta_BRS + schulter3,
    data = modelB3_df[-infl_B3, ]
  )
  
  cat("\nOriginal Model B3:\n")
  print(coef(summary(modelB3)))
  
  cat("\nSensitivity Model B3:\n")
  print(coef(summary(modelB3_sens)))
}

# =========================================================
# 9) Partial Regression Plots WOSI
# =========================================================

avPlot(
  modelB3,
  variable = "BRS_baseline",
  main = "Partial effect of BRS baseline on WOSI 3m"
)

avPlot(
  modelB3,
  variable = "delta_BRS",
  main = "Partial effect of delta BRS on WOSI 3m"
)

# Standardisierte Effekte für WOSI-Modell
modelB3_std <- lm(
  scale(WOSI_3m) ~ scale(BRS_baseline) +
    scale(delta_BRS) +
    schulter3,
  data = modelB3_df
)

summary(modelB3_std)

# =========================================================
# 10) Word-Export: kompakte Tabellen
# =========================================================

extract_regression_table <- function(model) {
  
  broom::tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Predictor = case_when(
        term == "BRS_baseline" ~ "BRS baseline",
        term == "delta_BRS" ~ "ΔBRS",
        term == "schulter3Überkopf" ~ "Shoulder load: overhead",
        term == "schulter3Kontakt" ~ "Shoulder load: contact",
        TRUE ~ term
      ),
      B = sprintf("%.2f", estimate),
      CI_95 = paste0(
        sprintf("%.2f", conf.low),
        " to ",
        sprintf("%.2f", conf.high)
      ),
      p_value = ifelse(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      )
    ) %>%
    select(Predictor, B, CI_95, p_value)
}

extract_model_info <- function(model, model_name) {
  
  s <- summary(model)
  fstat <- s$fstatistic
  
  data.frame(
    Model = model_name,
    N = nobs(model),
    R2 = round(s$r.squared, 3),
    Adjusted_R2 = round(s$adj.r.squared, 3),
    Model_p = ifelse(
      pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) < 0.001,
      "<0.001",
      sprintf("%.3f", pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE))
    )
  )
}

coef_A3 <- extract_regression_table(modelA3)
coef_B3 <- extract_regression_table(modelB3)

info_A3 <- extract_model_info(modelA3, "Model A3: SIRSI at 3 months")
info_B3 <- extract_model_info(modelB3, "Model B3: WOSI at 3 months")

ft_descr <- flextable(descr_table %>% select(Variable, N, Summary)) %>%
  autofit() %>%
  theme_box()

ft_wilcox <- flextable(wilcox_table) %>%
  autofit() %>%
  theme_box()

ft_info_A3 <- flextable(info_A3) %>%
  autofit() %>%
  theme_box()

ft_coef_A3 <- flextable(coef_A3) %>%
  set_header_labels(
    Predictor = "Predictor",
    B = "B",
    CI_95 = "95% CI",
    p_value = "p"
  ) %>%
  autofit() %>%
  theme_box()

ft_info_B3 <- flextable(info_B3) %>%
  autofit() %>%
  theme_box()

ft_coef_B3 <- flextable(coef_B3) %>%
  set_header_labels(
    Predictor = "Predictor",
    B = "B",
    CI_95 = "95% CI",
    p_value = "p"
  ) %>%
  autofit() %>%
  theme_box()

doc <- read_docx()

doc <- body_add_par(doc, "Thesis Analysis Results", style = "heading 1")

doc <- body_add_par(doc, "Descriptive statistics", style = "heading 2")
doc <- body_add_flextable(doc, ft_descr)

doc <- body_add_par(doc, "Wilcoxon test: BRS baseline vs 3 months", style = "heading 2")
doc <- body_add_flextable(doc, ft_wilcox)

doc <- body_add_par(doc, "Model A3: SIRSI at 3 months", style = "heading 2")
doc <- body_add_flextable(doc, ft_info_A3)
doc <- body_add_par(doc, "Regression coefficients", style = "heading 3")
doc <- body_add_flextable(doc, ft_coef_A3)

doc <- body_add_par(doc, "Model B3: WOSI at 3 months", style = "heading 2")
doc <- body_add_flextable(doc, ft_info_B3)
doc <- body_add_par(doc, "Regression coefficients", style = "heading 3")
doc <- body_add_flextable(doc, ft_coef_B3)

doc <- body_add_par(
  doc,
  "Reference category for shoulder load: Kein Überkopf / Zug.",
  style = "Normal"
)

doc <- body_add_par(
  doc,
  "Regression coefficients are reported as unstandardized B with 95% confidence intervals and p-values.",
  style = "Normal"
)

doc <- body_add_par(
  doc,
  "Robust standard errors (HC3) and sensitivity analyses were conducted as model checks.",
  style = "Normal"
)

out_file <- "Thesis_Analysis_Results_RTS_final.docx"
print(doc, target = out_file)

message("Fertig. Word-Datei erstellt: ", normalizePath(out_file))

# =========================================================
# Ende Master-Skript
# =========================================================

