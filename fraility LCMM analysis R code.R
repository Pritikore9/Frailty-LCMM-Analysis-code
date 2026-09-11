# =============================================================================
# Title:        Frailty Trajectory Group (LCMM) Association Analysis
# Description:  Baseline characteristic comparisons and univariate /
#               multivariable multinomial logistic regression models
#               examining predictors of latent-class frailty trajectory
#               group membership (Very Low / Moderate / Higher frailty).
# Repository:   <add repo URL here>
# License:      <add license, e.g. MIT>
# =============================================================================
#
# HOW TO USE
# ----------
# 1. Place the input Excel data file inside a folder named "data/" in the
#    project root (this script assumes it is run from the project root,
#    e.g. via an .Rproj file or `here::here()`).
# 2. Update RAW_DATA_FILE below if your filename differs.
# 3. Run the script top to bottom. All tables/figures are written to an
#    "outputs/" folder, created automatically if it does not exist.
#
# CONTENTS
# --------
#   0. Setup (packages, paths)
#   1. Data import
#   2. PART A - Baseline characteristics table (raw variables) + univariate
#      and multivariable multinomial models (initial analysis)
#   3. PART B - Revised baseline characteristics table (recoded factors),
#      updated univariate/multivariable selection, and post-hoc adjusted
#      vs. unadjusted predicted probabilities (revised analysis)
#
# =============================================================================


# -----------------------------------------------------------------------------
# 0. SETUP
# -----------------------------------------------------------------------------

# Required packages (install once with install.packages() if missing)
required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "gtsummary", "gt", "nnet", "broom", "emmeans", "openxlsx"
)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# --- CONFIG: edit these paths/filenames for your setup ---------------------
data_dir       <- "data"
output_dir     <- "outputs"
RAW_DATA_FILE  <- "data_with_lcmm_groups.xlsx"
# -----------------------------------------------------------------------------

if (!dir.exists(output_dir)) dir.create(output_dir)


# -----------------------------------------------------------------------------
# 1. DATA IMPORT
# -----------------------------------------------------------------------------

df <- read_xlsx(file.path(data_dir, RAW_DATA_FILE))
df$Depression <- as.numeric(df$Depression)
df$Anxiety    <- as.numeric(df$Anxiety)


# =============================================================================
# PART A - INITIAL ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# A1. Baseline characteristics table by frailty group (raw variables)
# -----------------------------------------------------------------------------

table_baseline_raw <- df %>%
  select(
    lcmm_group,
    dgender,
    Age_Cat,
    dracehisp,
    Marital_status,
    education,
    BMI_cat,
    Self_rated_health,
    HEART_ATTACK,
    Heart_Disease,
    High_BP,
    ARTHRITIS,
    OSTEOPOROSIS,
    DIABETES,
    LUNG_DISEASE,
    DEMENTIA_OR_ALZ,
    CANCER,
    Depression,
    Anxiety,
    Depression_Anxiety_score,
    Word_recall_score_strk
  ) %>%
  tbl_summary(
    by = lcmm_group,
    missing = "no",
    percent = "column",
    type = list(
      c(Depression, Anxiety, Depression_Anxiety_score, Word_recall_score_strk) ~ "continuous",
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} \u00b1 {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ 2
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      all_continuous() ~ "anova"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(
    label ~ "**Variable**",
    p.value ~ "**P-value**"
  ) %>%
  bold_labels()

table_baseline_raw %>%
  as_gt() %>%
  gtsave(file.path(output_dir, "Table_Baseline_Characteristics_MeanSD.docx"))


# -----------------------------------------------------------------------------
# A2. Recode variables for regression modeling
# -----------------------------------------------------------------------------

df <- df %>%
  mutate(
    lcmm_group = factor(
      lcmm_group,
      levels = c("Very Low frailty", "Moderate frailty", "Higher frailty")
    ),
    sex     = factor(dgender, levels = c("Male", "Female")),
    age_cat = factor(Age_Cat, levels = c("65-74", "75-84", "85 and Above")),
    race    = factor(
      dracehisp,
      levels = c(
        "1 White, non-hispanic",
        "2 Black, non-hispanic",
        "3 Other (Am Indian/Asian/Native Hawaiian/Pacific Islander/other specify), non-Hispanic",
        "4 Hispanic",
        "6 DKRF"
      )
    ),
    marital = factor(Marital_status, levels = c("Married or Living with partner", "Single")),
    educ    = factor(education, levels = c("College Degree or Higher", "Less than College degree", "No School")),
    bmi_cat = factor(BMI_cat, levels = c("Normal_weight", "Overweight", "Obese", "Underweight")),
    srh     = factor(Self_rated_health, levels = c("Very Good", "Excellent", "Good", "Fair", "Poor")),
    
    heart_attack  = factor(HEART_ATTACK, levels = c("No", "Yes")),
    heart_disease = factor(Heart_Disease, levels = c("No", "Yes")),
    high_bp       = factor(High_BP, levels = c("No", "Yes")),
    arthritis     = factor(ARTHRITIS, levels = c("No", "Yes")),
    osteoporosis  = factor(OSTEOPOROSIS, levels = c("No", "Yes")),
    diabetes      = factor(DIABETES, levels = c("No", "Yes")),
    lung_disease  = factor(LUNG_DISEASE, levels = c("No", "Yes")),
    dementia_alz  = factor(DEMENTIA_OR_ALZ, levels = c("No", "Yes")),
    cancer_hist   = factor(CANCER, levels = c("No", "Yes"))
  )

write.csv(df, file.path(output_dir, "data_final_recoded.csv"), row.names = FALSE)
saveRDS(df, file.path(output_dir, "data_final_recoded.rds"))


# -----------------------------------------------------------------------------
# A3. Univariate multinomial models (crude ORs)
# -----------------------------------------------------------------------------

uni_vars <- c(
  "sex", "age_cat", "race", "marital", "educ", "bmi_cat", "srh",
  "heart_attack", "heart_disease", "high_bp",
  "arthritis", "osteoporosis", "diabetes",
  "lung_disease", "dementia_alz", "cancer_hist",
  "Depression", "Anxiety", "Depression_Anxiety_score", "Word_recall_score_strk"
)

uni_results <- lapply(uni_vars, function(v) {
  form  <- as.formula(paste("lcmm_group ~", v))
  m_uni <- multinom(form, data = df, trace = FALSE)
  tidy(m_uni, exponentiate = TRUE, conf.int = TRUE)
})
names(uni_results) <- uni_vars

univariate_all <- bind_rows(uni_results, .id = "Variable") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Outcome_Comparison = case_when(
      y.level == "Moderate frailty" ~ "Moderate vs Very Low",
      y.level == "Higher frailty"   ~ "Higher vs Very Low",
      TRUE ~ y.level
    ),
    OR_95CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")")
  )

uni_long <- univariate_all %>%
  mutate(
    term_clean = case_when(
      Variable %in% c("Depression", "Anxiety", "Depression_Anxiety_score", "Word_recall_score_strk") ~ Variable,
      TRUE ~ str_replace(term, paste0("^", Variable), "")
    ),
    p.value = as.character(round(p.value, 3))
  ) %>%
  select(Variable, term_clean, Outcome_Comparison, OR_95CI, p.value)

uni_wide <- uni_long %>%
  pivot_longer(cols = c(OR_95CI, p.value), names_to = "stat", values_to = "val") %>%
  mutate(
    col_name = paste(Outcome_Comparison, ifelse(stat == "OR_95CI", "OR_95CI", "p"), sep = " - ")
  ) %>%
  select(Variable, term_clean, col_name, val) %>%
  pivot_wider(names_from = col_name, values_from = val) %>%
  arrange(Variable, term_clean)

write.csv(uni_wide, file.path(output_dir, "Table_Univariate_ORs_Wide_with_p.csv"), row.names = FALSE)


# -----------------------------------------------------------------------------
# A4. Multivariable multinomial model (adjusted ORs, fixed covariate set)
# -----------------------------------------------------------------------------

final_covars <- c(
  "age_cat", "sex", "race", "educ", "bmi_cat",
  "srh", "diabetes", "dementia_alz",
  "Depression", "Anxiety", "Depression_Anxiety_score", "Word_recall_score_strk"
)

form_multi   <- as.formula(paste("lcmm_group ~", paste(final_covars, collapse = " + ")))
model_multi  <- multinom(form_multi, data = df, trace = FALSE)

multi_tidy <- tidy(model_multi, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Outcome_Comparison = case_when(
      y.level == "Moderate frailty" ~ "Moderate vs Very Low",
      y.level == "Higher frailty"   ~ "Higher vs Very Low",
      TRUE ~ y.level
    ),
    OR_95CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    p.value = as.character(round(p.value, 3))
  ) %>%
  select(Outcome_Comparison, term, OR_95CI, p.value)

multi_long <- multi_tidy %>%
  mutate(
    Variable = case_when(
      grepl("^sex", term)          ~ "sex",
      grepl("^age_cat", term)      ~ "age_cat",
      grepl("^race", term)         ~ "race",
      grepl("^educ", term)         ~ "educ",
      grepl("^bmi_cat", term)      ~ "bmi_cat",
      grepl("^srh", term)          ~ "srh",
      grepl("^diabetes", term)     ~ "diabetes",
      grepl("^dementia_alz", term) ~ "dementia_alz",
      grepl("Depression", term)    ~ "Depression",
      grepl("Anxiety", term)       ~ "Anxiety",
      grepl("^Depression_Anxiety_score", term) ~ "Depression_Anxiety_score",
      grepl("^Word_recall_score_strk", term)   ~ "Word_recall_score_strk",
      TRUE ~ term
    ),
    term_clean = case_when(
      Variable %in% c("Depression_Anxiety_score", "Word_recall_score_strk") ~ Variable,
      TRUE ~ str_replace(term, paste0("^", Variable), "")
    )
  ) %>%
  select(Variable, term_clean, Outcome_Comparison, OR_95CI, p.value)

multi_wide <- multi_long %>%
  pivot_longer(cols = c(OR_95CI, p.value), names_to = "stat", values_to = "val") %>%
  mutate(
    col_name = paste(Outcome_Comparison, ifelse(stat == "OR_95CI", "OR_95CI", "p"), sep = " - ")
  ) %>%
  select(Variable, term_clean, col_name, val) %>%
  pivot_wider(names_from = col_name, values_from = val) %>%
  arrange(Variable, term_clean)

write.csv(multi_wide, file.path(output_dir, "Table_Multivariable_Multinomial_ORs_Wide_with_p.csv"), row.names = FALSE)


# =============================================================================
# PART B - REVISED ANALYSIS
# =============================================================================
# Re-applies the recoding (kept here in case Part A is skipped), then produces
# an updated baseline table (Kruskal-Wallis for continuous variables),
# a univariate screen with p < 0.20 variable selection for the multivariable
# model, and post-hoc adjusted vs. unadjusted predicted probabilities.
# =============================================================================

df <- df %>%
  mutate(
    lcmm_group = factor(lcmm_group, levels = c("Very Low frailty", "Moderate frailty", "Higher frailty")),
    sex     = factor(dgender, levels = c("Male", "Female")),
    age_cat = factor(Age_Cat, levels = c("65-74", "75-84", "85 and Above")),
    race    = factor(
      dracehisp,
      levels = c(
        "1 White, non-hispanic",
        "2 Black, non-hispanic",
        "3 Other (Am Indian/Asian/Native Hawaiian/Pacific Islander/other specify), non-Hispanic",
        "4 Hispanic",
        "6 DKRF"
      )
    ),
    marital = factor(Marital_status, levels = c("Married or Living with partner", "Single")),
    educ    = factor(education, levels = c("College Degree or Higher", "Less than College degree", "No School")),
    bmi_cat = factor(BMI_cat, levels = c("Normal_weight", "Overweight", "Obese", "Underweight")),
    srh     = factor(Self_rated_health, levels = c("Very Good", "Excellent", "Good", "Fair", "Poor")),
    
    heart_attack  = factor(HEART_ATTACK, levels = c("No", "Yes")),
    heart_disease = factor(Heart_Disease, levels = c("No", "Yes")),
    high_bp       = factor(High_BP, levels = c("No", "Yes")),
    arthritis     = factor(ARTHRITIS, levels = c("No", "Yes")),
    osteoporosis  = factor(OSTEOPOROSIS, levels = c("No", "Yes")),
    diabetes      = factor(DIABETES, levels = c("No", "Yes")),
    lung_disease  = factor(LUNG_DISEASE, levels = c("No", "Yes")),
    dementia_alz  = factor(DEMENTIA_OR_ALZ, levels = c("No", "Yes")),
    cancer_hist   = factor(CANCER, levels = c("No", "Yes"))
  )


# -----------------------------------------------------------------------------
# B1. Baseline characteristics table by frailty group (recoded factors)
# -----------------------------------------------------------------------------

table_baseline_recoded <- df %>%
  select(
    lcmm_group, sex, age_cat, marital, educ, bmi_cat, race, srh,
    heart_attack, heart_disease, high_bp, arthritis, osteoporosis,
    diabetes, lung_disease, dementia_alz, cancer_hist,
    Depression, Anxiety, Depression_Anxiety_score, Word_recall_score_strk
  ) %>%
  tbl_summary(
    by = lcmm_group,
    missing = "no",
    percent = "column",
    type = list(
      c(Depression, Anxiety, Depression_Anxiety_score, Word_recall_score_strk) ~ "continuous",
      all_dichotomous() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} \u00b1 {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0, 1),
      all_continuous() ~ 2
    )
  ) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      all_continuous() ~ "kruskal.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label ~ "**Variable**", p.value ~ "**P-value**") %>%
  bold_labels()

table_baseline_recoded_df <- as_tibble(table_baseline_recoded) %>% as.data.frame()

write.xlsx(
  table_baseline_recoded_df,
  file.path(output_dir, "Table_Baseline_Characteristics_Frailty_Recoded.xlsx"),
  overwrite = TRUE
)


# -----------------------------------------------------------------------------
# B2. Univariate screen (used for p < 0.20 variable selection below)
# -----------------------------------------------------------------------------

uni_vars <- c(
  "sex", "age_cat", "race", "marital", "educ", "bmi_cat", "srh",
  "heart_attack", "heart_disease", "high_bp",
  "arthritis", "osteoporosis", "diabetes",
  "lung_disease", "dementia_alz", "cancer_hist",
  "Depression", "Anxiety", "Depression_Anxiety_score", "Word_recall_score_strk"
)

uni_list <- lapply(uni_vars, function(v) {
  mod <- multinom(as.formula(paste("lcmm_group ~", v)), data = df, trace = FALSE)
  tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(Predictor = v)
})

univ_table <- bind_rows(uni_list)

univ_clean <- univ_table %>%
  mutate(
    Covariate = case_when(
      Predictor == "sex"     ~ "Gender",
      Predictor == "marital" ~ "Marital status",
      Predictor == "educ"    ~ "Education",
      Predictor == "bmi_cat" ~ "BMI category",
      Predictor == "race"    ~ "Race",
      Predictor == "srh"     ~ "Self-rated health",
      TRUE ~ Predictor
    ),
    Level = case_when(
      str_detect(term, "Male$")       ~ "Male vs Female",
      str_detect(term, "Single$")     ~ "Single vs Married",
      str_detect(term, "Yes$")        ~ "Yes vs No",
      str_detect(term, "Overweight")  ~ "Overweight vs Normal",
      str_detect(term, "Obese")       ~ "Obese vs Normal",
      str_detect(term, "Underweight") ~ "Underweight vs Normal",
      str_detect(term, "College")     ~ "College+ vs <College",
      TRUE ~ term
    ),
    OR_CI   = paste0(round(estimate, 2), " (", round(conf.low, 2), "\u2013", round(conf.high, 2), ")"),
    p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  )

univ_or <- univ_clean %>%
  select(Covariate, Level, y.level, OR_CI) %>%
  pivot_wider(names_from = y.level, values_from = OR_CI)

univ_p <- univ_clean %>%
  select(Covariate, Level, y.level, p.value) %>%
  pivot_wider(names_from = y.level, values_from = p.value, names_prefix = "p_")

univ_final <- left_join(univ_or, univ_p, by = c("Covariate", "Level"))

write.xlsx(univ_final, file.path(output_dir, "Table_Univariate_Frailty.xlsx"), overwrite = TRUE)


# -----------------------------------------------------------------------------
# B3. Multivariable model: p < 0.20 selection + clinically forced covariates
# -----------------------------------------------------------------------------

vars_p20 <- univ_clean %>%
  mutate(
    p.value = case_when(
      p.value == "<0.001" ~ 0.0005,
      TRUE ~ as.numeric(p.value)
    )
  ) %>%
  filter(p.value < 0.20) %>%
  pull(Predictor) %>%
  unique()

force_vars  <- c("sex", "educ")  # retained for clinical relevance regardless of p-value
final_vars  <- unique(c(vars_p20, force_vars))

form_multi  <- as.formula(paste("lcmm_group ~", paste(final_vars, collapse = " + ")))
model_multi <- multinom(form_multi, data = df, trace = FALSE, maxit = 200)

multi_clean <- tidy(model_multi, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    aOR_CI  = paste0(round(estimate, 2), " (", round(conf.low, 2), "\u2013", round(conf.high, 2), ")"),
    p.value = ifelse(p.value < 0.001, "<0.001", as.character(round(p.value, 3)))
  )

multi_or <- multi_clean %>%
  select(term, y.level, aOR_CI) %>%
  pivot_wider(names_from = y.level, values_from = aOR_CI)

multi_p <- multi_clean %>%
  select(term, y.level, p.value) %>%
  pivot_wider(names_from = y.level, values_from = p.value, names_prefix = "p_")

multi_final <- left_join(multi_or, multi_p, by = "term")

write.xlsx(multi_final, file.path(output_dir, "Table_Multivariable_Frailty.xlsx"), overwrite = TRUE)


# -----------------------------------------------------------------------------
# B4. Post-hoc: adjusted vs. unadjusted predicted probabilities (emmeans)
# -----------------------------------------------------------------------------

model_multi_posthoc <- multinom(
  lcmm_group ~ age_cat + sex + race + marital + educ + bmi_cat +
    srh + heart_disease + diabetes + lung_disease +
    dementia_alz + Depression_Anxiety_score,
  data  = df,
  trace = FALSE
)

posthoc_vars <- c(
  "age_cat", "sex", "race", "marital", "educ", "bmi_cat", "srh",
  "heart_disease", "diabetes", "lung_disease", "dementia_alz",
  "Depression_Anxiety_score"
)

#' Get adjusted (model-based) predicted probabilities for one predictor
#' @param var Character name of the predictor column in `df`.
get_adjusted_probs <- function(var) {
  emmeans(
    model_multi_posthoc,
    specs     = as.formula(paste("~", var)),
    type      = "response",
    rg.limit  = 100000,   # allow a large reference grid
    cov.reduce = mean     # average over continuous covariates
  ) %>%
    as.data.frame() %>%
    rename(Level = !!sym(var)) %>%
    mutate(Variable = var, Type = "Adjusted")
}

#' Get unadjusted (crude, single-predictor model) predicted probabilities
#' @param var Character name of the predictor column in `df`.
get_unadjusted_probs <- function(var) {
  mod <- multinom(as.formula(paste("lcmm_group ~", var)), data = df, trace = FALSE)
  emmeans(mod, as.formula(paste("~", var)), type = "response") %>%
    as.data.frame() %>%
    rename(Level = !!sym(var)) %>%
    mutate(Variable = var, Type = "Unadjusted")
}

prob_table <- map_dfr(
  posthoc_vars,
  ~ bind_rows(get_unadjusted_probs(.x), get_adjusted_probs(.x))
)

final_prob_table <- prob_table %>%
  mutate(
    Probability = round(prob, 3),
    CI = paste0(round(lower.CL, 3), "\u2013", round(upper.CL, 3))
  ) %>%
  select(Variable, Level, Type, lcmm_group, Probability, CI) %>%
  pivot_wider(
    names_from  = lcmm_group,
    values_from = c(Probability, CI),
    names_sep   = "_"
  ) %>%
  arrange(Variable, Level, Type) %>%
  mutate(
    Variable = recode(
      Variable,
      age_cat = "Age group",
      sex = "Sex",
      race = "Race / Ethnicity",
      marital = "Marital status",
      educ = "Education",
      bmi_cat = "BMI category",
      srh = "Self-rated health",
      heart_disease = "Heart disease",
      diabetes = "Diabetes",
      lung_disease = "Lung disease",
      dementia_alz = "Dementia / Alzheimer's",
      Depression_Anxiety_score = "Depression\u2013Anxiety score"
    )
  )

write.xlsx(
  final_prob_table,
  file.path(output_dir, "PostHoc_Adjusted_vs_Unadjusted_Predicted_Probabilities.xlsx"),
  overwrite = TRUE
)

# =============================================================================
# End of script
# =============================================================================