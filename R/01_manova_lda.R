## Multivariate differences in entrepreneurial profiles: MANOVA and descriptive LDA
## Data preparation and analysis code
## Run from the repository root: Rscript R/01_manova_lda.R

# This script implements the analysis plan:
# 1. Run separate one-way MANOVAs for Gender and Entrepreneurial Inclination.
# 2. Use author-provided composite scale variables from the published dataset.
# 3. Check core MANOVA assumptions and flag multivariate outliers.
# 4. Follow omnibus MANOVAs and LDA summaries.

# -----------------------------
# 0. Packages and paths
# -----------------------------
packages <- c(
  "tidyverse", "psych", "car", "biotools", "MASS", "MVN"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(psych)
library(car)
library(biotools)
library(MASS)
library(MVN)

dat_raw <- read.csv("data/entrepreneurship.csv")

str(dat_raw)
dim(dat_raw)
names(dat_raw)

# -----------------------------
# 1. Recode grouping variables
# -----------------------------
# Coding follows the published article's demographic table:
# Gender: 1 = Male, 2 = Female
# Inclination: 1 = Yes/Inclined, 2 = No/Not inclined
# Age: 1 = Less than 22, 2 = 22 to 25, 3 = Above 25
# Class: 1 = UG, 2 = PG

dat <- dat_raw %>%
  mutate(
    Gender_f = factor(
      Gender,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    Inclination_f = factor(
      Inclination,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    Age_f = factor(
      Age,
      levels = c(1, 2, 3),
      labels = c("Less than 21", "22 to 25", "Above 25")
    ),
    Class_f = factor(
      Class,
      levels = c(1, 2),
      labels = c("UG", "PG")
    )
  )

table(dat$Gender_f, useNA = "ifany")
table(dat$Inclination_f, useNA = "ifany")
table(dat$Age_f, useNA = "ifany")
table(dat$Class_f, useNA = "ifany")

# Use the author-provided composite variables reported in the published dataset.
# These preserve the dataset's published scoring and descriptives.
dv_names <- c(
  "PBC",
  "Intention",
  "SSN",
  "Attitude",
  "Opportunity",
  "SocialCap",
  "Education",
  "Innova",
  "Locus",
  "Risk"
)

# Complete-case analysis dataset for MANOVA/LDA variables.
# The current dataset has no missing values, but this keeps all model-facing
# tables and tests aligned if missingness appears later.
dat_analysis <- dat %>%
  filter(if_all(all_of(c("Gender_f", "Inclination_f", dv_names)), ~ !is.na(.x)))

analysis_sample_summary <- tibble(
  full_sample_n = nrow(dat),
  analytic_sample_n = nrow(dat_analysis),
  excluded_from_analysis_n = nrow(dat) - nrow(dat_analysis)
)

print(analysis_sample_summary)

# check minimum sample size for analysis
table(dat_analysis$Gender_f)
table(dat_analysis$Inclination_f)

p <- length(dv_names)

min(table(dat_analysis$Gender_f)) > p
min(table(dat_analysis$Inclination_f)) > p

min(table(dat_analysis$Gender_f)) / p
min(table(dat_analysis$Inclination_f)) / p

# -----------------------------
# 2. Sample descriptives
# -----------------------------
freq_pct <- function(x) {
  tibble(value = x) %>%
    count(value, name = "n") %>%
    mutate(percent = 100 * n / sum(n))
}

sample_table <- bind_rows(
  freq_pct(dat$Gender_f) %>%
    mutate(Variable = "Gender"),
  freq_pct(dat$Age_f) %>%
    mutate(Variable = "Age group"),
  freq_pct(dat$Class_f) %>%
    mutate(Variable = "Level of education"),
  freq_pct(dat$Inclination_f) %>%
    mutate(Variable = "Entrepreneurial inclination")
) %>%
  rename(Category = value) %>%
  mutate(
    Percent = round(percent, 1),
    `n (%)` = paste0(n, " (", Percent, "%)")
  ) %>%
  dplyr::select(Variable, Category, n, Percent, `n (%)`)

print(sample_table, n = Inf)

# -----------------------------
# 3. DV descriptives and correlations
# -----------------------------
overall_desc <- psych::describe(dat_analysis[, dv_names]) %>%
  as.data.frame() %>%
  rownames_to_column("variable")

desc_gender <- dat_analysis %>%
  group_by(Gender_f) %>%
  summarise(
    across(
      all_of(dv_names),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

desc_incl <- dat_analysis %>%
  group_by(Inclination_f) %>%
  summarise(
    across(
      all_of(dv_names),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

cor_mat <- round(cor(dat_analysis[, dv_names], use = "complete.obs"), 2)

print(overall_desc)
print(desc_gender)
print(desc_incl)
print(cor_mat)

# -----------------------------
# 4. Assumption checks
# -----------------------------
# Box's M tests homogeneity of covariance matrices. Pillai's trace is retained
# as the primary omnibus statistic in the exported MANOVA tables.
boxM_gender <- biotools::boxM(dat_analysis[, dv_names], dat_analysis$Gender_f)
boxM_incl <- biotools::boxM(dat_analysis[, dv_names], dat_analysis$Inclination_f)

print(boxM_gender)
print(boxM_incl)

boxm_to_table <- function(boxm_obj, analysis_name) {
  tibble(
    Analysis = analysis_name,
    BoxM_chisq = as.numeric(boxm_obj$statistic),
    df = as.numeric(boxm_obj$parameter),
    p_value = boxm_obj$p.value,
    covariance_test_result = if_else(
      p_value < .05,
      "p < .05",
      "p >= .05"
    )
  )
}

boxm_table <- bind_rows(
  boxm_to_table(boxM_gender, "Gender"),
  boxm_to_table(boxM_incl, "Entrepreneurial Inclination")
)

print(boxm_table)

mvn_all <- mvn(
  data = dat_analysis[, dv_names],
  mvn_test = "mardia"
)

mvn_gender <- dat_analysis %>%
  group_by(Gender_f) %>%
  group_split() %>%
  setNames(levels(dat_analysis$Gender_f)) %>%
  map(~ mvn(data = .x[, dv_names], mvn_test = "mardia"))

mvn_inclination <- dat_analysis %>%
  group_by(Inclination_f) %>%
  group_split() %>%
  setNames(levels(dat_analysis$Inclination_f)) %>%
  map(~ mvn(data = .x[, dv_names], mvn_test = "mardia"))

mvn_to_table <- function(mvn_obj, analysis_name, group_name = "Overall") {
  mvn_obj$multivariate_normality %>%
    as_tibble() %>%
    mutate(
      Analysis = analysis_name,
      Group = group_name,
      .before = 1
    )
}

mvn_multivariate_table <- bind_rows(
  mvn_to_table(mvn_all, "Overall", "Overall"),
  imap_dfr(mvn_gender, ~ mvn_to_table(.x, "Gender", .y)),
  imap_dfr(mvn_inclination, ~ mvn_to_table(.x, "Entrepreneurial Inclination", .y))
)

print(mvn_multivariate_table)

# Multivariate outliers using Mahalanobis distance.
X <- as.matrix(dat_analysis[, dv_names])

mahal <- mahalanobis(
  X,
  center = colMeans(X),
  cov = cov(X)
)

mahal_cutoff <- qchisq(.999, df = length(dv_names))

outlier_cases <- dat_analysis %>%
  mutate(
    mahal = mahal,
    mahal_cutoff = mahal_cutoff,
    outlier = mahal > mahal_cutoff
  ) %>%
  filter(outlier) %>%
  arrange(desc(mahal))

outlier_summary <- tibble(
  n_cases = nrow(dat_analysis),
  n_outliers = nrow(outlier_cases),
  mahal_cutoff = mahal_cutoff,
  percent_outliers = 100 * n_outliers / n_cases
)

print(outlier_summary)
print(outlier_cases %>% 
        dplyr::select(ID, Gender_f, Inclination_f, mahal, mahal_cutoff))

# detect multicollinearity
R <- cor(dat_analysis[, dv_names], use = "complete.obs")

max_abs_r <- max(abs(R[upper.tri(R)]))

S_total <- cov(dat_analysis[, dv_names])
eigen_total <- eigen(S_total, symmetric = TRUE)$values

det_total <- det(S_total)
min_eigen_total <- min(eigen_total)
condition_number_total <- max(eigen_total) / min_eigen_total

multicollinearity_diag <- tibble(
  Analysis = "Overall covariance",
  max_abs_correlation = max_abs_r,
  determinant_cov = det_total,
  min_eigenvalue = min_eigen_total,
  condition_number = condition_number_total
)

pooled_cov <- function(data, group_var, dv_names) {
  groups <- split(data[, dv_names], data[[group_var]])
  ns <- sapply(groups, nrow)
  covs <- lapply(groups, cov)
  pooled <- Reduce(`+`, Map(function(S, n) (n - 1) * S, covs, ns)) /
    (sum(ns) - length(ns))
  pooled
}

S_pool_gender <- pooled_cov(dat_analysis, "Gender_f", dv_names)
S_pool_incl <- pooled_cov(dat_analysis, "Inclination_f", dv_names)

pooled_cov_diag <- function(S) {
  ev <- eigen(S, symmetric = TRUE)$values
  tibble(
    determinant_cov = det(S),
    min_eigenvalue = min(ev),
    condition_number = max(ev) / min(ev)
  )
}

pooled_cov_diag_gender <- pooled_cov_diag(S_pool_gender) %>%
  mutate(
    Analysis = "Gender pooled within-group covariance",
    max_abs_correlation = NA_real_,
    .before = 1
  )

pooled_cov_diag_incl <- pooled_cov_diag(S_pool_incl) %>%
  mutate(
    Analysis = "Inclination pooled within-group covariance",
    max_abs_correlation = NA_real_,
    .before = 1
  )

multicollinearity_table <- bind_rows(
  multicollinearity_diag,
  pooled_cov_diag_gender,
  pooled_cov_diag_incl
)

print(multicollinearity_table)

# -----------------------------
# 5. Helper functions for MANOVA and follow-up ANOVAs
# -----------------------------
extract_manova_test <- function(manova_obj, effect_name, analysis_name,
                                sample_name, test_name = "Pillai") {
  stats <- summary(manova_obj, test = test_name)$stats
  row <- stats[effect_name, ]

  stat_col <- switch(
    test_name,
    "Pillai" = "Pillai",
    "Wilks" = "Wilks",
    "Hotelling-Lawley" = "Hotelling-Lawley",
    "Roy" = "Roy"
  )

  tibble(
    Analysis = analysis_name,
    Sample = sample_name,
    Test = test_name,
    Statistic = as.numeric(row[stat_col]),
    approx_F = as.numeric(row["approx F"]),
    num_df = as.numeric(row["num Df"]),
    den_df = as.numeric(row["den Df"]),
    p_value = as.numeric(row["Pr(>F)"])
  )
}

extract_all_manova_tests <- function(manova_obj, effect_name, analysis_name,
                                     sample_name) {
  bind_rows(
    extract_manova_test(manova_obj, effect_name, analysis_name, sample_name, "Pillai"),
    extract_manova_test(manova_obj, effect_name, analysis_name, sample_name, "Wilks"),
    extract_manova_test(manova_obj, effect_name, analysis_name, sample_name, "Hotelling-Lawley"),
    extract_manova_test(manova_obj, effect_name, analysis_name, sample_name, "Roy")
  )
}

# Multivariate effect size for the omnibus MANOVA.
# For one-way, two-group MANOVAs, the effect df is 1. In that case,
# multivariate eta squared based on Wilks' lambda is 1 - lambda, which is
# numerically identical to Pillai's trace. 

# Archived, not reporting, reference only
# run_univariate_followups <- function(data, group_var, dv_names) {
#   lapply(dv_names, function(y) {
#     fit <- lm(reformulate(group_var, response = y), data = data)
#     aov_tab <- car::Anova(fit, type = 2)
# 
#     ss_effect <- aov_tab[group_var, "Sum Sq"]
#     ss_error <- aov_tab["Residuals", "Sum Sq"]
#     eta_p2 <- ss_effect / (ss_effect + ss_error)
# 
#     tibble(
#       outcome = y,
#       F_value = aov_tab[group_var, "F value"],
#       p_value = aov_tab[group_var, "Pr(>F)"],
#       partial_eta2 = eta_p2
#     )
#   }) %>%
#     bind_rows() %>%
#     mutate(
#       p_adjusted_bonferroni = p.adjust(p_value, method = "bonferroni")
#     )
# }
# 
# -----------------------------
# 6. One-way MANOVA for Gender
# -----------------------------
manova_gender <- manova(
  as.matrix(dat_analysis[, dv_names]) ~ Gender_f,
  data = dat_analysis
)

summary(manova_gender, test = "Pillai")
# summary(manova_gender, test = "Wilks")
# summary(manova_gender, test = "Hotelling-Lawley")
# summary(manova_gender, test = "Roy")

# summary_aov_gender <- summary.aov(manova_gender)
# print(summary_aov_gender)

# -----------------------------
# 7. One-way MANOVA for Entrepreneurial Inclination
# -----------------------------
manova_incl <- manova(
  as.matrix(dat_analysis[, dv_names]) ~ Inclination_f,
  data = dat_analysis
)

summary(manova_incl, test = "Pillai")
# summary(manova_incl, test = "Wilks")
# summary(manova_incl, test = "Hotelling-Lawley")
# summary(manova_incl, test = "Roy")
# 
# summary_aov_incl <- summary.aov(manova_incl)
# print(summary_aov_incl)

manova_tests_table <- bind_rows(
  extract_all_manova_tests(manova_gender, "Gender_f", "Gender", "Full sample"),
  extract_all_manova_tests(manova_incl, "Inclination_f", 
                           "Entrepreneurial Inclination", "Full sample")
)

manova_omnibus_pillai <- manova_tests_table %>%
  filter(Test == "Pillai")

print(manova_tests_table)
print(manova_omnibus_pillai)

# -----------------------------
# 8. Sensitivity analysis without multivariate outliers
# -----------------------------
outlier_ids <- outlier_cases$ID

dat_no_outliers <- dat_analysis %>%
  filter(!(ID %in% outlier_ids))

manova_gender_no_out <- manova(
  as.matrix(dat_no_outliers[, dv_names]) ~ Gender_f,
  data = dat_no_outliers
)

manova_incl_no_out <- manova(
  as.matrix(dat_no_outliers[, dv_names]) ~ Inclination_f,
  data = dat_no_outliers
)

summary(manova_gender_no_out, test = "Pillai")
summary(manova_incl_no_out, test = "Pillai")

sensitivity_table <- bind_rows(
  extract_manova_test(manova_gender, "Gender_f", "Gender", "Full sample", "Pillai"),
  extract_manova_test(
    manova_gender_no_out,
    "Gender_f",
    "Gender",
    "Without Mahalanobis outliers",
    "Pillai"
  ),
  extract_manova_test(
    manova_incl,
    "Inclination_f",
    "Entrepreneurial Inclination",
    "Full sample",
    "Pillai"
  ),
  extract_manova_test(
    manova_incl_no_out,
    "Inclination_f",
    "Entrepreneurial Inclination",
    "Without Mahalanobis outliers",
    "Pillai"
  )
)

print(sensitivity_table)

# -----------------------------
# 9. LDA follow-ups
# -----------------------------
# LDA summaries are computed for the grouping variables as descriptive MANOVA
# follow-ups, not as prediction/classification models.
#
# With two groups, each LDA has one function: LD1. The sign of LD1 is arbitrary;
# interpret variables mainly by absolute structure correlations and the relative
# positions of group centroids.

extract_lda_eigenvalues <- function(data, group_var, dv_names) {
  X <- as.matrix(data[, dv_names])
  group <- droplevels(data[[group_var]])
  group_levels <- levels(group)

  grand_mean <- colMeans(X)
  SW <- matrix(0, nrow = length(dv_names), ncol = length(dv_names))
  SB <- matrix(0, nrow = length(dv_names), ncol = length(dv_names))

  for (g in group_levels) {
    X_g <- X[group == g, , drop = FALSE]
    n_g <- nrow(X_g)
    mean_g <- colMeans(X_g)

    SW <- SW + (n_g - 1) * cov(X_g)

    mean_diff <- matrix(mean_g - grand_mean, ncol = 1)
    SB <- SB + n_g * (mean_diff %*% t(mean_diff))
  }

  eigen_values <- eigen(solve(SW, SB), only.values = TRUE)$values %>%
    Re()

  eigen_values[eigen_values < 0 & abs(eigen_values) < 1e-10] <- 0

  sort(eigen_values[eigen_values > 1e-10], decreasing = TRUE)
}

run_lda_followup <- function(data, group_var, dv_names, analysis_name) {
  lda_formula <- as.formula(
    paste(group_var, "~", paste(dv_names, collapse = " + "))
  )

  group <- droplevels(data[[group_var]])

  lda_fit <- MASS::lda(lda_formula, data = data)

  raw_coef <- lda_fit$scaling[, 1]
  centered_X <- scale(
    as.matrix(data[, dv_names]),
    center = colMeans(data[, dv_names]),
    scale = FALSE
  )
  ld1 <- as.vector(centered_X %*% raw_coef[dv_names])

  pooled_sd <- sapply(dv_names, function(v) {
    vars <- tapply(data[[v]], group, var)
    ns <- tapply(data[[v]], group, length)
    sqrt(sum((ns - 1) * vars) / (sum(ns) - length(ns)))
  })

  lda_coef_table <- tibble(
    Analysis = analysis_name,
    Outcome = dv_names,
    Raw_Coefficient = raw_coef[dv_names],
    Standardized_Coefficient = raw_coef[dv_names] * pooled_sd[dv_names],
    Structure_Correlation = sapply(
      dv_names,
      function(v) cor(data[[v]], ld1, use = "complete.obs")
    )
  ) %>%
    mutate(abs_structure = abs(Structure_Correlation)) %>%
    arrange(desc(abs_structure))

  lda_centroids <- tibble(
    Analysis = analysis_name,
    Group = names(tapply(ld1, group, mean)),
    LD1_Centroid = as.numeric(tapply(ld1, group, mean))
  )

  # Canonical correlation is computed from the discriminant eigenvalue/root:
  # r_c = sqrt(eigenvalue / (1 + eigenvalue)).
  lda_eigenvalues <- extract_lda_eigenvalues(data, group_var, dv_names)
  proportion_trace <- lda_eigenvalues[1] / sum(lda_eigenvalues)

  lda_function_summary <- tibble(
    Analysis = analysis_name,
    LD_Function = "LD1",
    Proportion_of_Trace = proportion_trace,
    Eigenvalue = lda_eigenvalues[1],
    Canonical_Correlation = sqrt(lda_eigenvalues[1] / (1 + lda_eigenvalues[1])),
    Prior_Probabilities = paste(
      paste(names(lda_fit$prior), round(lda_fit$prior, 3), sep = " = "),
      collapse = "; "
    )
  )

  list(
    fit = lda_fit,
    coefficients = lda_coef_table,
    centroids = lda_centroids,
    function_summary = lda_function_summary
  )
}

lda_gender <- run_lda_followup(
  data = dat_analysis,
  group_var = "Gender_f",
  dv_names = dv_names,
  analysis_name = "Gender"
)

lda_incl <- run_lda_followup(
  data = dat_analysis,
  group_var = "Inclination_f",
  dv_names = dv_names,
  analysis_name = "Entrepreneurial Inclination"
)

print(lda_gender$function_summary)
print(lda_gender$centroids)
print(lda_gender$coefficients)

print(lda_incl$function_summary)
print(lda_incl$centroids)
print(lda_incl$coefficients)
# -----------------------------
# 10. Additional sensitivity analyses for RQ2
# -----------------------------
# The following sensitivity checks are used to evaluate the stability of the
# entrepreneurial-inclination analysis under alternate analytic choices.

# # 10a. LDA follow-up without Mahalanobis outliers.
# # This mirrors the omnibus MANOVA sensitivity analysis and exports the same
# # descriptive LDA summaries used for the full-sample analysis.
# lda_incl_no_outliers <- run_lda_followup(
#   data = dat_no_outliers,
#   group_var = "Inclination_f",
#   dv_names = dv_names,
#   analysis_name = "Entrepreneurial Inclination Without Mahalanobis Outliers"
# )
# 
# print(lda_incl_no_outliers$coefficients)
# print(lda_incl_no_outliers$centroids)
# print(lda_incl_no_outliers$function_summary)

# 10b. Inclination MANOVA without entrepreneurial intention.
# This excludes the intention composite because it is conceptually close to the
# inclination grouping item.
dv_names_no_intention <- setdiff(dv_names, "Intention")

manova_incl_no_intention <- manova(
  as.matrix(dat_analysis[, dv_names_no_intention]) ~ Inclination_f,
  data = dat_analysis
)

summary(manova_incl_no_intention, test = "Pillai")
summary(manova_incl_no_intention, test = "Wilks")

manova_incl_no_intention_tests <- extract_all_manova_tests(
  manova_incl_no_intention,
  "Inclination_f",
  "Entrepreneurial Inclination",
  "Full sample without Intention DV"
)

print(manova_incl_no_intention_tests)

# 10c. Inclination LDA without entrepreneurial intention.
# This keeps the LDA follow-up aligned with the no-Intention MANOVA sensitivity
# analysis.
# lda_incl_no_intention <- run_lda_followup(
#   data = dat_analysis,
#   group_var = "Inclination_f",
#   dv_names = dv_names_no_intention,
#   analysis_name = "Entrepreneurial Inclination Without Intention DV"
# )
