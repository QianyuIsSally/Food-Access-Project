# Load libraries 
library(dplyr) ## For data wrangling
devtools::install_github("sarahlotspeich/auditDesignR") ## Run this once
library(auditDesignR) ## For different sampling designs
library(ggplot2)
devtools::install_github("sarahlotspeich/possum") ## Run this once 
library(possum) ## For the MLE with covariate misclassification

# Read in data from GitHub 
food_dat <- read.csv(file = "https://raw.githubusercontent.com/sarahlotspeich/food_access_imputation/refs/heads/main/piedmont-triad-data/analysis_data.csv") |> 
  select(GEOID, CountyName, Xstar, X_full, O_POP, Y_OBESITY, METRO) |> ## Keep only the most relevant variables (for now)
  rename(TRACT_ID = GEOID, ## Rename columns to be more informative 
         COUNTY = CountyName, 
         PROXIMITY_STRAIGHT = Xstar, 
         PROXIMITY_MAP = X_full)

# Task 1: Fit the "Gold Standard" Poisson regression model using Y_OBESITY ~ PROXIMITY_MAP + log(O_POP)
gold_standard_pmodel <- glm(Y_OBESITY ~ PROXIMITY_MAP + offset(log(O_POP)), 
                           data = food_dat, 
                           family = poisson)

# Task 2: Fit the "Naive" Poisson regression model using Y_OBESITY ~ PROXIMITY_STRAIGHT + log(POP)
naive_pmodel <- glm(Y_OBESITY ~ PROXIMITY_STRAIGHT + offset(log(O_POP)), 
                            data = food_dat, 
                            family = poisson)

# Task 3: Build a data visualization comparing PROXIMITY_MAP vs. PROXIMITY_STRAIGHT
# both are numeric, scatter plot used
ggplot(food_dat, aes(x = PROXIMITY_STRAIGHT, y = PROXIMITY_MAP)) +
  geom_point(alpha = 0.6, color = "blue") +  
  labs(title = "Comparison of PROXIMITY_MAP vs. PROXIMITY_STRAIGHT",
       x = "PROXIMITY_STRAIGHT",
       y = "PROXIMITY_MAP") +
  theme_minimal()

# Task 4: Simple random sample validated
## Be reproducible 
## assume we only know the  PROXIMITY_MAP for 48 neighborhoods, randomly selected using sample_srs()
set.seed(205)
food_dat$QUERIED <- sample_srs(phI = nrow(food_dat), 
                               phII = 48)
food_dat <- food_dat |> 
  mutate(PROXIMITY_MAP_PARTIAL = if_else(condition = QUERIED == 1, 
                                         true = PROXIMITY_MAP, 
                                         false = NA))

# Task 5: Fit the "Complete Case" using Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + log(POP)
# fit poisson model using only these 48 neighborhoods, estimate relationship between obesity and proximity_map_partial
complete_case_pmodel <- glm(Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), 
                            data = food_dat, 
                            family = poisson)

# Task 6: Fit the "Multiple Imputation" analysis 
# impute missing data using PROXIMITY_STRAIGHT
imputation_pmodel <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, ## X ~ X* + log(Y)
                               analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), ## Y ~ X + log(Offset)
                               data = food_dat, 
                               B = 20)

# Task 7: Try other sampling designs 
## Task 7a: Extreme tail sampling on X* (take 24 with the smallest X* (PROXIMITY_STRAIGHT) and 24 with the largest)
### i don't know why length(food_dat$QUERIED_ETS_XSTAR) = 16. it's supposed to be 48
food_dat <- food_dat |>
  ## sort data from smallest --> largest X*
  arrange(PROXIMITY_STRAIGHT) |>
  mutate(RANK_XSTAR = 1:n()) |> ## create a rank column 
  mutate(QUERIED_ETS_XSTAR = as.numeric(RANK_XSTAR <= 24 | RANK_XSTAR >= nrow(food_dat)-23))  ## define query indicator 
  
## Task 7b: Extreme tail sampling on Y/O_POP (take 24 with the smallest and 24 with the largest)
food_dat <- food_dat |>
  mutate(OBESITY_PREVALENCE = Y_OBESITY / O_POP) |>
  arrange(OBESITY_PREVALENCE) |>
  mutate(RANK_PREV = 1:n()) |>
  mutate(QUERIED_ETS_PREV = as.numeric(RANK_PREV <= 24 | RANK_PREV >= nrow(food_dat)-23))

## Task 7c: Balanced case-control on X* above/below median and Y/O_POP above/below median 
food_dat <- food_dat |> 
  mutate(PROXIMITY_STRAIGHT_CAT = as.numeric(PROXIMITY_STRAIGHT > median(PROXIMITY_STRAIGHT)), 
         OBESITY_PREV_CAT = as.numeric((Y_OBESITY / O_POP) > median((Y_OBESITY / O_POP))))
with(food_dat, table(PROXIMITY_STRAIGHT_CAT, OBESITY_PREV_CAT))
food_dat$QUERIED_BCC <- sample_bcc(dat = food_dat, 
                                   phI = nrow(food_dat), 
                                   phII = 48, 
                                   sample_on = c("PROXIMITY_STRAIGHT_CAT", "OBESITY_PREV_CAT"))

## Task 7d: Residual sampling on naive model Y ~ X* + log(offset) 
food_dat$QUERIED_RESID <- sample_resid(formula = Y_OBESITY ~ PROXIMITY_STRAIGHT + offset(log(O_POP)), 
                                       family = "poisson", 
                                       dat = food_dat, 
                                       phI = nrow(food_dat), 
                                       phII = 48)

# Task 8: Re-fit the multiple imputation model using each of the designs (SRS, Tasks 7a -- 7d)
set.seed(416)
reps = 100
plot_data = data.frame()
for (r in 1:reps) {
  food_dat$QUERIED <- sample_srs(phI = nrow(food_dat), 
                                 phII = 48)
  food_dat <- food_dat |> 
    mutate(PROXIMITY_MAP_PARTIAL = if_else(condition = QUERIED == 1, 
                                           true = PROXIMITY_MAP, 
                                           false = NA))
  
  # Task 7: Try other sampling designs 
  ## Task 7a: Extreme tail sampling on X* (take 24 with the smallest X* (PROXIMITY_STRAIGHT) and 24 with the largest)
  ### i don't know why length(food_dat$QUERIED_ETS_XSTAR) = 16. it's supposed to be 48
  food_dat <- food_dat |>
    ## sort data from smallest --> largest X*
    arrange(PROXIMITY_STRAIGHT) |>
    mutate(RANK_XSTAR = 1:n()) |> ## create a rank column 
    mutate(QUERIED_ETS_XSTAR = as.numeric(RANK_XSTAR <= 24 | RANK_XSTAR >= nrow(food_dat)-23))  ## define query indicator 
  
  ## Task 7b: Extreme tail sampling on Y/O_POP (take 24 with the smallest and 24 with the largest)
  food_dat <- food_dat |>
    mutate(OBESITY_PREVALENCE = Y_OBESITY / O_POP) |>
    arrange(OBESITY_PREVALENCE) |>
    mutate(RANK_PREV = 1:n()) |>
    mutate(QUERIED_ETS_PREV = as.numeric(RANK_PREV <= 24 | RANK_PREV >= nrow(food_dat)-23))
  
  ## Task 7c: Balanced case-control on X* above/below median and Y/O_POP above/below median 
  food_dat <- food_dat |> 
    mutate(PROXIMITY_STRAIGHT_CAT = as.numeric(PROXIMITY_STRAIGHT > median(PROXIMITY_STRAIGHT)), 
           OBESITY_PREV_CAT = as.numeric((Y_OBESITY / O_POP) > median((Y_OBESITY / O_POP))))
  with(food_dat, table(PROXIMITY_STRAIGHT_CAT, OBESITY_PREV_CAT))
  food_dat$QUERIED_BCC <- sample_bcc(dat = food_dat, 
                                     phI = nrow(food_dat), 
                                     phII = 48, 
                                     sample_on = c("PROXIMITY_STRAIGHT_CAT", "OBESITY_PREV_CAT"))
  
  ## Task 7d: Residual sampling on naive model Y ~ X* + log(offset) 
  food_dat$QUERIED_RESID <- sample_resid(formula = Y_OBESITY ~ PROXIMITY_STRAIGHT + offset(log(O_POP)), 
                                         family = "poisson", 
                                         dat = food_dat, 
                                         phI = nrow(food_dat), 
                                         phII = 48)
  
  ## SRS: 
  imputation_SRS <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT,
                              analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)),
                              data = food_dat |> mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED == 1, PROXIMITY_MAP, NA)),
                              B = 20)
  
  ## errors in 7a and 7b, the rest is ok
  ## design 7a: Extreme tail sampling on X* (take 24 with the smallest X* (PROXIMITY_STRAIGHT) and 24 with the largest)
  imputation_ETS_XSTAR <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, 
                                    analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), 
                                    ## use query indicator
                                    data = food_dat |> 
                                      mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_ETS_XSTAR == 1, PROXIMITY_MAP, false = NA_real_)), 
                                    B = 20)
  
  ## design 7b: Extreme tail sampling on Y/O_POP (take 24 with the smallest and 24 with the largest)
  imputation_ETS_PREV <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, 
                                   analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), 
                                   ## use query indicator
                                   data = food_dat |> 
                                     mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_ETS_PREV == 1, PROXIMITY_MAP, false = NA_real_)), 
                                   B = 20)
  ## design 7c: Balanced case-control on X* above/below median and Y/O_POP above/below median 
  imputation_BCC <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, ## X ~ X* + log(Y)
                              analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), ## Y ~ X + log(Offset)
                              ## use query indicator
                              data = food_dat |> 
                                mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_BCC ==1, PROXIMITY_MAP,false = NA)), 
                              B = 20)
  ## design 7d: Residual sampling on naive model Y ~ X* + log(offset)
  imputation_RESID <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, ## X ~ X* + log(Y)
                                analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), ## Y ~ X + log(Offset)
                                data = food_dat |> 
                                  mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_RESID ==1, PROXIMITY_MAP,false = NA)), 
                                B = 20)
  
  # Task 9: Make a plot to compare coefficient estimates and confidence intervals between designs 
  ## Suggestion: Put the design on the x-axis, a point at the coefficient estimate, and an error bar with the 95% CI
  
  # Create a dataset that stacks all 5 of the designs
  plot_data = imputation_SRS |> 
    dplyr::mutate(Design = "SRS") |>
    dplyr::bind_rows(
      imputation_ETS_XSTAR |> 
        dplyr::mutate(Design = "ETS (X*)")
    ) |> 
    dplyr::bind_rows(
      imputation_ETS_PREV |> 
        dplyr::mutate(Design = "ETS (Y/POP)")
    ) |> 
    dplyr::bind_rows(
      imputation_BCC |> 
        dplyr::mutate(Design = "BCC*")
    ) |> 
    dplyr::bind_rows(
      imputation_RESID |> 
        dplyr::mutate(Design = "ETS (Residual)*")
    ) |> 
    dplyr::bind_rows(plot_data)
}

# Create plot from it 
plot_data |> 
  dplyr::filter(Coefficient == "PROXIMITY_MAP_PARTIAL") |> 
  ggplot(aes(x = Design, y = Estimate, color = Design)) + ## initialize plot
  geom_boxplot() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") + 
  geom_hline(yintercept = gold_standard_pmodel$coefficients[2], linetype = "dashed", color = "orange") + 
  theme_minimal() + ## change the theme 
  scale_color_discrete(guide = "none") + 
  labs(x = "Design", ## add labels to the x
       y = TeX("$\\hat{\\beta}$")) ## fill legend

## SRS: 
imputation_SRS <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT,
  analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)),
  data = food_dat |> mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED == 1, PROXIMITY_MAP, NA)),
  B = 20)

## errors in 7a and 7b, the rest is ok
## design 7a: Extreme tail sampling on X* (take 24 with the smallest X* (PROXIMITY_STRAIGHT) and 24 with the largest)
imputation_ETS_XSTAR <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, 
                               analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), 
                               ## use query indicator
                               data = food_dat |> 
                                 mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_ETS_XSTAR == 1, PROXIMITY_MAP, false = NA_real_)), 
                               B = 20)
                              
## design 7b: Extreme tail sampling on Y/O_POP (take 24 with the smallest and 24 with the largest)
imputation_ETS_PREV <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, 
                               analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), 
                               ## use query indicator
                               data = food_dat |> 
                                 mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_ETS_PREV == 1, PROXIMITY_MAP, false = NA_real_)), 
                               B = 20)
## design 7c: Balanced case-control on X* above/below median and Y/O_POP above/below median 
imputation_BCC <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, ## X ~ X* + log(Y)
                               analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), ## Y ~ X + log(Offset)
                               ## use query indicator
                               data = food_dat |> 
                                 mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_BCC ==1, PROXIMITY_MAP,false = NA)), 
                               B = 20)
## design 7d: Residual sampling on naive model Y ~ X* + log(offset)
imputation_RESID <- impPossum(imputation_formula = PROXIMITY_MAP_PARTIAL ~ PROXIMITY_STRAIGHT, ## X ~ X* + log(Y)
                               analysis_formula = Y_OBESITY ~ PROXIMITY_MAP_PARTIAL + offset(log(O_POP)), ## Y ~ X + log(Offset)
                              data = food_dat |> 
                                mutate(PROXIMITY_MAP_PARTIAL = if_else(QUERIED_RESID ==1, PROXIMITY_MAP,false = NA)), 
                              B = 20)

# Task 9: Make a plot to compare coefficient estimates and confidence intervals between designs 
## Suggestion: Put the design on the x-axis, a point at the coefficient estimate, and an error bar with the 95% CI

# Create a dataset that stacks all 5 of the designs
plot_data = imputation_SRS |> 
  dplyr::mutate(Design = "SRS") |>
  dplyr::bind_rows(
    imputation_ETS_XSTAR |> 
      dplyr::mutate(Design = "ETS (X*)")
  ) |> 
  dplyr::bind_rows(
    imputation_ETS_PREV |> 
      dplyr::mutate(Design = "ETS (Y/POP)")
  ) |> 
  dplyr::bind_rows(
    imputation_BCC |> 
      dplyr::mutate(Design = "BCC*")
  ) |> 
  dplyr::bind_rows(
    imputation_RESID |> 
      dplyr::mutate(Design = "ETS (Residual)*")
  )
  
# Create plot from it 
plot_data |> 
  dplyr::filter(Coefficient == "PROXIMITY_MAP_PARTIAL") |> 
  ggplot(aes(x = Design, y = Estimate, color = Design)) + ## initialize plot
  geom_point() + 
  geom_errorbar(aes(ymin = (Estimate - 1.96 * Standard.Error), ymax = (Estimate + 1.96 * Standard.Error))) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") + 
  geom_hline(yintercept = gold_standard_pmodel$coefficients[2], linetype = "dashed", color = "orange") + 
  theme_minimal() + ## change the theme 
  scale_color_discrete(guide = "none") + 
  labs(x = "Design", ## add labels to the x
       y = TeX("$\\hat{\\beta}$ (95\\% CI)")) ## fill legend

# Task 10: Create visualizations of which neighborhoods get queried under different designs
## Suggestion / Example: ETS on X* below 
# Example: ETS on X* below 
# food_dat |> 
#  arrange(PROXIMITY_STRAIGHT) |> ## sort data from smallest --> largest X*
#  mutate(RANK_XSTAR = 1:n()) |> ## create a rank column 
#  mutate(QUERIED_ETS_XSTAR = RANK_XSTAR <= 12 | RANK_XSTAR >= 376) |> ## define query indicator |> 
#  ggplot(aes(x = RANK_XSTAR, y = PROXIMITY_STRAIGHT, color = QUERIED_ETS_XSTAR)) + 
#  geom_point() + 
# geom_vline(xintercept = c(12, 376), linetype = 2, color = "grey")

## 7a: QUERIED_ETS_XSTAR, RANK_XSTAR
food_dat |> 
  arrange(PROXIMITY_STRAIGHT) |> ## sort data from smallest --> largest X*
  mutate(RANK_XSTAR = 1:n(),
         QUERIED_ETS_XSTAR = RANK_XSTAR <= 24 | RANK_XSTAR >= (nrow(food_dat)-23)) |> ## create a rank column 
  ggplot(aes(x = RANK_XSTAR, y = PROXIMITY_STRAIGHT, color = QUERIED_ETS_XSTAR)) + 
  geom_point() + 
  geom_vline(xintercept = c(24, 364), linetype = 2, color = "grey")

## 7b: QUERIED_ETS_PREV, RANK_PREV
food_dat |> 
  arrange(Y_OBESITY / O_POP) |> 
  mutate(RANK_PREV = 1:n(),
         QUERIED_ETS_PREV = RANK_PREV <= 24 | RANK_PREV >= (nrow(food_dat) - 23)) |> 
  ggplot(aes(x = RANK_PREV, y = Y_OBESITY / O_POP, color = QUERIED_ETS_PREV)) +
  geom_point() +
  geom_vline(xintercept = c(24, nrow(food_dat) - 23), linetype = 2, color = "grey") +
  theme_minimal()

## 7c： Balanced case-control on X* above/below median and Y/O_POP above/below median 
food_dat |> 
  mutate(PROXIMITY_STRAIGHT_CAT = as.numeric(PROXIMITY_STRAIGHT > median(PROXIMITY_STRAIGHT)), 
         OBESITY_PREV_CAT = as.numeric((Y_OBESITY / O_POP) > median((Y_OBESITY / O_POP))))
with(food_dat, table(PROXIMITY_STRAIGHT_CAT, OBESITY_PREV_CAT)) |> 
  ggplot(aes(x = RANK_PREV, y = Y_OBESITY / O_POP, color = QUERIED_BCC)) +
  geom_point() +
  geom_vline(xintercept = c(24, nrow(food_dat) - 23), linetype = 2, color = "grey") +
  theme_minimal()

food_dat <- food_dat |> 
  mutate(PROXIMITY_STRAIGHT_CAT = as.numeric(PROXIMITY_STRAIGHT > median(PROXIMITY_STRAIGHT)), 
         OBESITY_PREV_CAT = as.numeric((Y_OBESITY / O_POP) > median((Y_OBESITY / O_POP))))
with(food_dat, table(PROXIMITY_STRAIGHT_CAT, OBESITY_PREV_CAT))
food_dat$QUERIED_BCC <- sample_bcc(dat = food_dat, 
                                   phI = nrow(food_dat), 
                                   phII = 48, 
                                   sample_on = c("PROXIMITY_STRAIGHT_CAT", "OBESITY_PREV_CAT"))

## 7d: Residual sampling on naive model of Y/POP ~ X* 
food_dat$PRED_CASES = exp(predict(naive_pmodel))
food_dat$RESID = food_dat$Y_OBESITY - food_dat$PRED_CASES
food_dat |> 
  arrange(RESID) |> 
  mutate(RANK_RESID = 1:n(),
         QUERIED_ETS_RESID = RANK_RESID <= 24 | RANK_RESID >= (nrow(food_dat) - 23)) |> 
  ggplot(aes(x = RANK_RESID, y = RESID, color = QUERIED_ETS_RESID)) +
  geom_point() +
  geom_vline(xintercept = c(24, nrow(food_dat) - 23), linetype = 2, color = "grey") +
  theme_minimal()
