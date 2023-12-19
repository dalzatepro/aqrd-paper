library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(rstanarm)
library(gt)
library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(dotwhisker)

### Data Cleaning ---- 

# Read data 
aid_data <- read_dta("data/Final_Main.dta")

# Only keep obs in 1987 or greater
aid_data_miss <- aid_data |> 
  # (commenting out to replicate findings) mutate(across(everything(), ~ifelse(. == -99, NA, .))) |> 
  filter(year >= 1987)

# for descriptive table 
aid_data_miss_NA <- aid_data |> 
  mutate(across(everything(), ~ifelse(. == -99, NA, .))) |> 
  filter(year >= 1987)

# Creating dummy variables for missing values in polity2avg and new_empinxavg 
aid_data_miss <- aid_data_miss |> 
  mutate(polity2avg_ext = ifelse(is.na(polity2avg), -99, polity2avg), 
         new_empinxavg_ext = ifelse(is.na(new_empinxavg), -99, new_empinxavg))

aid_data_miss <- aid_data_miss |> 
  mutate(polity2avg_extF = ifelse(polity2avg_ext == -99, 1, 0), 
         new_empinxavg_extF = ifelse(new_empinxavg_ext == -99, 1, 0))

# Extracting all variables for analysis (with and without the "cov" prefix)
all_variables <- c("new_empinxavg", "new_empinxavg_ext", "new_empinxavg_extF", "polity2avg", "polity2avg_ext", "polity2avg_extF", "EV", "l2CPcol2",
                   grep("^cov", names(aid_data_miss), value = TRUE))

all_variables_NA <- c("new_empinxavg", "polity2avg", "EV", "l2CPcol2",
                   grep("^cov", names(aid_data_miss_NA), value = TRUE))

# Extracting only covariates
covars <- str_subset(colnames(aid_data_miss), "(^cov)")

# Extracting covariates, minus religiosity (low sample size)
covars_norel <- covars[covars != c("covwvs_rel", "covwvs_relF")] 

# Extracting covariates -- extension version (swapping GDP measures as DVs)
to_remove <- c("covloggdp", "covloggdpC", "covloggdpF", "covloggdpCF", "EV", "l2CPcol2", "new_empinxavg", "polity2avg")
covars_extension <- setdiff(all_variables, to_remove)

# Twice-lagging all covariates 
aid_data_miss <- aid_data_miss |> 
  group_by(ccode) |> 
  arrange(ccode, year)

for (var in c(covars)) {
  aid_data_miss <- aid_data_miss |> 
    mutate(
      (!!paste0("lag_", var)) := lag(!!as.name(var), 2))
  } |> 
  ungroup()

# Extracting lagged covariates 
lag_covars <- str_subset(colnames(aid_data_miss), "(^lag_cov)")
lag_covars_norel <- lag_covars[lag_covars != c("lag_covwvs_rel", "lag_covwvs_relF")] 

# Creating lead values for CIRI Human Empowerment Index and Polity IV Combined Score (up to t+5)

aid_data_miss <- aid_data_miss |> 
  group_by(ccode) |> 
  arrange(ccode, year) |> 
  mutate(
    polity2_lead1 = lead(polity2, 1),
    polity2_lead2 = lead(polity2, 2),
    polity2_lead3 = lead(polity2, 3), 
    polity2_lead4 = lead(polity2, 4),
    polity2_lead5 = lead(polity2, 5),
    new_empinx_lead1 = lead(new_empinx, 1),
    new_empinx_lead2 = lead(new_empinx, 2),
    new_empinx_lead3 = lead(new_empinx, 3),
    new_empinx_lead4 = lead(new_empinx, 4),
    new_empinx_lead5 = lead(new_empinx, 5),
  ) |> 
  ungroup()

# For extension: Create four-year averages for GDP, GDP per capita 

aid_data_miss <- aid_data_miss |>
  group_by(ccode) |> 
  arrange(ccode, year) |>
  mutate(
    avg_loggdp = ((covloggdp + lead(covloggdp, 1) + lead(covloggdp, 2) + lead(covloggdp, 3)) / 4), 
    avg_loggdpC = ((covloggdpC + lead(covloggdpC, 1) + lead(covloggdpC, 2) + lead(covloggdpC, 3)) / 4),
    ) |>
  ungroup()

# Replacing -99s with missing values in these new four-year GDP averages 
aid_data_miss$avg_loggdp[aid_data_miss$avg_loggdp == -99] <- NA
aid_data_miss$avg_loggdpC[aid_data_miss$avg_loggdpC == -99] <- NA

# For extension: Creating lead values for GDP, GDP per capita (up to t+5) (first creating new versions of these variables replacing -99 with NA)

aid_data_miss <- aid_data_miss |> 
  mutate(loggdp_miss = ifelse(covloggdp == -99, NA, covloggdp), 
         loggdpC_miss = ifelse(covloggdp == -99, NA, covloggdpC))

aid_data_miss <- aid_data_miss |> 
  group_by(ccode) |> 
  arrange(ccode, year) |> 
  mutate(
    loggdp_miss_lead1 = lead(loggdp_miss, 1),
    loggdp_miss_lead2 = lead(loggdp_miss, 2),
    loggdp_miss_lead3 = lead(loggdp_miss, 3), 
    loggdp_miss_lead4 = lead(loggdp_miss, 4),
    loggdp_miss_lead5 = lead(loggdp_miss, 5),
    loggdpC_miss_lead1 = lead(loggdpC_miss, 1),
    loggdpC_miss_lead2 = lead(loggdpC_miss, 2),
    loggdpC_miss_lead3 = lead(loggdpC_miss, 3),
    loggdpC_miss_lead4 = lead(loggdpC_miss, 4),
    loggdpC_miss_lead5 = lead(loggdpC_miss, 5),
  )

### Descriptive Table ---- 

# Create an empty tibble to store summary statistics
summary_tibble <- tibble(variable = character(), 
                         mean = numeric(), 
                         sd = numeric(), 
                         n = integer())

# Loop through each variable
for (var in all_variables_NA) {
  summary_stats <- aid_data_miss_NA |> 
    summarise(mean = mean(!!sym(var), na.rm = TRUE),
              sd = sd(!!sym(var), na.rm = TRUE),
              n = sum(!is.na(!!sym(var))))
  
  # Add the summary statistics to the tibble
  summary_tibble <- bind_rows(summary_tibble, 
                              tibble(variable = var, 
                                     mean = summary_stats$mean, 
                                     sd = summary_stats$sd, 
                                     n = summary_stats$n))
}

# Remove rows that end with "F" and clean variable names
summary_tibble_cleaned <- summary_tibble |> 
  filter(!grepl("F$", variable)) |> 
  mutate(variable = gsub("cov", "", variable))

# Change names of variables 

summary_tibble_cleaned <- summary_tibble_cleaned |> 
  mutate(
    variable = case_when(
      variable == "new_empinxavg" ~ "Human Empowerment Index, 4 yr avg",
      variable == "polity2avg" ~ "Polity IV combined score, 4 yr avg",
      variable == "EV" ~ "ODA net, millions of 1995 constant US dollars",
      variable == "l2CPcol2" ~ "Colony indicator variable (IV)",
      variable == "ihme_ayem" ~ "Avg Educational Attainment (Years)",
      variable == "wdi_exp" ~ "Exports (log, lagged)",
      variable == "wdi_imp" ~ "Imports (log, lagged)",
      variable == "wdi_fdi" ~ "FDI",
      variable == "wvs_rel" ~ "Religiosity",
      variable == "iNY_GDP_PETR_RT_ZS" ~ "Petroleum Imports (% GDP)",
      variable == "demregion" ~ "No. of democracies in region",
      variable == "loggdpC" ~ "GDP per capita (log)",
      variable == "loggdp" ~ "GDP (log)",
    )
  )

# Print the cleaned summary tibble
print(summary_tibble_cleaned)

## Formatting tibble into a nice table

gt_table <- summary_tibble_cleaned |> 
  gt() |> 
  fmt_number(decimals = 2) |>  
    cols_label(variable = "Variable", 
               mean = "Mean", 
               sd = "Standard Deviation", 
               n = "No. of Observations") |> 
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
     )

### Replicating Table 1 results ---- 


# Table 1, Column 1
ff1 <- new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2
fit1 <- feols(ff1, aid_data_miss, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Table 1, Column 2
ff2 <- glue("new_empinxavg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2") |> 
  as.formula()

fit2 <- feols(ff2, aid_data_miss,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

# Table 1, Column 3
ff3 <- polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2
fit3 <- feols(ff3, aid_data_miss, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Table 1, Column 4
ff4 <- glue("polity2avg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2") |>
  as.formula()

fit4 <- feols(ff4, aid_data_miss,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Results Summary
options(modelsummary_model_names = c("CIRI Human Empowerment Index", "CIRI Human Empowerment Index", "Polity IV Combined Score", "Polity IV Combined Score"))

ms <- modelsummary(list(fit1, fit2, fit3, fit4), 
             coef_omit = "^cov",
             coef_rename = ("fit_EV" = "Effect of Aid"),
             gof_map = c("nobs", "FE: ccode", "FE: year"), 
             add_rows = data.frame(
               "1" = "Covariates", 
               "2" = "No",
               "3" = "Yes",
               "4" = "No", 
               "5" = "Yes"),
             column_names = (c("one", "two", "three", "four")),
             # options(modelsummary_model_labels = "model"),
             title = "Replication of Carnegie and Marinov, 2017") 

### Extending Table 1 results ---- 

## Column 5 - log GDP (four-year avg), as DV, without covars
ff5 <- avg_loggdp ~ 1 | ccode + year | EV ~ l2CPcol2

fit5 <- feols(ff5, aid_data_miss, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Column 6 - log GDP (four-year avg), as DV, with covars
ff6 <- glue("avg_loggdp ~ {str_c(covars_extension, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2") |>
  as.formula()

fit6 <- feols(ff6, aid_data_miss,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Column 7 - log GDP per capita (four-year avg), as DV, without covars
ff7 <- avg_loggdpC ~ 1 | ccode + year | EV ~ l2CPcol2

fit7 <- feols(ff7, aid_data_miss, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Column 8 - log GDP per capita (four-year avg), as DV, with covars
ff8 <- glue("avg_loggdpC ~ {str_c(covars_extension, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2") |>
  as.formula()

fit8 <- feols(ff8, aid_data_miss,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Results Summary: Extended table

rowcovariates <- data.frame(
  "1" = "Covariates", 
  "2" = "No",
  "3" = "Yes",
  "4" = "No", 
  "5" = "Yes", 
  "6" = "No", 
  "7" = "Yes", 
  "8" = "No", 
  "9" = "Yes")

row_FEs <- data.frame(
  "1" = "Country and Time Fixed Effects", 
  "2" = "Yes",
  "3" = "Yes",
  "4" = "Yes", 
  "5" = "Yes", 
  "6" = "Yes", 
  "7" = "Yes", 
  "8" = "Yes", 
  "9" = "Yes")


rowsadd <- rbind(rowcovariates, row_FEs)
  

ms_extended <- modelsummary(list("(1) CIRI Index" = fit1, "(2) CIRI Index" = fit2, "(3) Polity IV" = fit3, "(4) Polity IV" = fit4, "(5) GDP (log)" = fit5, "(6) GDP (log)" = fit6, "(7) GDP per capita (log)" = fit7, "(8) GDP per capita (log)" = fit8), 
                   coef_omit = c(-1),
                   coef_rename = c('fit_EV' = 'Effect of EU Aid (log, IV estimate)'),
                   gof_map = c("nobs"), 
                   add_rows = rowsadd, 
                   stars = TRUE, 
                   notes = "Extension of Carnegie and Marinov, 2017. Each dependent variable is averaged over years t through t+3. Following covariates not shown in even columns: Avg. Years of Education, Log Exports, FDI, Log Imports, Religiosity, Petroleum Revenues, Democracies in Region, Log GDP and GDP Per capita (only for columns 1-4), CIRI and Polity IV indeces (only for columns 5-8). Dummies indicating missing values also not shown. Fixed effects held by country and year with robust standard errors.") 

print(ms_extended)

### Replicating Figure 1 results ---- 

## Regression specifications: CIRI (t until t+5)

# t = 0 
ciri_t <- feols(new_empinx ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t_tidy <- tidy(ciri_t)

ciri_t_tidy <- ciri_t_tidy |> 
  mutate(model = "t=0")

# t = 1 
ciri_t1 <- feols(new_empinx_lead1 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                cluster = ~ ccode + year,
                ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t1_tidy <- tidy(ciri_t1)

ciri_t1_tidy <- ciri_t1_tidy |> 
  mutate(model = "t=1")

# t = 2
ciri_t2 <- feols(new_empinx_lead2 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t2_tidy <- tidy(ciri_t2)

ciri_t2_tidy <- ciri_t2_tidy |> 
  mutate(model = "t=2")

# t = 3
ciri_t3 <- feols(new_empinx_lead3 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t3_tidy <- tidy(ciri_t3)

ciri_t3_tidy <- ciri_t3_tidy |> 
  mutate(model = "t=3")

# t = 4
ciri_t4 <- feols(new_empinx_lead4 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t4_tidy <- tidy(ciri_t4)

ciri_t4_tidy <- ciri_t4_tidy |> 
  mutate(model = "t=4")

# t = 5
ciri_t5 <- feols(new_empinx_lead5 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_t5_tidy <- tidy(ciri_t5)

ciri_t5_tidy <- ciri_t5_tidy |> 
  mutate(model = "t=5")

# Binding tables, CIRI
ciri_all_models <- bind_rows(ciri_t_tidy, ciri_t1_tidy, ciri_t2_tidy, ciri_t3_tidy, ciri_t4_tidy, ciri_t5_tidy)

ciri_all_models <- ciri_all_models |> 
  relabel_predictors(c(`fit_EV` = "Effect of Foreign Aid"))

# Coefficient plot, CIRI 

ciri_coefplot <- dwplot(ciri_all_models,
                        model_order = c("t=5", "t=4", "t=3", "t=2", "t=1", "t=0"),
                        vline = geom_vline(xintercept = 0)) + 
  coord_flip() +
  xlab("Coefficient on Foreign Aid (t-1, TSLS)") + 
  ylab("CITI, Years Forward") + 
  labs(caption = "Includes country and time fixed effects; other covariates excluded.") +
  theme(
    plot.title = element_text(size = 11),
    legend.title = element_blank(), 
    axis.title.y = element_text(size = 10)
  ) 

ggsave("figures/coefplotCIRI.png")

## Regression specifications, Polity IV 

# t = 0 
polity2_t <- feols(polity2 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                cluster = ~ ccode + year,
                ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t_tidy <- tidy(polity2_t)

polity2_t_tidy <- polity2_t_tidy |> 
  mutate(model = "t=0")

# t = 1 
polity2_t1 <- feols(polity2_lead1 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t1_tidy <- tidy(polity2_t1)

polity2_t1_tidy <- polity2_t1_tidy |> 
  mutate(model = "t=1")

# t = 2
polity2_t2 <- feols(polity2_lead2 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t2_tidy <- tidy(polity2_t2)

polity2_t2_tidy <- polity2_t2_tidy |> 
  mutate(model = "t=2")

# t = 3
polity2_t3 <- feols(polity2_lead3 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t3_tidy <- tidy(polity2_t3)

polity2_t3_tidy <- polity2_t3_tidy |> 
  mutate(model = "t=3")

# t = 4
polity2_t4 <- feols(polity2_lead4 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t4_tidy <- tidy(polity2_t4)

polity2_t4_tidy <- polity2_t4_tidy |> 
  mutate(model = "t=4")

# t = 5
polity2_t5 <- feols(polity2_lead5 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                 cluster = ~ ccode + year,
                 ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity2_t5_tidy <- tidy(polity2_t5)

polity2_t5_tidy <- polity2_t5_tidy |> 
  mutate(model = "t=5")

# Binding tables, Polity
polity2_all_models <- bind_rows(polity2_t_tidy, polity2_t1_tidy, polity2_t2_tidy, polity2_t3_tidy, polity2_t4_tidy, polity2_t5_tidy)

polity2_all_models <- polity2_all_models |> 
  relabel_predictors(c(`fit_EV` = "Effect of Foreign Aid"))

# Coefficient plot, Polity IV

polity2_coefplot <- dwplot(polity2_all_models,
                        model_order = c("t=5", "t=4", "t=3", "t=2", "t=1", "t=0"),
                        vline = geom_vline(xintercept = 0)) + 
  coord_flip() +
  xlab("Coefficient on Foreign Aid (t-1, TSLS)") + 
  ylab("Polity IV, Years Forward")  +
  labs(caption = "Includes country and time fixed effects; other covariates excluded.") +
  theme(
    plot.title = element_text(size = 11),
    legend.title = element_blank(), 
    axis.title.y = element_text(size = 10)
  ) 

ggsave("figures/coefplotPolity.png")

### Extending Figure 1 results ---- 

## Regression specifications, logGDP

# t = 0 
loggdp_t <- feols(loggdp_miss ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t_tidy <- tidy(loggdp_t)

loggdp_t_tidy <- loggdp_t_tidy |> 
  mutate(model = "t=0")

# t = 1 
loggdp_t1 <- feols(loggdp_miss_lead1 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                    cluster = ~ ccode + year,
                    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t1_tidy <- tidy(loggdp_t1)

loggdp_t1_tidy <- loggdp_t1_tidy |> 
  mutate(model = "t=1")

# t = 2
loggdp_t2 <- feols(loggdp_miss_lead2 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t2_tidy <- tidy(loggdp_t2)

loggdp_t2_tidy <- loggdp_t2_tidy |> 
  mutate(model = "t=2")

# t = 3
loggdp_t3 <- feols(loggdp_miss_lead3 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t3_tidy <- tidy(loggdp_t3)

loggdp_t3_tidy <- loggdp_t3_tidy |> 
  mutate(model = "t=3")

# t = 4
loggdp_t4 <- feols(loggdp_miss_lead4 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t4_tidy <- tidy(loggdp_t4)

loggdp_t4_tidy <- loggdp_t4_tidy |> 
  mutate(model = "t=4")

# t = 5
loggdp_t5 <- feols(loggdp_miss_lead5 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdp_t5_tidy <- tidy(loggdp_t5)

loggdp_t5_tidy <- loggdp_t5_tidy |> 
  mutate(model = "t=5")

# Binding tables, Log GDP
loggdp_all_models <- bind_rows(loggdp_t_tidy, loggdp_t1_tidy, loggdp_t2_tidy, loggdp_t3_tidy, loggdp_t4_tidy, loggdp_t5_tidy)

loggdp_all_models <- loggdp_all_models |> 
  relabel_predictors(c(`fit_EV` = "Effect of Foreign Aid"))

# Coefficient plot, LogGDP

loggdp_coefplot <- dwplot(loggdp_all_models,
                           model_order = c("t=5", "t=4", "t=3", "t=2", "t=1", "t=0"),
                           vline = geom_vline(xintercept = 0)) + 
  coord_flip() +
  xlab("Coefficient on Foreign Aid (t-1, TSLS)") + 
  ylab("Log GDP, Years Forward") + 
  labs(caption = "Includes country and time fixed effects; other covariates excluded.") +
  theme(
    plot.title = element_text(size = 11),
    legend.title = element_blank(), 
    axis.title.y = element_text(size = 10)
  ) 

ggsave("figures/coefplotLogGDP.png")

## Regression specifications, logGDP per capita

# t = 0 
loggdpC_t <- feols(loggdpC_miss ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                  cluster = ~ ccode + year,
                  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t_tidy <- tidy(loggdpC_t)

loggdpC_t_tidy <- loggdpC_t_tidy |> 
  mutate(model = "t=0")

# t = 1 
loggdpC_t1 <- feols(loggdpC_miss_lead1 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t1_tidy <- tidy(loggdpC_t1)

loggdpC_t1_tidy <- loggdpC_t1_tidy |> 
  mutate(model = "t=1")

# t = 2
loggdpC_t2 <- feols(loggdpC_miss_lead2 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t2_tidy <- tidy(loggdpC_t2)

loggdpC_t2_tidy <- loggdpC_t2_tidy |> 
  mutate(model = "t=2")

# t = 3
loggdpC_t3 <- feols(loggdpC_miss_lead3 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t3_tidy <- tidy(loggdpC_t3)

loggdpC_t3_tidy <- loggdpC_t3_tidy |> 
  mutate(model = "t=3")

# t = 4
loggdpC_t4 <- feols(loggdpC_miss_lead4 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t4_tidy <- tidy(loggdpC_t4)

loggdpC_t4_tidy <- loggdpC_t4_tidy |> 
  mutate(model = "t=4")

# t = 5
loggdpC_t5 <- feols(loggdpC_miss_lead5 ~ 1 | ccode + year | EV ~ l2CPcol2, aid_data_miss, 
                   cluster = ~ ccode + year,
                   ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

loggdpC_t5_tidy <- tidy(loggdpC_t5)

loggdpC_t5_tidy <- loggdpC_t5_tidy |> 
  mutate(model = "t=5")

# Binding tables, Log GDP
loggdpC_all_models <- bind_rows(loggdpC_t_tidy, loggdpC_t1_tidy, loggdpC_t2_tidy, loggdpC_t3_tidy, loggdpC_t4_tidy, loggdpC_t5_tidy)

loggdpC_all_models <- loggdpC_all_models |> 
  relabel_predictors(c(`fit_EV` = "Effect of Foreign Aid"))

# Coefficient plot, LogGDP

loggdpC_coefplot <- dwplot(loggdpC_all_models,
                          model_order = c("t=5", "t=4", "t=3", "t=2", "t=1", "t=0"),
                          vline = geom_vline(xintercept = 0)) + 
  coord_flip() +
  xlab("Coefficient on Foreign Aid (t-1, TSLS)") + 
  ylab("Log GDP per capita, Years Forward") + 
  labs(caption = "Includes country and time fixed effects; other covariates excluded.") +
  theme(
    plot.title = element_text(size = 11),
    legend.title = element_blank(), 
    axis.title.y = element_text(size = 10)
  ) 

ggsave("figures/coefplotLogGDPC.png")
