# Packages ----------------------------------------------------------------
if(require(urca) == F) install.packages('urca'); require(urca)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(lubridate) == F) install.packages('lubridate'); require(lubridate)
if(require(purrr) == F) install.packages('purrr'); require(purrr)
if(require(vars) == F) install.packages('vars'); require(vars)
if(require(tseries) == F) install.packages('tseries'); require(tseries)

# Conflicts
# dplyr::select, MASS:select
# dplyr::filter, stats::filter
# dplyr::lag, stats::lag

# STATIONARITY ------------------------------------------------------------
dataset <- readRDS("final_data/dataset.rds") %>%
  mutate(exchange_growth =
           ((exchange_rate - dplyr::lag(exchange_rate))*100
            /dplyr::lag(exchange_rate)),
         deficit_growth =
           ((deficit - dplyr::lag(deficit))*100
            /dplyr::lag(deficit)),
         revenue_growth =
           ((revenue - dplyr::lag(revenue))*100
            /dplyr::lag(revenue)),
         expenditure_growth =
           ((expenditure - dplyr::lag(expenditure))*100
            /dplyr::lag(expenditure)),
         d_fed_pol = fed_pol - dplyr::lag(fed_pol),
         d_over_selic = over_selic - dplyr::lag(over_selic),
         d_meta_selic = over_selic - dplyr::lag(meta_selic),
         d_debt_gdp = debt_gdp - dplyr::lag(debt_gdp),
         d_cmd_tot_idx = cmd_tot_idx - dplyr::lag(cmd_tot_idx),
         d_cmd_idx = cmd_idx - dplyr::lag(cmd_idx),
         lag_d_fed_pol = dplyr::lag(d_fed_pol),
         lag_d_cmd_idx = dplyr::lag(d_cmd_idx),
         election = if_else(
           lubridate::year(date) %in% c(seq(2002, 2018, by = 4)),
           1, 0)) %>%
  filter(date > "1999-05-01", date < "2020-01-01")

# STATIONARITY TESTS ------------------------------------------------------
apply_tests <- function(series) {
  lag_sel <- VARselect(series)
  adf_result <- adf.test(series, k = min(lag_sel$selection))
  pp_result <- pp.test(series)
  return(list(
    adf_lag_order = adf_result$parameter,
    adf_statistic = adf_result$statistic,
    adf_p_value = adf_result$p.value,
    pp_lag_order = pp_result$parameter,
    pp_statistic = pp_result$statistic,
    pp_p_value = pp_result$p.value
  ))
}

test_results <- lapply(dataset %>%
                         dplyr::select(-date), apply_tests)

results_df <- data.frame(
  variable = names(test_results),
  adf_lag_order = sapply(test_results, function(x) x$adf_lag_order),
  adf_statistic = sapply(test_results, function(x) x$adf_statistic),
  adf_p_value = sapply(test_results, function(x) x$adf_p_value),
  pp_lag_order = sapply(test_results, function(x) x$pp_lag_order),
  pp_statistic = sapply(test_results, function(x) x$pp_statistic),
  pp_p_value = sapply(test_results, function(x) x$pp_p_value)
) %>%
  mutate(adf_unit_root = case_when(
    adf_p_value < 0.01 ~ "***",
    adf_p_value < 0.05 & adf_p_value >= 0.01  ~ "**",
    adf_p_value < 0.10 & adf_p_value >= 0.05  ~ "*",
    adf_p_value >= 0.10 ~ "non-stationary"),
    pp_unit_root = case_when(
      pp_p_value < 0.01 ~ "***",
      pp_p_value < 0.05 & pp_p_value >= 0.01  ~ "**",
      pp_p_value < 0.10 & pp_p_value >= 0.05  ~ "*",
      pp_p_value >= 0.10 ~ "non-stationary"))

rownames(results_df) <- NULL

# VECTOR AUTOREGRESSION --------------------------------------------------
VARselect(dataset %>% dplyr::select(ipca, exchange_growth, deficit_growth,
          d_meta_selic),
          exogen =
            dataset %>% dplyr::select(d_fed_pol, d_cmd_idx,
                                      lag_d_fed_pol, lag_d_cmd_idx),
          type = "const")

VAR(y = dataset %>% dplyr::select(ipca, exchange_growth,
                           d_meta_selic),
    type = "const",
    exogen = dataset %>% dplyr::select(d_fed_pol, d_cmd_idx,
                                       lag_d_fed_pol, lag_d_cmd_idx),
    p = 1) %>% summary()

VAR(y = dataset %>% dplyr::select(ipca, exchange_growth,
                                  d_meta_selic),
    type = "const",
    exogen = dataset %>% dplyr::select(d_fed_pol, d_cmd_tot_idx,
                                       lag_d_fed_pol),
    p = 1) %>% summary()
