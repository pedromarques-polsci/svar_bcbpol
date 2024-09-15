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
         d_gs10 = gs10 - dplyr::lag(gs10),
         election = if_else(
           lubridate::year(date) %in% c(seq(2002, 2018, by = 4)),
           1, 0),
         subprime_crisis = if_else(
           lubridate::year(date) %in% c(2007, 2008),
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

## Endogenous VAR ---------------------------------------------------------
VARselect(dataset %>% dplyr::select(ipca, exchange_growth,
                                    meta_selic, d_debt_gdp),
          type = "const")

var1 <- VAR(y = dataset %>% dplyr::select(ipca, exchange_growth,
                                  meta_selic, d_debt_gdp),
    type = "const",
    p = 2)

var1 %>% summary()

plot(irf(var1, n.ahead = 10, impulse = "meta_selic",
         response = "ipca", ci = 0.90))

plot(irf(var1, n.ahead = 12, impulse = "exchange_growth",
         response = "ipca", ci = 0.95))

plot(irf(var1, n.ahead = 12, impulse = "d_debt_gdp",
         response = "meta_selic", ci = 0.95))

plot(irf(var1, n.ahead = 12, impulse = "d_debt_gdp",
         response = "ipca", ci = 0.95))

VARselect(dataset %>% dplyr::select(ipca, d_debt_gdp),
          type = "const")

var2 <- VAR(y = dataset %>% dplyr::select(ipca, d_debt_gdp),
            type = "const",
            p = 3)

var2 %>% summary()

plot(irf(var2, n.ahead = 10, impulse = "d_debt_gdp",
         response = "ipca", ci = 0.95))

plot(irf(var2, n.ahead = 10, impulse = "ipca",
         response = "d_debt_gdp", ci = 0.95))

VARselect(dataset %>% dplyr::select(ipca, exchange_growth, expenditure_growth,
                                    meta_selic, d_debt_gdp, revenue_growth),
          type = "const")

var3 <- VAR(y = dataset %>%
              dplyr::select(ipca, exchange_growth, expenditure_growth,
                            meta_selic, d_debt_gdp, revenue_growth),
            type = "const",
            p = 2)

var3 %>% summary()

plot(irf(var3, n.ahead = 10, impulse = "expenditure_growth",
         response = "ipca", ci = 0.95))

plot(irf(var3, n.ahead = 10, impulse = "revenue_growth",
         response = "ipca", ci = 0.95))

plot(irf(var3, n.ahead = 10, impulse = "ipca",
         response = "d_debt_gdp", ci = 0.95))

plot(irf(var3, n.ahead = 10, impulse = "meta_selic",
         response = "ipca", ci = 0.95))

# XVAR - Commodity Prices and Terms of Trade ------------------------------
VARselect(dataset %>% dplyr::select(ipca, exchange_growth, meta_selic,
                                    d_debt_gdp, revenue_growth,
                                    expenditure_growth),
          exogen = dataset %>% dplyr::select(d_cmd_idx),
          type = "const")

varx1 <- VAR(y = dataset %>% dplyr::select(ipca, exchange_growth,
                                  meta_selic, d_debt_gdp, revenue_growth,
                                  expenditure_growth),
    exogen = dataset %>% dplyr::select(d_cmd_idx),
    type = "const",
    p = 2)

varx1 %>% summary()

plot(irf(varx1, n.ahead = 10, impulse = "exchange_growth",
         response = "ipca", ci = 0.95))

plot(irf(varx1, n.ahead = 10, impulse = "meta_selic",
         response = "ipca", ci = 0.90))

plot(irf(varx1, n.ahead = 10, impulse = "revenue_growth",
         response = "ipca", ci = 0.90))

plot(irf(varx1, n.ahead = 10, impulse = "expenditure_growth",
         response = "ipca", ci = 0.90))

plot(irf(varx1, n.ahead = 10, impulse = "d_debt_gdp",
         response = "ipca", ci = 0.90))

varx2 <- VAR(y = dataset %>% dplyr::select(ipca, exchange_growth,
                                  meta_selic, d_debt_gdp,
                                  revenue_growth,
                                  expenditure_growth),
    exogen = dataset %>% dplyr::select(d_cmd_tot_idx),
    type = "const",
    p = 2)

varx2 %>% summary()

# STRUCTURAL VAR ----------------------------------------------------------
amat <- diag(3)
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA

## Terms of Trade -> Exchange Rates -> Inflation --------------------------
sdata1 <- dataset %>% dplyr::select(d_cmd_tot_idx, exchange_growth, ipca)

VARselect(sdata1, type="both")
std_var1 <- VAR(sdata1, exogen =
                  cbind(subprime_crisis = dataset$subprime_crisis,
                        fed_pol = dataset$fed_pol), p = 1)

svar1 <- SVAR(std_var1, Amat = amat, Bmat = NULL,
              estmethod = c("scoring", "direct"))

svar1

plot(irf(svar1, n.ahead = 10, impulse = "d_cmd_tot_idx",
         response = "d_cmd_tot_idx", ci = 0.95))

plot(irf(svar1, n.ahead = 10, impulse = "d_cmd_tot_idx",
         response = "exchange_growth", ci = 0.95))

plot(irf(svar1, n.ahead = 10, impulse = "d_cmd_tot_idx",
         response = "exchange_growth", ci = 0.95))

plot(irf(svar1, n.ahead = 10, impulse = "exchange_growth",
         response = "ipca", ci = 0.95))

## Fed Policy -> SELIC -> Inflation -----------------------------------
sdata2 <- dataset %>% dplyr::select(fed_pol, meta_selic, ipca)

VARselect(sdata2, type="both",
          exogen =
            cbind(subprime_crisis = dataset$subprime_crisis,
                  d_cmd_tot_idx = dataset$d_cmd_tot_idx))

std_var2 <- VAR(sdata2,
                exogen = cbind(subprime_crisis = dataset$subprime_crisis,
                        dataset$d_cmd_tot_idx),
                p = 3)

svar2 <- SVAR(std_var2, Amat = amat, Bmat = NULL,
              estmethod = c("scoring", "direct"))

svar2

plot(irf(svar2, n.ahead = 10, impulse = "fed_pol",
         response = "fed_pol", ci = 0.95))

plot(irf(svar2, n.ahead = 10, impulse = "fed_pol",
         response = "meta_selic", ci = 0.95))

plot(irf(svar2, n.ahead = 10, impulse = "meta_selic",
         response = "ipca", ci = 0.95))

## SELIC -> Debt -> Inflation -----------------------------------
sdata3 <- dataset %>% dplyr::select(meta_selic, d_debt_gdp, ipca)

VARselect(sdata3, type="both",
          exogen =
            cbind(subprime_crisis = dataset$subprime_crisis,
                  d_cmd_tot_idx = dataset$d_cmd_tot_idx))

std_var3 <- VAR(sdata3,
                exogen = cbind(subprime_crisis = dataset$subprime_crisis,
                               dataset$d_cmd_tot_idx),
                p = 3)

svar3 <- SVAR(std_var3, Amat = amat, Bmat = NULL,
              estmethod = c("scoring", "direct"))

svar3

plot(irf(svar3, n.ahead = 10, impulse = "meta_selic",
         response = "meta_selic", ci = 0.95))

plot(irf(svar3, n.ahead = 10, impulse = "meta_selic",
         response = "d_debt_gdp", ci = 0.95))

plot(irf(svar3, n.ahead = 10, impulse = "d_debt_gdp",
         response = "ipca", ci = 0.95))

## Terms of Trade -> ... -> SELIC -----------------------------------------
amat4 <- diag(4)
amat4[2,1] <- NA
amat4[3,1] <- NA
amat4[3,2] <- NA
amat4[4,1] <- NA
amat4[4,2] <- NA
amat4[4,3] <- NA

sdata4 <- dataset %>% dplyr::select(d_cmd_tot_idx, exchange_growth,
                                    ipca, meta_selic)

VARselect(sdata4, type="both",
          exogen =
            cbind(subprime_crisis = dataset$subprime_crisis,
                  d_fed_pol = dataset$d_fed_pol))

std_var4 <- VAR(sdata4,
                exogen =
                  cbind(subprime_crisis = dataset$subprime_crisis,
                        d_fed_pol = dataset$d_fed_pol),
                p = 2)

svar4 <- SVAR(std_var4, Amat = amat4, Bmat = NULL,
              estmethod = c("scoring", "direct"))

svar4

plot(irf(svar4, n.ahead = 10, impulse = "d_cmd_tot_idx",
         response = "d_cmd_tot_idx", ci = 0.95))

plot(irf(svar4, n.ahead = 10, impulse = "d_cmd_tot_idx",
         response = "exchange_growth", ci = 0.95))

plot(irf(svar4, n.ahead = 10, impulse = "exchange_growth",
         response = "ipca", ci = 0.95))

plot(irf(svar4, n.ahead = 10, impulse = "ipca",
         response = "meta_selic", ci = 0.95))
