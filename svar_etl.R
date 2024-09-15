# Packages ----------------------------------------------------------------
if(require(deflateBR) == F) install.packages('deflateBR'); require(deflateBR)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ipeadatar) == F) install.packages('ipeadatar'); require(ipeadatar)
if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(lubridate) == F) install.packages('lubridate'); require(lubridate)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# 1. DATA EXTRACTION ---------------------------------------------------------
ipeadatar::search_series(language = "br") %>%
  filter(freq == "Mensal") %>% View()

# Over / SELIC
selic_over <- ipeadata("PAN12_TJOVER12")

# Exchange Rates
exchange_rate <- ipeadata("BM12_ERC12")

# Public Debt (%GDP)
debt_gdp <- ipeadata("PAN12_DTSPY12")

# Central Government Revenue (Tesouro Nacional)
rev <- read.csv("raw_data/receita_total.csv", check.names = F,
                sep = ";", dec = ",") %>% clean_names() %>%
  dplyr::select(data, valor) %>%
  mutate(data = dmy(data),
         valor = deflate(valor, data, "01/2010", "ipca")) %>%
  rename(date = data, revenue = valor)


# Central Government Expenditure (Tesouro Nacional)
exp <- read.csv("raw_data/despesa_total.csv", check.names = F,
                sep = ";", dec = ",") %>% clean_names() %>%
  dplyr::select(data, valor) %>%
  mutate(data = dmy(data),
         valor = deflate(valor, data, "01/2010", "ipca")) %>%
  rename(date = data, expenditure = valor)

# Inflation
ipca <- ipeadata("BM12_IPCACOM12")

# Terms of Trade Index
tot_idx <- read.csv("raw_data/tot_idx.csv") %>%
  clean_names() %>%
  filter(country_name == "Brazil",
         indicator_code == "xm", type_code == "R_RW_IX") %>%
  pivot_longer(cols = x1980m1:x2024m6, names_to = "date",
               values_to = "cmd_tot_idx") %>%
  dplyr::select(-c(country_name:x)) %>%
  mutate(date = ym(sub('x', '', date)))

# Commodity Prices Index (UNCTAD)
cmd_idx <- read.csv("raw_data/unctad_cmd_idx.csv") %>%
  clean_names() %>%
  filter(commodity == "IN0001.20") %>%
  mutate(date = ym(period)) %>%
  dplyr::select(date, index_base_2015) %>%
  rename(cmd_idx = index_base_2015)

# Meta SELIC
temp <- tempfile()
download.file("https://data.bis.org/static/bulk/WS_CBPOL_csv_col.zip",temp)
x <- read.csv(unz(temp, "WS_CBPOL_csv_col.csv"))
unlink(temp)

# FED (GS10)
link <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GS10&scale=left&cosd=1953-04-01&coed=2024-08-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-09-15&revision_date=2024-09-15&nd=1953-04-01"

fed_mrkt <-read.csv(link) %>%
  clean_names() %>%
  mutate(date = ymd(date))


# 2. DATA ENRICHMENT ---------------------------------------------------------
cbpol <- x %>% pivot_longer(cols = starts_with("X"),
                       names_to = "date",
                       values_to = "cbpol") %>%
  clean_names() %>%
  filter(reference_area %in% c('Brazil', "United States"),
         freq == "M",
         !is.na(cbpol)) %>%
  dplyr::select(-c(freq:ref_area, time_format:series)) %>%
  mutate(date = ym(sub('X', '', date))) %>%
  pivot_wider(names_from = reference_area,
              values_from = cbpol) %>%
  clean_names() %>%
  rename(meta_selic = brazil, fed_pol = united_states) %>%
  left_join(selic_over %>% rename(over_selic = value) %>%
              dplyr::select(date, over_selic)) %>%
  left_join(fed_mrkt) %>%
  left_join(exchange_rate %>% rename(exchange_rate = value) %>%
              dplyr::select(date, exchange_rate)) %>%
  left_join(debt_gdp %>% rename(debt_gdp = value) %>%
              dplyr::select(date, debt_gdp)) %>%
  left_join(tot_idx) %>%
  left_join(cmd_idx) %>%
  left_join(ipca %>% rename(ipca = value) %>%
              dplyr::select(date, ipca)) %>%
  left_join(rev) %>%
  left_join(exp) %>%
  arrange(date) %>% filter(date > '1990-12-01') %>%
  mutate(log_revenue = log(revenue),
         log_expenditure = log(expenditure),
         deficit = expenditure - revenue)

# 3. DATA EXPORT -------------------------------------------------------------
write_rds(cbpol, "final_data/dataset.rds")
