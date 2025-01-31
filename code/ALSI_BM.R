Fund_Size_at_Start <- 1000

Rand_weights <- ALSI_bm %>%
    select(date, Tickers, reweight) %>%
    pivot_wider(names_from = "Tickers", values_from = "reweight") %>% tbl_xts()

df_Returns <- ALSI_bm %>%
    select(date, Tickers, Return) %>%
    pivot_wider(names_from = "Tickers", values_from = "Return")

df_Returns[is.na(df_Returns)] <- 0
xts_df_Returns <- df_Returns %>% tbl_xts()

Rand_RetPort <-
    rmsfuns::Safe_Return.portfolio(xts_df_Returns,

                                   weights = Rand_weights, lag_weights = TRUE,

                                   verbose = TRUE, contribution = TRUE,

                                   value = Fund_Size_at_Start, geometric = TRUE)


index_wts <- df_raw1 %>%
    filter(!ALSI ==is.na(ALSI)) %>%
    group_by(date) %>%
    mutate(fund_name_wts =  ALSI/ sum(ALSI)) %>%
    ungroup()

# Separate weights for the specified fund
wts <- index_wts %>%
    select(date, Tickers, fund_name_wts) %>%
    mutate(fund_name_wts = coalesce(fund_name_wts, 0)) %>%
    spread(Tickers, fund_name_wts) %>%
    tbl_xts()
wts[is.na(wts)] <- 0

# Create an xts data frame for returns
returns <- index_wts %>%
    select(date, Tickers, Return) %>%
    spread(Tickers, Return) %>%
    tbl_xts()
returns[is.na(returns)] <- 0

portfolio_rts <- Safe_Return.portfolio(returns, wts, lag_weights = TRUE,
                                       contribution = TRUE, verbose = TRUE, value = 1000, geometric = TRUE)


# Extract relevant components
cont <- portfolio_rts$"contribution" %>% xts_tbl()
BPwts <- portfolio_rts$"BOP.Weight" %>% xts_tbl()
value <- portfolio_rts$BOP.Value %>% xts_tbl()

# Bind all components together
ALSI_BM <- left_join(
    index_wts %>% select(date, Tickers, Return),
    BPwts %>% gather(Tickers, weights, -date),
    by = c("date", "Tickers")
) %>%
    left_join(., value %>% gather(Tickers, value_held, -date), by = c("date", "Tickers")) %>%
    left_join(., cont %>% gather(Tickers, Contribution, -date), by = c("date", "Tickers")) %>%
    group_by(date) %>%
    summarise(PortfolioReturn = sum(Return * weights, na.rm = TRUE)) %>%
    filter(PortfolioReturn != 0) %>%
    rename(ALSI = PortfolioReturn)


