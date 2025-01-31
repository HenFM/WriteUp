calculate_index_returns <- function(data, index_name, fund_name) {
    library(tidyverse)
    library(rmsfuns)
    library(glue)
    library(tbl2xts)


    if(fund_name == "SWIX"){
        # Filter data for the specified index
        index_wts <- data %>%
            filter(Index_Name == index_name) %>%
            filter(!ALSI ==is.na(ALSI)) %>%
            group_by(date) %>%
            mutate(fund_name_wts =  SWIX/ sum(SWIX)) %>%
            ungroup()}

    if(fund_name == "ALSI"){
        # Filter data for the specified index
        index_wts <- data %>%
            filter(Index_Name == index_name) %>%
            filter(!ALSI == is.na(ALSI)) %>%
            group_by(date) %>%
            mutate(fund_name_wts =  ALSI/ sum(ALSI)) %>%
            ungroup()}

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

    # Calculate Safe Portfolio returns
    portfolio_rts <- Safe_Return.portfolio(returns, wts, lag_weights = TRUE, contribution = TRUE, verbose = TRUE, value = 1000, geometric = TRUE)

    # Extract relevant components
    cont <- portfolio_rts$"contribution" %>% xts_tbl()
    BPwts <- portfolio_rts$"BOP.Weight" %>% xts_tbl()
    value <- portfolio_rts$BOP.Value %>% xts_tbl()

    # Bind all components together
    result <- left_join(
        index_wts %>% select(date, Tickers, Return),
        BPwts %>% gather(Tickers, weights, -date),
        by = c("date", "Tickers")
    ) %>%
        left_join(., value %>% gather(Tickers, value_held, -date), by = c("date", "Tickers")) %>%
        left_join(., cont %>% gather(Tickers, Contribution, -date), by = c("date", "Tickers")) %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return * weights, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    return(result)
}
