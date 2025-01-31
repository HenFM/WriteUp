

Moments_Comp <- function(funds, BM, Yrs_LookBack, NA_Check ){

    # BM <- "BM"
    # funds <-  Funds
    # Yrs_LookBack <-  5
    # NA_Check <-  0.8

    funds_considered <-
        funds %>% filter(date >= fmxdat::safe_year_min(datesel = last(date), N = Yrs_LookBack))

    Funds_Cons <-
        funds_considered %>%
        group_by(Tickers) %>%
        summarise(N_noNA = sum(!is.na(Ret)) / length(unique(funds_considered$date)) ) %>%
        filter(N_noNA > NA_Check) %>% pull(Tickers)

    Fundxts <-
        funds_considered %>% filter(Tickers %in% Funds_Cons) %>%
        tbl_xts(cols_to_xts = Ret, spread_by = Tickers, Colnames_Exact = T)

    BMxts <-
        funds_considered %>% filter(Tickers %in% BM) %>%
        tbl_xts(cols_to_xts = Ret, Colnames_Exact = T)


    Moms <-
        bind_rows(
            data.frame(Return.cumulative(Fundxts) ) %>% round(., 3),
            data.frame(Return.annualized(Fundxts, scale = 12, geometric = T)) %>% round(., 3),
            data.frame(PerformanceAnalytics::Return.annualized.excess(Fundxts, BMxts) ) %>% round(., 3),
            data.frame(sd.annualized(Fundxts, scale = 12, geometric = T)) %>% round(., 3),

            data.frame(PerformanceAnalytics::AdjustedSharpeRatio( Fundxts ) ) %>% round(., 3),
            data.frame(PainIndex(Fundxts, scale = 12, geometric = T)) %>% round(., 3),
            data.frame(AverageDrawdown(Fundxts, scale = 12)) %>% round(., 3),

            data.frame(fmxdat::Safe_TE(Ra = Fundxts, Rb = BMxts, scale = 12)) %>% round(., 3),
            data.frame(PerformanceAnalytics::InformationRatio(Ra = Fundxts, Rb = BMxts)) %>% round(., 3),
            data.frame(PerformanceAnalytics::CAPM.beta(Ra = Fundxts, Rb = BMxts, Rf = 0)) %>% round(., 3),
            data.frame(PerformanceAnalytics::CAPM.beta.bull(Ra = Fundxts, Rb = BMxts, Rf = 0)) %>% round(., 3),
            data.frame(PerformanceAnalytics::CAPM.beta.bear(Ra = Fundxts, Rb = BMxts, Rf = 0)) %>% round(., 3),
            data.frame(PerformanceAnalytics::UpDownRatios(Ra = Fundxts, Rb = BMxts, method = "Percent", side = "Up")) %>% round(., 3),
            data.frame(PerformanceAnalytics::CVaR(R = Fundxts, p = 0.05, method = "modified")) %>% round(., 3)
        ) %>%

        tibble::rownames_to_column("Info") %>%
        mutate(Period = glue::glue("Last {Yrs_LookBack} Years"), Info = c("Cum Returns", "Returns (Ann.)", "Returns Excess (Ann.)", "SD (Ann.)", "Adj. Sharpe Ratio", "Pain Index",
                                                                          "Avg DD", "Tracking Error", "Information Ratio", "Beta", "Beta Bull", "Beta Bear", "Up-Down Ratio", "Modified CVaR")) %>%
        relocate(Period, .before = Info) %>% as_tibble()

    # This line replaces the `.` with a space.
    # Note the forward slashes, as `.` there means everything, `\\.` means a full-stop
    colnames(Moms) <- gsub("\\.", " ", colnames(Moms))

    Moms
}


