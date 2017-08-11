Technicals - Summary
=====================================  
    
    Inputs {.sidebar}
-------------------------------------
    
    ```{r page3 sidebar}

## Add checkboxes
dateRangeInput("daterange4", "Data Date Range:",
               start = Sys.Date()-254,
               end = Sys.Date())

textInput("inText3","Ticker Search:","")

actionButton("clickMe3", label = "Search")

##############################################################

```

Column
-------------------------------------
    
    ### Future Return Distribution
    
    ```{r - return dist}

dist_summ_plt_ret <- eventReactive(input$clickMe3,{
    
    ticker <- toupper(input$inText3)
    start <- input$daterange4[1]
    end <- input$daterange4[2]
    
    input <- tq_get(ticker, get = "stock.prices", from = start, to = end)
    input <- input[!is.na(input$adjusted),]
    #add technical indicators
    try(input$ma50 <- SMA(input$adjusted,50),silent=TRUE)
    try(input$ma21 <- SMA(input$adjusted,21),silent=TRUE)
    try(input$ma9 <- SMA(input$adjusted,9),silent=TRUE)
    try(bbands <- as.data.frame(BBands(input$adjusted, n=20,sd=2)),silent=TRUE)
    try(bbands <- bbands[,c("up","dn")],silent = TRUE)
    try(colnames(bbands) <- c("bb_up","bb_dn"))
    try(input <- cbind(input,bbands),silent = TRUE)
    try(input$RSI <- RSI(input$adjusted))
    try(macd <- as.data.frame(MACD(input$adjusted)))
    try(input <- cbind(input,macd),silent = TRUE)
    try(input$macd_delt <- input$macd-input$signal)
    
    input2 = input
    
    # Percentiles for Moving Averages
    ########################
    perc.rank <- function(x) trunc(rank(x,na.last = NA))/sum(!is.na(x))
    
    #9 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma9)/input2$ma9)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma9_q <- xr
    
    #21 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma21)/input2$ma21)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma21_q <- xr
    
    #50 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma50)/input2$ma50)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma50_q <- xr
    
    #BB-up - ptile
    xr <- perc.rank((input2$adjusted-input2$bb_up)/input2$bb_up)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$bb_up_q <- xr
    
    #BB-dn - ptile
    xr <- perc.rank((input2$adjusted-input2$bb_dn)/input2$bb_dn)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$bb_dn_q <- xr
    
    #RSI - ptile
    xr <- perc.rank((input2$RSI))
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$RSI_q <- xr
    
    #MACD - ptile
    xr <- perc.rank((input2$macd_delt))
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$macd_delt_q <- xr
    
    
    #create q_tile bins
    # q_names <- c("ma9_q","ma21_q","ma50_q","bb_up_q","bb_dn_q","RSI_q","macd_delt_q")
    # 
    q_cuts <- seq(0,1,.2)
    
    input2$ma9_q_bins <- cut(input2$ma9_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$ma21_q_bins <- cut(input2$ma21_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$ma50_q_bins <- cut(input2$ma50_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$bb_up_q_bins <- cut(input2$bb_up_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$bb_dn_q_bins <- cut(input2$bb_dn_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$RSI_q_bins <- cut(input2$RSI_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$macd_delt_q_bins <- cut(input2$macd_delt_q,q_cuts,right=FALSE,include.lowest = TRUE)
    
    
    #future returns
    input2$fwd_5_return = lead(input2$adjusted,n=5)
    input2$fwd_10_return = lead(input2$adjusted,n=10)
    input2$fwd_15_return = lead(input2$adjusted,n=15)
    input2$fwd_20_return = lead(input2$adjusted,n=20)
    input2$fwd_30_return = lead(input2$adjusted,n=30)
    input2$fwd_40_return = lead(input2$adjusted,n=40)
    input2$fwd_50_return = lead(input2$adjusted,n=50)
    input2$fwd_60_return = lead(input2$adjusted,n=60)
    input2$fwd_70_return = lead(input2$adjusted,n=70)
    input2$fwd_80_return = lead(input2$adjusted,n=80)
    input2$fwd_90_return = lead(input2$adjusted,n=90)
    input2$fwd_100_return = lead(input2$adjusted,n=100)
    # Create dist summary table
    ############################
    input2 %>% 
        select(ends_with("_q")) -> q_names
    q_names <- colnames(q_names)
    
    first_q = q_names[1]
    for(q in q_names){
        q_cuts = seq(0,1,.2)
        x <- input2[,q]
        input2$bins = cut(x,q_cuts)
        input2 %>% 
            group_by(bins) %>%
            summarise(r5 = round(mean((fwd_5_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p5 = round(sum(ifelse(fwd_5_return>adjusted,1,0),na.rm=TRUE)/
                                     sum(ifelse(!is.na(fwd_5_return),1,0)),4),
                      r10 = round(mean((fwd_10_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p10 = round(sum(ifelse(fwd_10_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_10_return),1,0)),4),
                      r15 = round(mean((fwd_15_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p15 = round(sum(ifelse(fwd_15_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_15_return),1,0)),4),
                      r20 = round(mean((fwd_20_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p20 = round(sum(ifelse(fwd_20_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_20_return),1,0)),4),
                      r30 = round(mean((fwd_30_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p30 = round(sum(ifelse(fwd_30_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_30_return),1,0)),4),
                      r40 = round(mean((fwd_40_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p40 = round(sum(ifelse(fwd_40_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_40_return),1,0)),4),
                      r50 = round(mean((fwd_50_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p50 = round(sum(ifelse(fwd_50_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_50_return),1,0)),4),
                      r60 = round(mean((fwd_60_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p60 = round(sum(ifelse(fwd_60_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_60_return),1,0)),4),
                      r70 = round(mean((fwd_70_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p70 = round(sum(ifelse(fwd_70_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_70_return),1,0)),4),
                      r80 = round(mean((fwd_80_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p80 = round(sum(ifelse(fwd_80_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_80_return),1,0)),4),
                      r90 = round(mean((fwd_90_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p90 = round(sum(ifelse(fwd_90_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_90_return),1,0)),4),
                      r100 = round(mean((fwd_100_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p100 = round(sum(ifelse(fwd_100_return>adjusted,1,0),na.rm=TRUE)/
                                       sum(ifelse(!is.na(fwd_100_return),1,0)),4)
            ) -> dist_tbl
        
        dist_tbl$indicator = q
        dist_tbl %>% select(indicator, bins, everything()) -> dist_tbl
        dist_tbl %>% filter(!is.na(bins)) -> dist_tbl
        
        if (q==first_q){
            dist_tbl_all = dist_tbl
        } else {
            dist_tbl_all = rbind(dist_tbl_all,dist_tbl)
        }
    }
    
    dist_tbl_all$metric <- paste(dist_tbl_all$indicator,dist_tbl_all$bins)
    
    # dist_tbl_all %>% select("metric",
    #     "r5","r10",
    #     "r15","r20",
    #     "r30","r40",
    #     "r50","r60",
    #     "r70","r80",
    #     "r90","r100"
    # ) -> dist_tbl_all_returns
    
    dist_tbl_all %>% select(metric,
                            r5,r10,
                            r15,r20,
                            r30,r40,
                            r50,r60,
                            r70,r80,
                            r90,r100
    ) -> dist_tbl_all_returns
    
    dist_tbl_all2_returns <- dist_tbl_all_returns %>% gather(days, performance, r5:r100)
    dist_tbl_all2_returns$days <- factor(dist_tbl_all2_returns$days, levels=c("r5","r10",
                                                                              "r15","r20","r30","r40","r50","r60","r70","r80","r90","r100"))
    
    dist_tbl_all2_returns$ret_bins <- cut(dist_tbl_all2_returns$performance,
                                          breaks = c(-Inf,-.2,-.1,-.05,0,.05,.1,.2,Inf))
    
    g <- ggplot(data = dist_tbl_all2_returns, aes(x = days, y = metric)) + 
        geom_tile(aes(fill = ret_bins), color = "white", size = 1) + 
        # scale_fill_brewer(palette = "RdYlGn") +
        scale_fill_manual(breaks=c("\\[-Inf,-.2)", "\\[-.2,-.1)", "\\[-.1,-.05)", 
                                   "\\[-.05,0)", "\\[0,.05)", "\\[.05,.1)", 
                                   "\\[.1,.2)", "\\[.2,Inf)"),
                          values = c("red4", "red1", "tomato1",
                                     "orange", "yellow", "lightgreen",
                                     "springgreen4", "darkgreen")) +
        # scale_fill_gradient(low = "red", high = "green") + 
        xlab("Future Days") + ylab("") + theme_grey(base_size = 10) + 
        # ggtitle("Future Return Distribution") + 
        theme(axis.ticks = element_blank(), 
              panel.background = element_blank(), 
              plot.title = element_text(size = 12, colour = "gray50"),
              axis.text=element_text(size=12))
    
    metrics_df <- as.data.frame(unique(dist_tbl_all_returns$metric))
    colnames(metrics_df) <- "metric_name"
    metrics_df <- metrics_df %>% arrange(metric_name)
    metrics_splitted <- data.frame(do.call('rbind', strsplit(as.character(metrics_df$metric_name),' ',fixed=TRUE)))
    colnames(metrics_splitted) <- c("metric","bin")
    metrics_splitted$bin <- gsub("\\(","",metrics_splitted$bin)
    metrics_splitted$bin <- gsub("]","",metrics_splitted$bin)
    metrics_splitted$match <- NA
    
    curr_conditions <- input2[nrow(input2),c("ma9_q_bins","ma21_q_bins","ma50_q_bins","bb_up_q_bins","bb_dn_q_bins","RSI_q_bins","macd_delt_q_bins")]
    curr_conditions <- as.data.frame(t(curr_conditions))
    colnames(curr_conditions) <- c("bin_current")
    rownames(curr_conditions) <- gsub("_bins","",rownames(curr_conditions))
    
    metrics_splitted$match <- curr_conditions$bin_current[match(metrics_splitted$metric,rownames(curr_conditions))]
    metrics_splitted$match <- gsub("\\[","",metrics_splitted$match)
    metrics_splitted$match <- gsub("\\)","",metrics_splitted$match)
    metrics_splitted$match <- gsub("]","",metrics_splitted$match)
    metrics_splitted$match2 = ifelse(metrics_splitted$match==metrics_splitted$bin,1,NA)
    
    
    for (i in 1:nrow(metrics_splitted)) {
        if (!is.na(metrics_splitted$match2[i])){
            # g <- g + geom_text(x = .5, y = i, label = paste("*"),color="red")  
            g <- g + annotate("segment", x = .5, xend = 12.5, y = i-.5, yend = i-.5,
                              colour = "black", size = 1.5)
            g <- g + annotate("segment", x = .5, xend = 12.5, y = i+.5, yend = i+.5,
                              colour = "black", size = 1.5)
        }
    }
    
    g
    
})


renderPlot({
    dist_summ_plt_ret()
})


```

### Future Positive Return Distribution

```{r - prop diest}

dist_summ_plt_pos <- eventReactive(input$clickMe3,{
    
    ticker <- toupper(input$inText3)
    start <- input$daterange4[1]
    end <- input$daterange4[2]
    
    input <- tq_get(ticker, get = "stock.prices", from = start, to = end)
    input <- input[!is.na(input$adjusted),]
    #add technical indicators
    try(input$ma50 <- SMA(input$adjusted,50),silent=TRUE)
    try(input$ma21 <- SMA(input$adjusted,21),silent=TRUE)
    try(input$ma9 <- SMA(input$adjusted,9),silent=TRUE)
    try(bbands <- as.data.frame(BBands(input$adjusted, n=20,sd=2)),silent=TRUE)
    try(bbands <- bbands[,c("up","dn")],silent = TRUE)
    try(colnames(bbands) <- c("bb_up","bb_dn"))
    try(input <- cbind(input,bbands),silent = TRUE)
    try(input$RSI <- RSI(input$adjusted))
    try(macd <- as.data.frame(MACD(input$adjusted)))
    try(input <- cbind(input,macd),silent = TRUE)
    try(input$macd_delt <- input$macd-input$signal)
    
    input2 = input
    
    # Percentiles for Moving Averages
    ########################
    perc.rank <- function(x) trunc(rank(x,na.last = NA))/sum(!is.na(x))
    
    #9 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma9)/input2$ma9)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma9_q <- xr
    
    #21 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma21)/input2$ma21)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma21_q <- xr
    
    #50 day - ptile
    xr <- perc.rank((input2$adjusted-input2$ma50)/input2$ma50)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$ma50_q <- xr
    
    #BB-up - ptile
    xr <- perc.rank((input2$adjusted-input2$bb_up)/input2$bb_up)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$bb_up_q <- xr
    
    #BB-dn - ptile
    xr <- perc.rank((input2$adjusted-input2$bb_dn)/input2$bb_dn)
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$bb_dn_q <- xr
    
    #RSI - ptile
    xr <- perc.rank((input2$RSI))
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$RSI_q <- xr
    
    #MACD - ptile
    xr <- perc.rank((input2$macd_delt))
    xr_add <- rep(NA,nrow(input2)-length(xr))
    xr <- c(xr_add,xr)
    input2$macd_delt_q <- xr
    
    
    q_cuts <- seq(0,1,.2)
    
    input2$ma9_q_bins <- cut(input2$ma9_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$ma21_q_bins <- cut(input2$ma21_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$ma50_q_bins <- cut(input2$ma50_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$bb_up_q_bins <- cut(input2$bb_up_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$bb_dn_q_bins <- cut(input2$bb_dn_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$RSI_q_bins <- cut(input2$RSI_q,q_cuts,right=FALSE,include.lowest = TRUE)
    input2$macd_delt_q_bins <- cut(input2$macd_delt_q,q_cuts,right=FALSE,include.lowest = TRUE)
    
    
    #future returns
    input2$fwd_5_return = lead(input2$adjusted,n=5)
    input2$fwd_10_return = lead(input2$adjusted,n=10)
    input2$fwd_15_return = lead(input2$adjusted,n=15)
    input2$fwd_20_return = lead(input2$adjusted,n=20)
    input2$fwd_30_return = lead(input2$adjusted,n=30)
    input2$fwd_40_return = lead(input2$adjusted,n=40)
    input2$fwd_50_return = lead(input2$adjusted,n=50)
    input2$fwd_60_return = lead(input2$adjusted,n=60)
    input2$fwd_70_return = lead(input2$adjusted,n=70)
    input2$fwd_80_return = lead(input2$adjusted,n=80)
    input2$fwd_90_return = lead(input2$adjusted,n=90)
    input2$fwd_100_return = lead(input2$adjusted,n=100)
    # Create dist summary table
    ############################
    input2 %>% 
        select(ends_with("_q")) -> q_names
    q_names <- colnames(q_names)
    
    first_q = q_names[1]
    for(q in q_names){
        q_cuts = seq(0,1,.2)
        x <- input2[,q]
        input2$bins = cut(x,q_cuts)
        input2 %>% 
            group_by(bins) %>%
            summarise(r5 = round(mean((fwd_5_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p5 = round(sum(ifelse(fwd_5_return>adjusted,1,0),na.rm=TRUE)/
                                     sum(ifelse(!is.na(fwd_5_return),1,0)),4),
                      r10 = round(mean((fwd_10_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p10 = round(sum(ifelse(fwd_10_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_10_return),1,0)),4),
                      r15 = round(mean((fwd_15_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p15 = round(sum(ifelse(fwd_15_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_15_return),1,0)),4),
                      r20 = round(mean((fwd_20_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p20 = round(sum(ifelse(fwd_20_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_20_return),1,0)),4),
                      r30 = round(mean((fwd_30_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p30 = round(sum(ifelse(fwd_30_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_30_return),1,0)),4),
                      r40 = round(mean((fwd_40_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p40 = round(sum(ifelse(fwd_40_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_40_return),1,0)),4),
                      r50 = round(mean((fwd_50_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p50 = round(sum(ifelse(fwd_50_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_50_return),1,0)),4),
                      r60 = round(mean((fwd_60_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p60 = round(sum(ifelse(fwd_60_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_60_return),1,0)),4),
                      r70 = round(mean((fwd_70_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p70 = round(sum(ifelse(fwd_70_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_70_return),1,0)),4),
                      r80 = round(mean((fwd_80_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p80 = round(sum(ifelse(fwd_80_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_80_return),1,0)),4),
                      r90 = round(mean((fwd_90_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p90 = round(sum(ifelse(fwd_90_return>adjusted,1,0),na.rm=TRUE)/
                                      sum(ifelse(!is.na(fwd_90_return),1,0)),4),
                      r100 = round(mean((fwd_100_return-adjusted)/adjusted,na.rm=TRUE),4),
                      p100 = round(sum(ifelse(fwd_100_return>adjusted,1,0),na.rm=TRUE)/
                                       sum(ifelse(!is.na(fwd_100_return),1,0)),4)
            ) -> dist_tbl
        
        dist_tbl$indicator = q
        dist_tbl %>% select(indicator, bins, everything()) -> dist_tbl
        dist_tbl %>% filter(!is.na(bins)) -> dist_tbl
        
        if (q==first_q){
            dist_tbl_all = dist_tbl
        } else {
            dist_tbl_all = rbind(dist_tbl_all,dist_tbl)
        }
    }
    
    dist_tbl_all$metric <- paste(dist_tbl_all$indicator,dist_tbl_all$bins)
    
    # dist_tbl_all %>% select("metric",
    #     "p5","p10",
    #     "p15","p20",
    #     "p30","p40",
    #     "p50","p60",
    #     "p70","p80",
    #     "p90","p100"
    # ) -> dist_tbl_all_proportions
    
    dist_tbl_all %>% select(metric,
                            p5,p10,
                            p15,p20,
                            p30,p40,
                            p50,p60,
                            p70,p80,
                            p90,p100
    ) -> dist_tbl_all_proportions
    
    dist_tbl_all2_props <- dist_tbl_all_proportions %>% gather(days, performance, p5:p100)
    dist_tbl_all2_props$days <- factor(dist_tbl_all2_props$days, levels=c("p5","p10",
                                                                          "p15","p20","p30","p40","p50","p60","p70","p80","p90","p100"))
    
    g <- ggplot(data = dist_tbl_all2_props, aes(x = days, y = metric)) + 
        geom_tile(aes(fill = performance), color = "white", size = 1) + 
        scale_fill_gradient(low = "red", high = "green") + 
        xlab("future days") + ylab("") +
        theme_grey(base_size = 10) + 
        # ggtitle("Future Positive Return Distribution") + 
        theme(axis.ticks = element_blank(), 
              panel.background = element_blank(), 
              plot.title = element_text(size = 12, colour = "red"),
              axis.text=element_text(size=12)) 
    
    metrics_df <- as.data.frame(unique(dist_tbl_all_proportions$metric))
    colnames(metrics_df) <- "metric_name"
    metrics_df <- metrics_df %>% arrange(metric_name)
    metrics_splitted <- data.frame(do.call('rbind', strsplit(as.character(metrics_df$metric_name),' ',fixed=TRUE)))
    colnames(metrics_splitted) <- c("metric","bin")
    metrics_splitted$bin <- gsub("\\(","",metrics_splitted$bin)
    metrics_splitted$bin <- gsub("]","",metrics_splitted$bin)
    metrics_splitted$match <- NA
    
    curr_conditions <- input2[nrow(input2),c("ma9_q_bins","ma21_q_bins","ma50_q_bins","bb_up_q_bins","bb_dn_q_bins","RSI_q_bins","macd_delt_q_bins")]
    curr_conditions <- as.data.frame(t(curr_conditions))
    colnames(curr_conditions) <- c("bin_current")
    rownames(curr_conditions) <- gsub("_bins","",rownames(curr_conditions))
    
    metrics_splitted$match <- curr_conditions$bin_current[match(metrics_splitted$metric,rownames(curr_conditions))]
    metrics_splitted$match <- gsub("\\[","",metrics_splitted$match)
    metrics_splitted$match <- gsub("\\)","",metrics_splitted$match)
    metrics_splitted$match <- gsub("]","",metrics_splitted$match)
    metrics_splitted$match2 = ifelse(metrics_splitted$match==metrics_splitted$bin,1,NA)
    
    
    for (i in 1:nrow(metrics_splitted)) {
        if (!is.na(metrics_splitted$match2[i])){
            # g <- g + geom_text(x = .5, y = i, label = paste("*"),color="red")  
            g <- g + annotate("segment", x = .5, xend = 12.5, y = i-.5, yend = i-.5,
                              colour = "black", size = 1.5)
            g <- g + annotate("segment", x = .5, xend = 12.5, y = i+.5, yend = i+.5,
                              colour = "black", size = 1.5)
        }
    }
    
    g
    
})

renderPlot({
    dist_summ_plt_pos()
})

```
