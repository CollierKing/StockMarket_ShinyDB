```{r event_page2}
## Add checkboxes
textInput("inText9","Ticker Search:","AIG")
# textInput("inText2","Ticker Search:","")

metrics <- data.frame(Metric = c("Gross Margin",
                                 "Oper. Profit Margin",
                                 "EBIT Margin",
                                 "Return on Assets",
                                 "Return on Equity",
                                 "Return on Investment",
                                 "Free Cash Flow/Share",
                                 "Book Value/Share",
                                 "Oper Cash Flow/Share",
                                 "Asset Turnover",
                                 "Inventory Turnover",
                                 "Receivables Turnover",
                                 "Current Ratio",
                                 "LT Dept to Capital",
                                 "Debt to Equity",
                                 "Free Cash Flow",
                                 "Total Revenue",
                                 "COGS",
                                 "Gross Profit",
                                 "Operating Income",
                                 "Operating Expenses",
                                 "Employee Count"),
                      DF = c("gross_margin","oper_profit_margin","ebit_margin","g_roa",
                             "g_roe","g_roi","g_fcf_sh","g_bv_sh",                    "g_ocf_sh","g_ato","g_into","g_recto","g_curr","g_ltdebt_cap","g_debt_equity",                "g_fcf","g_rev","g_cogs","g_gross_profit","g_oper_inc","g_oper_exp","g_empl_cnt"), stringsAsFactors = FALSE)

selectInput("metric_type","Select Metrics", c(unique(metrics$DF) ), selected=NULL,multiple=TRUE)

actionButton("clickMeFinancials", label = "Search")
```

## Financial Summary

```{r finplot}

#Render larger plot
finPlot1 <- eventReactive(input$clickMeFinancials,{
    
    library(Quandl)
    library(grid)
    
    Quandl.api_key("w8BRC-MZFDskMZoy7iyn")
    # quandl.ApiConfig.api_key = "qSAUD4UYgpSUz_Bbxv6v"
    
    symbol <- toupper(input$inText9)
    
    fundamentals_c = Quandl.datatable("ZACKS/FC", ticker=symbol)
    fundamentals_r = Quandl.datatable("ZACKS/FR", ticker=symbol)
    fundamentals_c <- fundamentals_c[fundamentals_c$per_type=="Q",]
    fundamentals_r <- fundamentals_r[fundamentals_r$per_type=="Q",]
    
    x <- input$metric_type
    plot_list = list()
    for (i in 1:length(x)){
        
        metric <- x[i]
        
        y <-ggplot(data=fundamentals_r, aes_string(x="per_end_date",y=metric))+
            theme_minimal()+geom_line(color="black") + geom_point(color="blue")  + xlab("") + geom_point(data=fundamentals_r[nrow(fundamentals_r),], color="red", size=6, alpha=0.7) +
            geom_hline(data =fundamentals_r[nrow(fundamentals_r),], aes_string(yintercept = metric), linetype=2,alpha=0.8)
        
        plot_list[[i]] = y
    }
    
    cowplot::plot_grid(plotlist = plot_list, ncol = 1)
    
    # symbol
})


renderPlot({
    finPlot1()
})

# renderText({
#     finPlot1()
# })


```
