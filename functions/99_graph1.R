ddd_summary <-
  function(salesdata,
           cate,
           subcate,
           region,
           province,
           city,
           decile,
           note,
           value,
           period,
           # kpi,
           window) {
    
    salesdata = sales_data
    cate = "抗帕金森"
    subcate = "Catechol-o-methyltransferase inhibitor"
    region = "Beijing"
    province = "北京市"
    city = "北京"
    decile = "D2"
    note = 0
    value = "RMB"
    period = "rqtr"
    # kpi = c("abs", "gr")
    window = 1
    
    
    oriwindow <- window
    window <- min(5, window + 1)
    
    
    salesdata1 <- salesdata[which(salesdata$Category_CN  %in% cate), ]
    salesdata2 <-
      salesdata1[which(salesdata1$Sub.category  %in% subcate), ]
    
    fmr <-
      min(c(which(grepl(
        "RENMINBI", names(salesdata2)
      )),  which(grepl(
        "UNIT", names(salesdata2)
      )), which(grepl(
        "DOT", names(salesdata2)
      )))) - 1
    
    
    salesdata2 <-
      salesdata2[c(1:fmr, grep(value, colnames(salesdata2)))]
    
    if (period == "rqtr") {
      salesdata2 <-
        salesdata2[c(1:fmr, grep("qtr", colnames(salesdata2)))]
      
    } else{
      salesdata2 <-
        salesdata2[c(1:fmr, grep(period, colnames(salesdata2)))]
      
    }
    
    
    
    if (period == "mat" | period == "ytd" | period == "yrl") {
      salesdata2 <-
        salesdata2[c(1:fmr, (length(salesdata2) - window + 1):length(salesdata2))]
      
    } else{
      salesdata2 <-
        salesdata2[c(1:fmr, (length(salesdata2) - window * 12 + 1):length(salesdata2))]
    }
    
    if (period == "qtr") {
      nnn <-
        seq(length(salesdata2[grepl("qtr", colnames(salesdata2))]), 1, by = -3)
      
      remove.col <-
        colnames(salesdata2[grepl("qtr", colnames(salesdata2))])[-nnn]
      
      salesdata2 <-
        salesdata2[, !(colnames(salesdata2) %in% remove.col)]
    }
    
    
    if (period == "rqtr") {
      id.transpose <-
        colnames(salesdata2)[!grepl(paste("qtr", "_", value, "_", sep = ""), colnames(salesdata2))]
      
    } else{
      id.transpose <-
        colnames(salesdata2)[!grepl(paste(period, "_", value, "_", sep = ""),
                                    colnames(salesdata2))]
      
    }
    
    salesdata2 <- melt(salesdata2, id = id.transpose)
    
    if (period == "rqtr") {
      salesdata2$date <-
        gsub(paste("qtr", "_", value, "_", sep = ""),
             '',
             salesdata2$variable)
      
    } else{
      salesdata2$date <-
        gsub(paste(period, "_", value, "_", sep = ""),
             '',
             salesdata2$variable)
      
    }
    
    salesdata2$year <- substr(salesdata2$date, start = 1, stop = 4)
    salesdata2$month <- substr(salesdata2$date, start = 6, stop = 7)
    
    # salesdata3 <- salesdata2
    
    salesdata3 <- salesdata2 %>%
      filter(Region == region, Province_CN == province,  
             City_CN == city, Decile == decile, Note == note) %>%
      group_by(Region, Province_CN, City_CN, Veeva.code, Veeva.name,
               Decile, Note, Category_CN, Sub.category, date) %>%
      summarise(prod.sum = sum(value, na.rm = TRUE),
                bi_prod.sum = sum(value * ifelse(Manufactory == "B.INGELHEIM", 1, 0),
                                  na.rm = TRUE)) %>%
      group_by(Veeva.code) %>%
      filter(row_number() / n() == 0.5 |  row_number() / n() == 1)
    
    salesdata3_dt <- setDT(salesdata3)
    
    salesdata3_dt <-
      data.table::dcast(salesdata3_dt,
                        Region + Province_CN + City_CN + Veeva.code + Veeva.name +
                          Decile + Note + Category_CN + Sub.category ~
                          date,
                        value.var = c("prod.sum", "bi_prod.sum"))
    
    
    salesdata4 <- setDF(salesdata3_dt)
    
    col_cnt <- ncol(salesdata4)
    
    salesdata4$total_gr <- salesdata4[, col_cnt - 2] / salesdata4[, col_cnt - 3]
    salesdata4$bi_gr <- salesdata4[, col_cnt] / salesdata4[, col_cnt - 1]
    
    salesdata4$total_sh <- 
      salesdata4[, col_cnt - 2] / sum(salesdata4[, col_cnt - 2], na.rm = TRUE)
    salesdata4$bi_sh <- 
      salesdata4[, col_cnt] / sum(salesdata4[, col_cnt], na.rm = TRUE)
    
    salesdata4$bi_ms <- salesdata4[, col_cnt] / salesdata4[, col_cnt - 2]
    
    salesdata4 <- salesdata4 %>%
      arrange(desc(total_sh)) %>%
      mutate(gr_idx = bi_gr / total_gr,
             cont_idx = bi_sh / total_sh,
             hosp_rank = row_number()) %>%
      arrange(desc(bi_sh)) %>%
      mutate(bi_rank = row_number()) %>%
      select(医院排名 = hosp_rank, BI产品贡献排名 = bi_rank, Region, 
                 省份 = Province_CN, 城市 = City_CN, `Veeva Code` = Veeva.code,
                 `Veeva Name` = Veeva.name, 医院等级 = Decile,
                 医院增长率 = total_gr, 
                 医院贡献率 = total_sh, 
                 BI产品增长率 = bi_gr,
                 BI产品贡献率 = bi_sh,
                 BI产品市场份额 = bi_sh,
                 增长指数 = gr_idx,
                 贡献指数 = cont_idx)
    
    
    
  
    
    
    
    
    
    
    
    
    
    
    
    return(final)
    
    
    
  }


