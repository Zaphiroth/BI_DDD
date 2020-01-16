graph2 <-
  function(salesdata,
           cate,
           subcate,
           value,
           period,
           kpi,
           window) {
    oriwindow <- window
    window <- min(5, window + 1)
    
    salesdata1 <- salesdata[which(salesdata$Category  %in% cate), ]
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
    
    
    
    if (period == "mat" | period == "ytd") {
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
    
    salesdata3 <- salesdata2
    salesdata4 <- salesdata2
    
    
    
    salesdata2 <-
      transform(salesdata2, prod.sum = ave(
        value,
        paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, date),
        FUN = sum.nona
      ))
    salesdata2 <-
      salesdata2[!duplicated(salesdata2[, c("AUDIT.DESC", "CORPORATE.DESC", "PRODUCT.DESC", "date")]),]
    salesdata2$value <- salesdata2$prod.sum
    
    salesdata2 <-
      salesdata2[order(salesdata2[, "AUDIT.DESC"], salesdata2[, "CORPORATE.DESC"], salesdata2[, "PRODUCT.DESC"], salesdata2[, "date"]),]
    
    
    
    
    
    calms <- function(x)
      x / sum(x) * 100
    
    
    calgr <- function(x)
      100 * x / c(NA, x[-length(x)])
    
    calmschange <- function(x)
      x - c(NA, x[-length(x)])
    
    
    
    
    
    if ("ms" %in% kpi) {
      salesdata2 <-
        transform(salesdata2, ms = ave(value, paste(AUDIT.DESC,   date), FUN = calms))
      
    }
    
    if ("gr" %in% kpi) {
      salesdata2 <-
        transform(salesdata2, gr = ave(
          value,
          paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      
    }
    
    
    if ("mc" %in% kpi) {
      if (!"ms" %in% names(salesdata2)) {
        salesdata2 <-
          transform(salesdata2, ms = ave(value, paste(AUDIT.DESC,   date), FUN = calms))
      }
      
      salesdata2 <-
        transform(salesdata2, mc = ave(
          ms,
          paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
          FUN = calmschange
        ))
      salesdata2$mc <- round(salesdata2$mc, 2)
    }
    
    if ("pi" %in% kpi) {
      chinaref <-
        salesdata2[which(salesdata2$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "value")]
      chinaref <- rename(chinaref, c("value" = "cn.value"))
      salesdata2 <-
        join(salesdata2,
             chinaref,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata2 <-
        transform(salesdata2, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      chinaref.cate <-
        salesdata2[which(salesdata2$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "sum.market")]
      chinaref.cate <-
        rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
      
      
      salesdata2 <-
        join(salesdata2,
             chinaref.cate,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata2$pi <-
        100 * salesdata2$value / salesdata2$cn.value / salesdata2$sum.market *
        salesdata2$cn.sum.market
      salesdata2$pi <- round(salesdata2$pi, 2)
    }
    
    if ("ev" %in% kpi) {
      if (!"gr" %in% names(salesdata2)) {
        salesdata2 <-
          transform(salesdata2, gr = ave(
            value,
            paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
            FUN = calgr
          ) - 100)
      }
      
      if (!"sum.market" %in% names(salesdata2)) {
        salesdata2 <-
          transform(salesdata2, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      }
      
      
      salesdata2 <-
        transform(salesdata2, gr.market = ave(
          sum.market,
          paste(AUDIT.DESC, CORPORATE.DESC,    PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      salesdata2 <-
        transform(salesdata2, ev = 100 * (gr + 100) / (gr.market + 100))
      salesdata2$ev <- round(salesdata2$ev, 2)
    }
    
    if ("abs" %in% kpi) {
      salesdata2$abs <- salesdata2$value
      salesdata2$abs <- (round(salesdata2$abs, 0))
    }
    
    if ("ms" %in% kpi) {
      salesdata2$ms <- round(salesdata2$ms, 2)
    }
    
    if ("gr" %in% kpi) {
      salesdata2$gr <- round(salesdata2$gr, 2)
    }
    
    
    
    
    salesdata3 <-
      transform(salesdata3, area.sum = ave(value, paste(AUDIT.DESC,  date), FUN =
                                             sum.nona))
    
    salesdata3 <-
      salesdata3[!duplicated(salesdata3[, c("AUDIT.DESC", "date")]),]
    
    salesdata3$value <- salesdata3$area.sum
    
    
    
    salesdata3 <-
      salesdata3[order(salesdata3[, "AUDIT.DESC"], salesdata3[, "date"]),]
    
    
    
    calms.reg <- function(x)
      x / max(x) * 100
    
    if ("ms" %in% kpi) {
      salesdata3 <-
        transform(salesdata3, ms = ave(value, paste(date), FUN = calms.reg))
      
    }
    
    if ("gr" %in% kpi) {
      salesdata3 <-
        transform(salesdata3, gr = ave(
          value,
          paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      
    }
    
    
    if ("mc" %in% kpi) {
      if (!"ms" %in% names(salesdata3)) {
        salesdata3 <-
          transform(salesdata3, ms = ave(value, paste(date), FUN = calms.reg))
      }
      
      salesdata3 <-
        transform(salesdata3, mc = ave(
          ms,
          paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
          FUN = calmschange
        ))
      salesdata3$mc <- round(salesdata3$mc, 2)
    }
    
    if ("pi" %in% kpi) {
      chinaref <-
        salesdata3[which(salesdata3$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "value")]
      chinaref <- rename(chinaref, c("value" = "cn.value"))
      salesdata3 <-
        join(salesdata3,
             chinaref,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata3 <-
        transform(salesdata3, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      chinaref.cate <-
        salesdata3[which(salesdata3$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "sum.market")]
      chinaref.cate <-
        rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
      
      
      salesdata3 <-
        join(salesdata3,
             chinaref.cate,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata3$pi <-
        100 * salesdata3$value / salesdata3$cn.value / salesdata3$sum.market *
        salesdata3$cn.sum.market
      salesdata3$pi <- round(salesdata3$pi, 2)
    }
    
    if ("ev" %in% kpi) {
      if (!"gr" %in% names(salesdata3)) {
        salesdata3 <-
          transform(salesdata3, gr = ave(
            value,
            paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
            FUN = calgr
          ) - 100)
      }
      
      if (!"sum.market" %in% names(salesdata3)) {
        salesdata3 <-
          transform(salesdata3, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      }
      
      
      salesdata3 <-
        transform(salesdata3, gr.market = ave(
          sum.market,
          paste(AUDIT.DESC, CORPORATE.DESC,    PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      salesdata3 <-
        transform(salesdata3, ev = 100 * (gr + 100) / (gr.market + 100))
      salesdata3$ev <- round(salesdata3$ev, 2)
    }
    
    if ("abs" %in% kpi) {
      salesdata3$abs <- salesdata3$value
      salesdata3$abs <- (round(salesdata3$abs, 0))
    }
    
    if ("ms" %in% kpi) {
      salesdata3$ms <- round(salesdata3$ms, 2)
    }
    
    if ("gr" %in% kpi) {
      salesdata3$gr <- round(salesdata3$gr, 2)
    }
    
    
    
    
    
    
    salesdata3$CORPORATE.DESC <- "Overall"
    
    salesdata3$PRODUCT.DESC <- "Overall"
    
    salesdata3$Category <- "Overall"
    salesdata3$Sub.category <- "Overall"
    
    salesdata3$COMPS.DESC <- "Overall"
    
    salesdata3$pro.code <- "Overall"
    
    
    salesdata3$MANUF.TYPE.DESC <- "Overall"
    salesdata3$TC.III.SHORT.DESC <- "Overall"
    salesdata3$TC.III.DESC <- "Overall"
    
    salesdata3$Category_CN <- "Overall"
    salesdata3$Molecule_CN <- "Overall"
    salesdata3$Brand_CN <- "Overall"
    salesdata3$MANU_CN <- "Overall"
    
    
    salesdata4 <-
      transform(salesdata4, corp.sum = ave(value, paste(AUDIT.DESC, CORPORATE.DESC, date), FUN =
                                             sum.nona))
    
    salesdata4 <-
      salesdata4[!duplicated(salesdata4[, c("AUDIT.DESC",  "CORPORATE.DESC", "date")]),]
    
    salesdata4$value <- salesdata4$corp.sum
    
    salesdata4 <-
      salesdata4[order(salesdata4[, "AUDIT.DESC"], salesdata4[, "CORPORATE.DESC"], salesdata4[, "date"]),]
    
    
    
    if ("ms" %in% kpi) {
      salesdata4 <-
        transform(salesdata4, ms = ave(value, paste(AUDIT.DESC,   date), FUN = calms))
      
    }
    
    if ("gr" %in% kpi) {
      salesdata4 <-
        transform(salesdata4, gr = ave(
          value,
          paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      
    }
    
    
    if ("mc" %in% kpi) {
      if (!"ms" %in% names(salesdata4)) {
        salesdata4 <-
          transform(salesdata4, ms = ave(value, paste(AUDIT.DESC,   date), FUN = calms))
      }
      
      salesdata4 <-
        transform(salesdata4, mc = ave(
          ms,
          paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
          FUN = calmschange
        ))
      salesdata4$mc <- round(salesdata4$mc, 2)
    }
    
    if ("pi" %in% kpi) {
      chinaref <-
        salesdata4[which(salesdata4$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "value")]
      chinaref <- rename(chinaref, c("value" = "cn.value"))
      salesdata4 <-
        join(salesdata4,
             chinaref,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata4 <-
        transform(salesdata4, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      chinaref.cate <-
        salesdata4[which(salesdata4$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "sum.market")]
      chinaref.cate <-
        rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
      
      
      salesdata4 <-
        join(salesdata4,
             chinaref.cate,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata4$pi <-
        100 * salesdata4$value / salesdata4$cn.value / salesdata4$sum.market *
        salesdata4$cn.sum.market
      salesdata4$pi <- round(salesdata4$pi, 2)
    }
    
    if ("ev" %in% kpi) {
      if (!"gr" %in% names(salesdata4)) {
        salesdata4 <-
          transform(salesdata4, gr = ave(
            value,
            paste(AUDIT.DESC, CORPORATE.DESC,  PRODUCT.DESC, month),
            FUN = calgr
          ) - 100)
      }
      
      if (!"sum.market" %in% names(salesdata4)) {
        salesdata4 <-
          transform(salesdata4, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      }
      
      
      salesdata4 <-
        transform(salesdata4, gr.market = ave(
          sum.market,
          paste(AUDIT.DESC, CORPORATE.DESC,    PRODUCT.DESC, month),
          FUN = calgr
        ) - 100)
      salesdata4 <-
        transform(salesdata4, ev = 100 * (gr + 100) / (gr.market + 100))
      salesdata4$ev <- round(salesdata4$ev, 2)
    }
    
    if ("abs" %in% kpi) {
      salesdata4$abs <- salesdata4$value
      salesdata4$abs <- (round(salesdata4$abs, 0))
    }
    
    if ("ms" %in% kpi) {
      salesdata4$ms <- round(salesdata4$ms, 2)
    }
    
    if ("gr" %in% kpi) {
      salesdata4$gr <- round(salesdata4$gr, 2)
    }
    
    
    salesdata4$PRODUCT.DESC <- "Overall"
    
    salesdata4$Category <- "Overall"
    salesdata4$Sub.category <- "Overall"
    
    salesdata4$COMPS.DESC <- "Overall"
    
    salesdata4$pro.code <- "Overall"
    
    salesdata4$MANUF.TYPE.DESC <- "Overall"
    salesdata4$TC.III.SHORT.DESC <- "Overall"
    salesdata4$TC.III.DESC <- "Overall"
    
    salesdata4$Category_CN <- "Overall"
    salesdata4$Molecule_CN <- "Overall"
    salesdata4$Brand_CN <- "Overall"
    salesdata4$MANU_CN <- "Overall"
    
    
    
    maindata <- rbind.fill(salesdata4, salesdata3, salesdata2)
    
    
    maindata1 <-
      maindata[, c(
        "AUDIT.DESC",
        "Category",
        "Sub.category",
        "CORPORATE.DESC",
        "PRODUCT.DESC",
        "COMPS.DESC",
        "pro.code",
        "MANUF.TYPE.DESC",
        "TC.III.SHORT.DESC",
        "TC.III.DESC",
        "Category_CN",
        "Molecule_CN",
        "Brand_CN",
        "MANU_CN",
        "date",
        kpi
      )]
    
    final <-
      data.table::melt(
        setDT(maindata1),
        id.vars = c(
          "AUDIT.DESC",
          "Category",
          "Sub.category",
          "CORPORATE.DESC",
          "PRODUCT.DESC",
          "COMPS.DESC",
          "pro.code",
          "MANUF.TYPE.DESC",
          "TC.III.SHORT.DESC",
          "TC.III.DESC",
          "Category_CN",
          "Molecule_CN",
          "Brand_CN",
          "MANU_CN",
          "date"
        ),
        variable.name = "Index"
      )
    measurement <- value
    
    
    final <- transform(final, Measure = measurement)
    
    final <-
      data.table::dcast(
        setDT(final),
        AUDIT.DESC + Category + Sub.category + CORPORATE.DESC + PRODUCT.DESC + COMPS.DESC +
          pro.code +
          MANUF.TYPE.DESC + TC.III.SHORT.DESC + TC.III.DESC
        + Category_CN +  Molecule_CN + Brand_CN + MANU_CN +
          Measure + Index
        ~ date,
        value.var = "value"
      )
    
    final <- as.data.frame(final)
    
    
    
    
    
    if (period == "ytd" | period == "mat" | period == "yrl") {
      final <-
        cbind(final[c(
          "AUDIT.DESC",
          "Category",
          "Sub.category",
          "CORPORATE.DESC",
          "PRODUCT.DESC",
          "COMPS.DESC",
          "pro.code",
          "MANUF.TYPE.DESC",
          "TC.III.SHORT.DESC",
          "TC.III.DESC",
          "Category_CN",
          "Molecule_CN",
          "Brand_CN",
          "MANU_CN",
          "Measure",
          "Index"
        )], final[(length(final) - window + 1):length(final)])
      
    } else if (period == "mth" | period == "rqtr") {
      final <-
        cbind(final[c(
          "AUDIT.DESC",
          "Category",
          "Sub.category",
          "CORPORATE.DESC",
          "PRODUCT.DESC",
          "COMPS.DESC",
          "pro.code",
          "MANUF.TYPE.DESC",
          "TC.III.SHORT.DESC",
          "TC.III.DESC",
          "Category_CN",
          "Molecule_CN",
          "Brand_CN",
          "MANU_CN",
          "Measure",
          "Index"
        )], final[(length(final) - 12 * window + 1):length(final)])
    } else if (period == "qtr") {
      final <-
        cbind(final[c(
          "AUDIT.DESC",
          "Category",
          "Sub.category",
          "CORPORATE.DESC",
          "PRODUCT.DESC",
          "COMPS.DESC",
          "pro.code",
          "MANUF.TYPE.DESC",
          "TC.III.SHORT.DESC",
          "TC.III.DESC",
          "Category_CN",
          "Molecule_CN",
          "Brand_CN",
          "MANU_CN",
          "Measure",
          "Index"
        )], final[(length(final) - 4 * window + 1):length(final)])
      
    }
    
    fd <-  which(names(final) == "Index") + 1
    
    
    final[fd:length(final)] <-
      apply(final[fd:length(final)], 2, function(x)  {
        x[which(x == Inf)] <- NA
        x
      })
    final[fd:length(final)] <-
      apply(final[fd:length(final)], 2, function(x)  {
        x[which(is.nan(x))] <- NA
        x
      })
    
    #if(kpi=="pi"|kpi=="ev"|kpi=="mc"|kpi=="gr"){
    
    #  final<- cbind(final[1:(fd-1)], final[(fd+length(final)/as.numeric(window)):length(final)])
    
    #}
    
    
    final$Index <- as.character(final$Index)
    
    final$Index[which(final$Index == "abs")] = "ABS"
    final$Index[which(final$Index == "ms")] = "MS%"
    final$Index[which(final$Index == "pi")] = "PI"
    final$Index[which(final$Index == "mc")] = "MS+/- %"
    final$Index[which(final$Index == "gr")] = "GR%"
    final$Index[which(final$Index == "ev")] = "EV"
    
    
    if (oriwindow <= 4) {
      re1year <- which(grepl("Index", names(final)))
      
      if (period == "rqtr" | period == "mth") {
        final <-
          cbind(final[1:re1year], final[(re1year + 13):length(final)])
        
      } else if (period == "mat" | period == "ytd") {
        final <- cbind(final[1:re1year], final[(re1year + 2):length(final)])
        
        
      } else if (period == "qtr") {
        final <- cbind(final[1:re1year], final[(re1year + 5):length(final)])
      }
      
    }
    
    return(final)
    
  }
