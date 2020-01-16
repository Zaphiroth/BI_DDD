sum.nona <- function(x) {
  sum(as.numeric(x), na.rm = TRUE) 
}


graph1m <-
  function(salesdata,
           cate,
           subcate,
           value,
           period,
           kpi,
           window,
           level = NULL,
           top) {
    
    # salesdata = summary
    # cate="Anti-thrombus"
    # subcate="Others"
    # value="RENMINBI"
    # period="mat"
    # kpi="abs"
    # window=1
    # level = "corporation"
    # top = as.integer(5)
    
    
    salesdata[] <- lapply(salesdata,
                          function(x) {
                            if (is.integer(x)) {
                              as.numeric(x)
                            } else {
                              x
                            }
                          })
    
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
    salesdata3 <- salesdata2
    
    if (!is.null(level) && level %in% c("corporation", "molecule")) {
      salesdata4 <- salesdata2
    }
    
    
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
      salesdata2[order(salesdata2[, "AUDIT.DESC"], 
                       salesdata2[, "CORPORATE.DESC"], 
                       salesdata2[, "PRODUCT.DESC"],
                       salesdata2[, "date"]),]
    
    
    calms <- function(x)
      x / sum(x) * 100
    
    
    calgr <- function(x)
      100 * x / c(NA, x[-length(x)])
    
    calmschange <- function(x)
      x - c(NA, x[-length(x)])
    
    
    
    
    
    if ("ms" %in% kpi) {
      salesdata2 <-
        transform(salesdata2, 
                  ms = ave(value, paste(AUDIT.DESC, date), FUN = calms))
      
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
      chinaref <- plyr::rename(chinaref, c("value" = "cn.value"))
      salesdata2 <-
        join(salesdata2,
             chinaref,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata2 <-
        transform(salesdata2, sum.market = ave(value, paste(AUDIT.DESC,   date), FUN = sum.nona))
      chinaref.cate <-
        salesdata2[which(salesdata2$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "sum.market")]
      chinaref.cate <-
        plyr::rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
      
      
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
            paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
            FUN = calgr
          ) - 100)
      }
      
      if (!"sum.market" %in% names(salesdata2)) {
        salesdata2 <-
          transform(salesdata2, 
                    sum.market = ave(value, 
                                     paste(AUDIT.DESC, date),
                                     FUN = sum.nona))
      }
      
      
      salesdata2 <-
        transform(salesdata2, gr.market = ave(
          sum.market,
          paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
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
      transform(salesdata3,
                area.sum = ave(value, paste(AUDIT.DESC,  date), 
                               FUN = sum.nona))
    
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
          paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
          FUN = calmschange
        ))
      salesdata3$mc <- round(salesdata3$mc, 2)
    }
    
    if ("pi" %in% kpi) {
      chinaref <-
        salesdata3[which(salesdata3$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "value")]
      chinaref <- plyr::rename(chinaref, c("value" = "cn.value"))
      salesdata3 <-
        join(salesdata3,
             chinaref,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata3 <-
        transform(salesdata3,
                  sum.market = ave(value, paste(AUDIT.DESC, date), 
                                   FUN = sum.nona))
      chinaref.cate <-
        salesdata3[which(salesdata3$AUDIT.DESC == "China"), c("CORPORATE.DESC", "PRODUCT.DESC", "date", "sum.market")]
      chinaref.cate <-
        plyr::rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
      
      
      salesdata3 <-
        join(salesdata3,
             chinaref.cate,
             by = c("CORPORATE.DESC", "PRODUCT.DESC", "date"))
      
      salesdata3$pi <-
        100 * salesdata3$value / salesdata3$cn.value / salesdata3$sum.market * salesdata3$cn.sum.market
      salesdata3$pi <- round(salesdata3$pi, 2)
    }
    
    if ("ev" %in% kpi) {
      if (!"gr" %in% names(salesdata3)) {
        salesdata3 <-
          transform(salesdata3, 
                    gr = ave(value, 
                             paste(AUDIT.DESC, CORPORATE.DESC, PRODUCT.DESC, month),
                             FUN = calgr) - 100)
      }
      
      if (!"sum.market" %in% names(salesdata3)) {
        salesdata3 <-
          transform(salesdata3, 
                    sum.market = ave(value, paste(AUDIT.DESC, date), 
                                     FUN = sum.nona))
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
    
    maindata <- rbind.fill(salesdata3, salesdata2)
    
    if (!is.null(level) && level %in% c("corporation", "molecule")) {
      if (level == "corporation") {
        level1 <- "CORPORATE.DESC"
        salesdata4 <-
          transform(salesdata4, prod.sum = ave(
            value,
            paste(AUDIT.DESC,
                  # level1,
                  CORPORATE.DESC,
                  # PRODUCT.DESC,
                  date),
            FUN = sum.nona
          ))
        salesdata4 <-
          salesdata4[!duplicated(salesdata4[, c("AUDIT.DESC", 
                                                level1,
                                                # "CORPORATE.DESC", 
                                                # "PRODUCT.DESC",
                                                "date")]),]
        salesdata4$value <- salesdata4$prod.sum
        
        salesdata4 <-
          salesdata4[order(salesdata4[, "AUDIT.DESC"], 
                           salesdata4[, level1], 
                           # salesdata4[, "CORPORATE.DESC"], 
                           # salesdata4[, "PRODUCT.DESC"],
                           salesdata4[, "date"]),]
        
        
        calms <- function(x)
          x / sum(x) * 100
        
        
        calgr <- function(x)
          100 * x / c(NA, x[-length(x)])
        
        calmschange <- function(x)
          x - c(NA, x[-length(x)])
        
        
        
        
        
        if ("ms" %in% kpi) {
          salesdata4 <-
            transform(salesdata4, 
                      ms = ave(value, paste(AUDIT.DESC, date), FUN = calms))
          
        }
        
        if ("gr" %in% kpi) {
          salesdata4 <-
            transform(salesdata4, gr = ave(
              value,
              paste(AUDIT.DESC, 
                    CORPORATE.DESC,
                    # PRODUCT.DESC,
                    # level1,
                    month),
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
              paste(AUDIT.DESC,
                    CORPORATE.DESC,
                    # PRODUCT.DESC,
                    # level1,
                    month),
              FUN = calmschange
            ))
          salesdata4$mc <- round(salesdata4$mc, 2)
        }
        
        if ("pi" %in% kpi) {
          chinaref <-
            salesdata4[which(salesdata4$AUDIT.DESC == "China"),
                       c(#"CORPORATE.DESC",
                         #"PRODUCT.DESC",
                         level1,
                         "date",
                         "value")]
          chinaref <- plyr::rename(chinaref, c("value" = "cn.value"))
          salesdata4 <-
            join(salesdata4,
                 chinaref,
                 by = c(#"CORPORATE.DESC", 
                   #"PRODUCT.DESC", 
                   level1,
                   "date"))
          
          salesdata4 <-
            transform(salesdata4, sum.market = ave(value, paste(AUDIT.DESC,  date), FUN = sum.nona))
          chinaref.cate <-
            salesdata4[which(salesdata4$AUDIT.DESC == "China"), 
                       c(#"CORPORATE.DESC",
                         #"PRODUCT.DESC",
                         level1,
                         "date", 
                         "sum.market")]
          chinaref.cate <-
            plyr::rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
          
          
          salesdata4 <-
            join(salesdata4,
                 chinaref.cate,
                 by = c(#"CORPORATE.DESC",
                   #"PRODUCT.DESC", 
                   level1,
                   "date"))
          
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
                paste(AUDIT.DESC,
                      CORPORATE.DESC,
                      # PRODUCT.DESC,
                      # level1,
                      month),
                FUN = calgr
              ) - 100)
          }
          
          if (!"sum.market" %in% names(salesdata4)) {
            salesdata4 <-
              transform(salesdata4, 
                        sum.market = ave(value, 
                                         paste(AUDIT.DESC, date),
                                         FUN = sum.nona))
          }
          
          
          salesdata4 <-
            transform(salesdata4, gr.market = ave(
              sum.market,
              paste(AUDIT.DESC, 
                    CORPORATE.DESC,
                    # PRODUCT.DESC, 
                    # level1,
                    month),
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
          # salesdata4$CORPORATE.DESC <- "Overall"
          
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
          
      } else {
        
        level1 <- "COMPS.DESC"
        salesdata4 <-
          transform(salesdata4, prod.sum = ave(
            value,
            paste(AUDIT.DESC,
                  # level1,
                  # CORPORATE.DESC,
                  # PRODUCT.DESC,
                  COMPS.DESC,
                  date),
            FUN = sum.nona
          ))
        salesdata4 <-
          salesdata4[!duplicated(salesdata4[, c("AUDIT.DESC", 
                                                level1,
                                                # "CORPORATE.DESC", 
                                                # "PRODUCT.DESC",
                                                "date")]),]
        salesdata4$value <- salesdata4$prod.sum
        
        salesdata4 <-
          salesdata4[order(salesdata4[, "AUDIT.DESC"], 
                           salesdata4[, level1], 
                           # salesdata4[, "CORPORATE.DESC"], 
                           # salesdata4[, "PRODUCT.DESC"],
                           salesdata4[, "date"]),]
        
        
        calms <- function(x)
          x / sum(x) * 100
        
        
        calgr <- function(x)
          100 * x / c(NA, x[-length(x)])
        
        calmschange <- function(x)
          x - c(NA, x[-length(x)])
        
        if ("ms" %in% kpi) {
          salesdata4 <-
            transform(salesdata4, 
                      ms = ave(value, paste(AUDIT.DESC, date), FUN = calms))
          
        }
        
        if ("gr" %in% kpi) {
          salesdata4 <-
            transform(salesdata4, gr = ave(
              value,
              paste(AUDIT.DESC, 
                    # CORPORATE.DESC, PRODUCT.DESC,
                    # level1,
                    COMPS.DESC,
                    month),
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
              paste(AUDIT.DESC,
                    COMPS.DESC,
                    #CORPORATE.DESC,  PRODUCT.DESC,
                    # level1,
                    month),
              FUN = calmschange
            ))
          salesdata4$mc <- round(salesdata4$mc, 2)
        }
        
        if ("pi" %in% kpi) {
          chinaref <-
            salesdata4[which(salesdata4$AUDIT.DESC == "China"),
                       c(#"CORPORATE.DESC",
                         #"PRODUCT.DESC",
                         level1,
                         # Molecule_CN,
                         "date",
                         "value")]
          chinaref <- plyr::rename(chinaref, c("value" = "cn.value"))
          salesdata4 <-
            join(salesdata4,
                 chinaref,
                 by = c(#"CORPORATE.DESC", 
                   #"PRODUCT.DESC", 
                   level1,
                   # Molecule_CN,
                   "date"))
          
          salesdata4 <-
            transform(salesdata4, sum.market = ave(value, paste(AUDIT.DESC,  date), FUN = sum.nona))
          chinaref.cate <-
            salesdata4[which(salesdata4$AUDIT.DESC == "China"), 
                       c(#"CORPORATE.DESC",
                         #"PRODUCT.DESC",
                         level1,
                         # Molecule_CN,
                         "date", 
                         "sum.market")]
          chinaref.cate <-
            plyr::rename(chinaref.cate, c("sum.market" = "cn.sum.market"))
          
          
          salesdata4 <-
            join(salesdata4,
                 chinaref.cate,
                 by = c(#"CORPORATE.DESC",
                   #"PRODUCT.DESC", 
                   level1,
                   "date"))
          
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
                paste(AUDIT.DESC,
                      # CORPORATE.DESC, 
                      # PRODUCT.DESC,
                      # level1,
                      COMPS.DESC,
                      month),
                FUN = calgr
              ) - 100)
          }
          
          if (!"sum.market" %in% names(salesdata4)) {
            salesdata4 <-
              transform(salesdata4, 
                        sum.market = ave(value, 
                                         paste(AUDIT.DESC, date),
                                         FUN = sum.nona))
          }
          
          
          salesdata4 <-
            transform(salesdata4, gr.market = ave(
              sum.market,
              paste(AUDIT.DESC, 
                    # CORPORATE.DESC,
                    # PRODUCT.DESC, 
                    # level1,
                    COMPS.DESC,
                    month),
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
        salesdata4$CORPORATE.DESC <- "Overall"
        
        salesdata4$PRODUCT.DESC <- "Overall"
        salesdata4$Category <- "Overall"
        salesdata4$Sub.category <- "Overall"
        
        # salesdata4$COMPS.DESC <- "Overall"
        
        salesdata4$pro.code <- "Overall"
        
        salesdata4$MANUF.TYPE.DESC <- "Overall"
        salesdata4$TC.III.SHORT.DESC <- "Overall"
        salesdata4$TC.III.DESC <- "Overall"
        
        salesdata4$Category_CN <- "Overall"
        salesdata4$Molecule_CN <- "Overall"
        salesdata4$Brand_CN <- "Overall"
        salesdata4$MANU_CN <- "Overall"
      }
      
      
      maindata <- rbind.fill(maindata, salesdata4)
    }
    
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
    final$date <- gsub("\\.", "", final$date)
    
    if (level == "corporation") {
      level1 <- quo(CORPORATE.DESC)
    } else {
      level1 <- quo(COMPS.DESC)
    }
    
    date_tmp <- colnames(salesdata)[length(colnames(salesdata))]
    date_tmp <- strsplit(date_tmp, split = "_")
    date_tmp <- unlist(date_tmp)[length(unlist(date_tmp))]
    date_tmp <- gsub("\\.", "", date_tmp)
    
    if (top == 0) {
      final1 <- filter(final, date == date_tmp) %>%
        arrange(AUDIT.DESC, !!(level1), desc(value)) %>%
        group_by(AUDIT.DESC, !!level1) %>%
        filter(PRODUCT.DESC == "Overall") %>%
        ungroup() %>%
        arrange(AUDIT.DESC, desc(value))
    } else {
      final1 <- filter(final, date == date_tmp) %>%
        arrange(AUDIT.DESC, !!(level1), desc(value)) %>%
        group_by(AUDIT.DESC, !!level1) %>%
        filter(row_number() <= 1 + top) %>%
        ungroup() %>%
        arrange(AUDIT.DESC, !!level1)
    }
    
    
      
      
    
   
    return(final1)
    
    
    
  }


