# sales_data <- 
#   read.csv("C:\\Users\\huiji\\Documents\\Projects\\2017\\BI_Shiny\\02_Inputs\\DDD+clean+up+raw+data\\final data\\ddd_Anti-PD_csv_format.csv",
#            stringsAsFactors = FALSE)

ddd_summary <-
  function(salesdata,
           cate,
           subcate,
           region,
           province,
           city,
           decile,
           # veeva,
           # hosp_name,
           # note,
           value,
           period,
           brand) {
    
    # salesdata = read.csv("D:/WORK/BI/test_data/ddd_Anti-PD_csv_format.csv", stringsAsFactors = FALSE)
    # cate = unique(salesdata$Category)
    # subcate = unique(salesdata$Sub.category)
    # region = unique(salesdata$Region)
    # province = unique(salesdata$Province_CN)
    # city = unique(salesdata$City_CN)
    # decile = unique(salesdata$Decile)
    # brand = unique(salesdata$Brand_CN)
    # brand = c("森福罗")
    # subcate = c("LABA",
    #             "LAMA",
    #             "Others")
    # region = "Beijing"
    # province = "北京市"
    # city = "北京"
    # decile = c("D9", "D2", "D7", "D3", "D10", "D4", "D1", "D5", "D6", "D8", "Others", "#N/A")
    # veeva = c("HAAAD", "HAAAM", "HAABD", "HAABF", "HAABN", "HAABY", "HAACM", "HAADA", "HAADE",
    #           "HAADF", "HAADH", "HAAEJ", "HAAFA", "HAAFB", "HAAFI", "HAAGH", "HAAGO", "HAAHV",
    #           "HAAIK", "HAAJW", "HAAKP", "HAALD", "HAALP", "HAALZ", "HAAMA", "HAAMV","HAAOB",
    #           "HAAOM", "HAAOS", "HAAPB", "HAAPQ", "HAARE", "HAASU", "HAASV" ,"HAAUE", "HAAUN",
    #           "HBDSD", "HBDUD", "HBEDO", "HBEDP", "HBEDQ", "HAAAE", "HAAAQ", "HAABH", "HAABO",
    #           "HAABX", "HAACD", "HAACF", "HAACI", "HAACJ", "HAADC", "HAADG", "HAADO", "HAADQ",
    #           "HAAEK", "HAAGV", "HAAHG", "HAAHU", "HAAIV", "HAAIW", "HAAKI", "HAAKJ", "HAAKO",
    #           "HAAKR", "HAAPU", "HAAQH", "HAASN", "HAATK", "HAATT", "HAATX", "HBDUJ")
    # hosp_name = c(
    #   "北京市隆福医院",
    #   "卫生部北京医院",
    #   "北京中医药大学东直门医院",
    #   "北京协和医院(东院)" ,
    #   "北京大学第一医院" ,
    #   "北京市肛肠医院",
    #   "首都医科大学附属北京天坛医院",
    #   "北京市普仁医院",
    #   "首都医科大学附属北京友谊医院" ,
    #   "首都医科大学宣武医院" ,
    #   "北京市宣武区中医医院"  ,
    #   "民航总医院"  ,
    #   "中国中医科学院望京医院" ,
    #   "中日友好医院",
    #   "北京市垂杨柳医院"  ,
    #   "北京地坛医院" ,
    #   "首都医科大学附属北京安贞医院" ,
    #   "北京华信医院"  ,
    #   "北京中医药大学东方医院" ,
    #   "北京航天总医院",
    #   "清华大学玉泉医院",
    #   "北京水利医院" ,
    #   "航天中心医院" ,
    #   "北京世纪坛医院",
    #   "中国中医研究院西苑医院",
    #   "北京市海淀医院",
    #   "北京市中关村医院",
    #   "北京市门头沟区医院" ,
    #   "北京市房山区中医医院" ,
    #   "北京市房山区第一医院",
    #   "首都医科大学附属北京潞河医院" ,
    #   "北京市昌平区医院",
    #   "北京市大兴区红星医院",
    #   "大兴区人民医院",
    #   "北京市密云县医院" ,
    #   "北京大学第三医院延庆医院"  ,
    #   "北京中国人民解放军空军总医院",
    #   "中国人民解放军空军航空医学研究所附属医院",
    #   "北京中国人民解放军总医院",
    #   "北京中国人民解放军第三零六医院",
    #   "北京中国人民解放军第三零四医院",
    #   "首都医科大学中医药学院附属鼓楼中医医院",
    #   "北京市第六医院"   ,
    #   "北京市和平里医院"  ,
    #   "首都医科大学附属北京安定医院" ,
    #   "北京市第二医院" ,
    #   "首都医科大学附属复兴医院",
    #   "北京大学人民医院",
    #   "北京积水潭医院" ,
    #   "首都医科大学附属北京儿童医院",
    #   "北京市西城区广外医院" ,
    #   "北京市健宫医院"  ,
    #   "中国中医研究院广安门医院" ,
    #   "北京市回民医院" ,
    #   "航空工业中心医院",
    #   "煤炭总医院",
    #   "北京市第一中西医结合医院",
    #   "首都医科大学附属北京朝阳医院" ,
    #   "北京丰台医院(桥北部)" ,
    #   "北京佑安医院" ,
    #   "中国中医研究院眼科医院",
    #   "北京大学首钢医院" ,
    #   "北京市石景山医院",
    #   "北京市石景山区五里坨医院"  ,
    #   "北京市胸科医院" ,
    #   "北京市顺义区医院"  ,
    #   "北京市仁和医院" ,
    #   "大兴区精神病医院",
    #   "北京市怀柔区第一医院" ,
    #   "北京市平谷区医院" ,
    #   "北京中国人民解放军第三零七医院"
    # )
    # note = c("core hospital", NA)
    # value = "RMB"
    # period = "mat"
    # brand = c("思合华", "平适")
    # kpi = c("abs", "gr")
    # window = 1
    
    
    
    
    # oriwindow <- window
    # window <- min(5, window + 1)
    window <- 2
    
    # salesdata1 <- salesdata[which(salesdata$Category  %in% cate), ]
    # salesdata2 <- salesdata1[which(salesdata1$Sub.category  %in% subcate), ]
    salesdata2 <- salesdata
    
    fmr <-
      min(c(which(grepl(
        "RMB", names(salesdata2)
      )),  which(grepl(
        "UNIT", names(salesdata2)
      )), which(grepl(
        "DOT", names(salesdata2)
      )))) - 1
    
    
    salesdata3 <- salesdata2[c(1:fmr, grep(value, colnames(salesdata2)))]
    salesdata3 <- salesdata3[c(1:fmr, grep(period, colnames(salesdata3)))]
    
    # if (period == "rqtr") {
    #   salesdata2 <-
    #     salesdata2[c(1:fmr, grep("qtr", colnames(salesdata2)))]
    # 
    # } else{
    #   salesdata2 <-
    #     salesdata2[c(1:fmr, grep(period, colnames(salesdata2)))]
    # 
    # }
    
    if (period == "mat" | period == "ytd") {
      # salesdata2 <-
      #   salesdata2[c(1:fmr, (length(salesdata2) - window + 1):length(salesdata2))]
      salesdata4 <- salesdata3[c(1:fmr, length(salesdata3)-1, length(salesdata3))]
      
    } else if (period == "qtr" | period == "mth") {
      # salesdata2 <-
      #   salesdata2[c(1:fmr, (length(salesdata2) - window * 12 + 1):length(salesdata2))]
      salesdata4 <- salesdata3[c(1:fmr, length(salesdata3)-12, length(salesdata3))]
    }
    
    names(salesdata4)<- c(names(salesdata4)[1:fmr], "pp", "cc")
    
    # if (period == "qtr" | period == "mth") {
    #   nnn <-
    #     seq(length(salesdata2[grepl("qtr", colnames(salesdata2))]), 1, by = -3)
    #   
    #   remove.col <-
    #     colnames(salesdata2[grepl("qtr", colnames(salesdata2))])[-nnn]
    #   
    #   salesdata2 <-
    #     salesdata2[, !(colnames(salesdata2) %in% remove.col)]
    # }
    
    
    # if (period == "rqtr") {
    #   id.transpose <-
    #     colnames(salesdata2)[!grepl(paste("qtr", "_", value, "_", sep = ""), colnames(salesdata2))]
    #   
    # } else{
    #   id.transpose <-
    #     colnames(salesdata2)[!grepl(paste(period, "_", value, "_", sep = ""),
    #                                 colnames(salesdata2))]
    #   
    # }
    
    # salesdata2 <- melt(salesdata2, id = id.transpose)
    
    # if (period == "rqtr") {
    #   salesdata2$date <-
    #     gsub(paste("qtr", "_", value, "_", sep = ""),
    #          '',
    #          salesdata2$variable)
    #   
    # } else{
    #   salesdata2$date <-
    #     gsub(paste(period, "_", value, "_", sep = ""),
    #          '',
    #          salesdata2$variable)
    #   
    # }
    
    # salesdata2$date <-
    #   gsub(paste(period, "_", value, "_", sep = ""),
    #        '',
    #        salesdata2$variable)
    
    # salesdata2$year <- substr(salesdata2$date, start = 1, stop = 4)
    # salesdata2$month <- substr(salesdata2$date, start = 6, stop = 7)
    
    ## all
    salesdata5 <- salesdata4 %>% 
      filter(Region %in% region,
             Province_CN %in% province,
             City_CN %in% city,
             Decile %in% decile) %>% 
      group_by(Region, Province_CN, City_CN, Veeva.code, Veeva.name, Decile) %>% 
      summarise(pp = sum(pp, na.rm = TRUE),
                cc = sum(cc, na.rm = TRUE)
                # bi_pp = sum(pp * ifelse(Manufactory == "B.INGELHEIM", 1, 0), na.rm = TRUE),
                # bi_cp = sum(cc * ifelse(Manufactory == "B.INGELHEIM", 1, 0), na.rm = TRUE)
                ) %>% 
      ungroup() %>% 
      setDF()
    
    ## selected
    salesdata6 <- salesdata4 %>%
      filter(Brand_CN %in% brand,
             Region %in% region,
             Province_CN %in% province,
             City_CN %in% city,
             Decile %in% decile) %>%
      group_by(Region, Province_CN, City_CN, Veeva.code, Veeva.name, Decile) %>%
      summarise(pp_sel = sum(pp, na.rm = TRUE),
                cc_sel = sum(cc, na.rm = TRUE)
                # bi_pp_sel = sum(pp * ifelse(Manufactory == "B.INGELHEIM", 1, 0), na.rm = TRUE),
                # bi_cc_sel = sum(cc * ifelse(Manufactory == "B.INGELHEIM", 1, 0), na.rm = TRUE)
                ) %>% 
      ungroup() %>% 
      setDF()
    
    # salesdata3_m <- salesdata3 %>%
    #   group_by(Veeva.code) %>%
    #   filter(row_number() / n() == 1)
    
    # t1 <- unique(salesdata3_m$date)
    # t2 <- sprintf("%.2f", as.numeric(t1) - 1)
    
    # salesdata3_m1 <- salesdata3 %>%
    #   filter(date == t1 | date == t2)
      # group_by(Veeva.code) %>%
      # filter(row_number() / n() == 0.5 |  row_number() / n() == 1)
    
    # salesdata3_dt <- setDT(salesdata3_m1)
    
    # salesdata3_dt <-
    #   data.table::dcast(salesdata3_dt,
    #                     Region + Province_CN + City_CN + Veeva.code + Veeva.name +
    #                       Decile  ~ date,
    #                     value.var = c("prod.sum", "bi_prod.sum"))
    
    
    # salesdata4 <- setDF(salesdata3_dt)
    
    # col_cnt <- ncol(salesdata4)
    
    salesdata7 <- salesdata5 %>%
      left_join(salesdata6, by = c("Region", "Province_CN", "City_CN", "Veeva.code", "Veeva.name", "Decile"))
    # salesdata7[is.na(salesdata7)] <- 0
    
    salesdata8 <- salesdata7 %>% 
      mutate(total_gr = cc / pp - 1,
             total_sh = cc / sum(cc, na.rm = TRUE),
             selected_gr = cc_sel / pp_sel - 1,
             selected_sh = cc_sel / sum(cc_sel, na.rm = TRUE),
             selected_ms = cc_sel / cc,
             gr_idx = (1 + selected_gr) / (1 + total_gr) * 100,
             cont_idx = selected_sh / total_sh * 100) %>%
      mutate(hosp_rank = rank(-total_sh, ties.method = "min"),
             bi_rank = rank(-selected_sh, ties.method = "min")) %>% 
      # arrange(desc(total_sh)) %>%
      # mutate(hosp_rank = row_number()) %>%
      # arrange(desc(selected_sh)) %>%
      # mutate(bi_rank = row_number()) %>%
      dplyr::select("医院排名" = "hosp_rank", 
                    "产品贡献排名" = "bi_rank",
                    "Region", 
                    "省份" = "Province_CN", 
                    "城市" = "City_CN", 
                    "Veeva Code" = "Veeva.code",
                    "Veeva Name" = "Veeva.name", 
                    "医院等级" = "Decile",
                    "医院产出" = "cc",
                    "医院增长率" = "total_gr", 
                    "医院贡献率" = "total_sh", 
                    "所选产品产出" = "cc_sel",
                    "所选产品增长率" = "selected_gr",
                    "所选产品贡献率" = "selected_sh",
                    "所选产品市场份额" = "selected_ms",
                    "增长指数" = "gr_idx",
                    "贡献指数" = "cont_idx") %>%
      arrange(`医院排名`)
      # mutate_all(function(x) {ifelse(is.na(x), NA, x)}) %>% 
      # mutate_all(function(x) {ifelse(is.infinite(x), NA, x)})
    # salesdata7[is.na(salesdata7)] <- 0
    # salesdata7[salesdata7 == Inf] <- 1
    
    salesdata9 <- salesdata7 %>% 
      summarise(pp = sum(pp, na.rm = TRUE),
                cc = sum(cc, na.rm = TRUE),
                pp_sel = sum(pp_sel, na.rm = TRUE),
                cc_sel = sum(cc_sel, na.rm = TRUE)) %>% 
      mutate(total_gr = cc / pp - 1,
             total_sh = cc / sum(cc, na.rm = TRUE),
             selected_gr = cc_sel / pp_sel - 1,
             selected_sh = cc_sel / sum(cc_sel, na.rm = TRUE),
             selected_ms = cc_sel / cc,
             gr_idx = (1 + selected_gr) / (1 + total_gr) * 100,
             cont_idx = selected_sh / total_sh * 100) %>%
      mutate(hosp_rank = NA,
             bi_rank = NA,
             Region = NA,
             Province_CN  = NA,
             City_CN = NA,
             Veeva.code = NA,
             Veeva.name = "Total",
             Decile = NA) %>% 
      dplyr::select("医院排名" = "hosp_rank", 
                    "产品贡献排名" = "bi_rank",
                    "Region", 
                    "省份" = "Province_CN", 
                    "城市" = "City_CN", 
                    "Veeva Code" = "Veeva.code",
                    "Veeva Name" = "Veeva.name", 
                    "医院等级" = "Decile",
                    "医院产出" = "cc",
                    "医院增长率" = "total_gr", 
                    "医院贡献率" = "total_sh", 
                    "所选产品产出" = "cc_sel",
                    "所选产品增长率" = "selected_gr",
                    "所选产品贡献率" = "selected_sh",
                    "所选产品市场份额" = "selected_ms",
                    "增长指数" = "gr_idx",
                    "贡献指数" = "cont_idx") %>%
      arrange(`医院排名`)
    
    salesdata10 <- bind_rows(salesdata9, salesdata8)
    
    total_num <- length(unique(salesdata5$Veeva.name[which(salesdata5$cc != 0)]))
    selected_num <- length(unique(salesdata6$Veeva.name[which(salesdata6$cc_sel != 0)]))
    
    result <- list("table_data" = salesdata10,
                   "total_num" = total_num,
                   "selected_num" = selected_num)
    
    return(result)
  }


