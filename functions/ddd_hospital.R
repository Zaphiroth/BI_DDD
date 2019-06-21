
ddd_hospital <- function(salesdata, main, category, subcategory, period) {
  
  # fmr <-
  #   min(c(which(grepl(
  #     "RMB", names(salesdata)
  #   )),  which(grepl(
  #     "UNIT", names(salesdata)
  #   )), which(grepl(
  #     "DOT", names(salesdata)
  #   )))) - 1
  
  # info <- salesdata[c(1:fmr)]
  # rmb <- salesdata[grep("RMB", colnames(salesdata))]
  # unit <- salesdata[grep("UNIT", colnames(salesdata))]
  # dot <- salesdata[grep("DOT", colnames(salesdata))]
  
  # rmb_mat <- rmb[grep("mat", colnames(rmb))]
  # rmb_ytd <- rmb[grep("ytd", colnames(rmb))]
  # rmb_qtr <- rmb[grep("qtr", colnames(rmb))][, 12:24]
  # rmb_mth <- rmb[grep("mth", colnames(rmb))]
  # 
  # unit_mat <- unit[grep("mat", colnames(unit))]
  # unit_ytd <- unit[grep("ytd", colnames(unit))]
  # unit_qtr <- unit[grep("qtr", colnames(unit))][, 12:24]
  # unit_mth <- unit[grep("mth", colnames(unit))]
  # 
  # dot_mat <- dot[grep("mat", colnames(dot))]
  # dot_ytd <- dot[grep("ytd", colnames(dot))]
  # dot_qtr <- dot[grep("qtr", colnames(dot))][, 12:24]
  # dot_mth <- dot[grep("mth", colnames(dot))]
  
  data_names <- c("Sub.category", "Veeva.name", "Decile", "Brand_CN", "MANU_CN",
                  grep("mat_RMB", names(salesdata), value = TRUE),
                  grep("ytd_RMB", names(salesdata), value = TRUE),
                  grep("qtr_RMB", names(salesdata), value = TRUE),
                  grep("mth_RMB", names(salesdata), value = TRUE),
                  grep("mat_UNIT", names(salesdata), value = TRUE),
                  grep("ytd_UNIT", names(salesdata), value = TRUE),
                  grep("qtr_UNIT", names(salesdata), value = TRUE),
                  grep("mth_UNIT", names(salesdata), value = TRUE),
                  grep("mat_DOT", names(salesdata), value = TRUE),
                  grep("ytd_DOT", names(salesdata), value = TRUE),
                  grep("qtr_DOT", names(salesdata), value = TRUE),
                  grep("mth_DOT", names(salesdata), value = TRUE))
  
  data1 <- salesdata %>% 
    select(data_names)
  # %>% 
  #   filter(Sub.category %in% subcategory)

  # data2 <- data1 %>%
  #   group_by(Sub.category, Veeva.name, Decile, Brand_CN, MANU_CN) %>%
  #   summarise_all(sum) %>%
  #   ungroup()
  
  data3 <- data1 %>% 
    select(-`Sub.category`, -`Brand_CN`, -`MANU_CN`) %>%
    group_by(Veeva.name, Decile) %>%
    summarise_all(sum) %>%
    ungroup()
  names(data3) <- c("Veeva.name", "Decile", paste0("mkt_", names(data3)[3:length(data3)]))
  
  # data1 <- salesdata[c(1:fmr, grep("qtr", names(salesdata)))]
  # data1 <- data1[c(1:fmr, grep("RMB", names(data1)))]
  # data1 <- data1[c(1:fmr, (length(data1)-12):length(data1))]
  # data1[c((fmr+1):length(data1))] <- data1[c((fmr+1):length(data1))] / 3
  # names(data1)[(fmr+1):length(data1)] <- substr(gsub("[.]", "", names(data1)[(fmr+1):length(data1)]), 11, 15)
  
  # date <- names(data1)[(fmr+1):length(data1)]
  
  # data2 <- data1[c("Veeva.name", "Decile", "Brand_CN", "MANU_CN", date)] %>%
  #   group_by(Veeva.name, Decile, Brand_CN, MANU_CN) %>%
  #   summarise_all(sum) %>%
  #   ungroup()
  # 
  # hospital <- data2 %>%
  #   select(-Brand_CN, -Decile, -MANU_CN) %>%
  #   group_by(Veeva.name) %>%
  #   summarise_all(sum) %>%
  #   ungroup()
  # names(hospital) <- c("Veeva.name", paste0(names(hospital)[2:length(hospital)], "_mkt"))
  
  data4 <- data1 %>% 
    left_join(data3, by = c("Veeva.name", "Decile")) %>% 
    select(-`Sub.category`, -`Decile`, -`MANU_CN`) %>% 
    group_by(Veeva.name, "Decile", Brand_CN) %>% 
    summarise_all(sum) %>% 
    ungroup()
  
  for (i in data_names[6:length(data_names)]) {
    data4[paste0("ms_", i)] <- data4[i] / data4[paste0("mkt_", i)]
  }
  
  growth_names <- c(tail(grep("mat_RMB", names(salesdata), value = TRUE), 1),
                    tail(grep("ytd_RMB", names(salesdata), value = TRUE), 1),
                    tail(grep("qtr_RMB", names(salesdata), value = TRUE), 10),
                    tail(grep("mth_RMB", names(salesdata), value = TRUE), 12),
                    tail(grep("mat_UNIT", names(salesdata), value = TRUE), 1),
                    tail(grep("ytd_UNIT", names(salesdata), value = TRUE), 1),
                    tail(grep("qtr_UNIT", names(salesdata), value = TRUE), 10),
                    tail(grep("mth_UNIT", names(salesdata), value = TRUE), 12),
                    tail(grep("mat_DOT", names(salesdata), value = TRUE), 1),
                    tail(grep("ytd_DOT", names(salesdata), value = TRUE), 1),
                    tail(grep("qtr_DOT", names(salesdata), value = TRUE), 10),
                    tail(grep("mth_DOT", names(salesdata), value = TRUE), 12))
  
  for (i in grep("mat_", growth_names, value = TRUE)) {
    data4[paste0("gth_", i)] <- data4[i] / data4[which(names(data4) == i) - 1] - 1
  }
  
  for (i in grep("ytd_", growth_names, value = TRUE)) {
    data4[paste0("gth_", i)] <- data4[i] / data4[which(names(data4) == i) - 1] - 1
  }
  
  for (i in grep("qtr_", growth_names, value = TRUE)) {
    data4[paste0("gth_", i)] <- data4[i] / data4[which(names(data4) == i) - 12] - 1
  }
  
  for (i in grep("mth_", growth_names, value = TRUE)) {
    data4[paste0("gth_", i)] <- data4[i] / data4[which(names(data4) == i) - 12] - 1
  }
  
  date <- substr(tail(data_names, 1), 9, 15)
  
  table_names <- c("Veeva.name", "Brand_CN",
                   grep("mkt", grep(date, names(data4), value = TRUE), value = TRUE, invert = TRUE))
  data5 <- data4[table_names]
  
  # head <- data1[c("Category_CN",
  #                 "Region",
  #                 "Province_CN",
  #                 "City_CN",
  #                 "Decile",
  #                 "Veeva.code",
  #                 "Veeva.name",
  #                 "Brand_CN")] %>%
  #   distinct()
  # 
  # data5 <- data2 %>%
  #   mutate(growth = round(data2[[date[13]]] / data2[[date[1]]] - 1, 2),
  #          share = data4[[date[13]]],
  #          mean_mth = round(data2[[date[13]]])) %>%
  #   mutate(growth = ifelse(is.na(growth) | is.infinite(growth), "-", growth)) %>%
  #   left_join(head, by = c("Veeva.name", "Decile", "Brand_CN")) %>%
  #   dplyr::select("Category" = "Category_CN",
  #                 "Region",
  #                 "省份"= "Province_CN",
  #                 "城市" = "City_CN",
  #                 "医院等级" = "Decile",
  #                 "Veeva.code",
  #                 "Veeva Name" = "Veeva.name",
  #                 "产品" = "Brand_CN",
  #                 "增长率" = "growth",
  #                 "市场份额" = "share",
  #                 "月均产出(滚动季度数据)" = "mean_mth") %>%
  #   arrange(Category, Region, 省份, 城市, Veeva.code) %>%
  #   select(-Veeva.code)
  
  # data4 <- data4 %>%
  #   mutate(m = data4[[date[length(date)]]]) %>%
  #   arrange(Veeva.name) %>%
  #   select("Veeva.name", "Brand_CN", date)
  
  if (period == "mat" | period == "ytd") {
    df <- 1
  } else if (period == "qtr" | period == "mth") {
    df <- 12
  }
  
  data6 <- data3[c("Veeva.name", "Decile", 
                   names(data3)[which(names(data3) == paste0("mkt_", period, "_RMB_", date)) - df], 
                   paste0("mkt_qtr_RMB_", date))]
  names(data6) <- c("Veeva.name", "Decile", "past", "recent")
  data6 <- data6 %>% 
    mutate(growth = round(recent / past - 1, 2),
           hosp_rank = rank(-recent, ties.method = "min")) %>%
    select(-`past`)
  
  if (main == "Out hospital") {
    data7 <- data1[c("Brand_CN", "Veeva.name", "Decile", paste0("qtr_RMB_", date))] %>%
      filter(Brand_CN == "思合华" | Brand_CN == "思力华") %>%
      select(-`Brand_CN`)
    
  } else if (main == "HTN" & category == "ARB") {
    data7 <- data1[c("Sub.category", "Veeva.name", "Decile", "MANU_CN", paste0("qtr_RMB_", date))] %>% 
      filter(Sub.category %in% subcategory) %>%
      filter(MANU_CN == "德国勃林格殷格翰国际公司") %>%
      select(-`Sub.category`, -`MANU_CN`)
    
  } else {
    data7 <- data1[c("Veeva.name", "Decile", "MANU_CN", paste0("qtr_RMB_", date))] %>%
      filter(MANU_CN == "德国勃林格殷格翰国际公司") %>%
      select(-`MANU_CN`)
  }
  
  names(data7) <- c("Veeva.name", "Decile", "recent_bi")
  data7 <- data7 %>% 
    group_by(Veeva.name, Decile) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    mutate(BI_rank = rank(-recent_bi, ties.method = "min"))
  
  if (!(max(data7$recent_bi) > 0)) {
    data7$BI_rank <- "-"
  }
  
  if (!(max(data6$hosp_rank) > 0)) {
    data6$hosp_rank <- "-"
  }
  
  data8 <- data6 %>% 
    left_join(data7, by = c("Veeva.name", "Decile")) %>% 
    mutate(BI_rank = ifelse(is.na(BI_rank),
                            max(data7$BI_rank)+1,
                            BI_rank),
           hosp_rank = ifelse(is.na(hosp_rank),
                              max(data6$hosp_rank)+1,
                              hosp_rank)) %>%
    mutate(growth = ifelse(is.na(growth),
                           0,
                           ifelse(is.infinite(growth),
                                  1,
                                  growth))) %>%
    select("Veeva.name",
           "医院排名" = "hosp_rank",
           "BI 排名" = "BI_rank",
           "月平均单产(滚动季度数据)" = "recent",
           "品类增长率" = "growth",
           "全国医院等级" = "Decile")
  
  # data6 <- data2[c("Veeva.name", "Decile", date[1], date[length(date)])]
  # names(data6) <- c("Veeva.name", "Decile", "past", "recent")
  # data6 <- data6 %>%
  #   group_by(Veeva.name, Decile) %>%
  #   summarise(past = sum(past, na.rm = TRUE),
  #             recent = sum(recent, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(growth = round(recent / past - 1, 2),
  #          hosp_rank = rank(-recent, ties.method = "min"),
  #          recent = round(recent)) %>%
  #   select(-past)
  
  # data7 <- data2[c("Veeva.name", "Decile", "MANU_CN", date[length(date)])] %>%
  #   filter(MANU_CN == "德国勃林格殷格翰国际公司") %>%
  #   select(-MANU_CN)
  # names(data7) <- c("Veeva.name", "Decile", "recent")
  # data7 <- data7 %>%
  #   group_by(Veeva.name, Decile) %>%
  #   summarise(recent_bi = sum(recent, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(BI_rank = rank(-recent_bi, ties.method = "min"))
  
  # data8 <- data6 %>%
  #   left_join(data7, by = c("Veeva.name", "Decile")) %>%
  #   mutate(BI_rank = ifelse(is.na(recent_bi) | recent_bi <= 0,
  #                           "-",
  #                           BI_rank)) %>%
  #   mutate(hosp_rank = ifelse(is.na(recent) | recent <= 0,
  #                             "-",
  #                             hosp_rank)) %>%
  #   mutate(growth = ifelse(is.na(growth) | is.infinite(growth), "-", growth)) %>%
  #   select("Veeva.name",
  #          "医院排名" = "hosp_rank",
  #          "BI 排名" = "BI_rank",
  #          "月平均单产(滚动季度数据)" = "recent",
  #          "品类增长率" = "growth",
  #          "全国医院等级" = "Decile")
  # data8[is.na(data8)] <- "-"
  
  # data9 <- data2 %>%
  #   select("Veeva.name",
  #          "Brand_CN",
  #          date)
  
  bi_brand <- salesdata$Brand_CN[salesdata$MANU_CN == "德国勃林格殷格翰国际公司"]
  bi_brand <- bi_brand[!duplicated(bi_brand)]
  bi_brand <- as.character(bi_brand)[!is.na(bi_brand)]
  
  out <- list(rank = data8,
              table = data5,
              plot = data4,
              bi_brand = bi_brand)
  
  return(out)
}
