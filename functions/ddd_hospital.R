
ddd_hospital <- function(salesdata) {
  
  fmr <-
    min(c(which(grepl(
      "RMB", names(salesdata)
    )),  which(grepl(
      "UNIT", names(salesdata)
    )), which(grepl(
      "DOT", names(salesdata)
    )))) - 1
  
  data1 <- salesdata[c(1:fmr, grep("qtr", names(salesdata)))]
  data1 <- data1[c(1:fmr, grep("RMB", names(data1)))]
  data1 <- data1[c(1:fmr, (length(data1)-12):length(data1))]
  data1[c((fmr+1):length(data1))] <- data1[c((fmr+1):length(data1))] / 3
  names(data1)[(fmr+1):length(data1)] <- substr(gsub("[.]", "", names(data1)[(fmr+1):length(data1)]), 11, 15)
  
  date <- names(data1)[(fmr+1):length(data1)]
  
  data2 <- data1[c("Veeva.name", "Decile", "Brand_CN", "MANU_CN", date)] %>%
    group_by(Veeva.name, Decile, Brand_CN, MANU_CN) %>%
    summarise_all(sum) %>%
    ungroup()
  
  hospital <- data2 %>%
    select(-Brand_CN, -Decile, -MANU_CN) %>%
    group_by(Veeva.name) %>%
    summarise_all(sum) %>%
    ungroup()
  names(hospital) <- c("Veeva.name", paste0(names(hospital)[2:length(hospital)], "_mkt"))
  
  data3 <- data2 %>%
    left_join(hospital, by = "Veeva.name")
  
  data4 <- data3[c(1, 3)]
  for (i in 1:13) {
    x <- data3[4+i] / data3[4+i+13]
    x[is.na(x)] <- 0
    
    data4[date[i]] <- round(x, 3)
  }
  
  head <- data1[c("Category_CN",
                  "Region",
                  "Province_CN",
                  "City_CN",
                  "Decile",
                  "Veeva.code",
                  "Veeva.name",
                  "Brand_CN")] %>%
    distinct()
  
  data5 <- data2 %>%
    mutate(growth = round(data2[[date[13]]] / data2[[date[1]]] - 1, 2),
           share = data4[[date[13]]],
           mean_mth = round(data2[[date[13]]])) %>%
    mutate(growth = ifelse(is.na(growth) | is.infinite(growth), "-", growth)) %>%
    left_join(head, by = c("Veeva.name", "Decile", "Brand_CN")) %>%
    dplyr::select("Category" = "Category_CN",
                  "Region",
                  "省份"= "Province_CN",
                  "城市" = "City_CN",
                  "医院等级" = "Decile",
                  "Veeva.code",
                  "Veeva Name" = "Veeva.name",
                  "产品" = "Brand_CN",
                  "增长率" = "growth",
                  "市场份额" = "share",
                  "月均产出(滚动季度数据)" = "mean_mth") %>%
    arrange(Category, Region, 省份, 城市, Veeva.code) %>%
    select(-Veeva.code)
  
  data4 <- data4 %>%
    # mutate(m = data4[[date[length(date)]]]) %>%
    arrange(Veeva.name) %>%
    select("Veeva.name", "Brand_CN", date)
  
  data6 <- data2[c("Veeva.name", "Decile",date[1], date[length(date)])]
  names(data6) <- c("Veeva.name", "Decile", "past", "recent")
  data6 <- data6 %>%
    group_by(Veeva.name, Decile) %>%
    summarise(past = sum(past, na.rm = TRUE),
              recent = sum(recent, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(growth = round(recent / past - 1, 2),
           hosp_rank = rank(-recent, ties.method = "min"),
           recent = round(recent)) %>%
    select(-past)
  
  data7 <- data2[c("Veeva.name", "Decile", "MANU_CN", date[length(date)])] %>%
    filter(MANU_CN == "德国勃林格殷格翰国际公司") %>%
    select(-MANU_CN)
  names(data7) <- c("Veeva.name", "Decile", "recent")
  data7 <- data7 %>%
    group_by(Veeva.name, Decile) %>%
    summarise(recent_bi = sum(recent, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(BI_rank = rank(-recent_bi, ties.method = "min"))
  
  data8 <- data6 %>%
    left_join(data7, by = c("Veeva.name", "Decile")) %>%
    mutate(BI_rank = ifelse(is.na(recent_bi) | recent_bi <= 0,
                            "-",
                            BI_rank)) %>%
    mutate(hosp_rank = ifelse(is.na(recent) | recent <= 0,
                              "-",
                              hosp_rank)) %>%
    mutate(growth = ifelse(is.na(growth) | is.infinite(growth), "-", growth)) %>%
    select("Veeva.name",
           "医院排名" = "hosp_rank",
           "BI 排名" = "BI_rank",
           "月平均单产(滚动季度数据)" = "recent",
           "品类增长率" = "growth",
           "全国医院等级" = "Decile")
  data8[is.na(data8)] <- "-"
  
  data9 <- data2 %>%
    select("Veeva.name",
           "Brand_CN",
           date)
  
  bi_brand <- data2$Brand_CN[data2$MANU_CN == "德国勃林格殷格翰国际公司"]
  bi_brand <- bi_brand[!duplicated(bi_brand)]
  bi_brand <- as.character(bi_brand)[!is.na(bi_brand)]
  
  out <- list(rank = data8,
              table = data5,
              share = data4,
              mean_mth = data9,
              bi_brand = bi_brand,
              date = date)
  
  return(out)
}
