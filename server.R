require(DT)
require(reshape2)
require(plyr)
require(data.table)
library(shiny)
library(stringi)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(openxlsx)
library(shinydashboard)
library(rlang)
library(shinyjs)
library(webshot)
library(leaflet)
library(leafletCN)
library(shinyWidgets)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

load("./data/data.RData")
######################## load function ddd_summary #############################

source("./functions/ddd_summary.R", encoding = "UTF-8")
source("./functions/ddd_hospital.R", encoding = "UTF-8")

########################## shiny server core code###############################


server <- function(input, output, session) {
  
  summary <- reactive({
    if (is.null(input$summary))
      return(NULL)
    inFile.summary <- input$summary
    tmp <- read.csv(
      inFile.summary$datapath,
      na.strings = "NA",
      stringsAsFactors = F,
      fileEncoding = 'GB2312'
    )
    tmp$Note <- as.character(tmp$Note)
    tmp
  })
  
  main <- reactive({
    unique(summary()$Main)
  })
  
  output$main1 <- eventReactive(input$summary, {
    if (main() %in% c("Anti-PD", "Anti-thrombus", "In hospital", 
                      "Out hospital", "SPAF", "Onco", "IPF", "Pain")) {
      1
    }
  })
  outputOptions(output, "main1", suspendWhenHidden = FALSE)
  
  output$main2 <- eventReactive(input$summary, {
    if (main() == "Out hospital") {
      1
    } else if (main() == "Diabetes") {
      2
    }
  })
  outputOptions(output, "main2", suspendWhenHidden = FALSE)
  
  ##--- Category and Sub Category
  categorytype <- reactive({
    summary()$Category_CN[!duplicated(summary()$Category_CN)]
  })
  
  subcategorytype <- reactive({
    # if (is.null(input$category))
    #   return(NULL)
    subname <-
      summary()$Sub.category[which(summary()$Category_CN %in% input$category)]
    subname[!duplicated(subname)]
  })
  
  observeEvent(input$summary, {
    if (main() == "HTN") {
      updateSelectizeInput(session,
                           "category",
                           choices = categorytype(),
                           selected = "ARB",
                           options = list(
                             maxItems = 1
                           ))
    } else {
      updateSelectizeInput(session,
                           "category",
                           choices =  categorytype(),
                           selected = categorytype()[1],
                           options = list(
                             maxItems = 999
                           ))
    }
  })
  
  observeEvent(input$category, {
    if ("HTN" %in% main() & "ARB" %in% input$category) {
      updateSelectizeInput(session,
                           "subcategory",
                           choices = subcategorytype(),
                           selected = "Mono",
                           options = list(
                             maxItems = 1
                           ))
    } else if ("HTN" %in% main() & !("ARB" %in% input$category)) {
      updateSelectizeInput(session,
                           "subcategory",
                           choices = subcategorytype(),
                           selected = subcategorytype(),
                           options = list(
                             maxItems = 1
                           ))
    } else {
      updateSelectizeInput(session,
                           "subcategory",
                           choices = subcategorytype(),
                           selected = subcategorytype(),
                           options = list(
                             maxItems = 999
                           ))
    }
  })
  
  ##--- BI/Lilly
  # bl_list <- reactive({
  #   summary <- summary()
  #   summary <- summary[summary$Category_CN %in% input$category, ]
  #   summary <- summary[summary$Sub.category %in% input$subcategory, ]
  #   
  #   bl_list <- summary$Note[!duplicated(summary$Note)]
  # })
  
  observeEvent(c(input$category, input$subcategory), {
    updateSelectInput(session,
                      inputId = "bl",
                      label = "BI/Lilly",
                      choices = c("BI", "Lilly"),
                      selected = "BI")
  })
  
  ##--- Region information
  region <- reactive({
    # if ("ALL" %in% input$decile) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Decile %in% input$decile,]
    # }
    # summary <- summary()[summary()$Decile %in% input$decile,]
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    reglist <- summary()$Region[!duplicated(summary()$Region)]
    reglist <- reglist[order(reglist)]
    reglist <- c("ALL", reglist)
  })
  
  observeEvent(c(input$category, input$subcategory, input$bl), {
    updateSelectInput(session,
                      "region",
                      choices = region(),
                      selected = "ALL")
  })
  
  ##--- Province information
  province <- reactive({
    # if ("ALL" %in% input$decile) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Decile %in% input$decile,]
    # }
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    
    # if (main() %in% c("Anti-PD", "Anti-Thrombus", "In hospital", 
    #                   "Out hospital", "SPAF", "Onco", "IPF", "Pain")) {
    #   if ("ALL" %in% input$region) {
    #     summary <- summary
    #   } else {
    #     summary <- summary[summary$Region %in% input$region, ]
    #   }
    # } else {
    #   summary <- summary
    # }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    # summary <- summary()[summary()$Region %in% input$region,]
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    provlist <- summary$Province_CN[!duplicated(summary$Province_CN)]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(c(input$category, input$subcategory, input$bl, input$region), {
    updateSelectInput(session, 
                      "province",
                      choices =  province(),
                      selected = "ALL")
  })
  
  ##--- City information
  city <- reactive({
    # if ("ALL" %in% input$decile) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Decile %in% input$decile,]
    # }
    
    # if ("ALL" %in% input$region) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Region %in% input$region, ]
    # }
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province, ]
    }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    citylist <- summary$City_CN[!duplicated(summary$City_CN)]
    citylist <- citylist[order(citylist)]
    citylist <- c("ALL", citylist)
  })
  
  observeEvent(c(input$category, input$subcategory, input$bl, 
                 input$region, input$province), {
    updateSelectInput(session,
                      "city", 
                      choices =  city(),
                      selected = "ALL")
  })
  
  ##--- Decile information
  decile <- reactive({
    # if ("ALL" %in% input$region) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Region %in% input$region, ]
    # }
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province, ]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    decile_list <- summary$Decile[!duplicated(summary$Decile)]
    decile_list <- decile_list[order(decile_list)]
    decile_list <- c("ALL", decile_list)
  })

  observeEvent(c(input$category, input$subcategory, input$bl, 
                 input$region, input$province, input$city), {
    updateSelectInput(session,
                      "decile",
                      choices = decile(),
                      selected = "ALL")
  })
  
  ##--- Note
  note <- reactive({
    # if ("ALL" %in% input$decile) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Decile %in% input$decile,]
    # }
    # 
    # if ("ALL" %in% input$region) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Region %in% input$region,]
    # }
    # 
    # if ("ALL" %in% input$province) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Province_CN %in% input$province,]
    # }
    # 
    # if ("ALL" %in% input$city) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$City_CN %in% input$city, ]
    # }
    # 
    # if ("ALL" %in% input$veeva) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Veeva.code %in% input$veeva,]
    # }
    # 
    # if ("ALL" %in% input$hospital) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Veeva.name %in% input$hospital,]
    # }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    # if ("ALL" %in% input$region) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Region %in% input$region, ]
    # }
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province, ]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    if ("ALL" %in% input$decile) {
      summary <- summary
    } else {
      summary <- summary[summary$Decile %in% input$decile, ]
    }
    
    notelist <- summary$Note[!duplicated(summary$Note)]
    notelist <- notelist[order(notelist)]
    notelist <- replace_na(notelist, "NA")
    # notelist <- c("ALL", notelist)
  })
  
  observeEvent(c(input$category, input$subcategory, input$bl, 
                 input$region, input$province, input$city, input$decile), {
    updateSelectInput(session,
                      inputId = "note",
                      label = "Note",
                      choices = note(),
                      selected = note())
  })
  
  ##--- Veeva Code and Name
  
  # veeva_code <- reactive({
  #   if ("ALL" %in% input$decile) {
  #     summary <- summary()
  #   } else {
  #     summary <- summary()[summary()$Decile %in% input$decile,]
  #   }
  #   
  #   if ("ALL" %in% input$region) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$Region %in% input$region,]
  #   }
  #   
  #   if ("ALL" %in% input$province) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$Province_CN %in% input$province,]
  #   }
  #   
  #   if ("ALL" %in% input$city) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$City_CN %in% input$city, ]
  #   }
  #   # summary1 <- summary[summary$Region %in% "Beijing", ]
  #   veevacodelist <-
  #     summary$Veeva.code[!duplicated(summary$Veeva.code)]
  #   veevacodelist <- veevacodelist[order(veevacodelist)]
  #   veevacodelist <- c("ALL", veevacodelist)
  # })
  # 
  # observeEvent(input$city, {
  #   updateSelectInput(session, 
  #                     "veeva", 
  #                     choices =  veeva_code(),
  #                     selected = "ALL")
  # })
  # 
  # veeva_name <- reactive({
  #   if ("ALL" %in% input$decile) {
  #     summary <- summary()
  #   } else {
  #     summary <- summary()[summary()$Decile %in% input$decile,]
  #   }
  #   
  #   if ("ALL" %in% input$region) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$Region %in% input$region,]
  #   }
  #   
  #   if ("ALL" %in% input$province) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$Province_CN %in% input$province,]
  #   }
  #   
  #   if ("ALL" %in% input$city) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$City_CN %in% input$city, ]
  #   }
  #   
  #   if ("ALL" %in% input$veeva) {
  #     summary <- summary
  #   } else {
  #     summary <- summary[summary$Veeva.code %in% input$veeva,]
  #   }
  #   # summary1 <- summary[summary$Region %in% "Beijing", ]
  #   hosplist <- summary$Veeva.name[!duplicated(summary$Veeva.name)]
  #   hosplist <- hosplist[order(hosplist)]
  #   hosplist <- c("ALL", hosplist)
  # })
  # 
  # observeEvent(input$veeva, {
  #   updateSelectInput(session, "hospital",
  #                     choices =  veeva_name(),
  #                     selected = "ALL")
  # })
  
  ##--- Brand
  brand <- reactive({
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    # if ("ALL" %in% input$region) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Region %in% input$region, ]
    # }
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province, ]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    if ("ALL" %in% input$decile) {
      summary <- summary
    } else {
      summary <- summary[summary$Decile %in% input$decile, ]
    }
    
    if (main() == "Out hospital") {
      summary <- summary[replace_na(summary$Note, "NA") %in% input$note, ]
    }
    
    brand_list <- summary()$Brand_CN[!duplicated(summary()$Brand_CN)]
    brand_list <- brand_list[order(brand_list)]
    brand_list <- c("ALL", brand_list)
  })
  
  observeEvent(c(input$category, input$subcategory, input$bl, 
                 input$region, input$province, input$city, 
                 input$decile, input$note), {
    updateSelectInput(session,
                      inputId = "brand",
                      label = "Brand",
                      choices = brand(),
                      selected = "ALL")
  })
  
  ##--- Top
  # toplist <- reactive({
  #   summary1 <- summary()
  #   tmp <- ddd_summary(
  #     # summary,
  #     # cate = input$category,
  #     # subcate = input$subcategory,
  #     # value = "RENMINBI",
  #     # period = input$period,
  #     # kpi = input$kpi,
  #     # window = as.numeric(input$window),
  #     # level = "corporation"
  #     salesdata = summary1,
  #     cate = input$category,
  #     subcate = input$subcategory,
  #     region = input$region,
  #     province = input$province,
  #     city = input$city,
  #     decile = input$decile,
  #     note = input$note,
  #     value = "RMB",
  #     period = input$period,
  #     # kpi = c("abs", "gr")
  #     window = 1
  #   )
  #   tmp
  # })
  
  
  result1 <- reactive({
    
    # if ("ALL" %in% input$decile) {
    #   summary <- summary()
    # } else {
    #   summary <- summary()[summary()$Decile %in% input$decile, ]
    # }
    # 
    # if ("ALL" %in% input$region) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Region %in% input$region, ]
    # }
    # 
    # if ("ALL" %in% input$province) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Province_CN %in% input$province, ]
    # }
    # 
    # if ("ALL" %in% input$city) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$City_CN %in% input$city, ]
    # }
    # 
    # if ("ALL" %in% input$veeva) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Veeva.code %in% input$veeva,]
    # }
    # 
    # if ("ALL" %in% input$hospital) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Veeva.name %in% input$hospital,]
    # }
    # 
    # if ("ALL" %in% input$note) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Note %in% input$note,]
    # }
    
    # if ("ALL" %in% input$decile) {
    #   decile <- decile()
    # } else {
    #   summary <- summary[summary$Note %in% input$note,]
    # }
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    

    # if ("ALL" %in% input$region) {
    #   summary <- summary
    # } else {
    #   summary <- summary[summary$Region %in% input$region, ]
    # }
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province, ]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    if ("ALL" %in% input$decile) {
      summary <- summary
    } else {
      summary <- summary[summary$Decile %in% input$decile, ]
    }
    
    if (main() == "Out hospital") {
      summary <- summary[replace_na(summary$Note, "NA") %in% input$note, ]
    }
    
    if ("ALL" %in% input$brand) {
      summary <- summary
    } else {
      summary <- summary[summary$Brand_CN %in% input$brand, ]
    }
    
    category <- unique(summary$Category)
    subcategory <- unique(summary$Sub.category)
    
    region <- unique(summary$Region)
    province <- unique(summary$Province_CN)
    city <- unique(summary$City_CN)
    decile <- unique(summary$Decile)
    # if ("ALL" %in% input$note) {
    #   decile <- unique(summary$Decile)
    # } else {
    #   decile <- input$decile
    # }
    # veeva <- unique(summary$Veeva.code)
    # hosp_name <- unique(summary$Veeva.name)
    note <- unique(summary$Note)
    brand <- unique(summary$Brand_CN)
    
    rank_info <- rbind.fill(
      ddd_summary(
        salesdata = summary(),
        cate = input$category,
        subcate = input$subcategory,
        region = region,
        province = province,
        city = city,
        decile = decile,
        # veeva = veeva,
        # hosp_name = hosp_name,
        note = note,
        value = "RMB",
        period = input$period,
        brand = brand,
        # kpi = c("abs", "gr")
        # window = as.numeric(input$window)
        window = 1
      )
    )
    rank_info <- distinct(rank_info)
    rank_info_m <- rank_info[, c("医院排名", "BI产品贡献排名", "Veeva Code", "Veeva Name")]
    
    if ("RMB"  %in% input$value) {
      summary <- summary()
      # if ("ALL" %in% input$province) {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
      # } else {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
      # }
      
      # rmb <- rbind.fill(
      #   ddd_summary(
      #     salesdata = summary,
      #     cate = input$category,
      #     subcate = input$subcategory,
      #     region = region,
      #     province = province,
      #     city = city,
      #     decile = decile,
      #     # veeva = veeva,
      #     # hosp_name = hosp_name,
      #     note = note,
      #     value = "RMB",
      #     period = input$period,
      #     brand = brand,
      #     # kpi = c("abs", "gr")
      #     # window = as.numeric(input$window)
      #     window = 1
      #   )
      # )
      # rmb <- distinct(rmb)
      rmb <- rank_info
    } else{
      rmb <- NULL
    }
    
    
    if ("UNIT" %in% input$value) {
      summary <- summary()
      # if ("ALL" %in% input$province) {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
      #
      # } else{
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
      # }
      
      unit <- rbind.fill(
        ddd_summary(
          salesdata = summary,
          cate = input$category,
          subcate = input$subcategory,
          region = region,
          province = province,
          city = city,
          decile = decile,
          # veeva = veeva,
          # hosp_name = hosp_name,
          note = note,
          value = "UNIT",
          period = input$period,
          brand = brand,
          # kpi = c("abs", "gr")
          # window = as.numeric(input$window)
          window = 1
        )
      )
      unit <- distinct(unit) %>% 
        dplyr::select(-`医院排名`, -`BI产品贡献排名`) %>% 
        left_join(rank_info_m, by = c("Veeva Code", "Veeva Name")) %>% 
        dplyr::select("医院排名", "BI产品贡献排名", "Region", "省份", "城市", "Veeva Code", 
                      "Veeva Name", "医院等级", "医院增长率", "医院贡献率", "BI产品增长率", 
                      "BI产品贡献率", "BI产品市场份额", "增长指数", "贡献指数")
      
    } else{
      unit <- NULL
    }
    
    if ("DOT"  %in% input$value) {
      summary <- summary()
      # if ("ALL" %in% input$province) {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
      # } else{
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
      # }
      
      dot <- rbind.fill(
        ddd_summary(
          salesdata = summary,
          cate = input$category,
          subcate = input$subcategory,
          region = region,
          province = province,
          city = city,
          decile = decile,
          # veeva = veeva,
          # hosp_name = hosp_name,
          note = note,
          value = "DOT",
          period = input$period,
          brand = brand,
          # kpi = c("abs", "gr")
          # window = as.numeric(input$window)
          window = 1
        )
      )
      dot <- distinct(dot) %>% 
        dplyr::select(-`医院排名`, -`BI产品贡献排名`) %>% 
        left_join(rank_info_m, by = c("Veeva Code", "Veeva Name")) %>% 
        dplyr::select("医院排名", "BI产品贡献排名", "Region", "省份", "城市", "Veeva Code", 
                      "Veeva Name", "医院等级", "医院增长率", "医院贡献率", "BI产品增长率", 
                      "BI产品贡献率", "BI产品市场份额", "增长指数", "贡献指数")
    } else{
      dot <- NULL
    }
    result1 <- rbind(rmb, unit, dot)
  })
  
  
  ot <- reactive({
    if (input$goButton == 0)
      return(NULL)
    
    input$goButton
    
    isolate({
      outputtable <- result1() %>%
        ungroup()
      outputtable
    })
  })
  
  # pagenumber <- reactive({
  #   if (input$top == "20") {
  #     pageno = 20
  #   } else if (input$top == "50") {
  #     pageno = 20
  #   } else if (input$top == "100") {
  #     pageno = 20
  #   } else{
  #     pageno = 20
  #   }
  #   pageno
  # })
  
  output$contents <- renderDataTable({
    input$goButton
    isolate({
      if (is.null(ot()))
        return(NULL)
      ot <- ot()
      ot <- as.data.frame(ot)
      dat <- DT::datatable(
        ot,
        rownames = FALSE,
        extensions = c('FixedColumns', 'Buttons'),
        options = list(
          autoWidth = TRUE,
          dom = '<"bottom">Bfrtpl',
          buttons = I('colvis'),
          # scrollX = TRUE,
          paging = TRUE,
          columnDefs = list(list(
            className = 'dt-center',
            targets = seq(0, 14)
          )),
          autoWidth = TRUE,
          pageLength = 20,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#1F497D', 'color': '#fff'});",
            "}"
          )
        )
      ) %>%
       
        formatStyle(
          c(
            "医院排名",
            "BI产品贡献排名",
            "Region",
            "省份",
            "城市",
            "Veeva Code",
            "Veeva Name",
            "医院等级",
            "医院增长率",
            "医院贡献率",
            "BI产品增长率",
            "BI产品贡献率",
            "BI产品市场份额",
            "增长指数",
            "贡献指数"
          ),
          fontWeight = 'bold'
        ) %>%
        formatStyle(c("增长指数", "贡献指数"),
                    color = styleInterval(100, c('red', 'green')),
                    fontWeight = styleInterval(100, c('bold', 'normal'))) %>%
        formatStyle(c("医院增长率", "BI产品增长率"),
                    color = styleInterval(0, c('red', 'green')),
                    fontWeight = styleInterval(0, c('bold', 'normal'))) %>%
        formatPercentage(c("医院增长率", "医院贡献率", "BI产品增长率",
                           "BI产品贡献率", "BI产品市场份额"), 2) %>%
        formatRound(c("增长指数", "贡献指数"), 0)
      return(dat)
    })
    
  })
  
  ##-- hospital
  ##--- category
  categorytype1 <- reactive({
    summary()$Category_CN[!duplicated(summary()$Category_CN)]
  })
  
  observeEvent(input$summary, {
    if (main() == "HTN") {
      updateSelectizeInput(session,
                           "category1",
                           choices = categorytype1(),
                           selected = "ARB",
                           options = list(
                             maxItems = 1
                           ))
    } else {
      updateSelectizeInput(session,
                           "category1",
                           choices =  categorytype1(),
                           selected = categorytype1()[1],
                           options = list(
                             maxItems = 999
                           ))
    }
  })
  
  ##--- Subcategory
  subcategorytype1 <- reactive({
    # if (is.null(input$category1))
    #   return(NULL)
    subname <- summary()$Sub.category[which(summary()$Category_CN %in% input$category1)]
    subname[!duplicated(subname)]
  })
  
  observeEvent(input$category1, {
    if ("HTN" %in% main() & "ARB" %in% input$category1) {
      updateSelectizeInput(session,
                           "subcategory1",
                           choices = subcategorytype1(),
                           selected = "Mono",
                           options = list(
                             maxItems = 1
                           ))
    } else if ("HTN" %in% main() & !("ARB" %in% input$category1)) {
      updateSelectizeInput(session,
                           "subcategory1",
                           choices = subcategorytype1(),
                           selected = subcategorytype1(),
                           options = list(
                             maxItems = 1
                           ))
    } else {
      updateSelectizeInput(session,
                           "subcategory1",
                           choices = subcategorytype1(),
                           selected = subcategorytype1(),
                           options = list(
                             maxItems = 999
                           ))
    }
  })
  
  ##--- BI/Lilly
  observeEvent(c(input$category1, input$subcategory1), {
    updateSelectInput(session,
                      inputId = "bl1",
                      label = "BI/Lilly",
                      choices = c("BI", "Lilly"),
                      selected = "BI")
  })
  
  observeEvent(input$subcategory1, {
    updateSelectInput(session,
                      inputId = "bl1",
                      label = "BI/Lilly",
                      choices = c("BI", "Lilly"),
                      selected = "BI")
  })
  
  ##--- region
  region <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    reglist <- summary()$Region[!duplicated(summary()$Region)]
    reglist <- reglist[order(reglist)]
    reglist <- c("ALL", reglist)
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1), {
    updateSelectInput(session,
                      "region1",
                      choices = region(),
                      selected = "ALL")
  })
  ##--- province
  province1 <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    provlist <- summary$Province_CN[!duplicated(summary$Province_CN)]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1, input$region1), {
    updateSelectInput(session,
                      "province1",
                      choices =  province1(),
                      selected = "ALL")
  })
  
  ##--- city
  city1 <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    if ("ALL" %in% input$province1) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province1, ]
    }
    citylist <- summary$City_CN[!duplicated(summary$City_CN)]
    citylist <- citylist[order(citylist)]
    citylist <- c("ALL", citylist)
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1, 
                 input$region1, input$province1), {
    updateSelectInput(session,
                      "city1",
                      choices =  city1(),
                      selected = "ALL")
  })
  
  ##--- decile
  decile1 <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    if ("ALL" %in% input$province1) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province1, ]
    }
    
    if ("ALL" %in% input$city1) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city1, ]
    }
    
    decile_list <- summary$Decile[!duplicated(summary$Decile)]
    decile_list <- decile_list[order(decile_list)]
    decile_list <- c("ALL", decile_list)
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1, 
                 input$region1, input$province1, input$city1), {
    updateSelectInput(session,
                      "decile1",
                      choices = decile1(),
                      selected = "ALL")
  })
  
  ##--- Note
  note1 <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    if ("ALL" %in% input$province1) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province1, ]
    }
    
    if ("ALL" %in% input$city1) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city1, ]
    }
    
    if ("ALL" %in% input$decile1) {
      summary <- summary
    } else {
      summary <- summary[summary$Decile %in% input$decile1, ]
    }
    
    notelist <- summary$Note[!duplicated(summary$Note)]
    notelist <- notelist[order(notelist)]
    notelist <- replace_na(notelist, "NA")
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1, 
                 input$region1, input$province1, input$city1, 
                 input$decile1), {
    updateSelectInput(session,
                      inputId = "note1",
                      label = "Note",
                      choices = note1(),
                      selected = note1())
  })
  
  ##--- code and name
  c_n <- reactive({
    
    summary <- summary()
    summary <- summary[summary$Category_CN %in% input$category1, ]
    summary <- summary[summary$Sub.category %in% input$subcategory1, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl1, ]
    }
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    if ("ALL" %in% input$province1) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province1, ]
    }
    
    if ("ALL" %in% input$city1) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city1, ]
    }
    
    if ("ALL" %in% input$decile1) {
      summary <- summary
    } else {
      summary <- summary[summary$Decile %in% input$decile1, ]
    }
    
    if (main() == "Out hospital") {
      summary <- summary[replace_na(summary$Note, "NA") %in% input$note1, ]
    }
    
    c_n <- summary[, c("Veeva.code", "Veeva.name")]
    c_n <- distinct(c_n)
  })
  
  observeEvent(c(input$category1, input$subcategory1, input$bl1,
                 input$region1, input$province1, input$city1,
                 input$decile1, input$note1, input$code), ignoreInit = TRUE, {
    updateSelectInput(session,
                      inputId = "name",
                      label = "Hospital Name",
                      choices = c_n()$`Veeva.name`,
                      selected = c_n()[c_n()$`Veeva.code` == input$code, "Veeva.name", drop = TRUE])
  })

  observeEvent(c(input$category1, input$subcategory1, input$bl1,
                 input$region1, input$province1, input$city1,
                 input$decile1, input$note1, input$name), ignoreInit = TRUE, {
    updateSelectInput(session,
                      inputId = "code",
                      label = "Veeva Code",
                      choices = c_n()$`Veeva.code`,
                      selected = c_n()[c_n()$`Veeva.name` == input$name, "Veeva.code", drop = TRUE])
  })
  
  ##--- result2
  result2 <- reactive({
    if (is.null(summary()))
      return(NULL)
    
    ddd_hospital(summary())
  })
  
  ##--- rank
  rank <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    r <- result2()$rank
    r <- r %>%
      filter(Veeva.name == input$name)
    
    if (dim(r)[1] == 0)
      return(NULL)
    
    return(r)
  })
  
  output$rank1 <- renderDataTable({
    
    if (is.null(rank())) {
      r1 <- tibble(`医院排名` = "-", `BI 排名` = "-")
    } else {
      r1 <- rank()
      r1 <- r1[c(2, 3)]
    }
    
    r <- DT::datatable(
      r1,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = seq(0, 1)
        )),
        autoWidth = TRUE,
        pageLength = 1,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        c(
          "医院排名",
          "BI 排名"
        ),
        `font-size` = '45px',
        fontWeight = 'bold',
        color = '#1F497D'
      )
    
    return(r)
  })
  
  output$rank2 <- renderDataTable({
    
    if (is.null(rank())) {
      r1 <- tibble(`月平均单产(滚动季度数据)` = "-")
    } else {
      r1 <- rank()
      r1 <- r1[4]
    }
    
    r <- DT::datatable(
      r1,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = c(0)
        )),
        autoWidth = TRUE,
        pageLength = 1,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        c(
          "月平均单产(滚动季度数据)"
        ),
        `font-size` = '45px',
        fontWeight = 'bold',
        color = '#1F497D'
      ) %>%
      formatRound(c("月平均单产(滚动季度数据)"), 0)
    
    return(r)
  })
  
  output$rank3 <- renderDataTable({
    
    if (is.null(rank())) {
      r1 <- tibble(`品类增长率` = "-")
    } else {
      r1 <- rank()
      r1 <- r1[5]
    }
    
    r <- DT::datatable(
      r1,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = c(0)
        )),
        autoWidth = TRUE,
        pageLength = 1,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        c(
          "品类增长率"
        ),
        `font-size` = '45px',
        color = '#1F497D',
        # color = styleInterval(0, c('red', 'green')),
        # fontWeight = styleInterval(0, c('bold', 'normal')),
        fontWeight = 'bold') %>%
      formatPercentage(c("品类增长率"), 0)
    
    return(r)
  })
  
  output$rank4 <- renderDataTable({
    
    if (is.null(rank())) {
      r1 <- tibble(`全国医院等级` = "-")
    } else {
      r1 <- rank()
      r1 <- r1[6]
    }
    
    r <- DT::datatable(
      r1,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        scrollX = FALSE,
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = c(0)
        )),
        autoWidth = TRUE,
        pageLength = 1,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      
      formatStyle(
        c(
          "全国医院等级"
        ),
        `font-size` = '51px',
        fontWeight = 'bold',
        color = '#1F497D'
      )
    
    return(r)
  })
  
  ##--- table contents
  bi_brand <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    result2()$bi_brand
  })
  
  ot1 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    ot1 <- result2()$table
    ot1 <- ot1 %>%
      filter(`Veeva.name` == input$name)
    
    if (dim(ot1)[1] == 0)
      return(NULL)
    
    ot1_names <- c("Brand_CN", grep(paste0(input$period1, "_", input$value1), names(ot1), value = TRUE))
    ot1 <- ot1[ot1_names]
    
    if (length(ot1) == 3) {
      ot1["growth"] <- " "
    }
    
    colnames(ot1) <- c("产品", "产出", "市场份额", "增长率")
    
    t1 <- ot1[ot1$产品 %in% bi_brand(), ] %>%
      arrange(-`产出`)
    
    t2 <- ot1[!(ot1$产品 %in% bi_brand()), ] %>%
      arrange(-`产出`)
    
    t <- bind_rows(t1, t2) %>% 
      mutate_all(function(x) {ifelse(is.na(x),
                                     0,
                                     ifelse(is.infinite(x),
                                            1,
                                            x))})
    
    return(t)
  })

  output$contents_hosp <- renderDataTable({
    
    if (is.null(ot1())) {
      ot1 <- tibble(`产品` = " ", `产出` = " ", `市场份额` = " ", `增长率` = " ")
    } else {
      ot1 <- ot1()
    }
    
    rows <- (dim(ot1)[1] %/% 18 + 1) * 18
    ot <- tibble(`产品` = rep(" ", rows), `产出` = rep(" ", rows), `市场份额` = rep(" ", rows), `增长率` = rep(" ", rows))
    
    for (i in 1:dim(ot1)[1]) {
      ot[i, ] <- ot1[i, ]
    }
    
    ot <- ot %>%
      mutate(序号 = 1:rows) %>%
      dplyr::select(
        "序号",
        "产品",
        "增长率",
        "市场份额",
        "产出"
      )
    
    dat <- DT::datatable(
      ot,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        # scrollX = FALSE,
        # scrollY = FALSE,
        ordering = FALSE,
        paging = TRUE,
        bLengthChange = FALSE,
        searching = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = seq(0, 4)
        )),
        autoWidth = TRUE,
        pageLength = 18,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        c(
          "序号",
          "产品"
        ),
        fontWeight = 'bold',
        `font-size` = '15px',
        color = '#000'
      ) %>%
      formatStyle(
        c(
          "增长率",
          "市场份额",
          "产出"
        ),
        fontWeight = 'bold',
        `font-size` = '15px',
        color = '#1F497D'
      ) %>%
      formatStyle(c("增长率"),
                  color = styleInterval(0, c('red', 'green')),
                  # fontWeight = styleInterval(0, c('bold', 'normal')),
                  fontWeight = 'bold') %>%
      formatStyle("序号",
                  target = "row",
                  backgroundColor = styleEqual(1:rows, rep(c('#DCE6F0', 'white'), rows/2))) %>%
      formatPercentage(c("增长率"), 0) %>%
      formatPercentage(c("市场份额"), 1) %>%
      formatRound(c("产出"), 0)
    
    return(dat)
  })
  
  ##--- plot contents
  plot1 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(-Veeva.name)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    pd_names <- c("Brand_CN", grep("ms", grep(paste0(input$period1, "_", input$value1), names(pd), value = TRUE), value = TRUE))
    pd1 <- pd[pd_names]
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      names(pd1) <- c("Brand_CN", 1, 2)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`2`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`2`)
      
    } else if (input$period1 == "qtr") {
      pd1 <- pd1[c("Brand_CN", tail(names(pd1), 13))]
      names(pd1) <- c("Brand_CN", 1:13)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`13`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`13`)
      
    } else if (input$period1 == "mth") {
      names(pd1) <- c("Brand_CN", 1:24)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`24`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`24`)
    }
    
    pd2 <- bind_rows(d1, d2)
    pd2[is.na(pd2)] <- 0
    
    pd_names1 <- tail(pd_names, length(pd2)-1)
    for (i in 1:(length(pd2)-1)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd2)[i+1] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- pd2$Brand_CN
    
    pd3 <- pd2 %>%
      melt(id.vars = "Brand_CN", variable.name = "Date", value.name = "Share") %>%
      mutate(Share = Share * 100,
             Share = round(Share, 2))
    
    p <- plot_ly(hoverinfo = "name + x + y")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = pd3[pd3$Brand_CN == i, "Share"],
                  type = "scatter",
                  mode = "lines + markers",
                  marker = list(size = 5),
                  name = i)
      
      if (!is.na(d1$Brand_CN[1])) {
        if (i == d1$Brand_CN[1]) {
          p <- p %>%
            add_text(x = pd3[pd3$Brand_CN == i, "Date"],
                     y = pd3[pd3$Brand_CN == i, "Share"],
                     text = paste0(pd3[pd3$Brand_CN == i, "Share"], "%"),
                     textfont = list(size = 13),
                     textposition = "top",
                     name = i,
                     showlegend = TRUE)
        }
      }
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE
        # collaborate = FALSE
      ) %>%
      layout(
        annotations = list(
          text = paste0("市场份额趋势 (", toupper(input$period1), ")"),
          xref = "paper",
          x = 0.5,
          yref = "paper",
          y = 1,
          yshift = 30,
          showarrow = FALSE,
          font = list(size = 18)
        ),
        showlegend = TRUE,
        xaxis = list(
          zeroline = FALSE,
          title = "",
          showline = TRUE,
          mirror = "ticks"
        ),
        yaxis = list(
          zeroline = FALSE,
          title = paste0("Market share (", input$value1, ")"),
          ticksuffix = "%",
          showline = TRUE,
          mirror = "ticks"
        )
      )
    
    return(p)
  })
  
  output$plot1 <- renderPlotly({
    if (is.null(plot1())) {
      plotly_empty()
    } else {
      plot1()
    }
  })
  
  plot2 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(-Veeva.name)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    pd_names <- c("Brand_CN", grep("mkt|ms|gth", grep(paste0(input$period1, "_", input$value1), names(pd), value = TRUE), invert = TRUE, value = TRUE))
    pd1 <- pd[pd_names]
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      names(pd1) <- c("Brand_CN", 1, 2)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`2`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`2`)
      
    } else if (input$period1 == "qtr") {
      pd1 <- pd1[c("Brand_CN", tail(names(pd1), 13))]
      names(pd1) <- c("Brand_CN", 1:13)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`13`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`13`)
      
    } else if (input$period1 == "mth") {
      names(pd1) <- c("Brand_CN", 1:24)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`24`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`24`)
    }
    
    pd2 <- bind_rows(d1, d2)
    pd2[is.na(pd2)] <- 0
    
    pd_names1 <- tail(pd_names, length(pd2)-1)
    for (i in 1:(length(pd2)-1)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd2)[i+1] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- pd2$Brand_CN
    
    pd3 <- pd2 %>%
      melt(id.vars = "Brand_CN", variable.name = "Date", value.name = "Sales")
    
    p <- plot_ly(hoverinfo = "name + x + y")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = round(pd3[pd3$Brand_CN == i, "Sales"]),
                  type = "scatter",
                  mode = "lines + markers",
                  marker = list(size = 5),
                  name = i)
      
      if (!is.na(d1$Brand_CN[1])) {
        if (i == d1$Brand_CN[1]) {
          p <- p %>%
            add_text(x = pd3[pd3$Brand_CN == i, "Date"],
                     y = round(pd3[pd3$Brand_CN == i, "Sales"]),
                     text = round(pd3[pd3$Brand_CN == i, "Sales"], 0),
                     textfont = list(size = 13),
                     textposition = "top",
                     name = i,
                     showlegend = TRUE)
        }
      }
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE
        # collaborate = FALSE
      ) %>%
      layout(
        annotations = list(
          text = paste0("单产金额趋势 (", toupper(input$period1), ")"),
          xref = "paper",
          x = 0.5,
          yref = "paper",
          y = 1,
          yshift = 30,
          showarrow = FALSE,
          font = list(size = 18)
        ),
        showlegend = TRUE,
        xaxis = list(
          zeroline = FALSE,
          title = "",
          showline = TRUE,
          mirror = "ticks"
        ),
        yaxis = list(
          zeroline = FALSE,
          title = paste0("Production (", input$value1, ")"),
          ticksuffix = "",
          showline = TRUE,
          mirror = "ticks"
        )
      )
    
    return(p)
  })
  
  output$plot2 <- renderPlotly(
    if (is.null(plot2())) {
      plotly_empty()
    } else {
      plot2()
    }
  )
  
  plot3 <- reactive({
    if (is.null(result2()) | input$period1 == "mat" | input$period1 == "ytd")
      return(NULL)
    
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(-Veeva.name)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    pd_names <- c("Brand_CN", grep("gth", grep(paste0(input$period1, "_", input$value1), names(pd), value = TRUE), value = TRUE))
    pd1 <- pd[pd_names]
    
    if (input$period1 == "qtr") {
      # pd1 <- pd1[c("Brand_CN", tail(names(pd1), 10))]
      names(pd1) <- c("Brand_CN", 1:10)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`10`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`10`)
      
    } else if (input$period1 == "mth") {
      names(pd1) <- c("Brand_CN", 1:12)
      
      d1 <- pd1[pd1$Brand_CN %in% bi_brand(), ] %>% 
        arrange(-`12`)
      d2 <- pd1[!(pd1$Brand_CN %in% bi_brand()), ] %>% 
        arrange(-`12`)
    }
    
    pd2 <- bind_rows(d1, d2)
    pd2[is.na(pd2)] <- 0
    
    pd_names1 <- tail(pd_names, length(pd2)-1)
    for (i in 1:(length(pd2)-1)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd2)[i+1] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- pd2$Brand_CN
    
    pd3 <- pd2 %>%
      melt(id.vars = "Brand_CN", variable.name = "Date", value.name = "Growth") %>%
      mutate(Growth = Growth * 100,
             Growth = round(Growth, 2))
    
    p <- plot_ly(hoverinfo = "name + x + y")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = pd3[pd3$Brand_CN == i, "Growth"],
                  type = "scatter",
                  mode = "lines + markers",
                  marker = list(size = 5),
                  name = i)
      
      if (!is.na(d1$Brand_CN[1])) {
        if (i == d1$Brand_CN[1]) {
          p <- p %>%
            add_text(x = pd3[pd3$Brand_CN == i, "Date"],
                     y = pd3[pd3$Brand_CN == i, "Growth"],
                     text = paste0(pd3[pd3$Brand_CN == i, "Growth"], "%"),
                     textfont = list(size = 13),
                     textposition = "top",
                     name = i,
                     showlegend = TRUE)
        }
      }
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE
        # collaborate = FALSE
      ) %>%
      layout(
        annotations = list(
          text = paste0("增长率趋势 (", toupper(input$period1), ")"),
          xref = "paper",
          x = 0.5,
          yref = "paper",
          y = 1,
          yshift = 30,
          showarrow = FALSE,
          font = list(size = 18)
        ),
        showlegend = TRUE,
        xaxis = list(
          zeroline = FALSE,
          title = "",
          showline = TRUE,
          mirror = "ticks"
        ),
        yaxis = list(
          zeroline = FALSE,
          title = paste0("Growth Rate (", input$value1, ")"),
          ticksuffix = "%",
          showline = TRUE,
          mirror = "ticks"
        )
      )
    
    return(p)
  })
  
  output$plot3 <- renderPlotly({
    if (is.null(plot3())) {
      plotly_empty()
    } else {
      plot3()
    }
  })

  ##-- download the data
  
  writeDown <- function(data1, name) {
    wb <- createWorkbook()
    ## 1
    addWorksheet(wb, name)
    writeDataTable(
      wb,
      sheet = name,
      x = data1,
      withFilter = F,
      startRow = 1,
      rowNames = F,
      colNames = T
    )
    # ## 2
    # addWorksheet(wb, "Corporation")
    # writeDataTable(
    #   wb,
    #   sheet = "Corporation",
    #   x = data2,
    #   withFilter = F,
    #   startRow = 1,
    #   rowNames = F,
    #   colNames = T
    # )
    # ## 3
    # addWorksheet(wb, "Molecule")
    # writeDataTable(
    #   wb,
    #   sheet = "Molecule",
    #   x = data3,
    #   withFilter = F,
    #   startRow = 1,
    #   rowNames = F,
    #   colNames = T
    # )
    return(wb)
  }
  
  otdown <- reactive({
    if (input$goButton == 0)
      return(NULL)
    
    input$goButton
    
    isolate({
      if (is.null(ot()))
        return(NULL)
      ot <- ot()
      ot <- as.data.frame(ot)
      ot
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ddd_performance_",
            input$period,
            "_",
            # input$window,
            1,
            "year",
            '.xlsx',
            sep = '')
    },
    content = function(file) {
      saveWorkbook(writeDown(otdown(), "ddd_performance"),
                   file,
                   overwrite = TRUE)
      
    }
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$veeva1, ".xlsx")
    },
    
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, input$veeva1)
      writeDataTable(
        wb,
        sheet = input$veeva1,
        x = rank(),
        withFilter = F,
        startRow = 1,
        rowNames = F,
        colNames = T
      )
      writeDataTable(
        wb,
        sheet = input$veeva1,
        x = ot1(),
        withFilter = F,
        startRow = 5,
        rowNames = F,
        colNames = T
      )
      
      saveWorkbook(wb,
                   file,
                   overwrite = TRUE)
    }
  )
  
}
