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
    ) %>% 
      mutate(Region = as.character(Region),
             Region = ifelse(is.na(Region),
                             "-",
                             Region),
             Province_CN = as.character(Province_CN),
             Province_CN = ifelse(is.na(Province_CN),
                                  "-",
                                  Province_CN),
             City_CN = as.character(City_CN),
             City_CN = ifelse(is.na(City_CN),
                              "-",
                              City_CN),
             Veeva.code = as.character(Veeva.code),
             Veeva.code = ifelse(is.na(Veeva.code),
                                 "-",
                                 Veeva.code),
             Veeva.name = as.character(Veeva.name),
             Veeva.name = ifelse(is.na(Veeva.name),
                                 "-",
                                 Veeva.name),
             Decile = as.character(Decile),
             Decile = ifelse(is.na(Decile),
                             "-",
                             Decile),
             Brand_CN = as.character(Brand_CN),
             Brand_CN = ifelse(is.na(Brand_CN),
                               "-",
                               Brand_CN),
             MANU_CN = as.character(MANU_CN),
             MANU_CN = ifelse(is.na(MANU_CN),
                              "-",
                              MANU_CN))
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
    summary()$Category[!duplicated(summary()$Category)]
  })
  
  subcategorytype <- reactive({
    if (is.null(input$category))
      return(NULL)
    subname <-
      summary()$Sub.category[which(summary()$Category %in% input$category)]
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
                           selected = categorytype(),
                           options = list(
                             maxItems = 99
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
  
  observeEvent(c(input$category, input$subcategory), {
    updateSelectInput(session,
                      inputId = "bl",
                      label = "BI/Lilly",
                      choices = c("BI", "Lilly"),
                      selected = "BI")
  })
  
  ##--- Note
  note <- reactive({
    if (is.null(summary()))
      return(NULL)
    
    summary <- summary()
    summary <- summary[summary$Category %in% input$category, ]
    summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      summary <- summary[summary$Note %in% input$bl, ]
    }
    
    notelist <- summary$Note[!duplicated(summary$Note)]
    notelist <- notelist[order(notelist)]
    notelist <- replace_na(notelist, "NA")
    # notelist <- c("ALL", notelist)
  })
  
  ##--- BI/Lilly
  observeEvent(c(input$category, input$subcategory, input$bl), {
    updateSelectInput(session,
                      inputId = "note",
                      label = "Note",
                      choices = note(),
                      selected = note())
  })
  
  ##--- cate
  cate_data <- eventReactive(input$goButton, {
    if (is.null(summary()) & is.null(main()))
      return(NULL)
    
    cate_data <- summary()
    cate_data <- cate_data[cate_data$Category %in% input$category, ]
    cate_data <- cate_data[cate_data$Sub.category %in% input$subcategory, ]
    
    if (main() == "Diabetes") {
      cate_data <- cate_data[cate_data$Note %in% input$bl, ]
      cate_data$Region <- cate_data$Note
    }
    
    if (main() == "Out hospital") {
      cate_data <- cate_data[replace_na(cate_data$Note, "NA") %in% input$note, ]
    }
    
    return(cate_data)
  })
  
  ##--- Brand
  brand <- reactive({
    if (is.null(cate_data()))
      return(NULL)
    
    summary <- cate_data()
    
    brand_list <- summary$Brand_CN[!duplicated(summary$Brand_CN)]
    brand_list <- brand_list[order(brand_list)]
    # brand_list <- c("ALL", brand_list)
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 # input$region, input$province, input$city, input$decile
                 cate_data()), {
                   updateSelectInput(session,
                                     inputId = "brand",
                                     label = "Brand",
                                     choices = brand(),
                                     selected = brand()[1])
                 })
  
  ##--- Region information
  region <- reactive({
    if (is.null(cate_data()) | is.null(input$brand))
      return(NULL)
    
    summary <- cate_data()
    # summary <- summary[summary$Category %in% input$category, ]
    # summary <- summary[summary$Sub.category %in% input$subcategory, ]
    
    # if (main() == "Diabetes") {
    #   summary <- summary[summary$Note %in% input$bl, ]
    # }
    # 
    # if (main() == "Out hospital") {
    #   summary <- summary[replace_na(summary$Note, "NA") %in% input$note, ]
    # }
    
    summary <- summary[summary$Brand_CN %in% input$brand, ]
    
    reglist <- summary$Region[!duplicated(summary$Region)]
    reglist <- reglist[order(reglist)]
    if (length(reglist) > 0) {
      reglist <- c("ALL", reglist)
    }
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$brand), {
    updateSelectInput(session,
                      "region",
                      choices = region(),
                      selected = region()[1])
  })
  
  ##--- Province information
  province <- reactive({
    if (is.null(cate_data()) | is.null(input$brand) | is.null(input$region))
      return(NULL)
    
    summary <- cate_data()
    summary <- summary[summary$Brand_CN %in% input$brand, ]
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region, ]
    }
    
    provlist <- summary$Province_CN[!duplicated(summary$Province_CN)]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$brand, input$region), {
    updateSelectInput(session, 
                      "province",
                      choices =  province(),
                      selected = province()[1])
  })
  
  ##--- City information
  city <- reactive({
    if (is.null(cate_data()) | is.null(input$brand) | is.null(input$region) | is.null(input$province))
      return(NULL)
    
    summary <- cate_data()
    summary <- summary[summary$Brand_CN %in% input$brand, ]
    
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
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$brand, input$region, input$province), {
    updateSelectInput(session,
                      "city", 
                      choices = city(),
                      selected = city()[1])
  })
  
  ##--- Decile information
  decile <- reactive({
    if (is.null(cate_data()) | is.null(input$brand) | is.null(input$region) | is.null(input$province) | is.null(input$city))
      return(NULL)
    
    summary <- cate_data()
    summary <- summary[summary$Brand_CN %in% input$brand, ]
    
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
    
    total_list <- c("ALL", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "Others", "#N/A")
    total_list[total_list %in% decile_list]
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$brand, input$region, input$province, input$city), {
    updateSelectInput(session,
                      "decile",
                      choices = decile(),
                      selected = decile()[1])
  })
  
  result1 <- reactive({
    
    if (is.null(cate_data()) | is.null(input$brand) | is.null(input$region)
        | is.null(input$province) | is.null(input$city) | is.null(input$decile))
      return(NULL)
    
    summary <- cate_data()
    
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
    
    category <- unique(summary$Category)
    subcategory <- unique(summary$Sub.category)
    
    region <- unique(summary$Region)
    province <- unique(summary$Province_CN)
    city <- unique(summary$City_CN)
    decile <- unique(summary$Decile)
    note <- unique(summary$Note)
    
    result <- ddd_summary(
      salesdata = cate_data(),
      cate = category,
      subcate = subcategory,
      region = region,
      province = province,
      city = city,
      decile = decile,
      # veeva = veeva,
      # hosp_name = hosp_name,
      # note = note,
      value = input$value,
      period = input$period,
      brand = input$brand
    )
    
    rank_info <- ddd_summary(
      salesdata = cate_data(),
      cate = category,
      subcate = subcategory,
      region = region,
      province = province,
      city = city,
      decile = decile,
      # veeva = veeva,
      # hosp_name = hosp_name,
      # note = note,
      value = "RMB",
      period = input$period,
      brand = input$brand
    )
    rank_info <- distinct(rank_info$table_data)
    rank_info_m <- rank_info[, c("医院排名", "产品贡献排名", "Veeva Code", "Veeva Name")]
    
    table_data <- distinct(result$table_data) %>%
      dplyr::select(-`医院排名`, -`产品贡献排名`) %>%
      right_join(rank_info_m, by = c("Veeva Code", "Veeva Name")) %>%
      dplyr::select("医院排名", "产品贡献排名", "Region", "省份", "城市", "Veeva Code", "Veeva Name", 
                    "医院等级", "医院产出", "医院增长率", "医院贡献率", "所选产品产出", "所选产品增长率", 
                    "所选产品贡献率", "所选产品市场份额", "增长指数", "贡献指数")
    
    result1 <- list("table_data" = table_data,
                    "total_num" = result$total_num,
                    "selected_num" = result$selected_num)
    
    return(result1)
  })
  
  output$total.num <- renderInfoBox({
    if (is.null(result1())) {
      infoBox(
        "Number of Total Hospitals",
        0,
        icon = icon("hospital"),
        color = "blue"
      )
    } else {
      infoBox(
        "Number of Total Hospitals",
        format(result1()$total_num, big.mark = ","),
        icon = icon("hospital"),
        color = "blue"
      )
    }
  })
  
  output$sel.num <- renderInfoBox({
    if (is.null(result1())) {
      infoBox(
        "Number of Covered Hospitals",
        0,
        icon = icon("pills"),
        color = "blue"
      )
    } else {
      infoBox(
        "Number of Covered Hospitals",
        format(result1()$selected_num, big.mark = ","),
        icon = icon("pills"),
        color = "blue"
      )
    }
  })
  
  ot <- reactive({
    # if (input$goButton == 0)
    #   return(NULL)
    
    # input$goButton
    
    # isolate({
    if (is.null(result1()))
      return(NULL)
    
    ot <- result1()$table_data %>%
      ungroup() %>% 
      as.data.frame()
    ot[is.na(ot)] <- "-"
    ot[ot == NaN] <- "-"
    ot[ot == Inf] <- "-"
    
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
          "产品贡献排名",
          "Region",
          "省份",
          "城市",
          "Veeva Code",
          "Veeva Name",
          "医院等级",
          "医院产出",
          "医院增长率",
          "医院贡献率",
          "所选产品产出",
          "所选产品增长率",
          "所选产品贡献率",
          "所选产品市场份额",
          "增长指数",
          "贡献指数"
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(c("增长指数", "贡献指数"),
                  color = styleInterval(100, c('red', 'green')),
                  fontWeight = styleInterval(100, c('bold', 'normal'))) %>%
      formatStyle(c("医院增长率", "所选产品增长率"),
                  color = styleInterval(0, c('red', 'green')),
                  fontWeight = styleInterval(0, c('bold', 'normal'))) %>%
      formatPercentage(c("医院增长率", "医院贡献率", "所选产品增长率",
                         "所选产品贡献率", "所选产品市场份额"), 2) %>%
      formatRound(c("增长指数", "贡献指数"), 0) %>% 
      formatRound(c("医院产出", "所选产品产出"), 0)
    
    return(dat)
    # })
  })
  
  output$contents <- renderDataTable({
    
    ot()
  })
  
  ## Detail ----
  ##--- region
  region1 <- reactive({
    if (is.null(cate_data()))
      return(NULL)
    
    summary <- cate_data()
    
    reglist <- summary$Region[!duplicated(summary$Region)]
    reglist <- reglist[order(reglist)]
    reglist <- c("ALL", reglist)
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data()), {
    updateSelectInput(session,
                      "region1",
                      choices = region1(),
                      selected = region1()[1])
  })
  ##--- province
  province1 <- reactive({
    if (is.null(cate_data()) | is.null(input$region1))
      return(NULL)
    
    summary <- cate_data()
    
    if ("ALL" %in% input$region1) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region1, ]
    }
    
    provlist <- summary$Province_CN[!duplicated(summary$Province_CN)]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$region1), {
    updateSelectInput(session,
                      "province1",
                      choices =  province1(),
                      selected = province1()[1])
  })
  
  ##--- city
  city1 <- reactive({
    
    if (is.null(cate_data()) | is.null(input$region1) | is.null(input$province1))
      return(NULL)
    
    summary <- cate_data()
    
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
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$region1, input$province1), {
                   updateSelectInput(session,
                                     "city1",
                                     choices =  city1(),
                                     selected = city1()[1])
                 })
  
  ##--- decile
  decile1 <- reactive({
    
    if (is.null(cate_data()) | is.null(input$region1) | is.null(input$province1) | is.null(input$city1))
      return(NULL)
    
    summary <- cate_data()
    
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
    
    total_list <- c("ALL", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "Others", "#N/A")
    total_list[total_list %in% decile_list]
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$region1, input$province1, input$city1), {
                   updateSelectInput(session,
                                     "decile1",
                                     choices = decile1(),
                                     selected = decile1()[1])
                 })
  
  ##--- code and name
  c_n <- reactive({
    
    if (is.null(cate_data()) | is.null(input$region1) | is.null(input$province1) | is.null(input$city1) | is.null(input$decile1))
      return(NULL)
    
    summary <- cate_data()
    
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
    
    c_n <- summary[, c("Veeva.code", "Veeva.name")]
    
    if (nrow(c_n) == 0) {
      c_n <- data.frame("Veeva.code" = "-",
                        "Veeva.name" = "-")
    } else {
      c_n <- distinct(c_n)
    }
  })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$region1, input$province1, input$city1, input$decile1), {
                   updateSelectInput(session,
                                     inputId = "name",
                                     label = "Hospial Name",
                                     choices = c_n()$`Veeva.name`,
                                     selected = c_n()$`Veeva.name`[1])
                 })
  
  observeEvent(c(# input$category, input$subcategory, input$bl, input$note, 
                 cate_data(), input$region1, input$province1, input$city1, input$decile1), {
                   updateSelectInput(session,
                                     inputId = "code",
                                     label = "Veeva Code",
                                     choices = c_n()$`Veeva.code`,
                                     selected = c_n()$`Veeva.code`[1])
                 })
  
  observeEvent(c(input$code), ignoreInit = TRUE, {
    updateSelectInput(session,
                      inputId = "name",
                      label = "Hospital Name",
                      choices = c_n()$`Veeva.name`,
                      selected = c_n()[c_n()$`Veeva.code` == input$code, "Veeva.name"])
  })
  
  observeEvent(c(input$name), ignoreInit = TRUE, {
    updateSelectInput(session,
                      inputId = "code",
                      label = "Veeva Code",
                      choices = c_n()$`Veeva.code`,
                      selected = c_n()[c_n()$`Veeva.name` == input$name, "Veeva.code"])
  })
  
  ##--- result2
  result2 <- reactive({
    if (is.null(cate_data()) | is.null(input$region1) | is.null(input$province1) | is.null(input$city1) 
        | is.null(input$decile1) | is.null(input$value1) | is.null(input$period1))
      return(NULL)
    
    summary <- cate_data()
    
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
    
    # region <- unique(summary$Region)
    # province <- unique(summary$Province_CN)
    # city <- unique(summary$City_CN)
    # decile <- unique(summary$Decile)
    
    ddd_hospital(summary, main(), input$value1, input$period1)
  })
  
  ##--- rank
  rank <- reactive({
    if (is.null(result2()) | is.null(input$name))
      return(NULL)
    
    r <- result2()$rank
    r <- r %>%
      filter(Veeva.name == input$name)
    
    if (dim(r)[1] == 0)
      return(NULL)
    
    r[is.na(r)] <- "-"
    r[r == NaN] <- "-"
    r[r == Inf] <- "-"
    
    return(r)
  })
  
  output$rank1 <- renderDataTable({
    if (is.null(rank())) {
      r1 <- tibble(`医院排名` = "-", `BI 排名` = "-")
    } else {
      r1 <- rank()
      r1 <- r1[c(2, 3)]
    }
    
    input$update
    
    isolate({
      r <- DT::datatable(
        r1,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        options = list(
          autoWidth = TRUE,
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          scrollY = FALSE,
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
  })
  
  output$rank2 <- renderDataTable({
    if (is.null(rank())) {
      r1 <- tibble(`医院产出` = "-")
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
        scrollY = FALSE,
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
          "医院产出"
        ),
        `font-size` = '45px',
        fontWeight = 'bold',
        color = '#1F497D'
      ) %>%
      formatRound(c("医院产出"), 0)
    
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
        scrollY = FALSE,
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
        fontWeight = 'bold'
        # color = styleInterval(0, c('red', 'green')),
        # fontWeight = styleInterval(0, c('bold', 'normal'))
      ) %>%
      formatPercentage(c("品类增长率"), 2)
    
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
        scrollY = FALSE,
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
        `font-size` = '54px',
        fontWeight = 'bold',
        color = '#1F497D'
      )
    
    return(r)
  })
  
  ##--- table contents
  ot1 <- reactive({
    if (is.null(result2()) | is.null(input$name))
      return(NULL)
    
    ot1 <- result2()$table
    ot1 <- ot1 %>%
      filter(`Veeva.name` == input$name)
    
    if (dim(ot1)[1] == 0)
      return(NULL)
    
    rank_data <- ot1[c("Brand_CN", "MANU_CN", 
                       grep("ms|gth", 
                            grep(paste0(input$period1, "_RMB"), names(ot1), value = TRUE), 
                            invert = TRUE, value = TRUE))]
    names(rank_data) <- c("Brand_CN", "MANU_CN", "ranking")
    rank_data <- rank_data %>% 
      arrange(-`ranking`) %>% 
      select(`Brand_CN`, `MANU_CN`)
    
    ot1_names <- c("Brand_CN", "MANU_CN", grep(paste0(input$period1, "_", input$value1), names(ot1), value = TRUE))
    ot1 <- ot1[ot1_names]
    
    t <- rank_data %>% 
      left_join(ot1, by = c("Brand_CN", "MANU_CN"))
    colnames(t) <- c("产品", "厂商", "产出", "市场份额", "增长率")
    
    return(t)
  })
  
  output$contents_hosp <- renderDataTable({
    if (is.null(ot1())) {
      ot1 <- tibble(`产品` = " ", `厂商` = " ", `产出` = " ", `市场份额` = " ", `增长率` = " ")
    } else {
      ot1 <- ot1()
    }
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      pageLength <- 12
    } else {
      pageLength <- 20
    }
    
    rows <- (dim(ot1)[1] %/% pageLength + 1) * pageLength
    ot <- tibble(`产品` = rep(" ", rows), `厂商` = rep(" ", rows), `产出` = rep(" ", rows), `市场份额` = rep(" ", rows), `增长率` = rep(" ", rows))
    
    for (i in 1:dim(ot1)[1]) {
      ot[i, ] <- ot1[i, ]
    }
    
    ot <- ot %>%
      mutate(`排名` = 1:rows) %>%
      dplyr::select(
        "排名",
        "产品",
        "厂商",
        "增长率",
        "市场份额",
        "产出"
      )
    ot[is.na(ot)] <- "-"
    ot[ot == Inf] <- "-"
    ot[ot == NaN] <- "-"
    
    dat <- DT::datatable(
      ot,
      rownames = FALSE,
      # extensions = c('FixedColumns', 'Buttons'),
      options = list(
        autoWidth = TRUE,
        # dom = '<"bottom">Bfrtpl',
        # buttons = I('colvis'),
        # scrollX = TRUE,
        scrollY = FALSE,
        ordering = FALSE,
        paging = TRUE,
        bLengthChange = FALSE,
        searching = FALSE,
        bInfo = FALSE,
        columnDefs = list(list(
          className = 'dt-center',
          targets = seq(0, 5)
        )),
        autoWidth = TRUE,
        pageLength = pageLength,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    ) %>%
      formatStyle(
        c(
          "排名",
          "产品",
          "厂商"
        ),
        # fontWeight = 'bold',
        `font-size` = '15px',
        color = '#000'
      ) %>%
      formatStyle(
        c(
          "增长率",
          "市场份额",
          "产出"
        ),
        # fontWeight = 'bold',
        `font-size` = '15px',
        color = '#1F497D'
      ) %>%
      formatStyle(c("增长率"),
                  color = styleInterval(0, c('red', 'green'))
                  # fontWeight = styleInterval(0, c('bold', 'normal')),
                  # fontWeight = 'bold'
      ) %>%
      formatStyle("排名",
                  target = "row",
                  backgroundColor = styleEqual(1:rows, rep(c('#DCE6F0', 'white'), rows/2))) %>%
      formatPercentage(c("增长率"), 2) %>%
      formatPercentage(c("市场份额"), 2) %>%
      formatRound(c("产出"), 0)
    
    return(dat)
  })
  
  ##--- plot contents
  brand_3 <- eventReactive(ot1(), {
    if (is.null(ot1()))
      return(NULL)
    
    ot1()$`产品`
  })
  
  observeEvent(ot1(), {
    updateSelectInput(session,
                      inputId = "brand_3",
                      label = "Brand",
                      choices = brand_3(),
                      selected = brand_3()[c(1, 2, 3)])
  })
  
  plot1 <- eventReactive(c(input$value1, input$period1, input$brand_3), {
    if (is.null(result2()) | is.null(brand_3())
        # | is.null(input$brand1)
    )
      return(NULL)
    
    pd_names <- c("Brand_CN", "MANU_CN", 
                  grep("ms", grep(paste0(input$period1, "_", input$value1), 
                                  names(result2()$plot), value = TRUE), value = TRUE))
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(pd_names)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1, 2)
      
    } else if (input$period1 == "qtr") {
      pd <- pd[c("Brand_CN", "MANU_CN", tail(names(pd), 13))]
      names(pd) <- c("Brand_CN", "MANU_CN", 1:13)
      
    } else if (input$period1 == "mth") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1:24)
    }
    
    pd_names1 <- tail(pd_names, length(pd)-2)
    for (i in 1:(length(pd)-2)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd)[i+2] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- input$brand_3
    
    pd3 <- pd %>%
      melt(id.vars = c("Brand_CN", "MANU_CN"), variable.name = "Date", value.name = "Share") %>%
      mutate(Share = Share * 100,
             Share = round(Share, 2)) %>% 
      distinct() %>% 
      arrange(Brand_CN, Date)
    
    p <- plot_ly(hoverinfo = "name + x + y")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = pd3[pd3$Brand_CN == i, "Share"],
                  type = "scatter",
                  mode = "lines+markers",
                  marker = list(size = 7),
                  name = i)
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          # format = "svg",
          filename = paste0(input$name, "(", c_n()$`Veeva.code`[which(c_n()$`Veeva.name` == input$name)], 
                            ")-市场份额趋势 (", toupper(input$period1), ")")
          # width = 600,
          # height = 700
        )
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
          font = list(size = 15,
                      color = '#1F497D')
        ),
        showlegend = TRUE,
        xaxis = list(
          range = c(pd3$Date[1], pd3$Date[length(pd3$Date)]),
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
    
    return(list(plot = p,
                plot_data = pd))
  })
  
  output$plot1 <- renderPlotly({
    if (is.null(plot1())) {
      plotly_empty()
    } else {
      plot1()$plot
    }
  })
  
  plot2 <- eventReactive(c(input$value1, input$period1, input$brand_3), {
    if (is.null(result2()) | is.null(brand_3())
        # | is.null(input$brand1)
    )
      return(NULL)
    
    pd_names <- c("Brand_CN", "MANU_CN", 
                  grep("mkt|ms|gth", grep(paste0(input$period1, "_", input$value1), 
                                          names(result2()$plot), value = TRUE), invert = TRUE, value = TRUE))
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(pd_names)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1, 2)
      
    } else if (input$period1 == "qtr") {
      pd <- pd[c("Brand_CN", "MANU_CN", tail(names(pd), 13))]
      names(pd) <- c("Brand_CN", "MANU_CN", 1:13)
      
    } else if (input$period1 == "mth") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1:24)
    }
    
    pd_names1 <- tail(pd_names, length(pd)-2)
    for (i in 1:(length(pd)-2)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd)[i+2] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- input$brand_3
    # brand <- unique(pd$Brand_CN)
    
    pd3 <- pd %>%
      melt(id.vars = c("Brand_CN", "MANU_CN"), variable.name = "Date", value.name = "Sales") %>% 
      distinct() %>% 
      arrange(Brand_CN, Date)
    
    p <- plot_ly(hoverinfo = "name+text")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = round(pd3[pd3$Brand_CN == i, "Sales"], 0),
                  type = "scatter",
                  mode = "lines+markers",
                  marker = list(size = 7),
                  name = i,
                  text = paste0("(", pd3[pd3$Brand_CN == i, "Date"], ", ", format(round(pd3[pd3$Brand_CN == i, "Sales"], 0), big.mark = ","), ")"))
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          # format = "svg",
          filename = paste0(input$name, "(", c_n()$`Veeva.code`[which(c_n()$`Veeva.name` == input$name)], 
                            ")-产出趋势 (", toupper(input$period1), ")")
          # width = 600,
          # height = 700
        )
        # collaborate = FALSE
      ) %>%
      layout(
        annotations = list(
          text = paste0("产出趋势 (", toupper(input$period1), ")"),
          xref = "paper",
          x = 0.5,
          yref = "paper",
          y = 1,
          yshift = 30,
          showarrow = FALSE,
          font = list(size = 15,
                      color = '#1F497D')
        ),
        showlegend = TRUE,
        xaxis = list(
          range = c(pd3$Date[1], pd3$Date[length(pd3$Date)]),
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
    
    return(list(plot = p,
                plot_data = pd))
  })
  
  output$plot2 <- renderPlotly(
    if (is.null(plot2())) {
      plotly_empty()
    } else {
      plot2()$plot
    }
  )
  
  plot3 <- eventReactive(c(input$value1, input$period1, input$brand_3), {
    if (is.null(result2()) | is.null(brand_3()) |
        # is.null(input$brand1) |
        input$period1 == "mat" | input$period1 == "ytd")
      return(NULL)
    
    pd_names <- c("Brand_CN", "MANU_CN", 
                  grep("gth", grep(paste0(input$period1, "_", input$value1), 
                                   names(result2()$plot), value = TRUE), value = TRUE))
    pd <- result2()$plot %>% 
      filter(Veeva.name == input$name) %>%
      select(pd_names)
    
    if (dim(pd)[1] == 0)
      return(NULL)
    
    if (input$period1 == "qtr") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1:10)
      
    } else if (input$period1 == "mth") {
      names(pd) <- c("Brand_CN", "MANU_CN", 1:12)
    }
    
    pd_names1 <- tail(pd_names, length(pd)-2)
    for (i in 1:(length(pd)-2)) {
      x <- pd_names1[i]
      y <- regexpr(paste0(input$period1, "_", input$value1, "_"), x, useBytes = TRUE)
      z <- attr(y,"match.length")
      names(pd)[i+2] <- substring(gsub("[.]", "", x), y[1]+z)
    }
    
    brand <- input$brand_3
    # brand <- pd$Brand_CN
    
    pd3 <- pd %>%
      melt(id.vars = c("Brand_CN", "MANU_CN"), variable.name = "Date", value.name = "Growth") %>%
      mutate(Growth = Growth * 100,
             Growth = round(Growth, 2)) %>% 
      distinct() %>% 
      arrange(Brand_CN, Date)
    
    p <- plot_ly(hoverinfo = "name + x + y")
    
    for (i in brand) {
      p <- p %>%
        add_trace(x = pd3[pd3$Brand_CN == i, "Date"],
                  y = pd3[pd3$Brand_CN == i, "Growth"],
                  type = "scatter",
                  mode = "lines+markers",
                  marker = list(size = 7),
                  name = i)
    }
    
    p <- p %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(
          # format = "svg",
          filename = paste0(input$name, "(", c_n()$`Veeva.code`[which(c_n()$`Veeva.name` == input$name)], 
                            ")-增长率趋势 (", toupper(input$period1), ")")
          # width = 600,
          # height = 700
        )
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
          font = list(size = 15,
                      color = '#1F497D')
        ),
        showlegend = TRUE,
        xaxis = list(
          range = c(pd3$Date[1], pd3$Date[length(pd3$Date)]),
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
    
    return(list(plot = p,
                plot_data = pd))
  })
  
  output$plot3 <- renderPlotly({
    if (is.null(plot3())) {
      plotly_empty()
    } else {
      plot3()$plot
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
    # if (input$goButton == 0)
    #   return(NULL)
    # 
    # input$goButton
    # 
    # isolate({
    if (is.null(result1()$table_data))
      return(NULL)
    ot <- result1()$table_data
    ot <- as.data.frame(ot)
    ot[ot == Inf] <- NA
    ot[is.na(ot)] <- NA
    ot
    # })
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
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0(input$name, "(", input$code, ")_detail.xlsx")
    },
    
    content = function(file) {
      ot1 <- ot1() %>% 
        as.data.frame()
      ot1[ot1 == Inf] <- NA
      ot1[is.na(ot1)] <- NA
      
      wb <- createWorkbook()
      addWorksheet(wb, "Detail")
      writeDataTable(
        wb,
        sheet = "Detail",
        x = ot1,
        withFilter = F,
        rowNames = F,
        colNames = T
      )
      
      saveWorkbook(wb,
                   file,
                   overwrite = TRUE)
    }
  )
  
  plot_data <- reactive({
    if (is.null(plot1()$plot_data))
      return(NULL)
    
    pd1 <- plot1()$plot_data %>% 
      right_join(ot1()[, "产品"], by = c("Brand_CN" = "产品")) %>% 
      as.data.frame()
    pd1[pd1 == Inf] <- NA
    pd1[is.na(pd1)] <- NA
    
    pd2 <- plot2()$plot_data %>% 
      right_join(ot1()[, "产品"], by = c("Brand_CN" = "产品")) %>% 
      as.data.frame()
    pd2[pd2 == Inf] <- NA
    pd2[is.na(pd2)] <- NA
    
    if (input$period1 == "mat" | input$period1 == "ytd") {
      return(list(pd1 = pd1,
                  pd2 = pd2))
    }
      
    pd3 <- plot3()$plot_data %>% 
      right_join(ot1()[, "产品"], by = c("Brand_CN" = "产品")) %>% 
      as.data.frame()
    pd3[pd3 == Inf] <- NA
    pd3[is.na(pd3)] <- NA
    
    return(list(pd1 = pd1,
                pd2 = pd2,
                pd3 = pd3))
  })
  
  output$downloadPlotData <- downloadHandler(
    filename = function() {
      paste0(input$name, "(", input$code, ")_plot.xlsx")
    },
    
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb, paste0("市场份额(", input$period1, ")"))
      writeDataTable(
        wb,
        sheet = paste0("市场份额(", input$period1, ")"),
        x = plot_data()$pd1,
        withFilter = F,
        rowNames = F,
        colNames = T
      )
      
      addWorksheet(wb, paste0("产出(", input$period1, ")"))
      writeDataTable(
        wb,
        sheet = paste0("产出(", input$period1, ")"),
        x = plot_data()$pd2,
        withFilter = F,
        rowNames = F,
        colNames = T
      )
      
      if (input$period1 == "qtr" | input$period1 == "mth") {
        addWorksheet(wb, paste0("增长率(", input$period1, ")"))
        writeDataTable(
          wb,
          sheet = paste0("增长率(", input$period1, ")"),
          x = plot_data()$pd3,
          withFilter = F,
          rowNames = F,
          colNames = T
        )
      }
      
      saveWorkbook(wb,
                   file,
                   overwrite = TRUE)
    }
  )
  
}
