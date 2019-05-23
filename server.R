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
  
  output$main <- reactive({
    unique(summary()$Main)
  })
  
  ##--- Category and Sub Category
  categorytype <- reactive({
    summary()$Category_CN[!duplicated(summary()$Category_CN)]
  })
  
  subcategorytype <- reactive({
    if (is.null(input$category))
      return(NULL)
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
    updateSelectInput(session,
                      "subcategory",
                      choices =  subcategorytype(),
                      selected = subcategorytype())
  })
  
  ##--- Decile information
  # decile_list <- reactive({
  #   decile_list <- summary()$Decile[!duplicated(summary()$Decile)]
  #   decile_list <- decile_list[order(decile_list)]
  #   decile_list <- c("ALL", decile_list)
  # })
  # 
  # observeEvent(input$summary, {
  #   updateSelectInput(session,
  #                     "decile",
  #                     choices = decile_list(),
  #                     selected = "D1")
  # })
  
  ##--- Region information
  reglist <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    # summary <- summary()[summary()$Decile %in% input$decile,]
    reglist <- summary()$Region[!duplicated(summary()$Region)]
    reglist <- reglist[order(reglist)]
    reglist <- c("ALL", reglist)
  })
  
  observeEvent(input$summary, {
    updateSelectInput(session,
                      "region",
                      choices = reglist(),
                      selected = "ALL")
  })
  
  ##--- Province information
  province <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region,]
    }
    # summary <- summary()[summary()$Region %in% input$region,]
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    provlist <-
      summary$Province_CN[!duplicated(summary$Province_CN)]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(input$region, {
    updateSelectInput(session, 
                      "province",
                      choices =  province(),
                      selected = "ALL")
  })
  
  
  ##--- City information
  city <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region,]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province,]
    }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    citylist <- summary$City_CN[!duplicated(summary$City_CN)]
    citylist <- citylist[order(citylist)]
    citylist <- c("ALL", citylist)
  })
  
  observeEvent(input$province, {
    updateSelectInput(session,
                      "city", 
                      choices =  city(),
                      selected = "ALL")
  })
  
  ##--- Veeva Code and Name
  
  veeva_code <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region,]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province,]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    veevacodelist <-
      summary$Veeva.code[!duplicated(summary$Veeva.code)]
    veevacodelist <- veevacodelist[order(veevacodelist)]
    veevacodelist <- c("ALL", veevacodelist)
  })
  
  observeEvent(input$city, {
    updateSelectInput(session, 
                      "veeva", 
                      choices =  veeva_code(),
                      selected = "ALL")
  })
  
  veeva_name <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region,]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province,]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    if ("ALL" %in% input$veeva) {
      summary <- summary
    } else {
      summary <- summary[summary$Veeva.code %in% input$veeva,]
    }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    hosplist <- summary$Veeva.name[!duplicated(summary$Veeva.name)]
    hosplist <- hosplist[order(hosplist)]
    hosplist <- c("ALL", hosplist)
  })
  
  observeEvent(input$veeva, {
    updateSelectInput(session, "hospital",
                      choices =  veeva_name(),
                      selected = "ALL")
  })
  
  ##--- Note
  note <- reactive({
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile,]
    }
    
    if ("ALL" %in% input$region) {
      summary <- summary
    } else {
      summary <- summary[summary$Region %in% input$region,]
    }
    
    if ("ALL" %in% input$province) {
      summary <- summary
    } else {
      summary <- summary[summary$Province_CN %in% input$province,]
    }
    
    if ("ALL" %in% input$city) {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN %in% input$city, ]
    }
    
    if ("ALL" %in% input$veeva) {
      summary <- summary
    } else {
      summary <- summary[summary$Veeva.code %in% input$veeva,]
    }
    
    if ("ALL" %in% input$hospital) {
      summary <- summary
    } else {
      summary <- summary[summary$Veeva.name %in% input$hospital,]
    }
    # summary1 <- summary[summary$Region %in% "Beijing", ]
    notelist <- summary$Note[!duplicated(summary$Note)]
    notelist <- notelist[order(notelist)]
    notelist <- c("ALL", notelist)
  })
  
  observeEvent(input$hospital, {
    updateSelectInput(session, "note", choices = note(), selected = "ALL")
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
    
    if ("ALL" %in% input$decile) {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Decile %in% input$decile, ]
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
    
    if ("ALL" %in% input$veeva) {
      summary <- summary
    } else {
      summary <- summary[summary$Veeva.code %in% input$veeva,]
    }
    
    if ("ALL" %in% input$hospital) {
      summary <- summary
    } else {
      summary <- summary[summary$Veeva.name %in% input$hospital,]
    }
    
    if ("ALL" %in% input$note) {
      summary <- summary
    } else {
      summary <- summary[summary$Note %in% input$note,]
    }
    
    # if ("ALL" %in% input$decile) {
    #   decile <- decile()
    # } else {
    #   summary <- summary[summary$Note %in% input$note,]
    # }
    
    
    
    province <- unique(summary$Province_CN)
    city <- unique(summary$City_CN)
    decile <- unique(summary$Decile)
    # if ("ALL" %in% input$note) {
    #   decile <- unique(summary$Decile)
    # } else {
    #   decile <- input$decile
    # }
    region <- unique(summary$Region)
    veeva <- unique(summary$Veeva.code)
    hosp_name <- unique(summary$Veeva.name)
    note <- unique(summary$Note)
    
    if ("RMB"  %in% input$value) {
      summary <- summary()
      # if ("ALL" %in% input$province) {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
      # } else {
      #   summary <-
      #     summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
      # }
      
      rmb <- rbind.fill(
        ddd_summary(
          salesdata = summary,
          cate = input$category,
          subcate = input$subcategory,
          region = region,
          province = province,
          city = city,
          decile = decile,
          veeva = veeva,
          hosp_name = hosp_name,
          note = note,
          value = "RMB",
          period = input$period,
          # kpi = c("abs", "gr")
          # window = as.numeric(input$window)
          window = 1
        )
      )
      rmb <- distinct(rmb)
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
          veeva = veeva,
          hosp_name = hosp_name,
          note = note,
          value = "UNIT",
          period = input$period,
          # kpi = c("abs", "gr")
          # window = as.numeric(input$window)
          window = 1
        )
      )
      unit <- distinct(unit)
      
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
          veeva = veeva,
          hosp_name = hosp_name,
          note = note,
          value = "DOT",
          period = input$period,
          # kpi = c("abs", "gr")
          # window = as.numeric(input$window)
          window = 1
        )
      )
      dot <- distinct(dot)
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
        ungroup() %>%
        filter(row_number() <= as.integer(input$top))
      outputtable
    })
  })
  
  pagenumber <- reactive({
    if (input$top == "20") {
      pageno = 20
    } else if (input$top == "50") {
      pageno = 20
    } else if (input$top == "100") {
      pageno = 20
    } else{
      pageno = 20
    }
    pageno
  })
  
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
  observeEvent(input$summary, {
    updateSelectInput(session,
                      "category1",
                      choices = c("ALL", categorytype()),
                      selected = "ALL")
  })
  ##--- region
  region1 <- reactive({
    reg <- summary()$Region[!duplicated(summary()$Region)]
    # reg <- reg[order(reg)]
    reg <- c("ALL", reg)
  })
  
  observeEvent(input$category1, {
    updateSelectInput(session,
                      "region1",
                      choices = region1(),
                      selected = "ALL")
  })
  ##--- province
  # province1 <- reactive({
  #   if (input$region1 == "ALL") {
  #     summary <- summary()
  #   } else {
  #     summary <- summary()[summary()$Region == input$region1, ]
  #   }
  #   
  #   prov <- summary$Province_CN[!duplicated(summary$Province_CN)]
  #   # prov <- prov[order(prov)]
  #   prov <- c("ALL", prov)
  # })
  # 
  # observeEvent(c(input$category1, input$region1), {
  #   updateSelectInput(session, 
  #                     "province1",
  #                     choices =  province1(),
  #                     selected = "ALL")
  # })
  ##--- city
  city1 <- reactive({
    if (input$region1 == "ALL") {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Region == input$region1, ]
    }
    
    city <- summary$City_CN[!duplicated(summary$City_CN)]
    # city <- city[order(city)]
    city <- c("ALL", city)
  })
  
  observeEvent(c(input$category1, input$region1
                 # , input$province1
                 ), {
    updateSelectInput(session,
                      "city1",
                      choices =  city1(),
                      selected = "ALL")
  })
  ##--- decile
  decile1 <- reactive({
    if (input$region1 == "ALL") {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Region == input$region1, ]
    }
    
    if (input$city1 == "ALL") {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN == input$city1, ]
    }
    
    summary$Decile[is.na(summary$Decile)] <- "NA"
    decile <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "Others", "#N/A", "NA")
    decile <- decile[which(decile %in% summary$Decile)]
    decile <- c("ALL", decile)
  })
  
  observeEvent(c(input$category1, input$region1,
                 # input$province1,
                 input$city1), {
    updateSelectInput(session,
                      "decile1",
                      choices = decile1(),
                      selected = "ALL")
  })
  ##--- veeva
  veeva1 <- reactive({
    if (input$region1 == "ALL") {
      summary <- summary()
    } else {
      summary <- summary()[summary()$Region == input$region1, ]
    }
    
    if (input$city1 == "ALL") {
      summary <- summary
    } else {
      summary <- summary[summary$City_CN == input$city1, ]
    }
    
    if (input$decile1 == "ALL") {
      summary <- summary
    } else {
      summary <- summary[summary$Decile == input$decile1 | is.na(summary$Decile), ]
    }
    
    hosp <- summary$Veeva.name[!duplicated(summary$Veeva.name)]
  })

  observeEvent(c(input$category1, input$region1,
                 # input$province1,
                 input$city1, input$decile1), {
    updateSelectInput(session, "veeva1",
                      choices =  veeva1())
  })
  
  ##--- result2
  result2 <- reactive({
    if (is.null(summary()))
      return(NULL)
    
    result2 <- ddd_hospital(summary())
    
    return(result2)
  })
  
  ##--- rank
  rank <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    # input$search
    
    # isolate({
      r <- result2()$rank
      
      r <- r %>%
        filter(Veeva.name == input$veeva1)
      
      return(r)
    # })
  })
  
  output$rank1 <- renderDataTable({
    
    # input$search
    
    # isolate({
      if (is.null(rank()))
        return(NULL)
      
      r1 <- rank()
      r1 <- r1[c(2, 3)]
      
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
          `font-size` = '46px',
          fontWeight = 'bold',
          color = '#1F497D'
        )
      
      return(r)
    # })
  })
  
  output$rank2 <- renderDataTable({
    
    # input$search
    
    # isolate({
      if (is.null(rank()))
        return(NULL)
      
      r1 <- rank()
      r1 <- r1[4]
      
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
          `font-size` = '46px',
          fontWeight = 'bold',
          color = '#1F497D'
        ) %>%
        formatRound(c("月平均单产(滚动季度数据)"), 0)
      
      return(r)
    # })
  })
  
  output$rank3 <- renderDataTable({
    
    # input$search
    
    # isolate({
      if (is.null(rank()))
        return(NULL)
      
      r1 <- rank()
      r1 <- r1[5]
      
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
          `font-size` = '46px',
          color = '#1F497D',
          # color = styleInterval(0, c('red', 'green')),
          # fontWeight = styleInterval(0, c('bold', 'normal')),
          fontWeight = 'bold') %>%
        formatPercentage(c("品类增长率"), 0)
      
      return(r)
    # })
  })
  
  output$rank4 <- renderDataTable({
    
    # input$search
    
    # isolate({
      if (is.null(rank()))
        return(NULL)
      
      r1 <- rank()
      r1 <- r1[6]
      
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
          `font-size` = '49px',
          fontWeight = 'bold',
          color = '#1F497D'
        )
      
      return(r)
    # })
  })
  
  ##--- table contents
  date <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    result2()$date
  })
  
  bi <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    result2()$bi_brand
  })
  
  ot1 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    # input$search
    
    # isolate({
      ot1 <- result2()$table
      ot1 <- as.data.frame(ot1)
      
      ot1 <- ot1 %>%
        filter(`Veeva Name` == input$veeva1) %>%
        select("产品",
               "增长率",
               "市场份额",
               "月均产出(滚动季度数据)")
      
      t1 <- ot1[ot1$产品 %in% bi(), ] %>%
        arrange(-`月均产出(滚动季度数据)`)
      
      t2 <- ot1[!(ot1$产品 %in% bi()), ] %>%
        arrange(-`月均产出(滚动季度数据)`)
      
      t <- bind_rows(t1, t2)
    # })
  })

  output$contents_hosp <- renderDataTable({
    
    # input$search
    
    # isolate({
      if (is.null(ot1()))
        return(NULL)
      
      ot1 <- ot1()
      
      l <- (dim(ot1)[1] %/% 10 + 1) * 10
      
      ot <- as.data.frame(array(dim = c(l, 4)))
      colnames(ot) <- colnames(ot1)
      
      for (i in 1:dim(ot1)[1]) {
        ot[i, ] <- ot1[i, ]
      }
      
      ot[is.na(ot)] <- " "
      ot <- ot %>%
        mutate(序号 = 1:l) %>%
        dplyr::select(
          "序号",
          "产品",
          "增长率",
          "市场份额",
          "月均产出(滚动季度数据)"
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
          searching = FALSE,
          bInfo = FALSE,
          columnDefs = list(list(
            className = 'dt-center',
            targets = seq(0, 4)
          )),
          autoWidth = TRUE,
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
            # "Category",
            # "Region",
            # "省份",
            # "城市",
            # "医院等级",
            # "Veeva Code",
            # "Veeva Name",
            # "序号",
            # "产品",
            "增长率",
            "市场份额",
            "月均产出(滚动季度数据)"
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
                    backgroundColor = styleEqual(1:l, rep(c('#DCE6F0', 'white'), l/2))) %>%
        formatPercentage(c("增长率"), 0) %>%
        formatPercentage(c("市场份额"), 1) %>%
        formatRound(c("月均产出(滚动季度数据)"), 0)
      
      return(dat)
    # })
  })
  
  ##--- plot contents
  plot1 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    # input$search
    
    # isolate({
      pd <- result2()$share
      
      pd <- pd %>%
        filter(Veeva.name == input$veeva1) %>%
        select(-Veeva.name)
      
      names(pd) <- c("Brand_CN", 0:12)
      
      d1 <- pd[pd$Brand_CN %in% bi(), ] %>%
        arrange(-`12`)
      
      d2 <- pd[!(pd$Brand_CN %in% bi()), ] %>%
        arrange(-`12`)
      
      pd <- bind_rows(d1, d2)
      
      names(pd) <- c("Brand_CN", date())
      
      brand <- pd$Brand_CN
      
      pd <- pd %>%
        melt(id.vars = "Brand_CN", variable.name = "Date", value.name = "Share") %>%
        mutate(Share = Share * 100)
      
      p <- plot_ly(hoverinfo = "name + x + y")
      
      for (i in brand) {
        p <- p %>%
          add_trace(x = pd[pd$Brand_CN == i, "Date"],
                    y = pd[pd$Brand_CN == i, "Share"],
                    type = "scatter",
                    mode = "lines + markers",
                    marker = list(size = 5),
                    name = i)
        
        if (!is.na(d1$Brand_CN[1])) {
          if (i == d1$Brand_CN[1]) {
            p <- p %>%
              add_text(x = pd[pd$Brand_CN == i, "Date"],
                       y = pd[pd$Brand_CN == i, "Share"],
                       text = paste0(pd[pd$Brand_CN == i, "Share"], "%"),
                       textfont = list(size = 13),
                       textposition = "top",
                       name = i,
                       showlegend = TRUE)
          }
        }
      }
      
      p <- p %>%
        config(
          displaylogo = FALSE,
          collaborate = FALSE
        ) %>%
        layout(
          annotations = list(
            text = "市场份额趋势(滚动季度数据)",
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
            title = "Market share (RMB)",
            ticksuffix = "%",
            showline = TRUE,
            mirror = "ticks"
          )
        )
      
      return(p)
    # })
  })
  
  plot2 <- reactive({
    if (is.null(result2()))
      return(NULL)
    
    # input$search
    
    # isolate({
      pd <- result2()$mean_mth
      
      pd <- pd %>%
        filter(Veeva.name == input$veeva1) %>%
        select(-Veeva.name)
      
      names(pd) <- c("Brand_CN", 0:12)
      
      d1 <- pd[pd$Brand_CN %in% bi(), ] %>%
        arrange(-`12`)
      
      d2 <- pd[!(pd$Brand_CN %in% bi()), ] %>%
        arrange(-`12`)
      
      pd <- bind_rows(d1, d2)
      
      names(pd) <- c("Brand_CN", date())
      
      brand <- pd$Brand_CN
      
      pd <- pd %>%
        melt(id.vars = "Brand_CN", variable.name = "Date", value.name = "Sales")
      
      p <- plot_ly(hoverinfo = "name + x + y")
      
      for (i in brand) {
        p <- p %>%
          add_trace(x = pd[pd$Brand_CN == i, "Date"],
                    y = round(pd[pd$Brand_CN == i, "Sales"]),
                    type = "scatter",
                    mode = "lines + markers",
                    marker = list(size = 5),
                    name = i)
        
        if (!is.na(d1$Brand_CN[1])) {
          if (i == d1$Brand_CN[1]) {
            p <- p %>%
              add_text(x = pd[pd$Brand_CN == i, "Date"],
                       y = round(pd[pd$Brand_CN == i, "Sales"]),
                       text = round(pd[pd$Brand_CN == i, "Sales"], 0),
                       textfont = list(size = 13),
                       textposition = "top",
                       name = i,
                       showlegend = TRUE)
          }
        }
      }
      
      p <- p %>%
        
        config(
          displaylogo = FALSE,
          collaborate = FALSE
        ) %>%
        
        layout(
          annotations = list(
            text = "月均单产金额趋势(滚动季度数据)",
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
            title = "Production (RMB)",
            ticksuffix = "",
            showline = TRUE,
            mirror = "ticks"
          )
        )
      
      return(p)
    # })
  })
  
  output$plot1 <- renderPlotly(
    if (is.null(plot1())) {
      plotly_empty()
    } else {
      plot1()
    }
  )
  
  output$plot2 <- renderPlotly(
    if (is.null(plot2())) {
      plotly_empty()
    } else {
      plot2()
    }
  )

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
