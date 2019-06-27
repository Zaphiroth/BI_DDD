# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  BI Shiny Bidding
# Purpose:      Shiny dashboard
# programmer:   Xin Huang
# Date:         09-08-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##-- load the required packages
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(leaflet)
library(leafletCN)
library(shinyWidgets)

##-- App part
ui <- dashboardPage(
  dashboardHeader(title = "DDD Data"),
  dashboardSidebar(
    tags$head(includeCSS('./www/fix_siderbar.css')),
    collapsed = FALSE,
    fluidRow(column(
      12, fileInput('summary', 'Upload DDD Data')
    )),
    fluidRow(tags$div(
      tags$div(
        column(12, selectInput("category", label = "Category", "", multiple = TRUE, width = "230px")),
        column(12, selectInput("subcategory", label = "Subcategory",  "", multiple = TRUE, width = "230px")),
        conditionalPanel(
          condition = "output.main2 == 2",
          column(12, selectInput("bl", label = "BI/Lilly", choices = c("BI", "Lilly"), multiple = TRUE, width = "230px"))
        ),
        column(12, style = "padding: 20px"),
        column(12, actionButton("goButton", "Update", width = "200px")),
        style = "display:inline-block;margin-down: 1px;vertical-align:middle"),
      tags$style(".skin-blue .sidebar a { color: #444; }")
      # tags$div(column(
      #   #offset = 1,
      #   1,
      #   downloadButton(outputId = "downloadData",
      #                  label = "Download")
      # ),
      # style = "display:inline-block;margin-down: 1px;vertical-align:middle")
    ))
    
    # selectInput("window",
    #             "Length",
    #             c("1 year" = 1,
    #               "2 years" = 2),
    #             selected = 1),
    
    
  ),
  
  
  dashboardBody(# tags$head( tags$script(type="text/javascript",'$(document).ready(function(){
    #                          $(".main-sidebar").css("height","100%");
    #                        $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow-y": "auto"})
    #                        })')),
    useShinyjs(),
    br(),
    tabsetPanel(
      tabPanel(
        strong("Hospital Summary"), value = 1,
        fluidRow(
          box(
            title = "DDD Hospital Performance",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  # column(4, selectInput("category", label = "Category", "", multiple = TRUE)),
                  # column(8, selectInput("subcategory", label = "Subcategory",  "", multiple = TRUE)),
                  # conditionalPanel(
                  #   condition = "output.main2 == 2",
                  #   column(2, selectInput("bl", label = "BI/Lilly", choices = c("BI", "Lilly"), multiple = TRUE))
                  # ),
                  conditionalPanel(
                    condition = "output.main1 == 1",
                    column(2, selectInput("region", label = "Region", choices = "", multiple = TRUE))
                  ),
                  column(2, selectInput("province", "Province", "", multiple = TRUE)),
                  column(2, selectInput("city", "City", "", multiple = TRUE)),
                  column(4,
                         selectInput("decile",
                                     label = "Decile",
                                     c("ALL", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "Others", "#N/A"),
                                     # selected = "ALL",
                                     multiple = TRUE)),
                  conditionalPanel(
                    condition = "output.main2 == 1",
                    column(2, selectInput("note", label = "Note", "", multiple = TRUE))
                  ),
                  column(12, selectInput("brand", label = "Brand", "", multiple = TRUE)),
                  # column(2, selectInput("veeva", "Veeva Code", "", multiple = TRUE)),
                  # column(2, selectInput("hospital", "Hospital", "", multiple = TRUE)),
                  # column(2,
                  #        selectInput(
                  #          "top",
                  #          "Top No.",
                  #          c(
                  #            "20" = 20,
                  #            # "three" = 3,
                  #            "50" = 50,
                  #            "100" = 100,
                  #            "ALL" = 100000
                  #          ),
                  #          selected = 20,
                  #          multiple = FALSE
                  #        )),
                  column(2,
                         selectInput(
                           "value",
                           label = ("Measurement"),
                           choices = list(
                             "Value In RMB" = "RMB",
                             "UNIT" = "UNIT",
                             "DOT" = "DOT"
                           ),
                           selected = "RMB",
                           multiple = FALSE
                         )),
                  column(2,
                         selectInput(
                           "period",
                           label = ("Period"),
                           choices = list(
                             "MAT" = "mat",
                             "YTD" = "ytd",
                             "QTR" = "qtr",
                             # "Rolling QTR" = "rqtr",
                             "MTH" = "mth"
                             # ,"YEARLY" = "yrl"
                           ),
                           selected = "qtr",
                           multiple = FALSE
                         )),
                  column(6,
                         "排名原则：医院排名/产品贡献排名基于Value固定不变，不随所选Measurment更改。",
                         style = "color: #1F497D; padding: 30px;"),
                  column(2,
                         column(3),
                         column(9, downloadButton(outputId = "downloadData", label = "Download"),
                                style = "color: #1F497D; padding: 30px;")),
                  style = "color: #1F497D"
                ),
                style = "background: #DDD9C4"
              )
            ),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                fluidRow(
                  infoBoxOutput("total.num"),
                  infoBoxOutput("sel.num")
                ),
                fluidRow(column(
                  12, tags$div(DT::dataTableOutput("contents"), style = "font-size:80%")
                ))
              )
            )
          )
        )
      ),
      tabPanel(
        strong("Detailed Hospital Analysis"), value = 1,
        fluidRow(
          box(
            title = "DDD Hospital Performance",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  conditionalPanel(
                    condition = "output.main2 == 2",
                    column(2, selectInput("bl1", label = "BI/Lilly", choices = c("BI", "Lilly"), multiple = TRUE))
                  ),
                  conditionalPanel(
                    condition = "output.main1 == 1",
                    column(2, selectInput("region1", label = "Region", choices = "", multiple = TRUE))
                  ),
                  column(2, selectInput("province1", "Province", "", multiple = TRUE)),
                  column(2, selectInput("city1", "City", "", multiple = TRUE)),
                  column(4,
                         selectInput("decile1",
                                     label = "Decile",
                                     c("ALL", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "Others", "#N/A"),
                                     # selected = "ALL",
                                     multiple = TRUE)),
                  conditionalPanel(
                    condition = "output.main2 == 1",
                    column(2, selectInput("note1", label = "Note", "", multiple = TRUE))
                  ),
                  fluidRow(),
                  column(2,
                         selectInput(
                           "value1",
                           label = ("Measurement"),
                           choices = list(
                             "Value In RMB" = "RMB",
                             "UNIT" = "UNIT",
                             "DOT" = "DOT"
                           ),
                           selected = "RMB",
                           multiple = FALSE
                         )),
                  column(2,
                         selectInput(
                           "period1",
                           label = ("Period"),
                           choices = list(
                             "MAT" = "mat",
                             "YTD" = "ytd",
                             "QTR" = "qtr",
                             # "Rolling QTR" = "rqtr",
                             "MTH" = "mth"
                           ),
                           selected = "qtr",
                           multiple = FALSE
                         )),
                  column(2, selectInput("code", label = "Veeva Code", "", multiple = FALSE)),
                  column(4, selectInput("name", label = "Hospital Name", "", multiple = FALSE)),
                  # column(12, selectInput("brand1", label = "Brand", "", multiple = TRUE)),
                  style = "color: #1F497D"
                ),
                style = "background: #DDD9C4"
              )
            ),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                column(
                  2,
                  fluidRow(
                    column(12, tags$div(DT::dataTableOutput("rank1"), style = "font-size:100%")),
                    conditionalPanel(
                      condition = "input.period1 == 'qtr' | input.period1 == 'mth'",
                      # column(12, tags$div(style="margin-bottom:100px;"))
                      column(12, style = "padding: 50px")
                    ),
                    column(12, tags$div(DT::dataTableOutput("rank2"), style = "font-size:100%")),
                    conditionalPanel(
                      condition = "input.period1 == 'qtr' | input.period1 == 'mth'",
                      # column(12, tags$div(style="margin-bottom:100px;"))
                      column(12, style = "padding: 50px")
                    ),
                    column(12, tags$div(DT::dataTableOutput("rank3"), style = "font-size:100%")),
                    conditionalPanel(
                      condition = "input.period1 == 'qtr' | input.period1 == 'mth'",
                      # column(12, tags$div(style="margin-bottom:100px;"))
                      column(12, style = "padding: 50px")
                    ),
                    column(12, tags$div(DT::dataTableOutput("rank4"), style = "font-size:100%"))
                  )
                ),
                column(
                  4,
                  fluidRow(
                    column(12, tags$div(DT::dataTableOutput("contents_hosp"), style = "font-size:100%"))
                  )
                ),
                column(
                  6,
                  fluidRow(
                    column(12, plotlyOutput("plot1", width = "100%", height = "270px"))
                  ),
                  fluidRow(
                    column(12, plotlyOutput("plot2", width = "100%", height = "270px"))
                  ),
                  conditionalPanel(
                    condition = "input.period1 == 'qtr' | input.period1 == 'mth'",
                    fluidRow(
                      column(12, plotlyOutput("plot3", width = "100%", height = "270px"))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
