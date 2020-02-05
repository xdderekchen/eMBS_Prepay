#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(sparkline)
library(dplyr)
library(dygraphs)
library(reshape2)
library(shinydashboard)
library(highcharter)
library(rvest)
library(data.table)
library(lubridate)
source("./data_etl.R")
source("./data_utils.R")

###http://rstudio.github.io/shiny/tutorial/#scoping
#create Objects visible across all sessions
LoadedData <- init_data(mode="P") #Using python data processing code
#LoadedData <- init_data(mode="R") #Using R data processing code
#For this application, R implementation is faster.

#For this simple demo, we load all data at the beginning as the global variable.
myDateInfo <-  LoadedData[["date"]]

get_prepay_title <- function(currentDate)
{
  start_date  <- currentDate %m-% months(12)
  last_date   <- currentDate %m-% months(1)
  TITLE <- paste("Prepayment ( ", format(start_date, "%Y%m"), "--", format(last_date, "%Y%m"), ")")
  return (TITLE)
}

TITLE_PREPAY <- get_prepay_title(LoadedData[["date_date"]])
TITLE_IR     <- paste("Interest Rate of FRM30 and FRM50")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "eMBS flash report",  titleWidth = 200),
    dashboardSidebar(disable = TRUE),
    
    body=  dashboardBody(
      tags$div(
               tags$a(href="https://www.embs.com", "Exploring data imported from eMBS")
      ),
      
      fluidRow(
      column(width=4,
          selectInput("selProduct", 
                           label = "Product (FRM TERM)",
                           choices = unique(LoadedData[["FNM"]]$Term), ###c("30", "15"), #setNames(unique(LoadedData$Term), paste("FRM ", unique(LoadedData$Term))),
                           selected = "30") ),
      column(width=4,
             selectInput("selCoupon", 
                         label = "Coupon",
                         choices =  c("ALL", seq(2, 10, by=0.5)),
                         selected = "ALL") ),
      
      column(width=4,
             selectInput("selProd", 
                         label = "Production Year",
                         choices = sort(unique(LoadedData[["FNM"]]$`Prodn Yr`), decreasing = T),
                         selected = "ALL") )
    
    )
    ,
    fluidRow(
      
      shinydashboard::box( width=12, title = paste("Pools", " -- ", myDateInfo), status = "primary", solidHeader = TRUE,
                           div(DT::dataTableOutput("dtSelection"), style = "font-size:90%"))
      ,
      shinydashboard::box( width=12,   title = TITLE_PREPAY, status = "primary", solidHeader = TRUE,
                           highchartOutput("hcontainer", height='200px')
      )
                          
      ,shinydashboard::box( width=12,   title = TITLE_IR , status = "primary", solidHeader = TRUE,
                            dygraphOutput("dygraph", height='200px'))
      
      
      )
      
    
           
    )
)


server <- function(input, output, session) {
  
  #cb <- htmlwidgets::JS('function(){debugger;HTMLWidgets.staticRender();}')
  
  myData <- reactive({
   
    myTerm     <- input$selProduct
    myCoupon   <- as.numeric(input$selCoupon)
    myProdYear <- input$selProd
    if (input$selCoupon == "ALL")
    {
     
       FNMData = LoadedData[["FNM"]]  %>% filter(Term==myTerm, Coupon==input$selCoupon, `Prodn Yr`== myProdYear)
       FHLData = LoadedData[["FHL"]]  %>% filter(Term==myTerm, Coupon==input$selCoupon, `Prodn Yr`== myProdYear)
    } else
    {
       myCoupon   <- as.numeric(input$selCoupon)
       FNMData = LoadedData[["FNM"]]  %>% filter(Term==myTerm, as.numeric(Coupon)==myCoupon, `Prodn Yr`== myProdYear)
       FHLData = LoadedData[["FHL"]]  %>% filter(Term==myTerm, as.numeric(Coupon)==myCoupon, `Prodn Yr`== myProdYear)
    } 
    
    commonFields <- intersect(colnames(FNMData) ,  colnames(FHLData))
    crpIndex <- endsWith(commonFields, "CPR")
    cprFields <-   commonFields[crpIndex]
    mainFields <- commonFields[!crpIndex] 
    mainFields <- mainFields[mainFields != "Category"]
  
    mainData <- FNMData[FALSE, mainFields]
    cprData  <- FNMData[FALSE, cprFields]
    
    if (nrow(FNMData) > 0)
    {
      mainData = rbind(mainData, FNMData[, mainFields])
      cprData  = rbind(cprData, FNMData[, cprFields])
    }
    if (nrow(FHLData) > 0)
    {
      mainData = rbind(mainData, FHLData[, mainFields])
      cprData  = rbind(cprData, FHLData[, cprFields])
    }
    colnames(cprData) <-  gsub("CPR$", "", cprFields)
    rownames(cprData) <- mainData$Agency
    
    return (list(mainData,cprData ))
  })
  
  
  output$dtSelection = DT::renderDataTable({
    dat0 <-  myData() 
    DT::datatable(dat0[[1]], 
                  options = list(searching = FALSE, paging  = FALSE, info = FALSE, ordering=F)
                  , rownames = FALSE)
    
  }) 
 
  
  output$dygraph <-   renderDygraph({
    ir30 <- LoadedData[["IR30"]]
    ir15 <- LoadedData[["IR15"]]
    firstDate_Year  <- lubridate::year( ir30$Date[1])
    firstDate_Month <- lubridate::month(ir30$Date[1])
    
    IRData <- data.frame(IR30=ir30$IR, IR15=ir15$IR)
    
    z <- ts(IRData, frequency = 12, start=c(firstDate_Year, firstDate_Month))
    
    dygraph(z) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.5,
                  hideOnMouseOut = TRUE) %>%
      dyAxis("x", drawGrid = FALSE) %>%
      dyAxis("y", label = "Interest Rate") 
  })
  
  
  output$hcontainer <-   renderHighchart({
    cprData <- myData()[[2]]
    if (nrow(cprData) < 1) {
      return (highchart())
    }
    
    mycols_rev <- rev(colnames(cprData))
    newdat <- transpose(cprData[, mycols_rev])
    colnames(newdat) <- rownames(cprData)
    X = mycols_rev
    ys = colnames(newdat)
    
  
    colors <- c( "#07E4EF","#92FB8F", "#FB1108", "#2980b9","#ABD6E6","#9AD2E1","#FD150B","#FA7806","#FBE426","#FCFB8F",
                "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
    ###hc_title(text = title) 
    hc <- highchart() %>%
      hc_xAxis(categories = X) %>%
      hc_yAxis(title = list(text = "prepayment"), opposite = FALSE) %>%
      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                 shared = TRUE, borderWidth = 1) %>% 
      hc_exporting(enabled = TRUE)  %>% 
      hc_add_theme(hc_theme_elementary()) %>% hc_legend(align = "top", verticalAlign = "top", x = 200
                                                        )
    
    
    if (0 < length(ys)) {
      for (i in 1:length(ys)) {
        hc <- hc %>% hc_add_series(name = ys[i], data = newdat[, ys[i]],type = "column", color = colors[i])
      }
    }
    
    hc
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
