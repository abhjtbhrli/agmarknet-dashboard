# R Shiny Task Submission
# by Abhijit Bharali

# Creating a interactive dashboard

# Load required libraries

library(shiny)
library(shinythemes)
library(bslib)
library(tidyverse)
library(RPostgres)
library(DBI)
library(lubridate)

# Access database and extract the required dataset

con<-dbConnect(Postgres(),dbname='agmarket',
               host='think-data.c3yicave5fam.ap-south-1.rds.amazonaws.com',
               user='rtestuser',
               password='psql_password')

task_table<-dbReadTable(conn = con,name = 'agmarknet_commodity_subset')

# Write function for server side logic (plotfn() helps to generate the time series plot)

plotfn<-function(data,type){
    if (type=="Price"){
        plot<-ggplot(data = data,
                     aes(x=as.Date(Reported.Date)))+
            geom_line(aes(y=Modal.Price.Rs_per_Quintal),colour="#FB4447",size=1)+
            theme(axis.title.y = element_text(colour = "#FB4447"))
    }
    else if (type=="Arrivals"){
        plot<-ggplot(data = data,
                     aes(x=as.Date(Reported.Date)))+
            geom_line(aes(y=Arrivals.Tonnes),colour="black",size=1)+
            theme(axis.title.y = element_text(colour = "black"))
    }
    else if (type=="Both"){
        plot<-ggplot(data = data,
                     aes(x=as.Date(Reported.Date)))+
            geom_line(aes(y=Modal.Price.Rs_per_Quintal),colour="#FB4447",size=1)+
            geom_line(aes(y=Arrivals.Tonnes*50),colour="black",size=1)+
            scale_y_continuous(name = "Price",
                               sec.axis = sec_axis(~.*50,name="Arrivals*50"))+
            theme(axis.title.y.left = element_text(colour = "#FB4447",hjust = 1),
                  axis.title.y.right = element_text(colour = "black",hjust = 1))
    }
    
    return(plot)
    
}

# Building the application 

# Define UI 
ui <-
    fluidPage( 
    theme = bs_theme(
        bootswatch = "cerulean",
        bg = "white",
        fg = "black",
    ),
    
    titlePanel(div(style="font-weight: bold;
                                    font-family: Helvetica;
                                    text-align: center;",
                   tags$h2("AGMarket Dashboard"))),
    
    
    
    
    fluidPage(
    sidebarLayout(
        sidebarPanel(width = 2,
            selectizeInput("state",
                        tags$strong("Select State:"),
                        choices = unique(task_table$State.Name)),
            br(),
            selectizeInput("commodity",
                        tags$strong("Select Commodity:"),
                        choices = unique(task_table$Commodity)),
            br(),
            radioButtons("type",
                         tags$strong("Select Type:"),
                         choices = c("Price","Arrivals","Both"))
        ),
        mainPanel(
            tags$h6("Filters"),
            fluidRow(
                column(5,
                       uiOutput("mrkt"),
                       
                ),
                column(5,
                       offset = 2,
                       dateRangeInput(inputId = "date",
                                   label = "Date Range:",
                                   start = min(task_table$Reported.Date),
                                   end = max(task_table$Reported.Date),
                                   min = min(task_table$Reported.Date),
                                   max = max(task_table$Reported.Date)),
                       
                )
                
            ),
            fluidRow(
                column(4,
                       tags$strong(tags$u("SUMMARY")),
                       br(),
                       br(),
                       br(),
                       textOutput("summary1"),
                       br(),
                       textOutput("summary2"),
                       br(),
                       textOutput("summary3"),
                       br(),
                       textOutput("summary4"),
                       br(),
                       textOutput("summary5")
                    
                ),
                column(6,
                       plotOutput("trend")
                    
                ),
            )
        )
    )
)
)

# Define server logic required
server <- function(input, output) {
    
    output$mrkt<-renderUI({
        selectInput(inputId = "market",
                    label = "Choose Market:",
                    multiple = T,
                    choices = unique((task_table%>%
                                          filter(State.Name==input$state,Commodity==input$commodity))$Market.Name)
                    
            
        )
    })
    
    x<-eventReactive(c(input$state,input$market,input$commodity,input$date,input$type),{
        plotfn(data = task_table%>%
                   filter(Market.Name%in%input$market,
                          State.Name==input$state,
                          Commodity==input$commodity,
                          between(Reported.Date,as_datetime(input$date[1]),as_datetime(input$date[2]))),
               type = input$type)
    })
    
    
    output$trend<-renderPlot({
        x()+
            xlab("Date range")+
            ylab(input$type)+
            theme(plot.background = element_rect(fill = "white"),
                  panel.background = element_rect(fill = "white"),
                  plot.margin = margin(20,40,20,20),
                  panel.border = element_blank(),
                  axis.title = element_text(colour = "black",size = 15,face = "bold"),
                  axis.text = element_text(colour = "black",size=8),
                  panel.grid.major = element_line(colour = "grey",linetype = 3))
        
    },width = 800,height = "auto")
    
    output$summary1<-renderText({
        paste0("Commodity: ",input$commodity)
    })
    
    output$summary2<-renderText({
        paste0("State: ",input$state)
    })
    
    output$summary3<-renderText({
        paste0("Market: ",paste(input$market, collapse=", "))
    })
    
    output$summary4<-renderText({
        paste0("Date/Duration: ",input$date[1]," to ",input$date[2])
    })
    
    output$summary5<-renderText({
        paste0("Type: ",input$type)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
