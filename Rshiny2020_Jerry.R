library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)
library(ggcorrplot)
library(dashboardthemes)
library(devtools)

data <-read.csv('D:/Project/mydata.csv')
prep<-read.csv('D:/Project/NewData.csv')
h=round( cor(prep[c('selling_price', 'km_driven', 'Age','price_age', 'price_km')]), 3)  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro"),
    menuItem("Features", tabName = "features"),
    menuItem("Preprocessed Dataset", tabName = "dataset"),
    menuItem("EDA", tabName = 'cor'),
    menuItem('Regression', tabName = 'reg')
  ),
  width = 180
)


body <- dashboardBody(tags$head(tags$style(HTML('.content-wrapper,.right-side{background-color:#c4c4c4}'))),
                      tabItems(
                        tabItem(tabName = "intro",
                                fluidPage(
                                  fluidRow(
                                    column(br(),
                                           p("This is a shiny app built on a dataset obtained from the website cardekho.com. 
                                           The app was made to demonstrate the statistical analyses done on the dataset. The app will cover the preprocessing, EDA, datasets and the results. 
                                           The dataset contains information of 3351 cars and 8 features, all of which were sold in the year 2020. 
                                           The columns in the initial dataset were: name, year, selling_price, km_driven, fuel, seller_type, transmission and owner as show below.
                                           The initial dataset contained a lot of duplicate rows which were dropped in the preprocessing stage and a new dataset was prepared."),
                                           width = 12
                                    ),
                                    column(DT::dataTableOutput("RawData"),
                                           width = 12)
                                    
                                  )
                                  
                                )),
                        
                        tabItem(tabName = "dataset", 
                                fluidRow(column(
                                  p("This is the preprocessed dataset. As shown, the name and year columns was dropped and new columns Age, price_age and price_km were added.
                                  The price_age is an adjusted variable that contains the average selling price of the corresponding age of car.
                                    The price_km is another adjusted variable that contains the average selling price of the corresonding km_driven value."),
                                  DT::dataTableOutput("Data1"),
                                  width = 12))
                        ),
                        
                        tabItem(tabName = "features",
                                fluidPage(
                                  p("This page shows distribution of each feature of the initial dataset. 
                                    Since the relative frequecies of a few groups were very low they had to be dropped as well.
                                    These correspond to: trustmark dealer, fourth & above owner, test drive car, electric and lpg of their repective features."),
                                  titlePanel("Features"),
                                  
                                  navlistPanel(
                                    tabPanel("year",
                                             plotOutput('plot1')
                                    ),
                                    tabPanel("selling_price",
                                             plotOutput("plot2")
                                    ),
                                    tabPanel("km_driven",
                                             plotOutput("plot3")
                                    ),
                                    tabPanel("fuel",
                                             plotOutput("plot4")
                                    ),
                                    tabPanel("seller_type",
                                             plotOutput("plot5")
                                    ),
                                    tabPanel("transmission",
                                             plotOutput("plot6")
                                    ),
                                    tabPanel("owner",
                                             plotOutput("plot7")
                                    )
                                  )
                                )),
                        
                        tabItem(tabName = 'cor',
                                
                                fluidPage(
                                  plotOutput('corplot'),
                                  plotOutput("corplot1"),
                                  plotOutput("corplot2")
                                  
                                )
                        ),
                        tabItem(tabName = 'reg',
                                fluidRow(
                                  shinyDashboardThemes(
                                    theme="grey_light"),
                                  box(
                                    selectInput("X",
                                                "First Feauture:",
                                                choices=c("Age"="Age","km_driven"="km_driven")),
                                    
                                    selectInput("Y",
                                                "Second Feauture:",
                                                choices=c("price_age"="price_age","price_km"="price_km")),
                                    
                                  ),
                                  box(title="Scatter Plot For diffirent Feautures",background = "red",solidHeader = TRUE,plotOutput("regplot")),
                                  box(
                                    p(strong('Age vs price_age:')),
                                    br(),
                                    p("Intercept : 837895, r = -0.951528"),
                                    br(),
                                    p(strong("km_driven vs price_km:")),
                                    br(),
                                    p("Intercept : 610804.331, r = -0.2961252")
                                  )  
                                  
                                  
                                )
                                
                        )))


header <- dashboardHeader(title = "Analysis on selling price of cars", titleWidth = 350)


ui<-dashboardPage(skin = 'red',
                  header,
                  sidebar,
                  body
)

server <- function(input, output, session) {
  
  output$RawData <-DT::renderDataTable(
    DT::datatable({
      data
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({});",#'background-color': 'moccasin', #'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Name","Year","Sellingprice","KMdriven","Fuel","Sellertype","Transmission","Owner")
  ))
  
  output$Data1 <-DT::renderDataTable(
    DT::datatable({
      prep
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({});",#'background-color': 'moccasin', #'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Selling price","km driven","Fuel","Seller type","Transmission","Owner","Age", "Price_age", "Price_km" )
  ))
  
  output$plot1 <- renderPlot({
    ggplot(data, aes(x=year))+
      geom_histogram(color='darkblue', fill="steelblue")
    
  })
  output$plot2 <- renderPlot({
    ggplot(data, aes(x=sellingprice))+
      geom_histogram(color='darkblue',fill="steelblue")
    
  })
  output$plot3 <- renderPlot({
    
    ggplot(data, aes(x=kmdriven))+
      geom_histogram(color='darkblue',fill="steelblue")
    
  })
  output$plot4 <- renderPlot({
    ggplot(data, aes(fuel)) +
      geom_bar(aes(fuel), fill="steelblue", width = 0.5) + 
      theme(axis.text.x = element_text(angle=25, vjust=0.6))
  })
  output$plot5 <- renderPlot({
    
    ggplot(data, aes(seller_type)) +
      geom_bar(aes(sellertype), fill="steelblue", width = 0.5) + 
      theme(axis.text.x = element_text(angle=25, vjust=0.6))
  })
  output$plot6 <- renderPlot({
    ggplot(data, aes(transmission)) +
      geom_bar(aes(transmission), fill="steelblue", width = 0.5) + 
      theme(axis.text.x = element_text(angle=25, vjust=0.6))
    
  })
  output$plot7 <- renderPlot({
    
    ggplot(data, aes(owner)) +
      geom_bar(aes(owner), fill="steelblue", width = 0.5) + 
      theme(axis.text.x = element_text(angle=25, vjust=0.6)) 
  })
  output$corplot <-renderPlot({
    ggcorrplot(h, hc.order = TRUE, type = "lower", 
               lab = TRUE, lab_size = 3, 
               title="Correlogram of data", 
               ggtheme=theme_bw)
    
  })
  output$corplot1 <- renderPlot({
    ggplot(prep, aes(x=km_driven, y=price_km)) + 
      geom_point(aes(col=owner, size=transmission)) + 
      labs(subtitle="Km driven Vs Selling price of car", y="price_km", x="km_driven")
  })
  output$corplot2 <- renderPlot({
    ggplot(prep, aes(x=km_driven, y=price_km)) + 
      geom_point(aes(col=fuel, size=seller_type)) + 
      labs(subtitle="Km driven Vs Selling price of car", y="price_km", x="km_driven")
  })
  output$regplot <- renderPlot({
    ggplot(prep, aes_string(x = input$X, y = input$Y))  +
      geom_point(color = "dodgerblue", size=5) +
      geom_smooth(method="lm",se=F,color="red")+ylim(0, 2e+06)
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)