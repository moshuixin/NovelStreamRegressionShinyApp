
# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    #.navbar .navbar-nav {float: left}
                    #.navbar .navbar-header {float: left}
                    .navbar { margin-left: 30px;}
                    .navbar { margin-right: 30px;}
                    
                    "))
    ),
  
  # App title ----
  titlePanel("ShinyApp"),
  
  
  
  # Sidebar with a slider input for number of bins 
  navbarPage(h4(""),
             tabPanel( value="tab1",
                       h4("Welcome"),
                       fluidPage(
                         fluidRow(
                           
                           column(12,div(style = "height:200px; background-color: #f0f0f0;margin-left:-20px;margin-right:-20px;",
                                         #margin-left:20px;margin-right:20px;
                                         br(),
                                         h1(strong("Stream Regression Analysis"),style="margin-left:20px; margin-top:15px; font-size:36px; margin-bottom:0px; padding-bottom:5px;"),
                                         
                                         br(),
                                         h3("To achieve high-performace prediction or classification of stream data, novel methods were invented. This application aims to visualize these novel regression methods. ",style="margin-left:20px; margin-top:15px; margin-bottom:0px; padding-bottom:5px;")
                                         
                                         
                                         ))),
                         br(),
                         fluidRow(
                           column(4,div(style = "height:400px;background-color: #ffffff;", h3("Multiple Regression Method:"),
                                        p("Multiple regression is an extension of linear regression which includes more than one independent variable."),
                                        br()
                           )),
                           column(4,h3("Stream-LSE Method:"),
                                  p("The idea of this method is that next data improve the estimators evaluated in previous steps."),
                                  img(src='alg1.png', height="80%", width="80%", align = "center"),
                                  br()
                                  #actionButton("button2",icon = icon("th"),"View details"
                                  ),
                           column(4,h3("Time Stream Method:"),
                                  p("The idea of this method is that the parameters change with time."),
                                  img(src='alg2.png', height="80%", width="80%", align = "center"),
                                  br()
                                  #actionButton("button2",icon = icon("th"),"View details")
                           )
                           
                         ),
                         br()
      
                         )
                       
                       
             ),
             #tab1------------------------------
             tabPanel( value="tab0",h4("Overview"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "dataset",
                                      label = "Choose a dataset:",
                                      choices = c("Money","oekkennzd", "Simulation data")),
                          
                          selectInput("variable", "Select variable for x axis",
                                      choices = NULL),
                          
                          selectInput("group", "Select variable for y axis",
                                      choices = NULL),
                          # selectInput("plot.type","Plot Type:",
                          #                                             list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
                          #             ),
                          # checkboxInput("show.points", "show points", TRUE),
                          
                           #  help text to dataset 1 ----
                          conditionalPanel(
                           'input.dataset === "Money"',
                           helpText("Y= Bruttosozialprodukt, M = Mass fuer Geldmenge, P = Preisbereinigungsindex")),
                          # 
                          # help text to dataset 2 ----
                          conditionalPanel(
                            'input.dataset === "oekkennzd"',
                            helpText("IR = interest rate, SP = share price, CPI = consumer price index, ULC = unit labour costs")
                          )
                      
                        ),
             
                        # Show a plot of the generated distribution
                        mainPanel(
                          tabsetPanel(
                            
                            tabPanel("Summary of Dataset",verbatimTextOutput("summary")),
                            
                            # Output: Verbatim text for data summary ----
                            #tabPanel("Table",tableOutput("view")),
                      
                            tabPanel("Table",DT::dataTableOutput("table")),
                            
                          
                            tabPanel("Plot", plotOutput("plot1"),  value = 1)
                            
                          )
        
                        
                        )#mainpanel
                      )
             ),
  # tab1 finish!---------------------------------------------------------------
             #tab2--------------------------------------------------------
             tabPanel(value="tab1",h4("Analysis"),
                      sidebarLayout(
                        sidebarPanel(

                          
                          selectInput("dataset_", "Please choose a dataset:",
                                      choices = c("Money", "oekkennzd",  "Simulation data (1000)","Simulation data (10000)","Simulation data (100000)"),
                                      selected = "Money"),
                          tags$hr(),
                          # choose the number of blocks
                          numericInput("block", "Please choose a number of blocks:", 5, min = 1, max = NA),
                      
                          tags$hr(),
                          # constant 
                          radioButtons("learningrate", "Please choose a learning rate for Stream-LSE:",
                                       c(0.01,0.1,0.2, "Not Constant")),
                          
                          helpText("Note: Not Constant,",
                                   "it denpends on the number of data items."),
                          #code("Not constant, "),
                          tags$hr(),
                          div(p("Show Novel Stream Model:"), style = "color:blue"),

                          checkboxInput("showMd1", "Show/Hide Stream-LSE Model", 
                                        value = FALSE),
                          
                          checkboxInput("showMd2", "Show/Hide Time-Stream Model", 
                                        value = FALSE),
                          #div("bla bla bla bla ", style = "color:blue")
                          #br(),
                          tags$hr(),
                          div(p("Show Plot"), style = "color:blue"),
                          
                          checkboxInput("plotMd1", "Show/Hide Stream-LSE Plot", 
                                        value = FALSE),
                          checkboxInput("plotMd2", "Show/Hide Time-Stream Plot", 
                                        value = FALSE)
                          
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                        
                          tabsetPanel(
                            
                            tabPanel("Table",
                              fluidRow(
                                        column(12,  
                                              #h3(strong("Multiple Linear Rregression"),style = "color:blue")
                                        ),
                                        column(6,  
                                               h4("Data Summary",style = "color:blue"),
                                               br(),
                                               br(),
                                               DT::dataTableOutput("datasum")
                                               
                                        ),
                                       
                                        column(6,
                                               h4("Multiple Regression Model Summary", style = "color:blue"),
                                               DT::dataTableOutput("multipleregression"))
                              ),  
                              fluidRow(
                                column(12,  
                                       #h3(strong("Stream Regression"),style = "color:blue")
                                ),
                                column(6,  
                                       h4("Stream-LSE Model Summary",style = "color:blue"),
                                       DT::dataTableOutput("StreamLSE")
                                ),
                                column(6,
                                       h4("Time-Stream Model Summary",style = "color:blue"),
                                       DT::dataTableOutput("TimeStream"))
                              )
                                ),
                           
                          tabPanel("Plot",
                                   h4("Coefficient of multiple linear regression:",style = "color:blue"),
                                   verbatimTextOutput("plot0"),
                                   h4("Coefficient of novel stream regression:",style = "color:blue"),
                                  plotOutput("plot2"),value=2
                                  
                                  )
                          )
                          
                        )#mainpanel
                      )
             )

  )
)