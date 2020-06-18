#import the files
source("loadData.R")
source("Stream-LSE.R")
source("Time-Stream.R")

#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


#-------------------Server CODE---------------------------------

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "Money" = Q_Geld,
           "oekkennzd" = oekk, 
           "Simulation data" = simulationDT_1,
           "Earning" = income)
  })

  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ---- Datatable-------------
  output$table <- DT::renderDataTable({ 
    datatable( datasetInput(),
                  rownames = TRUE,
                  escape = FALSE,
                  class = 'compact cell-border stripe hover',
                  extensions = c('Scroller')
                )
      
  })
  
  
  # Variables based on the data
  observe({
    #browser()
    var.opts<-colnames(datasetInput())
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })
  
  selectedvar <- reactive({
    
    
    datasetInput()[, c(input$variable, input$group)]
  }) 
  
  
  # plot1 for dataset review--------------------------------------------------------
  output$plot1 <- renderPlot({

      df1 <- selectedvar()
      p <- ggplot(df1, aes(x = df1[,1], y = df1[,2]))
      p <- p + geom_point(size = 5) + geom_smooth(method = "lm")
      p <- p + labs(x = names(df1)[1], y = names(df1)[2])
      p

    
  })
  
  # plot for special ------------------------
  
  output$ggplot  <- renderPlot({
    
     if(input$dataset== "Money")
       {plot = plot_money}
     if(input$dataset=="oekkennzd")
     {plot = plot_oekk}
     if(input$dataset=="Earning")
     {plot = plot_income}
      if(input$dataset=="Simulation data")
      {plot = plot_simu}
     plot
    
  })
  
  #------------------------------------------------------------
  # tab 2 
  #------------------------------------------------------------
  observe(
    {
      rows.total <- switch(input$dataset_, 
                          "Money" = nrow(Q_Geld),
                          "oekkennzd" = nrow(oekkennzd), 
                          "Simulation data (1000)" = nrow(simulationDT_1),
                          "Simulation data (10000)" = nrow(simulationDT_2),
                          "Simulation data (100000)" = nrow(simulationDT_3),
                          "Earning" = nrow(earning)
                          )

      
      
      updateNumericInput(session, "block", max = rows.total)
    }
  )
  # reactiv function to summary the data stream ---------------------
  
  datasummary <-reactive({
    def.names = c("Total Rows","Blocks")
    summary <- switch(input$dataset_, 
                        "Money" = datatable(data.frame( x= nrow(Q_Geld),y= input$block),rownames = "Money",colnames = def.names),
                        "oekkennzd" = datatable(data.frame( x= nrow(oekkennzd),y= input$block),rownames = "oekkennzd",colnames = def.names), 
                        "Simulation data (1000)" = datatable(data.frame( x= nrow(simulationDT_1),y= input$block),rownames = "Simulation data (1000)",colnames = def.names),
                        "Simulation data (10000)" = datatable(data.frame( x= nrow(simulationDT_2),y= input$block),rownames = "Simulation data (10000)",colnames = def.names),
                        "Simulation data (100000)" = datatable(data.frame( x= nrow(simulationDT_3),y= input$block),rownames = "Simulation data (100000)",colnames = def.names),
                        "Earning" = datatable(data.frame( x= nrow(earning),y= input$block),rownames = "Earning",colnames = def.names)
    )
    summary
    
  })

  # reactive function to fit regression model ------------------
  results <-reactive({
  
    model <- switch(input$dataset_, 
                   "Money" =lm(model1, Q_Geld),
                   "oekkennzd" = lm(model2, oekkennzd), 
                   "Simulation data (1000)" = lm(model3,simulationDT_1),
                   "Simulation data (10000)" = lm(model3,simulationDT_2),
                   "Simulation data (100000)" = lm(model3,simulationDT_3),
                   "Earning" = lm(model4,earning)
    )
    sum.model = summary(model)
    datOutput = sum.model$coef 
    R.Squared = c(round(sum.model$r.squared, digits = 3),NA,NA,NA)
    MSE  = c(round(mean(model$residuals^2), digits = 3),NA,NA,NA)
    res = rbind(datOutput,R.Squared)
    res = rbind(res, MSE)
    return(res)
    })

  # reactive funtion to fit the Stream LSE model ---------------
  result.StreamLSE <- reactive ({
    
    #determin the learning coeff.
    lr <- switch(input$learningrate,
                 "0.01"= 0.01,
                 "0.1"= 0.1,
                 "0.2"= 0.2,
                 "Not Constant" = NA)
    
    res <- switch(input$dataset_, 
                    "Money" = streamMulti(X,y,input$block,lr),
                    "oekkennzd" = streamMulti(DS2_X,DS2_y,input$block,lr), 
                    "Simulation data (1000)" = streamMulti(DS3_X,DS3_y,input$block,lr),
                    "Simulation data (10000)" = streamMulti(sim2_DS3_X,sim2_DS3_y,input$block,lr),
                    "Simulation data (100000)" = streamMulti(sim3_DS3_X,sim3_DS3_y,input$block,lr),
                    "Earning" = streamMulti(earning_x, earning_y, input$block,lr)
                    
                  )
    return(res)
    

  })

  # reactive function to fit Time Stream algorithm -------------------
  
  results.timeStream <- reactive({
    
    sum.3 <- switch(input$dataset_, 
                    "Money" = timeStream(Q_Geld[,3:5], input$block , money_model),
                    "oekkennzd" = timeStream(oekkennzd[,2:5], input$block , oekk_model), 
                    "Simulation data (1000)" = timeStream(simulationDT_1, input$block , simu_model),
                    "Simulation data (10000)" = timeStream(simulationDT_2, input$block , simu_model),
                    "Simulation data (100000)" = timeStream(simulationDT_3, input$block , simu_model),
                    "Earning" = timeStream(earning, input$block,earning_model)
                    )
    
  return(sum.3)

  })
  
  lm.coff <-  reactive({
    p0 <- ggplot()
    if (input$dataset_== "oekkennzd"){
      lm = results()[1:4,1]
    }
    if (input$dataset_!= "oekkennzd")
    {
      lm = results()[1:3,1]
    }
    if (input$dataset_== "Earning"){
      lm = results()[1:5,1]
    }
    
    return(lm)
    
  })
  # ---------------------------------------------
  # diskplay datatables for mutilple regression model
  # ----------------------------------------------
  
 output$datasum <- DT::renderDataTable({datasummary() })
  
  output$multipleregression <- DT::renderDataTable({results() %>% datatable %>%
      formatRound(1:4, digits=3) } )

  output$StreamLSE <- DT::renderDataTable({
    
    res = result.StreamLSE()
    
    if (input$showMd1) {
      coeff <- data.matrix(res$Coefficients[,input$block])
      
      # r-squared
      r.squared <- t(res$summary[input$block,2:3])
      
      
      sum.model3 <- rbind(coeff,r.squared) %>% datatable %>% formatRound(1, digits=2)
      
      
      return(sum.model3) 
    }
    } )
  
  output$TimeStream <- DT::renderDataTable({
    
    sum.3 = results.timeStream()$summary

    if (input$showMd2) {
      datOut = sum.3$coef
      # 
      # R.Squared = c(round(sum.3$r.squared, digits = 3),NA,NA,NA)
      # MSE =  c(round(results.timeStream()$mse, digits = 3),NA,NA,NA)
      new = data.frame(MSE = c(results.timeStream()$mse,NA,NA,NA), R.Squared = c(sum.3$r.squared,NA,NA,NA))

      res = rbind(datOut,t(new)) %>% datatable %>% formatRound(1:4, digits=2)

      return(res)
    }
    } )
  
  #---------------------------------------------------------
  # Display plots
  #---------------------------------------------------------
  
  output$plot0 <-  renderPrint({
    lm.coff()
    df =  data.frame(lm.coff())
    return(lm.coff())
    
  })
  
  
  output$plot2 <-  renderPlot({
    #
    
    p1 <- ggplot()
    p2 <- ggplot()
    
   
    
    res2 = result.StreamLSE()$Coefficients
    res3 = results.timeStream()$coef
      
      
    if(input$plotMd1) {
      
        coefIter1 = data.frame(t(res2))
        names1 = colnames(coefIter1)
        coefIter1$ID <- seq.int(nrow(coefIter1))
        
        if (length(names1)==3){
            p1 = ggplot(data= coefIter1 ,aes(x = as.numeric(ID) ) )+ 
            geom_line(aes(y = coefIter1[,1], colour = names1[1] )) +
            geom_line(aes(y = coefIter1[,2], colour = names1[2] )) + 
            geom_line(aes(y = coefIter1[,3], colour = names1[3] )) 
          
        }
        if (length(names1)==4)
        {
          p1 = ggplot(data= coefIter1 ,aes(x = as.numeric(ID) ) )+ 
            geom_line(aes(y = coefIter1[,1], colour = names1[1] )) +
            geom_line(aes(y = coefIter1[,2], colour = names1[2] )) + 
            geom_line(aes(y = coefIter1[,3], colour = names1[3] )) +
            geom_line(aes(y = coefIter1[,4], colour = names1[4] ))
        }
        if (length(names1)==5)
        {
          p1 = ggplot(data= coefIter1 ,aes(x = as.numeric(ID) ) )+ 
            geom_line(aes(y = coefIter1[,1], colour = names1[1] )) +
            geom_line(aes(y = coefIter1[,2], colour = names1[2] )) + 
            geom_line(aes(y = coefIter1[,3], colour = names1[3] )) +
            geom_line(aes(y = coefIter1[,4], colour = names1[4] )) +
            geom_line(aes(y = coefIter1[,5], colour = names1[5] ))
        }
        
      
        p1 = p1 + xlab("The number of data stream") + ylab("Coeffcient") + ggtitle("The trend of the coef. in chosen dataset for Stream-LSE")+
          scale_x_continuous(limits = c(1,input$block))
      }
      
    if(input$plotMd2) {
        
        coefIter = data.frame(t(res3))
        names = colnames(coefIter)
        print(names)
        
        if (length(names)==4){
          p2 = ggplot(data= coefIter ,aes(x = as.numeric(row.names(coefIter)) ) )+ 
            geom_line(aes(y = coefIter[,1], colour = names[1]  )) +
            geom_line(aes(y = coefIter[,2], colour = names[2]  )) +
            geom_line(aes(y = coefIter[,3], colour = names[3]  )) +
            geom_line(aes(y = coefIter[,4], colour = names[4] ))
          
          
        }

        if (length(names)==3)
        {
          p2 = ggplot(data= coefIter ,aes(x = as.numeric(row.names(coefIter)) ) )+ 
            geom_line(aes(y = coefIter[,1], colour = names[1]  )) +
            geom_line(aes(y = coefIter[,2], colour = names[2]  )) +
            geom_line(aes(y = coefIter[,3], colour = names[3]  )) 
          
        }
        
        
        if (length(names)==5)
        {
          p2 = ggplot(data = coefIter ,aes(x = as.numeric(row.names(coefIter)) ) )+ 
            geom_line(aes(y = coefIter[,1], colour = names[1]  )) +
            geom_line(aes(y = coefIter[,2], colour = names[2]  )) +
            geom_line(aes(y = coefIter[,3], colour = names[3]  )) +
            geom_line(aes(y = coefIter[,4], colour = names[4] ))  +
            geom_line(aes(y = coefIter[,5], colour = names[5] ))
          
        }
        p2 = p2 + xlab("The number of data stream") + ylab("Coeffcient") + ggtitle("The trend of the coef. in chosen dataset for Time-Stream")+
          scale_x_continuous(limits = c(1,input$block))
      }
      
      
   
      return(grid.arrange(p1, p2, ncol = 2))
  })
  
  
#--------
}

