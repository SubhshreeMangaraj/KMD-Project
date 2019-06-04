library(shiny)

ui <-fluidPage(
  titlePanel("Comparision of Feature Selection Methods"),
  sidebarLayout(
    sidebarPanel(width = 15,
                 fluidRow(
                   
                   column(3, wellPanel(
                     selectInput("input_type", "Feature Selection MEthod",
                                 c("Filter Method","Wrapper Method")
                     )
                   )),
                   
                   column(3, wellPanel(
                     # This outputs the dynamic UI component
                     uiOutput("ui")
                   )),
                   
                   column(3, wellPanel(
                     selectInput("input_type2", "Model Selection Method",
                                 c("Regression","Classification")
                     ))),
                   
                   column(3, wellPanel(
                     # This outputs the dynamic UI component
                     uiOutput("ui2")
                   ))
                 )
    ),
    # Show the table
    mainPanel(
      
      tableOutput("view"),
      plotOutput("plot")
    )
    
  ))


server <-function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           
           
           "Filter Method" = selectInput("dynamic", "Choose the Filter Method",
                                         choices = c("FSelector_chi.squared"          = "FSelector_chi.squared",
                                                     "party_cforest.importance"       = "party_cforest.importance",
                                                     "FSelectorRcpp_information.gain" = "FSelectorRcpp_information.gain"),
                                         selected = "FSelector_chi.squared"
           ),
           
           "Wrapper Method" = selectInput("dynamic", "Choose the Wrapper Method",
                                          choices = c("Random"          = "Random",
                                                      "Exhaustive"      = "Exhaustive",
                                                      "GA"              = "GA",
                                                      "Sequential"      = "Sequential"),
                                          selected = "Random"
           )
    )
  })
  
  output$ui2 <- renderUI({
    if (is.null(input$input_type2))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type2,
           
           
           "Regression" = selectInput("dynamic2", "Choose the Regression Method",
                                      choices = c("K-Nearest-Neighbor regressiong"  = "K-Nearest-Neighbor regressiong",
                                                  "Conditional Inference Trees"     = "Conditional Inference Trees",
                                                  "Bayesian CART"                   = "Bayesian CART"),
                                      selected = "K-Nearest-Neighbor regressiong"
           ),
           
           "Classification" = selectInput("dynamic2", "Choose the Classification Method",
                                          choices = c("Binomial Regression"          = "Binomial Regression",
                                                      "Fast k-Nearest Neighbour"     = "Fast k-Nearest Neighbour",
                                                      "Linear Discriminant Analysis" = "Linear Discriminant Analysis"),
                                          selected = "Binomial Regression"
           )
    )
  })
  
  output$view <- renderTable({
    
    if(input$input_type  == "Filter Method"         &&
       input$dynamic     == "FSelector_chi.squared" &&
       input$input_type2 == "Regression"            &&
       input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_r1.csv"))
      df<-data.frame(df_val_r1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_r2.csv"))
      df<-data.frame(df_val_r2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_r3.csv"))
      df<-data.frame(df_val_r3)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_c1.csv"))
      df<-data.frame(df_val_c1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_c2.csv"))
      df<-data.frame(df_val_c2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_c3.csv"))
      df<-data.frame(df_val_c3)
    }
    
    #-----------------------------------------------------------------------------------
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_f2r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2r1.csv"))
      df<-data.frame(df_val_f2r1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_f2r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2r2.csv"))
      df<-data.frame(df_val_f2r2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_f2r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2r3.csv"))
      df<-data.frame(df_val_f2r3)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_f2c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2c1.csv"))
      df<-data.frame(df_val_f2c1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_f2c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2c2.csv"))
      df<-data.frame(df_val_f2c2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_f2c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f2c3.csv"))
      df<-data.frame(df_val_f2c3)
    }
    
    #-----------------------------------------------------------------------------
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_f3r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3r1.csv"))
      df<-data.frame(df_val_f3r1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_f3r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3r2.csv"))
      df<-data.frame(df_val_f3r2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_f3r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3r3.csv"))
      df<-data.frame(df_val_f3r3)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_f3c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3c1.csv"))
      df<-data.frame(df_val_f3c1)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_f3c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3c2.csv"))
      df<-data.frame(df_val_f3c2)
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_f3c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_f3c3.csv"))
      df<-data.frame(df_val_f3c3)
    }
    #--------------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_w1r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1r1.csv"))
      df<-data.frame(df_val_w1r1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_w1r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1r2.csv"))
      df<-data.frame(df_val_w1r2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_w1r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1r3.csv"))
      df<-data.frame(df_val_w1r3)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_w1c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1c1.csv"))
      df<-data.frame(df_val_w1c1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_w1c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1c2.csv"))
      df<-data.frame(df_val_w1c2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_w1c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w1c3.csv"))
      df<-data.frame(df_val_w1c3)
    }
    #--------------------------------------------------------------------------
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_w2r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2r1.csv"))
      df<-data.frame(df_val_w2r1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_w2r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2r2.csv"))
      df<-data.frame(df_val_w2r2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_w2r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2r3.csv"))
      df<-data.frame(df_val_w2r3)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_w2c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2c1.csv"))
      df<-data.frame(df_val_w2c1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_w2c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2c2.csv"))
      df<-data.frame(df_val_w2c2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_w2c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w2c3.csv"))
      df<-data.frame(df_val_w2c3)
    }
    #-------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_w3r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3r1.csv"))
      df<-data.frame(df_val_w3r1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_w3r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3r2.csv"))
      df<-data.frame(df_val_w3r2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_w3r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3r3.csv"))
      df<-data.frame(df_val_w3r3)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_w3c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3c1.csv"))
      df<-data.frame(df_val_w3c1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_w3c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3c2.csv"))
      df<-data.frame(df_val_w3c2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_w3c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w3c3.csv"))
      df<-data.frame(df_val_w3c3) 
    }
    #---------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      df_val_w4r1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4r1.csv"))
      df<-data.frame(df_val_w4r1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      df_val_w4r2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4r2.csv"))
      df<-data.frame(df_val_w4r2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      df_val_w4r3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4r3.csv"))
      df<-data.frame(df_val_w4r3)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      df_val_w4c1 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4c1.csv"))
      df<-data.frame(df_val_w4c1)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      df_val_w4c2 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4c2.csv"))
      df<-data.frame(df_val_w4c2)
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      df_val_w4c3 <- readr::read_csv(file.path(here::here(),"Documents","output", "df_val_w4c3.csv"))
      df<-data.frame(df_val_w4c3) 
    }
  }
  
  )
  #---------------------------------------------------------------------------------------------
  #------------------------PLOTS----------------------------------------------------------------
  
  
  output$plot <- renderPlot({
    
    if(input$input_type  == "Filter Method"         &&
       input$dynamic     == "FSelector_chi.squared" &&
       input$input_type2 == "Regression"            &&
       input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
      
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelector_chi.squared" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    #-----------------------------------------------------------------------------------
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f2r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f2r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f2r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f2c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f2c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "party_cforest.importance" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f2c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "party_cforest.importance")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    #-----------------------------------------------------------------------------
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f3r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f3r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_f3r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f3c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f3c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Filter Method"         &&
            input$dynamic     == "FSelectorRcpp_information.gain" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_f3c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelectorRcpp_information.gain")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    #--------------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w1r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w1r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w1r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
      
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w1c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w1c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Random" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w1c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    #--------------------------------------------------------------------------
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w2r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w2r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w2r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w2c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w2c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "GA" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w2c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    #-------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w3r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w3r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w3r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w3c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w3c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Sequential" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w3c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr() 
    }
    #---------------------------------------------------------------------------
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive"                 &&
            input$input_type2 == "Regression"             &&
            input$dynamic2    == "K-Nearest-Neighbor regressiong"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w4r1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Conditional Inference Trees"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w4r2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Regression"            &&
            input$dynamic2    == "Bayesian CART"){
      
      regr.task1<-makeRegrTask(
        data   = data.frame(df_tune_w4r3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = regr.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Binomial Regression"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w4c1), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr() 
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Fast k-Nearest Neighbour"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w4c2), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr() 
    }
    
    else if(input$input_type  == "Wrapper Method"         &&
            input$dynamic     == "Exhaustive" &&
            input$input_type2 == "Classification"            &&
            input$dynamic2    == "Linear Discriminant Analysis"){
      
      clasf.task1<-makeClassifTask(
        data   = data.frame(df_tune_w4c3), 
        target = "response")
      
      fv2 = generateFilterValuesData(task   = clasf.task1, 
                                     method = "FSelector_chi.squared")
      plotFilterValues(fv2, feat.type.cols = TRUE) + ggpubr::theme_pubr()  
    }
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)