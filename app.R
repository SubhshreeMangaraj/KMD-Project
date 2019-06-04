

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("Feature Selection Methods' comparision"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4,
                     
                     selectInput(inputId = "choice_1", 
                                 label = "Feature Selection Method",
                                 choices = list("Filter Method","Wrapper Method"), 
                                 selected = "Filter Method", 
                                 multiple = FALSE),
                     
                     conditionalPanel(
                         condition = "input.choice_1 == 'Filter Method'",
                         selectInput(inputId = "choice_2", 
                                     label = "Choose the Filter Method",
                                     choices = list("FSelector_chi.squared",
                                                    "party_cforest.importance",
                                                    "FSelectorRcpp_information.gain"), 
                                     selected = "FSelector_chi.squared", 
                                     multiple = FALSE)
                     ),
                     conditionalPanel(
                         condition = "input.choice_1 == 'Wrapper Method",
                         selectInput(inputId = "choice_2", 
                                     label = "Choose the Wrapper Method",
                                     choices = list("Random",
                                                    "Exhaustive",
                                                    "GA",
                                                    "Sequestial"), 
                                     selected = "Random", 
                                     multiple = FALSE)
                     ),
                     
                     selectInput(inputId = "choice_3", 
                                 label = "Choose Model Type",
                                 choices = list("Regression",
                                                "Classification"), 
                                 selected = "Regression", 
                                 multiple = FALSE),
                     
                     conditionalPanel(
                         condition = "input.choice_3 == 'Regression'",
                         selectInput(inputId = "choice_4", 
                                     label = "Choose the Regression Method",
                                     choices = list("K-Nearest-Neighbor regressiong",
                                                    "Conditional Inference Trees",
                                                    "Bayesian CART"), 
                                     selected = "K-Nearest-Neighbor regressiong", 
                                     multiple = FALSE)
                     ),
                     conditionalPanel(
                         condition = "input.choice_3 == 'Classification'",
                         selectInput(inputId = "choice_4", 
                                     label = "Choose the Classification Method",
                                     choices = list("Binomial Regression",
                                                    "Fast k-Nearest Neighbour",
                                                    "Linear Discriminant Analysis"), 
                                     selected = "Binomial Regression", 
                                     multiple = FALSE)
                     )
                     
                     #actionButton(inputId = "goButton", label = "Go!")
                     
        ),
        
        # Show the table
        mainPanel(
       
            tableOutput("view")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$view <- renderTable({
        
        if(input$choice_1 == 'Filter Method'){
            
            if(input$choice_2 == 'FSelector_chi.squared'){
                
                if(input$choice_3 == 'Regression'){
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(df_val_r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(df_val_r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(df_val_r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(df_val_c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(df_val_c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(df_val_c3)
                        
                    }
                    
                    
                }
                
            }
            
            else if(input$choice_2 == 'party_cforest.importance'){
                
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(df_val_f2r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(df_val_f2r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(df_val_f2r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(df_val_f2c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(df_val_f2c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(df_val_f2c3)
                        
                    }
                    
                    
                }
                
            }
            
            else if(input$choice_2 == 'FSelectorRcpp_information.gain'){
                
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(df_val_f3r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(df_val_f3r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(df_val_f3r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(df_val_f3c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(df_val_f3c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(df_val_f3c3)
                        
                    }
                    
                    
                }
                
            }
            
        }
        else if(input$choice_1 == 'Wrapper Method'){
            
            if(input$choice_2 == 'Random'){
                
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(res_w1r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(res_w1r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(res_w1r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(res_w1c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(res_w1c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(res_w1c3)
                        
                    }
                    
                    
                }
                
                
            }
            
            else if(input$choice_2 == 'GA'){
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(res_w2r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(res_w2r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(res_w2r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(res_w2c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(res_w2c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(res_w2c3)
                        
                    }
                    
                    
                }
                
            }
            
           
            
            else if(input$choice_2 == 'Sequestial'){ 
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(res_w3r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(res_w3r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(res_w3r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(res_w3c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(res_w3c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(res_w3c3)
                        
                    }
                    
                    
                }
                
            }
            
            else if(input$choice_2 == 'Exhaustive'){
                
                if(input$choice_3 == 'Regression'){
                    
                    
                    if(input$choice_4 == 'K-Nearest-Neighbor regressiong'){
                        
                        df<-data.frame(res_w4r1)
                        
                    }
                    
                    else if(input$choice_4 == 'Conditional Inference Trees'){
                        
                        df<-data.frame(res_w4r2)
                        
                    }
                    
                    else if(input$choice_4 == 'Bayesian CART'){
                        
                        df<-data.frame(res_w4r3)
                        
                    }
                    
                }
                
                else if(input$choice_3 == 'Classification'){
                    
                    
                    if(input$choice_4 == 'Binomial Regression'){
                        
                        df<-data.frame(res_w4c1)
                        
                    }
                    
                    else if(input$choice_4 == 'Fast k-Nearest Neighbour'){
                        
                        df<-data.frame(res_w4c2)
                        
                    }
                    
                    else if(input$choice_4 == 'Linear Discriminant Analysis'){
                        
                        df<-data.frame(res_w4c3)
                        
                    }
                    
                    
                }
                
            }
            
        }
        
        
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
