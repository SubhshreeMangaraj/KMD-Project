library(shinythemes)
library(shiny)
library(shinyjs)
library(ggplot2)
library(mlr)
library(iml)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                shinyjs::useShinyjs(),
    navbarPage(
        title = 'Tinnitus Analysis',
        
        tabPanel("Comparision of Feature Selection Methods", sidebarLayout(
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
            
        )),
        tabPanel('Gender Based Models',
                 sidebarLayout(
                     sidebarPanel(width = 15,
                                  fluidRow(
                                      
                                      column(3, wellPanel(
                                          selectInput("input_type1", "Gender Based Models",
                                                      c("RANDOM FOREST","LASSO REGRESSION", "RIDGE REGRESSION", "SVM", "PLS")
                                          )
                                      ))
                                      
                                  )),
                     # Show the table
                     mainPanel(
                         
                         plotOutput("plot1"),
                         plotOutput("plot2"),
                         h4("Important Features and score"),
                         verbatimTextOutput("view1")
                         #h5("Risk Factors in Female"),
                         #verbatimTextOutput("view2")
                     )
                     
                 )
                 
                 
                 ),
        tabPanel('About')
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #.........................Gender based............
    
    output$plot1 <- renderPlot({
        
        if(input$input_type1  == "RANDOM FOREST"){
            
            #vi_plot <- mod_train_male$variable.importance
            #vip(mod_train_male, width = 0.3, fill = "green3",num_features = 20)
            
            plot(imp.rf)
        }
        
        else if(input$input_type1  == "LASSO REGRESSION"){
            
            plot(imp.lasso)
        }
        
        else if(input$input_type1  == "RIDGE REGRESSION"){
            
            #shinyjs::disable("plot1")
            plot(imp.ridge)
        }
        
        else if(input$input_type1  == "SVM"){
            
            #shinyjs::disable("plot1")
            plot(imp.svm)
        }
        
        else if(input$input_type1 == "Partial Least Square"){
            
            plot(imp.pls)
            
        }
    
    })
    
    output$plot2 <- renderPlot({
        
        if(input$input_type1  == "RANDOM FOREST"){
            
            df_res<-data.frame(imp.rf$results)
            
            dd<-df_res %>% 
                mutate(newc = 90*importance)
            
            dd<-dd[order(-dd$newc),]
            
            dd$group = dd %>% group_indices(importance)
            dd$group<-as.factor(dd$group)
            
            dd = dd %>% arrange(group, newc)
            data<-dd
            
            # Set a number of 'empty bar' to add at the end of each group
            empty_bar <- 1
            to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
            colnames(to_add) <- colnames(data)
            to_add$group <- rep(levels(data$group), each=empty_bar)
            data <- rbind(data, to_add)
            data <- data %>% arrange(group)
            data$id <- seq(1, nrow(data))
            
            # Get the name and the y position of each label
            label_data <- data
            number_of_bar <- nrow(label_data)
            angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
            label_data$hjust <- ifelse( angle < -90, 1, 0)
            label_data$angle <- ifelse(angle < -90, angle+180, angle)
            
            # prepare a data frame for base lines
            base_data <- data %>% 
                group_by(group) %>% 
                summarize(start=min(id), end=max(id) - empty_bar) %>% 
                rowwise() %>% 
                mutate(title=mean(c(start, end)))
            
            # prepare a data frame for grid (scales)
            grid_data <- base_data
            grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
            grid_data$start <- grid_data$start - 1
            grid_data <- grid_data[-1,]
            
            # Make the plot
            p <- ggplot(data, aes(x=as.factor(id), y=newc, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                
                # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
                geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                
                # Add text showing the value of each 100/75/50/25 lines
                #annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", #"60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                ylim(-70,150) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm") 
                ) +
                coord_polar() + 
                geom_text(data=label_data, aes(x=id, y=newc+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
            
            # Add base line information
            #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = #"black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            # geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=1, colour = #"black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
            
            p
        }
        
        else if(input$input_type1  == "LASSO REGRESSION"){
            
            df_res<-data.frame(imp.lasso$results)
            
            dd<-df_res %>% 
                mutate(newc = 90*importance)
            
            dd<-dd[order(-dd$newc),]
            
            dd$group = dd %>% group_indices(importance)
            dd$group<-as.factor(dd$group)
            
            dd = dd %>% arrange(group, newc)
            data<-dd
            
            # Set a number of 'empty bar' to add at the end of each group
            empty_bar <- 1
            to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
            colnames(to_add) <- colnames(data)
            to_add$group <- rep(levels(data$group), each=empty_bar)
            data <- rbind(data, to_add)
            data <- data %>% arrange(group)
            data$id <- seq(1, nrow(data))
            
            # Get the name and the y position of each label
            label_data <- data
            number_of_bar <- nrow(label_data)
            angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
            label_data$hjust <- ifelse( angle < -90, 1, 0)
            label_data$angle <- ifelse(angle < -90, angle+180, angle)
            
            # prepare a data frame for base lines
            base_data <- data %>% 
                group_by(group) %>% 
                summarize(start=min(id), end=max(id) - empty_bar) %>% 
                rowwise() %>% 
                mutate(title=mean(c(start, end)))
            
            # prepare a data frame for grid (scales)
            grid_data <- base_data
            grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
            grid_data$start <- grid_data$start - 1
            grid_data <- grid_data[-1,]
            
            # Make the plot
            p <- ggplot(data, aes(x=as.factor(id), y=newc, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                
                # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
                geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                
                # Add text showing the value of each 100/75/50/25 lines
                #annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", #"60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                ylim(-70,150) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm") 
                ) +
                coord_polar() + 
                geom_text(data=label_data, aes(x=id, y=newc+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
            
            # Add base line information
            #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = #"black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            # geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=1, colour = #"black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
            
            p
        }
        
        else if(input$input_type1  == "RIDGE REGRESSION"){
            
            df_res<-data.frame(imp.ridge$results)
            
            dd<-df_res %>% 
                mutate(newc = 90*importance)
            
            dd<-dd[order(-dd$newc),]
            
            dd$group = dd %>% group_indices(importance)
            dd$group<-as.factor(dd$group)
            
            dd = dd %>% arrange(group, newc)
            data<-dd
            
            # Set a number of 'empty bar' to add at the end of each group
            empty_bar <- 1
            to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
            colnames(to_add) <- colnames(data)
            to_add$group <- rep(levels(data$group), each=empty_bar)
            data <- rbind(data, to_add)
            data <- data %>% arrange(group)
            data$id <- seq(1, nrow(data))
            
            # Get the name and the y position of each label
            label_data <- data
            number_of_bar <- nrow(label_data)
            angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
            label_data$hjust <- ifelse( angle < -90, 1, 0)
            label_data$angle <- ifelse(angle < -90, angle+180, angle)
            
            # prepare a data frame for base lines
            base_data <- data %>% 
                group_by(group) %>% 
                summarize(start=min(id), end=max(id) - empty_bar) %>% 
                rowwise() %>% 
                mutate(title=mean(c(start, end)))
            
            # prepare a data frame for grid (scales)
            grid_data <- base_data
            grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
            grid_data$start <- grid_data$start - 1
            grid_data <- grid_data[-1,]
            
            # Make the plot
            p <- ggplot(data, aes(x=as.factor(id), y=newc, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                
                # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
                geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                
                # Add text showing the value of each 100/75/50/25 lines
                #annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", #"60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                ylim(-70,150) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm") 
                ) +
                coord_polar() + 
                geom_text(data=label_data, aes(x=id, y=newc+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
            
            # Add base line information
            #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = #"black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            # geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=1, colour = #"black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
            
            p
        }
        
        else if(input$input_type1  == "SVM"){
            
            df_res<-data.frame(imp.svm$results)
            
            dd<-df_res %>% 
                mutate(newc = 90*importance)
            
            dd<-dd[order(-dd$newc),]
            
            dd$group = dd %>% group_indices(importance)
            dd$group<-as.factor(dd$group)
            
            dd = dd %>% arrange(group, newc)
            data<-dd
            
            # Set a number of 'empty bar' to add at the end of each group
            empty_bar <- 1
            to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
            colnames(to_add) <- colnames(data)
            to_add$group <- rep(levels(data$group), each=empty_bar)
            data <- rbind(data, to_add)
            data <- data %>% arrange(group)
            data$id <- seq(1, nrow(data))
            
            # Get the name and the y position of each label
            label_data <- data
            number_of_bar <- nrow(label_data)
            angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
            label_data$hjust <- ifelse( angle < -90, 1, 0)
            label_data$angle <- ifelse(angle < -90, angle+180, angle)
            
            # prepare a data frame for base lines
            base_data <- data %>% 
                group_by(group) %>% 
                summarize(start=min(id), end=max(id) - empty_bar) %>% 
                rowwise() %>% 
                mutate(title=mean(c(start, end)))
            
            # prepare a data frame for grid (scales)
            grid_data <- base_data
            grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
            grid_data$start <- grid_data$start - 1
            grid_data <- grid_data[-1,]
            
            # Make the plot
            p <- ggplot(data, aes(x=as.factor(id), y=newc, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                
                # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
                geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                
                # Add text showing the value of each 100/75/50/25 lines
                #annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", #"60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                ylim(-70,150) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm") 
                ) +
                coord_polar() + 
                geom_text(data=label_data, aes(x=id, y=newc+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
            
            # Add base line information
            #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = #"black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            # geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=1, colour = #"black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
            
            p
        }
        
        else if(input$input_type1  == "Partial Least Square"){
            
            df_res<-data.frame(imp.pls$results)
            
            dd<-df_res %>% 
                mutate(newc = 90*importance)
            
            dd<-dd[order(-dd$newc),]
            
            dd$group = dd %>% group_indices(importance)
            dd$group<-as.factor(dd$group)
            
            dd = dd %>% arrange(group, newc)
            data<-dd
            
            # Set a number of 'empty bar' to add at the end of each group
            empty_bar <- 1
            to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
            colnames(to_add) <- colnames(data)
            to_add$group <- rep(levels(data$group), each=empty_bar)
            data <- rbind(data, to_add)
            data <- data %>% arrange(group)
            data$id <- seq(1, nrow(data))
            
            # Get the name and the y position of each label
            label_data <- data
            number_of_bar <- nrow(label_data)
            angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
            label_data$hjust <- ifelse( angle < -90, 1, 0)
            label_data$angle <- ifelse(angle < -90, angle+180, angle)
            
            # prepare a data frame for base lines
            base_data <- data %>% 
                group_by(group) %>% 
                summarize(start=min(id), end=max(id) - empty_bar) %>% 
                rowwise() %>% 
                mutate(title=mean(c(start, end)))
            
            # prepare a data frame for grid (scales)
            grid_data <- base_data
            grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
            grid_data$start <- grid_data$start - 1
            grid_data <- grid_data[-1,]
            
            # Make the plot
            p <- ggplot(data, aes(x=as.factor(id), y=newc, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                
                # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
                geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.1 , inherit.aes = FALSE ) +
                
                # Add text showing the value of each 100/75/50/25 lines
                #annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", #"60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
                
                geom_bar(aes(x=as.factor(id), y=newc, fill=group), stat="identity", alpha=0.5) +
                ylim(-70,150) +
                theme_minimal() +
                theme(
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    panel.grid = element_blank(),
                    plot.margin = unit(rep(-1,4), "cm") 
                ) +
                coord_polar() + 
                geom_text(data=label_data, aes(x=id, y=newc+10, label=feature, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
            
            # Add base line information
            #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = #"black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
            # geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=1, colour = #"black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
            
            p
        }
        
    })
    
    
    
    output$view1 <- renderPrint({
        if(input$input_type1  == "LASSO REGRESSION"){
            df.imp.lasso<-data.frame(imp.lasso$results)
            df.imp.lasso<-df.imp.lasso[order(-df.imp.lasso$importance),]
            df.imp.lasso[1:20,]
            
        }
        
        else if(input$input_type1  == "RIDGE REGRESSION"){
            df.imp.ridge<-data.frame(imp.ridge$results)
            df.imp.ridge<-df.imp.ridge[order(-df.imp.ridge$importance),]
            df.imp.ridge[1:20,]
        }
        
        else if(input$input_type1  == "SVM"){
            df.imp.svm<-data.frame(imp.svm$results)
            df.imp.svm<-df.imp.svm[order(-df.imp.svm$importance),]
            df.imp.svm[1:20,]
        }
        else if(input$input_type1  == "RANDOM FOREST"){
            
            df.imp.rf<-data.frame(imp.rf$results)
            df.imp.rf<-df.imp.rf[order(-df.imp.rf$importance),]
            df.imp.rf[1:20,]
        }
        
        else if(input$input_type1  == "Partial Least Square"){
            
            df.imp.pls<-data.frame(imp.pls$results)
            df.imp.pls<-df.imp.pls[order(-df.imp.pls$importance),]
            df.imp.pls[1:20,]
        }
    })
    
   
    
    #.........................Gender Based............
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
            
            df_val_r1 <- readr::read_csv(file.path(here::here(),"output","df_val_r1.csv"))
            df<-data.frame(df_val_r1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_r2 <- readr::read_csv(file.path(here::here(), "output", "df_val_r2.csv"))
            df<-data.frame(df_val_r2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_r3.csv"))
            df<-data.frame(df_val_r3)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_c1.csv"))
            df<-data.frame(df_val_c1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_c2.csv"))
            df<-data.frame(df_val_c2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_c3.csv"))
            df<-data.frame(df_val_c3)
        }
        
        #-----------------------------------------------------------------------------------
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_f2r1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r1.csv"))
            df<-data.frame(df_val_f2r1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_f2r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r2.csv"))
            df<-data.frame(df_val_f2r2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_f2r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r3.csv"))
            df<-data.frame(df_val_f2r3)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_f2c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c1.csv"))
            df<-data.frame(df_val_f2c1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_f2c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c2.csv"))
            df<-data.frame(df_val_f2c2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_f2c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c3.csv"))
            df<-data.frame(df_val_f2c3)
        }
        
        #-----------------------------------------------------------------------------
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_f3r1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r1.csv"))
            df<-data.frame(df_val_f3r1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_f3r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r2.csv"))
            df<-data.frame(df_val_f3r2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_f3r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r3.csv"))
            df<-data.frame(df_val_f3r3)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_f3c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c1.csv"))
            df<-data.frame(df_val_f3c1)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_f3c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c2.csv"))
            df<-data.frame(df_val_f3c2)
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_f3c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c3.csv"))
            df<-data.frame(df_val_f3c3)
        }
        #--------------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_w1r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r1.csv"))
            df<-data.frame(df_val_w1r1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_w1r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r2.csv"))
            df<-data.frame(df_val_w1r2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_w1r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r3.csv"))
            df<-data.frame(df_val_w1r3)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_w1c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c1.csv"))
            df<-data.frame(df_val_w1c1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_w1c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c2.csv"))
            df<-data.frame(df_val_w1c2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_w1c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c3.csv"))
            df<-data.frame(df_val_w1c3)
        }
        #--------------------------------------------------------------------------
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_w2r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r1.csv"))
            df<-data.frame(df_val_w2r1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_w2r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r2.csv"))
            df<-data.frame(df_val_w2r2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_w2r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r3.csv"))
            df<-data.frame(df_val_w2r3)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_w2c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c1.csv"))
            df<-data.frame(df_val_w2c1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_w2c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c2.csv"))
            df<-data.frame(df_val_w2c2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_w2c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c3.csv"))
            df<-data.frame(df_val_w2c3)
        }
        #-------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_w3r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1=3r1.csv"))
            df<-data.frame(df_val_w3r1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_w3r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_w3r2.csv"))
            df<-data.frame(df_val_w3r2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_w3r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3r3.csv"))
            df<-data.frame(df_val_w3r3)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_w3c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c1.csv"))
            df<-data.frame(df_val_w3c1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_w3c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c2.csv"))
            df<-data.frame(df_val_w3c2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_w3c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c3.csv"))
            df<-data.frame(df_val_w3c3)
        }
        #---------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_val_w4r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r1.csv"))
            df<-data.frame(df_val_w4r1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_val_w4r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r2.csv"))
            df<-data.frame(df_val_w4r2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_val_w4r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r3.csv"))
            df<-data.frame(df_val_w4r3)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_val_w4c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c1.csv"))
            df<-data.frame(df_val_w4c1)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_val_w4c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c2.csv"))
            df<-data.frame(df_val_w4c2)
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_val_w4c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c3.csv"))
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
            
            df_tune_r1 <- readr::read_csv(file.path(here::here(),"output", "df_val_r1_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 ,feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_r2_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2,feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_r3_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_c1_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_c2_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelector_chi.squared" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_c3_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        #-----------------------------------------------------------------------------------
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_f2r1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r1_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f2r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "party_cforest.importance")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_f2r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r2_data.csv"))
            
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f2r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "party_cforest.importance")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_f2r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2r3_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f2r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "party_cforest.importance")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_f2c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c1_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f2c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "party_cforest.importance")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_f2c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c2_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f2c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "party_cforest.importance")
           # plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "party_cforest.importance" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_f2c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f2c3_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f2c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "party_cforest.importance")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        #-----------------------------------------------------------------------------
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_f3r1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r1_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f3r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelectorRcpp_information.gain")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_f3r2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r2_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f3r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelectorRcpp_information.gain")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_f3r3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3r3_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_f3r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelectorRcpp_information.gain")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="STEELBLUE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_f3c1 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c1_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f3c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelectorRcpp_information.gain")
           # plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_f3c2 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c2_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f3c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelectorRcpp_information.gain")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
             }
        
        else if(input$input_type  == "Filter Method"         &&
                input$dynamic     == "FSelectorRcpp_information.gain" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_f3c3 <- readr::read_csv(file.path(here::here(),"output", "df_val_f3c3_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_f3c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelectorRcpp_information.gain")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            
            tabb11<-tabb11[(1:20),]
            
            p<-ggplot(tabb11,aes(x= reorder(name,value),value))+
                geom_bar(stat ="identity",fill="ORANGE",width=.5, position = position_dodge(width = .25))+
                theme_bw()+coord_flip()
            
            p +
                xlab("Features") + ylab("Importance")
            }
        #--------------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_w1r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r1_data.csv"))
            df_tune_w1r1<-df_tune_w1r1[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w1r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() + coord_flip()
            
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            tabb11<-tabb11[order(-tabb11$value),]
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), 3, 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , 2, 1))
            tabb11<-tabb11[order(-tabb11$group),]
            tabb11$group<-as.factor(tabb11$group)
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1" = "#CD7F32",
                                             "2" = "#C0C0C0",
                                             "3" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_w1r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r2_data.csv"))
            df_tune_w1r2<-df_tune_w1r2[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w1r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() + coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_w1r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1r3_data.csv"))
            df_tune_w1r3<-df_tune_w1r3[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w1r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() + coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_w1c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c1_data.csv"))
            df_tune_w1c1<-df_tune_w1c1[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w1c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
            }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_w1c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c2_data.csv"))
            df_tune_w1c2<-df_tune_w1c2[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w1c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Random" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_w1c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w1c3_data.csv"))
            df_tune_w1c3<-df_tune_w1c3[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w1c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        #--------------------------------------------------------------------------
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_w2r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r1_data.csv"))
            df_tune_w2r1<-df_tune_w2r1[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w2r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_w2r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r2_data.csv"))
            df_tune_w2r2<-df_tune_w2r2[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w2r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() + coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_w2r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2r3_data.csv"))
            df_tune_w2r3<-df_tune_w2r3[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w2r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_w2c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c1_data.csv"))
            df_tune_w2c1<-df_tune_w2c1[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w2c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_w2c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c2_data.csv"))
            df_tune_w2c2<-df_tune_w2c2[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w2c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "GA" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_w2c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w2c3_data.csv"))
            df_tune_w2c3<-df_tune_w2c3[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w2c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        #-------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_w3r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3r1_data.csv"))
            df_tune_w3r1<-df_tune_w3r1[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w3r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_w3r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3r2_data.csv"))
            df_tune_w3r2<-df_tune_w3r2[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w3r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_w3r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3r3_data.csv"))
            df_tune_w3r3<-df_tune_w3r3[,c(-1)]
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w3r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_w3c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c1_data.csv"))
            df_tune_w3c1<-df_tune_w3c1[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w3c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_w3c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c2_data.csv"))
            df_tune_w3c2<-df_tune_w3c2[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w3c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Sequential" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_w3c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w3c3_data.csv"))
            df_tune_w3c3<-df_tune_w3c3[,c(-1)]
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w3c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip() 
            tabb1<-data.frame(fv2$data)

            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        #---------------------------------------------------------------------------
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive"                 &&
                input$input_type2 == "Regression"             &&
                input$dynamic2    == "K-Nearest-Neighbor regressiong"){
            
            df_tune_w4r1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r1_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w4r1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            #plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
            tabb1<-data.frame(fv2$data)
            tabb1<-tabb1[-c(1),]
            tabb1<-tabb1[,-c(2)]
            tabb1<-tabb1[order(-tabb1$value),]
            tabb11<-filter(tabb1, tabb1$value > 0.0000000)
            
            bin = (tabb11$value[1] - tabb11$value[nrow(tabb11)])/3
            
            tabb11$group = ifelse(tabb11$value <= (tabb11$value[nrow(tabb11)] + bin), "3rd", 
                                  ifelse(tabb11$value < (tabb11$value[nrow(tabb11)] + (2*bin)) 
                                         , "2nd", "1st"))
            
            p<-ggplot(tabb11, aes(x = tabb11$name, y = tabb11$value, fill = tabb11$group)) +
                geom_bar(stat="identity") +coord_flip() +
                scale_fill_manual(values = c("1st" = "#CD7F32",
                                             "2nd" = "#C0C0C0",
                                             "3rd" = "gold"))
            p+  ggtitle("Feature Importance Plot") +
                xlab("Features") + ylab("Importance")
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Conditional Inference Trees"){
            
            df_tune_w4r2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r2_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w4r2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Regression"            &&
                input$dynamic2    == "Bayesian CART"){
            
            df_tune_w4r3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4r3_data.csv"))
            
            regr.task1<-makeRegrTask(
                data   = data.frame(df_tune_w4r3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = regr.task1, 
                                           method = "FSelector_chi.squared")
            plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Binomial Regression"){
            
            df_tune_w4c1 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c1_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w4c1), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip() 
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Fast k-Nearest Neighbour"){
            
            df_tune_w4c2 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c2_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w4c2), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            plotFilterValues(fv2, feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()
        }
        
        else if(input$input_type  == "Wrapper Method"         &&
                input$dynamic     == "Exhaustive" &&
                input$input_type2 == "Classification"            &&
                input$dynamic2    == "Linear Discriminant Analysis"){
            
            df_tune_w4c3 <- readr::read_csv(file.path(here::here(),"wrapperoutput", "df_val_w4c3_data.csv"))
            
            clasf.task1<-makeClassifTask(
                data   = data.frame(df_tune_w4c3), 
                target = "response")
            
            fv2 = generateFilterValuesData(task   = clasf.task1, 
                                           method = "FSelector_chi.squared")
            plotFilterValues(fv2 , feat.type.cols = TRUE ) + ggpubr::theme_pubr() +coord_flip()  
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
