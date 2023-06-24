
library(shiny)
library(rlang)
library(readr)
library(readxl)
library(tidyverse)
library(shinyjs)
library(tableHTML)
library(shinydashboard)
library(ggplot2)
library(cli)
library(reshape2)
library(stringr)
library(triangle)



ui <- dashboardPage(
  
  dashboardHeader(titleWidth=870, title = span(" ", 
                                               span("Variable Distribution Estimator", 
                                                    style = "color: WHITE; font-size: 28px;font-family: Arial")),
                  tags$li(class = "dropdown", tags$a(HTML(paste("Click the horizontal lines to see/hide your input data ", textOutput("See explanations "))),style = "color: silver; font-size: 14px;font-family: Arial;"))
  ),
  
  
  dashboardSidebar(collapsed = TRUE, width = 600,
                  column(8,  
                   
                   br(),
                   tableOutput('table'),
                   br(),
                   br(),
                   actionButton("saveBtn", "PRESS HERE TO SEND YOUR DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
                   br(),
                   column(8,  offset=1,
                   tags$head(tags$style(HTML(".myclass3 pre {color: black;font-size:150%; background-color: lightgrey ;border-colour:lightgrey;  font-weight: bolder;font-size: 12px;font-family: Arial; width: 200px; }"))),
                   div(class = "myclass3",verbatimTextOutput("submitted", placeholder = FALSE))
                   
                  ))          
  ),
  
  dashboardBody(
    h6(textInput("name", em("Input your name here"), value = ("")),style="background-color:#ff8282;color:white; border-color:#ff8282;padding:2px; font-size:120%; font-family: Arial"),
    
    
    tabsetPanel(
      tabPanel(
        title = uiOutput("tab1" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title1"),style ="color:blue;font-weight: bold"),
        numericInput("lower1", "lower1",0 ),
        numericInput("upper1", "upper1",0 ),
        numericInput("median1", "median1",0 ),
        numericInput("threshold1", "threshold1",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab2" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title2"),style ="color:blue;font-weight: bold"),
        numericInput("lower2", "lower2",0 ),
        numericInput("upper2", "upper2",0 ),
        numericInput("median2", "median2",0 ),
        numericInput("threshold2", "threshold2",0 ),
        
      ),
      
      tabPanel(
        title = uiOutput("tab3" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title3"),style ="color:blue;font-weight: bold"),
        numericInput("lower3", "lower3",0 ),
        numericInput("upper3", "upper3",0 ),
        numericInput("median3", "median3",0 ),
        numericInput("threshold3", "threshold3",0 ),
        
      ),
      
      tabPanel(
        title = uiOutput("tab4" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title4"),style ="color:blue;font-weight: bold"),
        numericInput("lower4", "lower4",0 ),
        numericInput("upper4", "upper4",0 ),
        numericInput("median4", "median4",0 ),
        numericInput("threshold4", "threshold4",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab5" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title5"),style ="color:blue;font-weight: bold"),
        numericInput("lower5", "lower5",0 ),
        numericInput("upper5", "upper5",0 ),
        numericInput("median5", "median5",0 ),
        numericInput("threshold5", "threshold5",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab6" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title6"),style ="color:blue;font-weight: bold"),
        numericInput("lower6", "lower6",0 ),
        numericInput("upper6", "upper6",0 ),
        numericInput("median6", "median6",0 ),
        numericInput("threshold6", "threshold6",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab7" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title7"),style ="color:blue;font-weight: bold"),
        numericInput("lower7", "lower7",0 ),
        numericInput("upper7", "upper7",0 ),
        numericInput("median7", "median7",0 ),
        numericInput("threshold7", "threshold7",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab8" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title8"),style ="color:blue;font-weight: bold"),
        numericInput("lower8", "lower8",0 ),
        numericInput("upper8", "upper8",0 ),
        numericInput("median8", "median8",0 ),
        numericInput("threshold8", "threshold8",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab9" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title9"),style ="color:blue;font-weight: bold"),
        numericInput("lower9", "lower9",0 ),
        numericInput("upper9", "upper9",0 ),
        numericInput("median9", "median9",0 ),
        numericInput("threshold9", "threshold9",0 )
        
      ),
      
      tabPanel(
        title = uiOutput("tab10" ,style ="color:darkblue; font-size:14px"),
        h5(uiOutput("tab_title10"),style ="color:blue;font-weight: bold"),
        numericInput("lower10", "lower10",0 ),
        numericInput("upper10", "upper10",0 ),
        numericInput("median10", "median10",0 ),
        numericInput("threshold10", "threshold10",0 )
        
      ),
      
      tabPanel(""),
      tabPanel(""),
      tabPanel(""),
      tabPanel(""),
      tabPanel(
        h6("Plot distributions", style="background-color:white;font-size:75%; font-family: Arial"),
        
        fluidRow(
          column(2,
                 selectInput("dist","Choose the probability distribution: ", choices =
                               list(Continuas = list("Normal", "Beta","Triangular"),
                                    Discretas = list("Binomial", "Poisson")))
          ),
          column(2,
                 conditionalPanel(
                   condition = "input.dist == 'Normal'",
                   numericInput("media","Mean: ", value = 0),
                   numericInput("desv", "Standard deviation: ", value = 1,min=0)
                 ),
                 conditionalPanel(
                   condition = "input.dist == 'Triangular'",
                   numericInput("t1", "Lower Triangular: ", value = 1),
                   numericInput("t2", "Mid Triangular: ", value = 3),
                   numericInput("t3", "Upper Triangular: ", value = 5)
                 ),
                 conditionalPanel(
                   condition = "input.dist == 'Beta'",
                   numericInput("alfa","Alpha: ", value = 1,min=1),
                   numericInput("beta", "Beta: ", value = 1,min=1)
                 ),
                 conditionalPanel(
                   condition = "input.dist == 'Poisson'",
                   numericInput("lambda","Lambda: ", value = 1,min = 0)
                 ),
                 conditionalPanel(
                   condition = "input.dist == 'Binomial'",
                   numericInput("n","n: ", value = 10,step=1),
                   numericInput("p", "p: ", value = 0.5)
                 ),
          ),
          column(2,
                 checkboxInput(inputId="otradist",label="Plot two distributions", value = F)),
          column(2,
                 conditionalPanel(
                   condition = "input.otradist == true",
                   selectInput("dist2","Choose the distribution: ", choices =
                                 list( Continuas = list("Normal", "Beta", "Triangular"),
                                       Discretas = list("Binomial", "Poisson")))
                 )
                 
          ),
          column(2,
                 conditionalPanel(
                   condition = "input.dist2 == 'Normal' && input.otradist == true",
                   numericInput("media2","Mean: ", value = 0),
                   numericInput("desv2", "Standard deviation: ", value = 1,min = 0)
                 ),
                 conditionalPanel(
                   condition = "input.dist2 == 'Beta' && input.otradist == true",
                   numericInput("alfa2","Alpha: ", value = 1, min = 0),
                   numericInput("beta2", "Beta: ", value = 1, min = 0)
                 ),
                 conditionalPanel(
                   condition = "input.dist2 == 'Poisson' && input.otradist == true",
                   numericInput("lambda2","Lambda: ", value = 1),
                 ),
                 conditionalPanel(
                   condition = "input.dist2 == 'Triangular' && input.otradist == true",
                   numericInput("t1_2", "Lower Triangular: ", value = 1),
                   numericInput("t2_2", "Mid Triangular: ", value = 3),
                   numericInput("t3_2", "Upper Triangular: ", value = 5)
                 ),
                 conditionalPanel(
                   condition = "input.dist2 == 'Binomial' && input.otradist == true",
                   numericInput("n2","n: ", value = 10, min = 1),
                   numericInput("p2", "p: ", value = 0.5, min = 0)
                 ),
          )
          
        ),
        fluidRow(
          column(6, offset=2,
                 plotOutput("plot_d")
          ),
          
        )   
        
        
        
        
      ),
      tabPanel(""),
      
      tabPanel(
        h6("Moderator", style="background-color:white;font-size:75%; font-family: Arial"),
        
        tabsetPanel(
          
          tabPanel(
            
            h6("Collect Data"),
            fluidRow(column(3,
                            numericInput("password", "Password:", "enter", width = "100px"),
                            column(4,  
                                   tags$style("#name_openend {font-size:20px;
               color:red;
               display:block;
               font-style: italic;
              width:400px}"),
                                   
                                   uiOutput("name_openend"))
                            
            )),
            
            br(),
            actionButton("saveBtn2", "PRESS HERE TO COLLECT DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
            br(),
            br(),
            tags$head(
              tags$style(HTML(".myclass pre {
        color: blue;
        font-size:150%
        background-color: pink;
        font-weight: bolder;
        font-size: 12px;
        font-family: Arial;
        width: 180px;
      }"))),
            div(class = "myclass",verbatimTextOutput("done", placeholder = FALSE)),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage"))
            
          ),
          
          tabPanel(
            h6("Participants Submitted"),
            
            fluidRow(column(4, 
                            br(),
                            tableOutput('table3'))),
          ),
          
          tabPanel(
            h6("Results"),
            fluidRow(column(9, offset=1, 
                            br(),
                            br(),
                            tags$head(
                              tags$style(HTML(".myclass1 pre {
        color: blue;
        font-size:150%
        font-size: 12px;
        font-family: Arial;
        width: 220px;
      }"))),
                            #div(class = "myclass1",verbatimTextOutput("summ.tab", placeholder = FALSE)),
                            h4("Summary of Participant's Inputs",style="background-color:clear;color:blue;font-weight: bold; font-size:100%; font-family: Arial"),
                            br(),
                            tableOutput('table5'),
                            br(),
                            br(),
                            #div(class = "myclass1",verbatimTextOutput("all.tab", placeholder = FALSE)),
                            h4("Individual Participant Raw Data",style="background-color:clear;color:blue;font-weight: bold;  font-size:100%; font-family: Arial"),
                            br(),
                            
                            
                            #h6(selectInput("sort", "Sort by:", choices = c("Variable", "Participant")),style="font-size:75%; font-family: Arial; width:10px"),
                            h6(selectInput("filter", "Filter by:", choices = c("All", "Variable_1", "Variable_2","Variable_3","Variable_4","Variable_5","Variable_6","Variable_7","Variable_8","Variable_9","Variable_10")),style="font-size:75%; font-family: Arial; width:120px"),
                            br(),
                            tableOutput('table2'),
                            br(),
                            br(),
                            br()
            )
            )
          ),
          
          tabPanel(
            h6("Graphs"),
            fluidRow(column(8, offset=2, 
                            br(),
                            br(),
                            br(),
                            plotOutput("plot1", height = "400px"), 
                            br(),
                            br(),
                            br(),
                            plotOutput("plot2", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot3", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot4", height = "400px"), 
                            br(),
                            br(),
                            br(),
                            plotOutput("plot5", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot6", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot7", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot8", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot9", height = "400px"),
                            br(),
                            br(),
                            br(),
                            plotOutput("plot10", height = "400px"),
                            br(),
                            
            ))
          ),
          tabPanel(
            h6("Download Results Files"),
            br(),
            br(),
            downloadButton("saveBtn_raw", "PRESS HERE TO SAVE RAW DATA", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
            br(),   
            br(),
            br(),
            tableOutput('table_raw_dat'),
            br(),
            
            
            
            
          ),
          
          tabPanel("" ),
          tabPanel("" ),
          tabPanel("" ),
          tabPanel("" ),
          
          
          tabPanel(
            h6("Upload distribution names file"),
            br(),
            br(),
            h4("The data input (CSV) file requires one column WITHOUT A HEADING with up to 10 rows:"),
            h4("Column 1 should contain the choice of items to select "),
            br(),
            fileInput("file1", "Choose CSV File"),
            
            br(),
            
            tableOutput('contents'),
            
            br(),
            actionButton("saveBtn_input", "PRESS HERE SEND INPUT FORM - REQUIRES PASSWORD", style="background-color:#ff8282;color:white; border-color:#ff8282;padding:4px; font-size:110%; font-family: Arial"),
            br(),
            shinyjs::useShinyjs(),
            shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
            br(),
            actionButton("refresh2", "Refresh"),
            br(),
            br()
            
          )
          
          
        )
      )
    )
  ))

server <- function(input, output,session) {
  
  
  names_dist <- as.data.frame(read_csv("distribution_in_list_3786452.csv"))
                              

  if(nrow(names_dist)>10){
    names_dist_1 <- names_dist$V1[1:10]
  }else{
    names_dist_1<-names_dist$V1
  }
  
  
  
  output$tab1 <- renderText({
    names_dist_1[1]
    
  })
  
  output$tab2 <- renderText({
    if(nrow(names_dist)>1){
      names_dist_1[2]
    }else {
      ""
    }
     
  })
  output$tab3 <- renderText({
    if(nrow(names_dist)>2){
      names_dist_1[3]
    }else {
      ""
    }
  })
  output$tab4 <- renderText({
    if(nrow(names_dist)>3){
      names_dist_1[4]
    }else {
      ""
    }
  })
  output$tab5 <- renderText({
    if(nrow(names_dist)>4){
      names_dist_1[5]
    }else {
      ""
    }
  })
  output$tab6 <- renderText({
    if(nrow(names_dist)>5){
      names_dist_1[6]
    }else {
      ""
    }
  })
  output$tab7 <- renderText({
    if(nrow(names_dist)>6){
      names_dist_1[7]
    }else {
      ""
    }
  })
  output$tab8 <- renderText({
    if(nrow(names_dist)>7){
      names_dist_1[8]
    }else {
      ""
    }
  })
  output$tab9 <- renderText({
    if(nrow(names_dist)>8){
      names_dist_1[9]
    }else {
      ""
    }
  })
  output$tab10 <- renderText({
    if(nrow(names_dist)>9){
      names_dist_1[10]
    }else {
      ""
    }
  })
  #
  
  output$tab_title1 <- renderText({
    names_dist_1[1]
  })
  
  output$tab_title2 <- renderText({
    names_dist_1[2]
  })
  
  output$tab_title3 <- renderText({
    names_dist_1[3]
  })
  output$tab_title4 <- renderText({
    names_dist_1[4]
  })
  output$tab_title5 <- renderText({
    names_dist_1[5]
  })
  output$tab_title6 <- renderText({
    names_dist_1[6]
  })
  output$tab_title7 <- renderText({
    names_dist_1[7]
  })
  output$tab_title8 <- renderText({
    names_dist_1[8]
  })
  output$tab_title9 <- renderText({
    names_dist_1[9]
  })
  output$tab_title10 <- renderText({
    names_dist_1[10]
  })
  ##
  
  tab_title1_1 <-gsub(" ", "_", names_dist_1[1])
  tab_title1_2 <-gsub(" ", "_", names_dist_1[2])
  tab_title1_3 <-gsub(" ", "_", names_dist_1[3])
  tab_title1_4 <-gsub(" ", "_", names_dist_1[4])
  tab_title1_5 <-gsub(" ", "_", names_dist_1[5])
  tab_title1_6 <-gsub(" ", "_", names_dist_1[6])
  tab_title1_7 <-gsub(" ", "_", names_dist_1[7])
  tab_title1_8 <-gsub(" ", "_", names_dist_1[8])
  tab_title1_9 <-gsub(" ", "_", names_dist_1[9])
  tab_title1_10 <-gsub(" ", "_", names_dist_1[10])
  
  #
  
  
  abm_values2 <- reactive({
    
    validate(
      need(input$median1 >= input$lower1 & input$median1 <= input$upper1, "Median Variable 1 not between upper and lower values")
    )
    validate(
      need(input$median2 >= input$lower2 & input$median2 <= input$upper2, "Median Variable 2 not between upper and lower values")
    )
    validate(
      need(input$median3 >= input$lower3 & input$median3 <= input$upper3, "Median Variable 3 not between upper and lower values")
    )
    validate(
      need(input$median4 >= input$lower4 & input$median4 <= input$upper4, "Median Variable 4 not between upper and lower values")
    )
    validate(
      need(input$median5 >= input$lower5 & input$median5 <= input$upper5, "Median Variable 5 not between upper and lower values")
    )
    validate(
      need(input$median6 >= input$lower6 & input$median6 <= input$upper6, "Median Variable 6 not between upper and lower values")
    )
    validate(
      need(input$median7 >= input$lower7 & input$median7 <= input$upper7, "Median Variable 7 not between upper and lower values")
    )
    validate(
      need(input$median8 >= input$lower8 & input$median8 <= input$upper8, "Median Variable 8 not between upper and lower values")
    )
    validate(
      need(input$median9 >= input$lower9 & input$median9 <= input$upper9, "Median Variable 9 not between upper and lower values")
    )
    validate(
      need(input$median10 >= input$lower10 & input$median10 <= input$upper10, "Median Variable 10 not between upper and lower values")
    )
    
    
    
    
    df_in <-data.frame(
      Variable = c(paste0(names_dist$V1[1],"_", input$name), paste0(names_dist$V1[2],input$name_var2,"_",input$name),
                   paste0(names_dist$V1[3],input$name_var3,"_", input$name),paste0(names_dist$V1[4],input$name_var4,"_", input$name),
                   paste0(names_dist$V1[5],input$name_var5,"_", input$name),paste0(names_dist$V1[6],input$name_var6,"_", input$name),
                   paste0(names_dist$V1[7],input$name_var7,"_", input$name),paste0(names_dist$V1[8],input$name_var8,"_", input$name),
                   paste0(names_dist$V1[9],input$name_var9,"_", input$name),paste0(names_dist$V1[10],input$name_var10,"_", input$name)),
      "Lower" = c(input$lower1,
                  input$lower2,
                  input$lower3,
                  input$lower4,
                  input$lower5,
                  input$lower6,
                  input$lower7,
                  input$lower8,
                  input$lower9,
                  input$lower10),
      "Median" = c(input$median1,
                   input$median2,
                   input$median3,
                   input$median4,
                   input$median5,
                   input$median6,
                   input$median7,
                   input$median8,
                   input$median9,
                   input$median10
      ),
      "Upper" = c(input$upper1,
                  input$upper2,
                  input$upper3,
                  input$upper4,
                  input$upper5,
                  input$upper6,
                  input$upper7,
                  input$upper8,
                  input$upper9,
                  input$upper10
      ),
      "THRESHOLD" = c(input$threshold1,
                      input$threshold2,
                      input$threshold3,
                      input$threshold4,
                      input$threshold5,
                      input$threshold6,
                      input$threshold7,
                      input$threshold8,
                      input$threshold9,
                      input$threshold10
      ),
      
      stringsAsFactors = FALSE)
    
    df_out <- df_in[1:nrow(names_dist),]
    df_out
    
  })
  
  output$table <- renderTable({
    abm_values2()
    
  })
  
  
  ### CODE TO SAVE EXPERT INPUTS WITH SPEC OF DISTRIBUTION NAMES
  
  my_string<-sort(c(as.vector(substr(names_dist_1, start = 1, stop = 1)), as.vector(substr(names_dist_1, start = min(nchar(names_dist_1)) , stop = min(nchar(names_dist_1)) ))))
  updated_string <- paste(my_string,collapse='')
  updated_string1 <-gsub(" ", "", updated_string)
  
  
  if(length(names_dist_1) >9){
    updated_string2 <- paste0(updated_string1, length(names_dist_1))
  }else{
    updated_string2 <- paste0(updated_string1, "_", length(names_dist_1))
  }
  
  
  
  
  observeEvent(input$saveBtn, {
    
    write.csv(abm_values2(), paste0(input$name, "_ABM_5638762_" ,updated_string2, format(Sys.Date(), format="%b_%d_%y"), ".csv"),append = FALSE, sep = " ", dec = ".",
              row.names = FALSE, col.names = TRUE)
    
    output$submitted <- renderText({
      ".... file submitted thank you"
    })
    
  })
  
  ############ FOR MODERATOR TABS ###
  
  p_w <- reactive(input$password) 
  ##TICK APPEARS IF CORRECT  
  output$name_openend <- renderUI({
    req(input$password)
    if(input$password==1234){
      condition <- tags$div(
        tags$i(class = "fa fa-check")) #"Correct"
    } else{
      condition <- "PASSWORD INCORRECT"
    }
  })
  
  ### DATA COLLECTION AND DONE BOX WHEN DATA COLLECTED 
  
  observeEvent(input$saveBtn2, {
    
    if(p_w() == 1234){
    
    list_in <- as.data.frame(list.files(pattern="*.csv"))
    colnames(list_in)[1] <- "nme"
    files <- dplyr::filter(list_in, grepl(updated_string2,nme))
    files$name_included <- substr(files$nme,1,nchar(files$nme)-(26+ nchar(updated_string2)))
    
    files_all <- as.data.frame(files) ## FOR THE LATER SERVER VIEWING OF FILES
    
    
    db_all <- data.frame()
    for( i in 1:nrow(files)){
      
      test <- read_csv(files$nme[i])
      test$name <- files$nme[i]
      test$name <- gsub(paste0("ABM_5638762",updated_string2),'',test$name)
      test$rank <- seq(1, nrow(test),1)
      db_all <- rbind(db_all, test)
      
    }
    
    
      
      output$done <- renderText({
        paste0(nrow(files)," participants data received" )
      })
      
      
    }
    
    ### MODERATOR TAB 2 - NAMES PARTICIPANTS SUBMITTED
    
    names_done <- reactive({
      df_name <- as.data.frame(t(files$name_included))
      names(df_name) <- NULL
      df_name
    })
    output$table3 <- renderTable({
      names_done()
    })
    
    
    
    ### PLOT RESULTS
    
    
    Var_1 <- db_all %>% filter(grepl(names_dist$V1[1], Variable))
    Var_1_melt <- melt(Var_1)
    output$plot1 <- renderPlot({
      ggplot(Var_1_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[1])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_2 <- db_all %>% filter(grepl(names_dist$V1[2], Variable))
    Var_2_melt <- melt(Var_2)
    output$plot2 <- renderPlot({
      ggplot(Var_2_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[2])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_3 <- db_all %>% filter(grepl(names_dist$V1[3], Variable))
    Var_3_melt <- melt(Var_3)
    output$plot3 <- renderPlot({
      ggplot(Var_3_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[3])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_4 <- db_all %>% filter(grepl(names_dist$V1[4], Variable))
    Var_4_melt <- melt(Var_4)
    output$plot4 <- renderPlot({
      ggplot(Var_4_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[4])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_5 <- db_all %>% filter(grepl(names_dist$V1[5], Variable))
    Var_5_melt <- melt(Var_5)
    output$plot5 <- renderPlot({
      ggplot(Var_5_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[5])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_6 <- db_all %>% filter(grepl(names_dist$V1[6], Variable))
    Var_6_melt <- melt(Var_6)
    output$plot6 <- renderPlot({
      ggplot(Var_6_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[6])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_7 <- db_all %>% filter(grepl(names_dist$V1[7], Variable))
    Var_7_melt <- melt(Var_7)
    output$plot7 <- renderPlot({
      ggplot(Var_7_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[7])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_8 <- db_all %>% filter(grepl(names_dist$V1[8], Variable))
    Var_8_melt <- melt(Var_8)
    output$plot8 <- renderPlot({
      ggplot(Var_8_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[8])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_9 <- db_all %>% filter(grepl(names_dist$V1[9], Variable))
    Var_9_melt <- melt(Var_9)
    output$plot9 <- renderPlot({
      ggplot(Var_9_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[9])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    Var_10 <- db_all %>% filter(grepl(names_dist$V1[10], Variable))
    Var_10_melt <- melt(Var_10)
    output$plot10 <- renderPlot({
      ggplot(Var_10_melt, aes(x=value, y=variable, fill = variable)) +
        ylab("Elicited Quantity")+
        xlab("Value")+
        geom_boxplot()+ ggtitle(names_dist$V1[10])+
        scale_fill_manual(values = c("THRESHOLD" = 'red'))+ theme(legend.position = "none",plot.title = element_text(size=28),axis.title=element_text(size=16),axis.text=element_text(size=16))
    })
    
    
    
    ### SUMMARY TABLES
    
    new_tab <- as.data.frame(c(names_dist$V1[1], names_dist$V1[2],names_dist$V1[3],names_dist$V1[4],names_dist$V1[5],
                               names_dist$V1[6],names_dist$V1[7],names_dist$V1[8],names_dist$V1[9],names_dist$V1[10]))
    
    df_abm<- data.frame()
    for(i in 1:nrow(names_dist)){
      abm_filt <- db_all %>% filter(grepl(new_tab[i,1], Variable ))
      abm_filt$var <- new_tab[i,1]
      abm_filt$nmbr <- i
      df_abm <- rbind(df_abm, abm_filt)
    }
    head(df_abm, 20)
    
    
    
    abm_gp <- df_abm %>%
      group_by(var)%>%
      summarise(median_lower = median(Lower),
                median_median = median(Median),
                median_upper = median(Upper),
                median_threshold = median(THRESHOLD),
                order_var = mean(nmbr)) %>%
      arrange(order_var)
    
    abm_gp <- abm_gp[,1:5]
    
    colnames(abm_gp) <- c("Variable", "Median of Lower Estimate", "Median of Central Estimate", "Median of Upper Estimate", "Median of Threshold")
    
    
    
    output$table5 <- renderTable({
      if(p_w() == 1234){
        abm_gp }
    }, digits=1)
    
    
    
    df_dat <- data.frame(db_all)
    colnames(df_dat)[1] <- "Variable_name"
    
    df_dat$Participant <-sub(".*_", "", df_dat$Variable_name)#
    
    df_dat$Participant = df_dat$Participant %>% str_remove("-")
    
    
    output$table2 <- renderTable({
      
      df_dat <-dplyr::arrange(df_dat,Participant) #%>% dplyr::select(-ABM)
      colnames(df_dat)[1] <- "Variable_Participant"
      df_dat2 <- df_dat %>% dplyr::select(-Participant)
      df_dat2$sum_calc <- df_dat2$Lower+df_dat2$Median+df_dat2$Upper+df_dat2$THRESHOLD
      df_dat2 <- df_dat2 %>% filter(sum_calc >0)
      df_dat2 <- df_dat2 %>% select(-sum_calc)
      
      
      if(input$filter == "Variable_1"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[1], Variable_Participant))
        
      }
      else if(input$filter == "Variable_2"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[2], Variable_Participant))
        
      }
      else if(input$filter == "Variable_3"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[3], Variable_Participant))
        
      }
      else if(input$filter == "Variable_4"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[4], Variable_Participant))
        
      }
      else if(input$filter == "Variable_5"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[5], Variable_Participant))
        
      }
      else if(input$filter == "Variable_6"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[6], Variable_Participant))
        
      }
      else if(input$filter == "Variable_7"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[7], Variable_Participant))
        
      }
      else if(input$filter == "Variable_8"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[8], Variable_Participant))
        
      }
      else if(input$filter == "Variable_9"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[9], Variable_Participant))
        
      }
      else if(input$filter == "Variable_10"){
        dplyr::filter(df_dat2, grepl(names_dist$V1[10], Variable_name))
        
      }
      
      
      else{
        df_dat2 <- df_abm %>% arrange(nmbr)
        colnames(df_dat2)[1]<- "Variable_Participant"
        df_dat2[,1:5]
      }
      
      
    },digits=1)
    
    
    df_dat_raw <- df_abm %>% arrange(nmbr)
    colnames(df_dat_raw)[1]<- "Variable_Participant"
    df_dat_out <<- as.data.frame(df_dat_raw[,1:5])
    
  }) 
  
  output$saveBtn_raw <- downloadHandler(
    filename = function() {
      paste("Raw_Variable_data", ".csv", sep = "")
    },
    content = function(file) {
      write_csv(df_dat_out, file)
    }
  ) 
  
  ## RAW DATA TABLE ##
  
  output$table_raw_dat <- renderTable({
    df_dat_out
    
  })
  
  observeEvent(input$refresh2, {
    refresh()
  })
  
  ################### GRAPHING CODE ####
  
  dist<-reactive(input$dist)
  media<-reactive(input$media)
  sd<-reactive(input$desv)
  lambda<-reactive(input$lambda)
  n<-reactive(input$n)
  p<-reactive(input$p)
  alfa<-reactive(input$alfa)
  beta<-reactive(input$beta)
  t1<-reactive(input$t1)
  t2<-reactive(input$t2)
  t3<-reactive(input$t3)
  output$plot_d<-renderPlot({
    
    inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Poisson"=0,"Binomial"=0,"Triangular"= t1()-1)
    sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Poisson"=2*lambda(),"Binomial"=n(),"Triangular"=t3()+1)
    puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                   "Poisson"=inf:sup,"Binomial"=inf:sup, "Triangular"=seq(from=inf,to=sup,length.out = 1000))
    Density<-switch(dist(),"Normal"=dnorm(puntos,media(),sd()),
                    "Beta"=dbeta(puntos,alfa(),beta()),
                    "Poisson"=dpois(puntos,lambda()),
                    "Binomial"=dbinom(puntos,n(),p()),
                    "Triangular"=dtriangle(puntos,t1(),t3(), t2()))
    media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Poisson"=lambda(),"Binomial" = n()*p(),"Triangular" = t2())
    yl<<-c(0,4*max(Density)/3)
    
    inf2<-switch(dist2(),"Normal"=media2()-3*sd2(),"Beta"=0,"Poisson"=0,"Binomial"=0,"Triangular"= t1_2()-1)
    sup2<-switch(dist2(),"Normal"=media2()+3*sd2(),"Beta"=1,"Poisson"=2*lambda2(),"Binomial"=n2(),"Triangular"=t3_2()+1)
    puntos2<-switch(dist2(),"Normal"=seq(from=inf2,to=sup2,length.out=1000),"Beta"=seq(from=inf2,to=sup2,length.out=1000),
                    "Poisson"=inf2:sup2,"Binomial"=inf2:sup2, "Triangular"=seq(from=inf2,to=sup2,length.out = 1000))
    Density2<-switch(dist2(),"Normal"=dnorm(puntos2,media2(),sd2()),
                     "Beta"=dbeta(puntos2,alfa2(),beta2()),
                     "Poisson"=dpois(puntos2,lambda2()),
                     "Binomial"=dbinom(puntos2,n2(),p2()),
                     "Triangular"=dtriangle(puntos2,t1_2(),t3_2(), t2_2()))
    media2<-switch(dist2(),"Normal"=media2(),"Beta"=alfa2()/(alfa2()+beta2()),"Poisson"=lambda2(),"Binomial" = n2()*p2(),"Triangular" = t2())
    
    
    #plot(puntos,Density,type="l", col = "cyan3", xlab="Values",main=dist())
    if(input$otradist){
      new_vec <- c(puntos,puntos2)
      dens_vec <- c(Density, Density2)
      plot(puntos,Density,type="l", col = "cyan3",lwd=2.5,  xlab="Values", xlim=c(min(new_vec),max(new_vec)), ylim = c(0, max(dens_vec)), main="Comparison (Left hand distribution in blue)")
      lines(puntos2,Density2,type="l", lwd=2.5, col = "indianred2")
      
    }else{
      plot(puntos,Density,type="l", col = "cyan3", lwd=2.5, xlab="Values",main=dist())
    }
    abline(v = media, col="cyan2",lwd = 2, lty = 2)
  }
  )
  
  dist2<-reactive(input$dist2)
  media2<-reactive(input$media2)
  sd2<-reactive(input$desv2)
  lambda2<-reactive(input$lambda2)
  n2<-reactive(input$n2)
  p2<-reactive(input$p2)
  alfa2<-reactive(input$alfa2)
  beta2<-reactive(input$beta2)
  t1_2<-reactive(input$t1_2)
  t2_2<-reactive(input$t2_2)
  t3_2<-reactive(input$t3_2)
  
  
  ##################  LOAD NEW INPUT FILE
  
  mydata <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header=FALSE)#, sep=input$sep,  dec = input$dec)
    
    return(tbl)
  })
  
  observeEvent(input$saveBtn_input, {
    if(input$password==1234){
    
    write_csv(mydata(), paste0("distribution_in_list_3786452_2", ".csv"))
    
    input_new <- as.data.frame(read_csv(paste0("distribution_in_list_3786452_2", ".csv")))
    
    write_csv(input_new, "distribution_in_list_3786452.csv")
    
    
    }
  }) 
  
  output$contents <- renderTable({
    
    mydata()
    
  })
  
  
}


shinyApp(ui, server)

