#Load libraries
library(shiny)
library(dplyr)
library(readr)
library(bubbles)
library(ggplot2)
library(shinythemes)
library(colorspace)
library(plotly)

#Import dataset 
candy_data <- read_csv("candy-data.csv")

#Basic data cleaning
candy_data <- candy_data %>%
  mutate(competitorname = gsub("Õ", "'", candy_data$competitorname)) %>%
  mutate(sugarpercent = 100*sugarpercent, pricepercent = 100*pricepercent) %>%
  rename("Brand" = competitorname,
       "Chocolate" = chocolate,
       "Fruity" = fruity,
       "Nuts" = peanutyalmondy,
       "Nougat" = nougat,
       "Wafer" = crispedricewafer,
       "Hard" = hard,
       "Bar" = bar,
       "Caramel" = caramel,
       "Pluribus" = pluribus,
       "SugarPer" = sugarpercent,
       "PricePer" = pricepercent,
       "WinPer" = winpercent) %>%
  select(-Pluribus)

#Nice Labels
x_label <- data.frame(var = c("SugarPer", "PricePer", "WinPer"),
                        names = c("Sugar Percentile", "Price Percentile", "Win Percentage"))
y_label <- data.frame(var = c("SugarPer", "PricePer", "WinPer"),
                      names = c("Sugar Percentile", "Price Percentile", "Win Percentage"))


##### UI Side ######

# Use taglist layout - this allows us to have multiple navigation tabs
ui = tagList(
    navbarPage(
       theme = shinytheme("paper"),  # <--- To use a theme, uncomment this
      "shinythemes",
      
      #Define first navigation panel
      tabPanel("Find the perfect candy for you!",
    
        #Define sidebar
        sidebarPanel(
          helpText("What do you look for in a candy?"),
      
          #Create checkbox inputs
          checkboxInput(inputId = "choc", label = "Chocolate?", value = FALSE),
          checkboxInput(inputId = "frui", label = "Fruity?", value = FALSE),
          checkboxInput(inputId = "pean", label = "Peanuts & Almonds?", value = TRUE),
          checkboxInput(inputId = "noug", label = "Nougat?", value = FALSE),
          checkboxInput(inputId = "cris", label = "Crisped Rice Wafer?", value = FALSE),
          checkboxInput(inputId = "hard", label = "Hard?", value = FALSE),
          checkboxInput(inputId = "bar", label = "A bar?", value = FALSE),
          checkboxInput(inputId = "car", label = "Caramel?", value = FALSE),
          
          #Create radio button input for size
          radioButtons(inputId = "size", "Bubble Size:",
                   c("Sugar (Percentile)" = "SugarPer",
                     "Price (Percentile)" = "PricePer")),
          
          #Create slider input for filtering based on sugar percentile
          sliderInput("SugarPer", 
                  "Sugar Percentile:", 
                  min = 0,
                  max = 100,
                  value = c(0, 100)),
          
          #Create slider input for filtering based on price percentile
          sliderInput("PricePer", 
                  "Price Percentile:",
                  min = 0,
                  max = 100,
                  value = c(0, 100)),
          
      #Add text to interactivity bar
      helpText("This interactive app was created with the", a("Candy Power Ranking data", 
                href="https://github.com/fivethirtyeight/data/tree/master/candy-power-ranking"), 
               "from", a("FiveThirtyEight.", href="http://fivethirtyeight.com/")),
    
      #Download Button
      uiOutput("download_button")),
    
      #Create main panel with two tabs: one for visual and one for data
      mainPanel(
        tabsetPanel(type = "tabs",
                  tabPanel("Visual", 
                           h5("Candies in bubbles:"),
                           p("Please select your desired ingredients, sugar percentile, and price percentile in a candy
                             in the left sidebar. Sugar percentile is the percentile of sugar 
                            it falls under within the data set. Price percentile is the unit price percentile compared to 
                             the rest of the set."),
                           p("Please also select whether you would like the bubbles to be sized according
                           to the sugar percentile or the price percentile. A larger bubble corresponds to a
                           higher percentile on the 0-100% scale. We'll find the perfect candy for you."), 
                           p("Note that if no ingredients are selected, all the candies will be displayed. The colors
                             in the bubble are only for asthetics; their shades and saturation do not indicate any
                             categorization or ordering."), 
                           bubbles::bubblesOutput(outputId = "bubble")),
                  tabPanel("Data",
                           h5("Notes on the dataset:"),
                           p("For binary variables, 1 means yes, 0 means no. SugarPer is the percentile of sugar 
                            it falls under within the data set. PricePer is the unit price percentile compared to 
                             the rest of the set."),
                            DT::dataTableOutput(outputId = "table")))
        )
      ),
    
    #Define second navigation panel
    tabPanel("Explore different candies in a plot!",
             
             #Define sidebar
             sidebarPanel(
               
               #Create radio buttons for selecting x axis variable
               radioButtons(inputId = "xaxis", "X-axis Variable:",
                            c("Sugar (Percentile)" = "SugarPer",
                              "Price (Percentile)" = "PricePer",
                              "Win Percentage" = "WinPer")),
               
               #Create radio buttons for selecting y axis variable
               radioButtons(inputId = "yaxis", "Y-axis Variable:",
                            c("Sugar (Percentile)" = "SugarPer",
                              "Price (Percentile)" = "PricePer",
                              "Win Percentage" = "WinPer")),
               
               #Add descriptive text
               helpText("This interactive app was created with the", a("Candy Power Ranking data", 
      href="https://github.com/fivethirtyeight/data/tree/master/candy-power-ranking"), 
      "from", a("FiveThirtyEight.", href="http://fivethirtyeight.com/"))
               ),
             
             #Put output graphic and description of text in second main panel
             mainPanel(
                 tabsetPanel(type = "tabs",
                          tabPanel("Visual", 
                                  plotlyOutput(outputId = "plot")),
                          tabPanel("Description",
                                   h5("Notes on the dataset:"),
                                   p("In October of 2017, FiveThirtyEight ran an experiment where they had users on their site fill out a brief survey.
                             Each user who took the survey was faced with two candies and asked to chooose which they would prefer. In total, there were over 259,000
                             matchups. The winning percentage corresponds to the percent of matchups that a particular candy won. You can use the visual on the other tab 
                             to explore a potential relationship between price or sugar content and winning percentage. The full FiveThirtyEight article can be found", 
                            a("here.", href="http://fivethirtyeight.com/features/the-ultimate-halloween-candy-power-ranking/"))
                          )
                  )
            )
    )
)
)


##### Server Side #####
server<-function(input, output){
  
  #Label names
  xlab_var_name <- reactive({
    filter(x_label, var == input$xaxis) %>%
    select(names) #.$names
  })
  
  ylab_var_name <- reactive({
    y_label %>%
    filter(var == input$yaxis) %>%
    select(names) #.$names
  })
  
  #Create reactive dataset
  data_react <- reactive({
    candy_data %>%
      filter(if(input$choc == TRUE){Chocolate == 1}else{Chocolate %in% 0:1}) %>%
      filter(if(input$frui == TRUE){Fruity == 1}else{Fruity %in% 0:1}) %>%
      filter(if(input$pean == TRUE){Nuts == 1}else{Nuts %in% 0:1}) %>%
      filter(if(input$noug == TRUE){Nougat == 1}else{Nougat %in% 0:1}) %>%
      filter(if(input$cris == TRUE){Wafer == 1}else{Wafer %in% 0:1}) %>%
      filter(if(input$hard == TRUE){Hard == 1}else{Hard %in% 0:1}) %>%
      filter(if(input$bar == TRUE){Bar == 1}else{Bar %in% 0:1}) %>%
      filter(if(input$car == TRUE){Caramel == 1}else{Caramel %in% 0:1}) %>% 
      filter(PricePer >= input$PricePer[1], PricePer <= input$PricePer[2]) %>%
      filter(SugarPer >= input$SugarPer[1], SugarPer <= input$SugarPer[2]) %>%
      mutate(Size = get(input$size)) %>%
      select(-WinPer)
  })
  
  #Print data table
  output$table <- DT::renderDataTable({
    if (nrow(data_react()) == 0)
      return()
    DT::datatable(data = data_react(), rownames = FALSE)
  })
  
  #Make bubbles
  output$bubble <- bubbles::renderBubbles({
    if (nrow(data_react()) == 0)
      return()
    bubbles(value = data_react()$Size,
            label = data_react()$Brand,
            color = rainbow_hcl(nrow(data_react())))
  })
  
  #Create plotly output for second visual
  output$plot <- renderPlotly({
      ggplot(data = candy_data, mapping = aes_string(x = input$xaxis, y = input$yaxis, color = "Brand")) + 
      geom_point() + 
      scale_color_manual(values = rainbow_hcl(nrow(candy_data))) + 
      theme(legend.position = "none") +
      labs(x = as.character(xlab_var_name()[1,1]), y = as.character(ylab_var_name()[1,1]))
      
  })
  
  #Download file option
  output$download_button <- renderUI({
    if(!is.null(data_react())) {
      downloadButton('download', 'Download Output File')
    }
  })
  
  output$download <- downloadHandler(
    filename = "candies.csv",
    content = function(file) {
      write_csv(data_react(), file)
    }
  )
  
}

shinyApp(ui=ui, server=server) 