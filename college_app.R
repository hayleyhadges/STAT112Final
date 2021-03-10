library(shiny)
library(tidyverse)
library(states)
library(RColorBrewer)
library(extrafont)

loadfonts(device = "win", quiet = TRUE)

state_list <- 
  salary_potential %>% 
  distinct(state_name)

names(state_list) <-state.name

ui <- fluidPage(
  titlePanel("Comparing States for College"),
  
  sidebarLayout(
    
  sidebarPanel(
    width = 2,
  selectInput("state", 
              "Choose Your States", 
              choices = state_list,
              multiple = TRUE),
  selectInput("incomelvl", 
              "Pick Your Income Level", 
              c("0 to 30,000" = "0 to 30,000",
                "30,001 to 48,000" = "30,001 to 48,000",
                "48,001 to 75,000" = "48_001 to 75,000",
                "75,001 to 110,000" = "75,001 to 110,000",
                "Over 110,000" = "Over 110,000")),
  actionButton("button", "Compare States")
  ),
  
  
  mainPanel(
    width = 10,
  column(6,
  plotOutput(outputId = "firstplot"),
  plotOutput(outputId = "secondplot")
  ),
  
  column(6,
  plotOutput(outputId = "thirdplot"),
  plotOutput(outputId = "fourthplot")
  ))
))

server <- function(input, output) {
  
 observeEvent(input$button, 
  output$firstplot <- renderPlot({
    
    salary_potential %>% 
      pivot_longer(col = c("early_career_pay", "mid_career_pay"), 
                   names_to = "pay_type",
                   values_to = "pay") %>% 
      filter(state_name %in% input$state) %>% 
      ggplot(aes(y=pay, x=fct_reorder(state_name, pay, mean), color = pay_type, fill = state_name)) + 
      geom_boxplot(position="dodge", outlier.colour="transparent") +
      guides(fill = FALSE) +
      scale_fill_brewer(palette = "Paired") +
      scale_color_manual(name = "Pay", labels = c("Early Career Pay", "Mid Career Pay"), values = c("black","gray")) +
      coord_flip() +
      labs(title = "Early and Mid Career Pay",x = "", y = "") +
      theme(text =  element_text(family = "Segoe UI Light", size = 16),
            plot.title = element_text(hjust = 0.5,size = 24),
            legend.position = c(0.8, 0.2),
            legend.title = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =  element_line(colour = "gray"),
            panel.grid.minor.x =  element_line(colour = "gray"),
            panel.background = element_rect(fill = NA),
            axis.ticks = element_blank())
  })
 )
  observeEvent(input$button,
  output$secondplot <- renderPlot({
    
    tuition_income %>% 
      mutate(state_name = abbr2state(state)) %>% 
      filter(year == 2018,
             state_name %in% input$state,
             income_lvl == input$incomelvl) %>% 
      group_by(name, state) %>% 
      ggplot(aes(x = net_cost, y = fct_reorder(state_name, net_cost, mean), fill = state_name)) +
      geom_density_ridges(scale = 1.5, quantile_lines = TRUE, quantiles = 2) +
      scale_fill_brewer(palette = "Paired") +
      guides(fill = FALSE) +
      labs(title = "Net Cost Based on Income Level",x = "", y = "") +
      theme(text =  element_text(family = "Segoe UI Light", size = 16),
            plot.title = element_text(hjust = 0.5, size = 24),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =  element_line(colour = "gray"),
            panel.grid.minor.x =  element_line(colour = "gray"),
            panel.background = element_rect(fill = NA),
            axis.ticks = element_blank()) 
  })
  )
  
  observeEvent(input$button,
  output$thirdplot <- renderPlot({
    
    tuition_cost %>% 
      filter(degree_length == "4 Year",
             type == "Public",
             state %in% input$state) %>% 
      mutate(tuition_dif = out_of_state_tuition - in_state_tuition) %>% 
      group_by(state, type) %>%
      ggplot(aes(y=tuition_dif, x=fct_reorder(state, tuition_dif, mean), fill = state)) + 
      geom_violin(position="dodge", draw_quantiles = .5) +
      geom_jitter() +
      scale_fill_brewer(palette="Paired") +
      guides(fill = FALSE) +
      coord_flip()  +
      labs(title = "Difference in Out of State Tuition", x = "", y = "") +
      theme(text =  element_text(family = "Segoe UI Light", size = 16),
            plot.title = element_text(hjust = 0.5, size = 24),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =  element_line(colour = "gray"),
            panel.grid.minor.x =  element_line(colour = "gray"),
            panel.background = element_rect(fill = NA),
            axis.ticks = element_blank()) 
  })
  )
  
  observeEvent(input$button, 
    output$fourthplot <- renderPlot({
    
    diversity_school %>% 
      filter(state %in% input$state) %>% 
      filter(!category %in% c("Women", "Unknown", "Non-Resident Foreign", "Total Minority")) %>%  
      ggplot(aes(x = state, y = enrollment, fill = category)) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_brewer(palette="Paired") +
      labs(title = "Average Racial Makeup of Student Enrollment", x = "", y = "") +
      theme(text =  element_text(family = "Segoe UI Light", size = 16),
            plot.title = element_text(hjust = 0.5, size = 24),
            legend.title = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x =  element_blank(),
            panel.grid.minor.x =  element_blank(),
            panel.background = element_rect(fill = NA),
            axis.ticks = element_blank()) 
  })
  )
}

shinyApp(ui = ui, server = server)
