#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(graphics)
library(base)
library(plotly)
library(lubridate)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyverse)
library(gridExtra)

aglp1 <- read_xlsx("aGLP1 - Cas Erreur médicamenteuses_Surdosages.xlsx", 
                   sheet = "Complet")
source("/Users/rossetantonin/Desktop/data_projetpro/shiny_prep_projpro.R")

# UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Effets Sécondaires Indésirables", 
    titleWidth = 500
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Incidents par catégorie et sexe", tabName = "sexplot", icon = icon("chart-bar")),
      menuItem("Age Scatter Plot avec la Moyenne", tabName = "age_dist_plot", icon = icon("user")),
      menuItem("Distribution Plots", tabName = "dist_plots", icon = icon("chart-pie")),
      menuItem("Percentage of Gravity Plots", tabName = "gravity_plots", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
                      tabItems(
                        tabItem("sexplot",
                                fluidRow(
                                  box(
                                    title = "Incidents par catégorie et sexe",
                                    plotOutput("sexplot", height = 600), width = 10
                                  ),
                                  
                                  box(valueBox(value = "L'âge moyen suggère que la personne typique dans cet ensemble de données a environ 63,17 ans.",subtitle = NULL, color = "orange",icon = NULL, width = 200),
                                      valueBox(value ="L'écart type représente la dispersion ou la répartition des âges dans l'ensemble de données.",subtitle = NULL, color = "green",icon = NULL, width = 200),
                                      valueBox(value = "Un écart type plus élevé signifie que les âges sont plus dispersés, indiquant une plage d'âge plus large dans l'ensemble de données.",subtitle = NULL, color = "blue",icon = NULL, width = 200),
                                      valueBox(value = "Dans ce cas, l'âge moyen est quelque peu plus élevé par rapport à l'âge moyen dans une population générale, et la plage d'âge dans cet ensemble de données est plus large que la moyenne.",subtitle = NULL, color = "yellow",icon =NULL, width = 200))
                                ) 
                        ),
                        
                        tabItem("age_dist_plot",
                                fluidRow(
                                  box(
                                    title = "Age Plots",
                                    selectInput("age_selector", "Select a Category:",
                                                c("Par Moyenne", "Par Sexe"),
                                                selected = "Par Moyenne"),
                                    plotOutput("ageplot", height = 600), width = 20
                                  )
                                )
              
                        ),
                        
                        tabItem("dist_plots",
                                fluidRow( 
                                  box(
                                    title = "Distribution Pourcentages",
                                    selectInput("category_selector", "Select a Category:",
                                                c("Cas", "Déclaration", "Notification"),
                                                selected = "Cas"),
                                    plotOutput("pie", height = 600), width = 20 
                                  )
                                )
                        ),
                        
                        tabItem("gravity_plots",
                                fluidRow(
                                  box(
                                    title = "Gravity Plots",
                                    selectInput("gravity_selector", "Select a Category:",
                                                c("Age", "Sex"),
                                                selected = "Age"),
                                    plotOutput("grave", height = 600), width = 20 
                                  )
                                )
                        )
                      )
                    ),
                    # Center all boxes
                    tags$style(HTML("
                      .shiny-box {
                        margin: 0 auto;
                        float: none;
                      }
                    "))
)

# Server
server <- function(input, output) {
  
  # Define your plots as reactive functions
  output$sexplot <- renderPlot({
    ggplot(df_count, aes(x = Cas, y = n, fill = Sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Incidents par catégorie et sexe",
           x = "Catégorie",
           y = "Nombre d'incidents",
           fill = "Sexe") +
      theme_minimal() + 
      scale_fill_manual(values = c("lightblue", "pink"))
  })
  
  output$ageplot <- renderPlot({
    if (input$age_selector=="Par Moyenne"){
      ggplot(df_age, aes(x = Patient, y = Age, color = Sex)) +
        geom_point() +
        scale_x_continuous(breaks = seq(1, 18, 1))+
        labs(title = "Age Scatter Plot with Mean",
             x = "Patient",
             y = "Age",
             color = "Sex") +
        geom_abline(intercept = average_age, slope = 0, color = "red") +
        scale_color_manual(values = c("M" = "blue", "F" = "orange"))
    } else {
      age_distribution_male <- ggplot(df_age[df_age$Sex == "M", ], aes(x = Age)) +
        geom_density(color = "blue", fill = "lightblue") +
        labs(title = "Age Distribution for Male Patients",
             x = "Age",
             y = "Density")
      
      age_distribution_female <- ggplot(df_age[df_age$Sex == "F", ], aes(x = Age)) +
        geom_density(color = "red", fill = "pink") +
        labs(title = "Age Distribution for Female Patients",
             x = "Age",
             y = "Density")
      
      # Create a grid to place the graphs side by side
      grid.arrange(age_distribution_male, age_distribution_female, ncol = 2)
    }
    
    
  })

  output$pie <- renderPlot({
    if (input$category_selector=="Cas"){
      ggplot(cas_counts, aes(x = "", y = percentage, fill = `Typ Cas`)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Types of Cases Distribution",
             fill = "Cases",
             x = NULL,
             y = NULL) +
        theme_minimal() +
        theme(legend.position = "right")
    } else if (input$category_selector=="Déclaration"){
      ggplot(dec_counts, aes(x = "", y = percentage, fill = `Typ Décl`)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Types of Declaration Distribution",
             fill = "Declaration Methods",
             x = NULL,
             y = NULL) +
        theme_minimal() +
        theme(legend.position = "right")
    } else {
      ggplot(not_counts, aes(x = "", y = percentage, fill = `Typ Notif`)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Types of Notifications Distribution",
             fill = "Notifications",
             x = NULL,
             y = NULL) +
        theme_minimal() +
        theme(legend.position = "right")
    }
    
    
  })

  output$grave <- renderPlot({
    if (input$gravity_selector=="Age"){
      ggplot(percentage_data, aes(x = as.factor(Age), y = Percentage, fill = Grave)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(Sex ~ .) +
        labs(title = "Percentage of Gravity by Age, Colored by Sex",
             x = "Age",
             y = "Percentage") +
        scale_fill_manual(values = c("N" = "skyblue", "O" = "pink"),
                          labels = c("N" = "No", "O" = "Yes")) +
        geom_text(aes(label = sprintf("%.1f%%", Percentage)),
                  position = position_stack(vjust = 0.5), size = 3) +
        theme_minimal()
    } else {
      ggplot(data_grave, aes(x = "", fill = Grave)) +
        geom_bar(position = "fill", width = 1) +
        coord_polar(theta = "y") +
        facet_grid(Sex ~ .) +
        labs(title = "Percentage of Gravity, Colored by Sex",
             fill = "Outcome",
             x = NULL,
             y = NULL) +
        scale_fill_manual(values = c("N" = "skyblue", "O" = "pink"),
                          labels = c("N" = "No", "O" = "Yes")) +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank())
    }
    
    
  })

  
  # Add more renderPlot functions for additional plots

  # Display average age and standard deviation in value boxes
  output$avg_age_valuebox <- renderValueBox({
    avg_age <- mean(df_age$Age)
    valueBox(
      sprintf("Âge moyen: %.2f", avg_age),
      icon = icon("user"),
      color = "blue"
    )
  })
  
  output$std_dev_age_valuebox <- renderValueBox({
    std_dev_age <- sd(df_age$Age)
    valueBox(
      sprintf("Écart-type: %.2f", std_dev_age),
      icon = icon("user"),
      color = "orange"
    )
  })
}

# Run the Shiny app
shinyApp(ui, server)
