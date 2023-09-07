## Analysis code for Schwalb et al. 2023
## Distributed under CC BY 4.0
## RScript 10: Self-clearance.R

# Packages ==========
library(shiny)       # Build interactive applications
library(data.table)  # Faster than data.frame
library(tidyverse)   # To use tidyverse
library(deSolve)     # Solvers for ordinary differential equations

# 1. Self-clear model ==========
sc_model <- function(times, state, parms) {
  # Define states
  I <- state["I"]
  S <- state["S"]
  
  # Extract parameters based on time intervals
  gamma <- parms[findInterval(floor(times), parms$time), "gamma"]
  
  # Define differential equations
  dI <- - gamma * I
  dS <- gamma * I
  res <- list(c(dI, dS))
  return(res)
}

# 2. Model prep ==========
times <- seq(from = 0, to = 10, by = 1)

state <- c(I = 1000, S = 0)

parameters <- data.table(time = c(0, 1, 2), gamma = c(1.12, 1.12, 0.0)) # original (Horton et al.)
parameters <- data.table(time = c(0, 1, 2), gamma = c(1.83, 1.83, 0.0)) # revised (Horton et al.)
parameters <- data.table(time = c(0, 1, 2), gamma = c(0.23, 0.23, 0.23)) # constant

ode_results <- ode(y = state, times = times, func = sc_model, parms = parameters, method = "lsoda")

results <- as.data.frame(ode_results) %>%
  mutate(pS = S / 1000)

# 3. Plot ==========
ggplot(results, aes(x = time, y = pS)) +
  geom_line() +
  geom_segment(aes(x = 0, xend = 10, y = 0.9, yend = 0.9), colour = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +  
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time (years)", y = "Population self-cleared (%)") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

# 4. Shiny app ==========
ui <- fluidPage(
  titlePanel("Self-clearance Model"),
  fluidRow(
    column(width = 9, plotOutput("self_clear_plot"))),
  fluidRow(
    column(width = 3, sliderInput("gamma_a", "1st year infected", min = 0, max = 2, value = 1, step = 0.01)),
    column(width = 3, sliderInput("gamma_b", "2nd year infected", min = 0, max = 2, value = 1, step = 0.01)),
    column(width = 3, sliderInput("gamma_c", "3rd to 10th year infected", min = 0, max = 2, value = 0, step = 0.01))
  )
)

server <- function(input, output) {

  times <- seq(from = 0, to = 10, by = 1)
  
  state <- c(I = 1000, S = 0)
  
  reactive_parameters <- reactive({
    data.table(time = c(0, 1, 2), gamma = c(input$gamma_a, input$gamma_b, input$gamma_c))
  })
  
  ode_results <- reactive({
    ode(y = state, times = times, func = sc_model, parms = reactive_parameters(), method = "lsoda")
  })
  
  max_pS <- reactive({
    results <- as.data.frame(ode_results())
    max(results$S / 1000)
  })
  
  output$self_clear_plot <- renderPlot({
    results <- as.data.frame(ode_results()) %>%
      mutate(pS = S / 1000)
    
    max_pS_value <- max_pS()
    
    ggplot(results, aes(x = time, y = pS)) +
      geom_line() +
      geom_segment(aes(x = 0, xend = 10, y = 0.9, yend = 0.9), colour = "red", linetype = 2) +
      scale_x_continuous(breaks = seq(0, 10, 1)) +  
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "Time (years)", y = "Population self-cleared (%)") +
      theme_minimal() + 
      theme(panel.grid.minor = element_blank()) +
      geom_text(aes(x = 10, y = max_pS_value, label = paste("Max:", round(max_pS_value, 2))), hjust = -0.1, vjust = 0.5, color = "blue")
  })
}

shinyApp(ui, server)
