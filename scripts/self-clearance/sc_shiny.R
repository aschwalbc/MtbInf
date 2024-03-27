## Analysis code for Schwalb et al. 2024
## Distributed under CC BY 4.0
## RScript: SC_shiny.R

# Packages ==========
library(shiny)
library(data.table)
library(tidyverse)
library(deSolve)

# 1. Self-clear model ==========
sc_model <- function(times, state, parms) {
  I <- state["I"]
  S <- state["S"]
  
  gamma <- parms[findInterval(floor(times), parms$time), "gamma"]
  
  # Define differential equations
  dI <- - gamma * I
  dS <- gamma * I
  res <- list(c(dI, dS))
  return(res)
}

# 2. Model prep ==========
times <- seq(from = 0, to = 35, by = 1)

state <- c(I = 1000, S = 0)

parameters <- data.table(time = c(0, 1, 2, 10), gamma = c(1.650, 0.873, 0.134, 0.121))

ode_results <- ode(y = state, times = times, func = sc_model, parms = parameters, method = "lsoda")

results <- as.data.frame(ode_results) %>%
  mutate(pS = round((S / 1000) * 100, 1))

# 3. Plot ==========
ggplot(results, aes(x = time, y = pS)) +
  geom_line() +
  geom_segment(aes(x = 0, xend = 35, y = 0.9, yend = 0.9), colour = "grey", linetype = 2) +
  geom_segment(aes(x = 0, xend = 35, y = 0.95, yend = 0.95), colour = "grey", linetype = 2) +
  scale_x_continuous(breaks = seq(0, 35, 2)) +  
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
    column(width = 2, sliderInput("gamma_a", "1st year infected", min = 0, max = 2, value = 1.660, step = 0.01)),
    column(width = 2, sliderInput("gamma_b", "2nd year infected", min = 0, max = 2, value = 0.875, step = 0.01)),
    column(width = 2, sliderInput("gamma_c", "3rd to 9th year infected", min = 0, max = 2, value = 0.135, step = 0.01)),
    column(width = 2, sliderInput("gamma_d", "+10th year infected", min = 0, max = 2, value = 0.121, step = 0.01))
  )
)

server <- function(input, output) {
  
  times <- seq(from = 0, to = 25, by = 1)
  
  state <- c(I = 1000, S = 0)
  
  reactive_parameters <- reactive({
    data.table(time = c(0, 1, 2, 10), gamma = c(input$gamma_a, input$gamma_b, input$gamma_c, input$gamma_d))
  })
  
  ode_results <- reactive({
    ode(y = state, times = times, func = sc_model, parms = reactive_parameters(), method = "lsoda")
  })
  
  output$self_clear_plot <- renderPlot({
    results <- as.data.frame(ode_results()) %>%
      mutate(pS = S / 1000)
    
    max_pS_value <- max(results$pS)
    
    specific_values <- results %>%
      filter(time %in% c(1, 2, 10, 20)) %>%
      mutate(label = sprintf("%.1f%%", pS * 100)) 
    
    ggplot(results, aes(x = time, y = pS)) +
      geom_segment(aes(x = 0, xend = 25, y = 0.9, yend = 0.9), colour = "grey", linetype = 2) +
      geom_segment(aes(x = 0, xend = 25, y = 0.95, yend = 0.95), colour = "grey", linetype = 2) +
      geom_line() +
      scale_x_continuous(breaks = seq(0, 25, 2)) +
      scale_y_continuous(labels = scales::percent) +
      coord_cartesian(ylim = c(0, max_pS_value * 1.1)) +
      labs(x = "Time (years)", y = "Population self-cleared (%)") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      geom_text(data = specific_values, aes(x = time, y = pS, label = label), vjust = 0.5, size = 6, color = "blue")
  })
}

shinyApp(ui, server)
