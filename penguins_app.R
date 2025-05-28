# Load packages
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(plotly)
library(janitor)
library(palmerpenguins)
library(lubridate)

penguins <- as.data.frame(penguins_raw)

# Data Cleaning
penguins <- penguins %>%
  rename(
    study_name = studyName,
    delta_15_n = "Delta 15 N (o/oo)",
    delta_13_c = "Delta 13 C (o/oo)",
    culmen_length_mm = "Culmen Length (mm)",
    culmen_depth_mm = "Culmen Depth (mm)"
  ) %>%
  clean_names()

penguins <- penguins %>%
  mutate(
    species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
      TRUE ~ species
    ),
    year = year(date_egg)
  ) %>%
  select(-study_name, -region, -comments) %>%
  drop_na()

# Change data types
penguins$clutch_completion <- as.factor(penguins$clutch_completion)
penguins$sample_number <- as.integer(penguins$sample_number)

print(names(penguins))

count_days_per_year <- penguins %>%
  group_by(year) %>%
  summarise(count = n_distinct(date_egg))
print(count_days_per_year)

head(penguins)

################################################################################
# Navbar UI setup
ui <- navbarPage("Palmer Penguins Data Exploration",
                 # Data Exploration Tab
                 tabPanel("Data Exploration",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("species", "Select a Species:", 
                                          choices = c("All",
                                                      unique(penguins$species))),
                              selectInput("island", "Select Island:", 
                                          choices = c("All",
                                                      unique(penguins$island))),
                              selectInput("sex", "Select Gender:", 
                                          choices = c("All",
                                                      unique(penguins$sex))),
                              sliderInput("yearRange", "Select Year Range:",
                                          min = 2007,
                                          max = 2009,
                                          value = c(2007, 2009),
                                          step = 1,
                                          sep = "",
                                          pre = "",
                                          post = ""
                              ),
                              checkboxInput("showRegression",
                                            "Show Regression Line",
                                            value = FALSE),
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Culmen Dimensions",
                                         plotlyOutput("culmenPlot")),
                                tabPanel("Body Mass Distribution",
                                         plotlyOutput("massPlot")),
                                tabPanel("Flipper Length",
                                         plotlyOutput("flipperPlot")),
                                tabPanel("Delta Isotope Analysis",
                                         plotlyOutput("isotopePlot"))
                              )
                            )
                          )
                 ),
                 # ANOVA Analysis Tab
                 tabPanel("ANOVA Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("responseVar", "Choose a response variable:",
                                          choices = c("culmen_length_mm",
                                                      "culmen_depth_mm",
                                                      "flipper_length_mm",
                                                      "body_mass_g")),
                              selectInput("factorVar", "Choose a factor variable:",
                                          choices = c("species", "island", "sex")),
                              actionButton("goButton", "Run ANOVA")
                            ),
                            mainPanel(
                              verbatimTextOutput("anovaResult"),
                              plotOutput("diagnosticPlot")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  filtered_penguins <- reactive({
    data <- penguins %>%
      filter(
        (species == input$species | input$species == "All") &
          (year >= input$yearRange[1] & year <= input$yearRange[2]) &
          (island == input$island | input$island == "All") &
          (sex == input$sex | input$sex == "All")
      )
    data
  })
  
  # Keep colors consistent
  species_colors <- c("Adelie" = "steelblue",
                      "Chinstrap" = "lightgreen",
                      "Gentoo" = "orange")
  
  observeEvent(input$update, {})
  
  blank_plot <- function(message) {
    ggplot() + 
      annotate("text",
               x = 0.5,
               y = 0.5,
               label = message,
               vjust = 0.5,
               hjust = 0.5,
               size = 6) +
      theme_classic()
  }
  
  # Plot for Culmen Dimensions
  output$culmenPlot <- renderPlotly({
    if (nrow(filtered_penguins()) == 0) {
      ggplotly(blank_plot("No data available with current filters"))
    } else {
      p <- ggplot(filtered_penguins(), aes(x = culmen_length_mm,
                                       y = culmen_depth_mm,
                                       color = species)) +
        geom_point() +
        scale_color_manual(values = species_colors) +
        labs(title = "Culmen Length vs Depth by Species",
             x = "Culmen Length (mm)",
             y = "Culmen Depth (mm)") +
        theme_classic()
      
      # Add regression line
      if (input$showRegression) {
        p <- p + geom_smooth(method = "lm", formula = 'y ~ x', se = TRUE)
      }
      
      ggplotly(p)
    }
  })
  
  # Filtered massPlot
  output$massPlot <- renderPlotly({
    if (nrow(filtered_penguins()) == 0) {
      ggplotly(blank_plot("No data available with current filters"))
    } else {
      p <- ggplot(filtered_penguins(), aes(x = body_mass_g, fill = species)) +
        geom_histogram(binwidth = 100, color = "black") +
        scale_fill_manual(values = species_colors) +
        facet_wrap(~species) +
        labs(title = "Body Mass Distribution by Species",
             x = "Body Mass (g)",
             y = "Frequency") +
        theme_classic()
      ggplotly(p)
    }
  })
  
  # Filtered FlipperPlot
  output$flipperPlot <- renderPlotly({
    if (nrow(filtered_penguins()) == 0) {
      ggplotly(blank_plot("No data available with current filters"))
    } else {
      p <- ggplot(filtered_penguins(), aes(x = flipper_length_mm, fill = species)) +
        geom_histogram(binwidth = 5, color = "black") +
        scale_fill_manual(values = species_colors) +
        labs(title = "Flipper Length Distribution",
             x = "Flipper Length (mm)",
             y = "Frequency") +
        theme_classic() +
        facet_wrap(~species)
      ggplotly(p)
    }
  })
  
  output$isotopePlot <- renderPlotly({
    if (nrow(filtered_penguins()) == 0) {
      ggplotly(blank_plot("No data available with current filters"))
    } else {
      data <- filtered_penguins()
      p <- ggplot(data, aes(x = delta_13_c, y = delta_15_n, color = species)) +
        geom_point() +
        scale_color_manual(values = species_colors) +
        labs(title = "Isotopic Composition: Delta 15N vs Delta 13C",
             subtitle = if(input$showRegression) {
               fit <- lm(delta_15_n ~ delta_13_c, data = data)
               r_squared <- summary(fit)$r.squared
               paste("Linear Model Fit R² =", round(r_squared, 3))
             } else "",
             x = "Delta 15 C (‰)", 
             y = "Delta 15 N (‰)") +
        theme_classic()
      
      if (input$showRegression) {
        p <- p + geom_smooth(method = "lm", formula = 'y ~ x', se = TRUE)
      }
      
      ggplotly(p)
    }
  })
  
  # ANOVA Analysis
  observeEvent(input$goButton, {
    req(input$responseVar, input$factorVar)
    data <- filtered_penguins() %>%
      select(all_of(c(input$responseVar, input$factorVar))) %>%
      drop_na()
    
    anovaModel <- aov(reformulate(input$factorVar,
                                  response = input$responseVar), data = data)
    
    output$anovaResult <- renderPrint({
      summary(anovaModel)
    })
    
    # Extended diagnostic plots
    output$diagnosticPlot <- renderPlot({
      par(mfrow = c(2, 2))
      
      # Plot 1: Residuals vs Fitted
      plot(resid(anovaModel) ~ fitted(anovaModel),
           main="Residuals vs Fitted",
           xlab="Fitted values",
           ylab="Residuals")
      abline(h = 0, col = "red")
      
      # Plot 2: Normal Q-Q
      qqnorm(resid(anovaModel),
             main = "Normal Q-Q")
      qqline(resid(anovaModel), col = "steelblue")
      
      # Plot 3: Scale-Location (Spread vs. Level)
      plot(sqrt(abs(resid(anovaModel))) ~ fitted(anovaModel),
           main="Scale-Location",
           xlab="Fitted values",
           ylab="Sqrt(|Residuals|)")
      
      # Plot 4: Residuals vs Leverage
      plot(hatvalues(anovaModel),
           main="Residuals vs Leverage",
           xlab="Leverage",
           ylab="Residuals")
      abline(h = 0, col = "red")
      
      # Add Cook's distance lines to identify influential cases
      cooks.distance <- cooks.distance(anovaModel)
      abline(v = which.max(cooks.distance), col = "blue", lwd = 2)
    })
  })
}

shinyApp(ui = ui, server = server)
