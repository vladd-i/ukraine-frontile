library(shiny)
library(tidyverse)
library(ggforce)
library(readr)
library(shinythemes)

# Read in data

households <- read.csv("data/households.csv")
householsd_satisfaction <- read.csv("data/households_satisfaction.csv")

# Use objects from other R Script

source("model.R")
source("graphics.R")

# Set default style and scale for plots

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))

# Define UI for application

ui <- navbarPage(theme = shinytheme("cosmo"),
                 "Life on the Frontline",
                 
                 tabPanel("About", 
                          column(8,
                          titlePanel("Life on the Frontline: People's Access to 
                                     Services Amidst an Armed Conflict in Eastern 
                                     Ukraine"),
                          
                          h3("Project Background and Motivations"),
                          p("For the last 6 years, few topics have been affecting 
                          the everyday life of Ukrainians as much as the military 
                          conflict in the Donbass Region, in the East of the country.
                          My own family has been personally impacted, as my 
                          step-father served in the warzone for 3 years."),
                          p("But most importantly, for over half a decade, the 
                          military conflict continues to shape the everyday lives
                          of civilians. In this project, I decided to focus on 
                          people who live in the area along the frontline, 
                          analyzing their access to basic services amidst war and
                          their reliance on local governments for support and 
                          protection."),
                          p("I reached out to a number of international humanitarian 
                          organizations and think tanks that collect data in Eastern 
                          Ukraine. Eventually, for this project I decided to use 
                          datasets obtained from", 
                          a("the REACH Initiative", 
                          href = "https://www.reach-initiative.org/where-we-work/ukraine/")),
                          p("My code and original datasets are available on my",
                          a("GitHub", href = "https://github.com/vladd-i/")),
                          
                          h3("About Me"),
                          p("My name is Vlad, I'm an international student in the 
                          Class of 2023 from Ukraine studying Applied Math & Economics. 
                          You can reach me at vlad_ivanchuk@college.harvard.edu or 
                          connect with me on", 
                          a("LinkedIn.", 
                          href = "https://www.linkedin.com/in/vladyslav-ivanchuk/"))
                          )),
                 
                 tabPanel("Model",
                          fluidRow(
                          column(5,
                          h2("Regression of Trust in Local Government"),
                          p("I decided to build a Bayesian multiple linear 
                          regression model using trust in local government as a 
                          response variable, and levels of satisfaction with basic 
                          services as predictor variables. The purpose of creating 
                          this model was to determine the strength of predictors, 
                          i.e. to analyze people’s satisfaction with which of 
                          the basic services has the greatest impact on their 
                          trust in local government to take care of its citizens."),
                          p("The regression analysis illustrates that satisfaction
                          with certain groups of services has a strong effect on
                          people's trust in local government to take care of its 
                          citizens."),
                          p("For example, an increase in reported satisfaction 
                          with social support services by one point is predicted
                          to increase the household's trust in local government
                          by .15 points, on average."),
                          p("In contrast, satisfaction with financial services or
                          non-food markets seeems not to have a significant
                          relationship with the household's trust in local government,
                          since the 95% confidence interval includes a 0, and so 
                          the effect is uncertain.")
                          ),
                          
                          column(7,
                          gt_output(outputId = "regression_table")
                          )),
                          
                          hr(),
                          
                          sidebarLayout(
                          sidebarPanel(
                          h3("Select Posteriors to Display:"),
                          checkboxGroupInput(
                              "posteriors",
                              label = NULL,
                              choices = list("Social services" = "social",
                                             "Administrative services" = "admin",
                                             "Health services" = "health",
                                             "Pubic transportation" = "transport",
                                             "Food markets" = "food_markets",
                                             "Financial services" = "financial",
                                             "Non-food markets" = "non_food_markets"),
                              selected = c("health", "food_markets",
                                           "financial", "non_food_markets"))
                          
                          # radioButtons("graphic", h3("Radio buttons"),
                          #              choices = list("Density Curve" = 1,
                          #                             "Histogram" = 2),
                          #              selected = 1)
                          ),
                          
                          
                          mainPanel(
                          plotOutput(outputId = "posteriors_plot")
                          ))),
                 
                 tabPanel("Data",
                          fluidPage(
                              
                              # Application title
                              titlePanel("Household Data by Hromada (geographic unit)"),
                              
                              # setting up drop downs
                              selectInput("indicator_hh", "Indicator", 
                                          choices = names(households)),
                              plotOutput("plot_hh")
                              
                          )),
                 
                 tabPanel("Discussion",
                          titlePanel("Model Choices"),
                          
                          fluidRow(
                          column(6,
                          p("One problem I faced while constructing my multiple 
                          linear regression model is that all predictor variables 
                          had various levels of missing data. For example, out of
                          households that reported their trust in local government,
                          only 43% were also asked about their satisfaction with 
                          health services, 20% — with administrative services, and 
                          only 9% — with social support services (as shown in 
                          the graphic)."),
                          
                          p("Therefore, I faced the choice of either eliminating 
                          the observations (households) with missing data, or 
                          imputing values. However, after taking a closer look at
                          the survey, I realized that households were only asked 
                          to report their satisfaction with certain categories of 
                          services if they have used them in the last 3 months. 
                          For example, only those households that have sought some 
                          kind of medical care were asked to rate their satisfaction 
                          with health services. Therefore, the cases of missing 
                          satisfaction level data were not random and, instead, 
                          dependent on other variables — whether the household had
                          used this category of services."),
                          
                          p("Because of this, eliminating observations with missing
                          data was not a viable option, as it would would lead to
                          a bias in the model. For example, if I excluded all 
                          households that haven’t reported their levels of 
                          satisfaction with social services, the regression would
                          that have individuals benefiting from social support: 
                          pensioners, people with disabilities, internally displaced 
                          people, and other. This is definitely not representative 
                          of the entire population."),
                          
                          p("Instead, I decided to impute the missing values, so 
                          that all households could be used in regression analysis. 
                          The rationale was as follows: for the households which 
                          haven’t utilized, say, administrative services in the 
                          last 3 months, their trust in the local government was 
                          not going to be influenced  by this category of services. 
                          Therefore, it is reasonable to impute “Indifferent” as 
                          their level of satisfaction with this category. This 
                          translates to a numeric value of 0 to be used in the 
                          linear regression analysis (as opposed to positive/negative
                          values for households which used the service category 
                          and reported various satisfaction/dissatisfaction levels")
                          ),
                          
                          column(5,
                          plotOutput(outputId = "services_proportion_plot")
                          )))
                 )



# Define server logic

server <- function(input, output) {
    
    output$regression_table <- 
        render_gt(
            expr = gt_tbl
        )
    
    output$services_proportion_plot <- 
        renderPlot(
            expr = services_proportion_plot
        )
    
    output$posteriors_plot <-
        renderPlot({
            posteriors_tibble %>%
                filter(parameter %in% c(input$posteriors)) %>%
                ggplot(aes(value, fill = parameter)) +
                
                # Alternative appearance — set of histograms:
                
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               alpha = 0.7,
                               bins = 515,
                               position = "identity") +
                
                # Density curves create a less crowded graphic than histograms:
                
                geom_density(aes(y = after_stat(count/sum(count))),
                             alpha = .8) +
    
                scale_fill_manual(breaks = c("social", "admin", "health", 
                                             "transport", "food_markets", 
                                             "financial", "non_food_markets"),
                                  values = c("#383a3c", "#ffa600", "#ff0039",
                                               "#9a53bb", "#3fb719",
                                               "#ff7518", "#2780e3"),
                                  labels = c("social", "administrative", "health",
                                             "public transport", "food markets", 
                                             "financial", "non-food markets"),
                                  name = "Satisfaction with\na service category") +
                
                labs(title = "Posterior Probability Distributions",
                     subtitle = "Relative strengths of predictors of households'\ntrust in local government",
                     x = "Coefficient value",
                     y = "Probability") +
                scale_y_continuous(labels = scales::percent_format())
        })
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               column = geom_col(),
               jitter = geom_jitter()
        )
    })
    
    output$plot_hh <- renderPlot({
        ggplot(households, aes(hromada, .data[[input$indicator_hh]])) +
            geom_jitter()
    })
}


# Run the application 
shinyApp(ui, server)


