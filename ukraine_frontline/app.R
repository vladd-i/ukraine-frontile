# Load relevant libraries

library(shiny)
library(tidyverse)
library(ggforce)
library(readr)
library(shinythemes)

# Read in data

households <- read.csv("data/households.csv")
households_satisfaction <- read.csv("data/households_satisfaction.csv")

# Use objects from other R Script

source("model.R")
source("graphics.R")

# Set default style and scale for plots

ggplot2::theme_set(ggplot2::theme_minimal(base_size = 13))

# Define UI for application, setting a custom theme and defining a layout with
# fluidRow() and column() functions

ui <- navbarPage(theme = shinytheme("cosmo"),
                 
                 # Short project title
                 
                 "Life on the Frontline",
                 
                 tabPanel("About", 
                          column(8,
                                 
                          # Full project name
                          
                          titlePanel("Life on the Frontline: People's Access to 
                                     Services Amidst an Armed Conflict in Eastern 
                                     Ukraine"),
                          
                          h3("Project Background and Motivations"),
                          p("For the last 6 years, few topics have been affecting 
                          the everyday life of Ukrainians as much as the military 
                          conflict in the Donbass Region, in the east of the country.
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
                          organizations and think tanks that collect data in eastern 
                          Ukraine. For this project, I decided to use a 14,000-household
                          dataset which I obtained from the", 
                          a("REACH Initiative", 
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
                 
                 tabPanel("Demographics",
                          sidebarLayout(
                          sidebarPanel(
                          # Setting up a drop down menu to select which
                          # demographic indicator of households to display
                              
                          selectInput("indicator_hh", 
                                      "Select a demographic indicator:", 
                                      choices = c(
                                          "Sex" = "b1_hohh_sex",
                                          "Age" = "b2_hohh_age",
                                          "Education Level" = "b4_hohh_education_level",
                                          "Monthly Income (Ukrainian hryvnias)" = "b31_hohh_income",
                                          "Number of Children" = "children_sum"
                                      ),
                                      selected = "Age")
                          ),
                          
                          mainPanel(
                              
                          # Show a graph that visualizes household
                          # demographic indicators by a geographic unit, 
                          # allowing for comparison across different location
                         
                          plotOutput("plot_hh", height = 500)
                          ))
                          ),
                 
                 tabPanel("Trust in Government",
                          fluidRow(
                          column(5,
                          titlePanel("Regression of Trust in Local Government"),
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
                          
                          # Create a group of checkboxes so that viewer can 
                          # choose which posteriors to display on the graphic
                          
                          checkboxGroupInput(
                              "posteriors",
                              label = "Select posteriors to display:",
                              choices = list("Social services" = "social",
                                             "Administrative services" = "admin",
                                             "Health services" = "health",
                                             "Pubic transportation" = "transport",
                                             "Food markets" = "food_markets",
                                             "Financial services" = "financial",
                                             "Non-food markets" = "non_food_markets"),
                              
                              # Default to selecting these 4 posteriors, since
                              # they are pretty spaced out on the graphic and 
                              # create an informative first impression of the range
                              # of the values
                              
                              selected = c("health", "food_markets",
                                           "financial", "non_food_markets"))
                          ),

                          # Show the plot with posteriors from the model
                          
                          mainPanel(
                          plotOutput(outputId = "posteriors_plot")
                          ))),
                 
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
                          
                          # Display a plot that illustrates what percentage
                          # of households used & reported their trust in certain
                          # categories of services to back up the explanation of 
                          # the choices I made to build a model
                          
                          column(5,
                          plotOutput(outputId = "services_proportion_plot")
                          )))
                 )



# Define server logic

server <- function(input, output) {
    
    # Pull a regression summary table with the coefficient values from a 
    # model.R script
    
    output$regression_table <- 
        render_gt(
            expr = gt_tbl
        )
    
    # Pull a graphic with the levels of use of services by households from a 
    # graphics.R script
    
    output$services_proportion_plot <- 
        renderPlot(
            expr = services_proportion_plot
        )
    
    # Create a plot of posteriors using the tibble from graphics.R script
    
    output$posteriors_plot <-
        renderPlot({
            posteriors_tibble %>%
                
                # Select only the posteriors that the user chose in the checkboxes
                
                filter(parameter %in% c(input$posteriors)) %>%
                ggplot(aes(value, fill = parameter)) +
                
                # Create a set of corresponding histograms for each predictor:
                
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               alpha = 0.7,
                               bins = 515,
                               position = "identity") +
                
                # Overlay with density curves for aesthetics:
                
                geom_density(aes(y = after_stat(count/sum(count))),
                             alpha = .8) +
    
                # Set custom colors for aesthetics
                
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
    
    # Create a plot to visualize demographics of households in the dataset
    # by geographic unit (thus setting x coordinate to hromada)
    
    # output$plot_hh <- renderPlot({
    #     p = households %>%
    #         
    #         # Omit NAs in hromada names
    #         
    #         filter(!is.na(hromada)) %>%
    #         ggplot(aes(x = hromada,
    #                    y = .data[[input$indicator_hh]])) +
    #         geom_jitter(size = 1, alpha = .5) +
    #         labs(title = "Demographics of Households by Hromada (geographic unit)",
    #              subtitle = "Each dot represents 1 head of household of 14,000+ in the survey",
    #              x = "Hromada (geographic unit)",
    #              caption = "Source: AGORA Initiative") +
    #         
    #         # Rotate, move, and resize axes text to avoid overlapping and 
    #         # improve aesthetics
    #         
    #         theme(axis.text.y = element_text(size = 12),
    #               axis.text.x = element_text(size = 12,
    #                                          angle = 90, 
    #                                          hjust=1, 
    #                                          vjust = 0.5))
    #     
    #         # "Sex" = "b1_hohh_sex",
    #         # "Age" = "b2_hohh_age",
    #         # "Education Level" = "b4_hohh_education_level",
    #         # "Monthly Income (Ukrainian hryvnias)" = "b31_hohh_income",
    #         # "Number of Children" = "children_sum"
    #     
    #     # Add y axis labels depending on the inputted demographic indicator and
    #     # tweak other elements to make graph aesthetically pleasing
    #     
    #     if (input$indicator_hh == "b1_hohh_sex")
    #         p <- p + ylab("Sex")
    #     if (input$indicator_hh == "b2_hohh_age")
    #         p <- p + ylab("Age")
    #     if (input$indicator_hh == "b4_hohh_education_level")
    #         p <- p + ylab("Highest Education Level Achieved")
    #     if (input$indicator_hh == "b31_hohh_income")
    #         p <- p + ylab("Monthly Income (Ukrainian hryvnias)")
    #     if (input$indicator_hh == "children_sum")
    #         p <- p + ylab("Number of Children")
    #     
    #     p
    # })
    
    output$plot_hh <- renderPlot({
        p = households %>%
            
            # Omit NAs in hromada names
            # Also omit a household that didn't report the age to avoid 
            # an error message
            
            filter(!is.na(hromada), !is.na(b2_hohh_age)) %>%
            
            # Set the y aesthetic later depending on the indicator selected to 
            # ensure proper formatting
            
            ggplot(aes(x = hromada)) +
            labs(title = "Demographics of Households by Hromada (geographic unit)",
                 subtitle = "Each dot represents 1 head of household of 14,000+ in the survey",
                 x = "Hromada (geographic unit)",
                 caption = "Source: AGORA Initiative") +
            
            # Rotate, move, and resize axes text to avoid overlapping and 
            # improve aesthetics
            
            theme(axis.text.y = element_text(size = 12),
                  axis.text.x = element_text(size = 12,
                                             angle = 90, 
                                             hjust = 1, 
                                             vjust = 0.5))
        
        # Add y axis labels and tweak other elements depending on the inputted 
        # demographic indicator to make graph aesthetically pleasing
        
        if (input$indicator_hh == "b1_hohh_sex")
            p <- p + 
                geom_jitter(aes(y = b1_hohh_sex), 
                            size = 1, 
                            alpha = .5) +
                ylab("Sex")
        
        if (input$indicator_hh == "b2_hohh_age")
            p <- p + 
                geom_jitter(aes(y = b2_hohh_age), 
                            size = 1, 
                            alpha = .5,
                            color = "#e86913") +
                ylab("Age")
        
        if (input$indicator_hh == "b4_hohh_education_level"){
            
            # Set correct order of education levels
            
            level_order <- c("none", 
                            "preschool",
                            "primary_education",
                            "basic_secondary",
                            "complete_secondary",
                            "vocational",
                            "basic_higher",
                            "complete_higher",
                            "postgraduate")
            p <- p + 
                geom_jitter(aes(y = factor(b4_hohh_education_level,
                                           level = level_order)), 
                            size = .5, 
                            alpha = .2,
                            color = "#1e6ec9") +
                
                # Rename and reorder y ticks to make them more aesthetically pleasing
                
                scale_y_discrete(breaks = level_order,
                                 labels = c("None", 
                                           "Preschool",
                                           "Primary",
                                           "Basic Secondary",
                                           "Complete Secondary",
                                           "Vocational",
                                           "Basic Higher",
                                           "Complete Higher",
                                           "Postgraduate")) +
                ylab("Highest Education Level Achieved")
        }
            
        if (input$indicator_hh == "b31_hohh_income"){
            
            # Set correct order of income levels
            
            level_order <- c("0",
                             "less_2000",
                             "less_4000",
                             "2001_4000",
                             "4001_8000",
                             "8001_12000",
                             "more_12001")
            
            p <- p + 
                geom_jitter(aes(y = factor(b31_hohh_income,
                                           level = level_order)), 
                            size = .5, 
                            alpha = .2,
                            color = "#28820c") +
                
                # Rename and reorder y ticks to make them more aesthetically pleasing
                
                scale_y_discrete(breaks = level_order,
                                 labels = c("0", 
                                            "<2000",
                                            "<4000",
                                            "2,000-4,000",
                                            "4,000-8,000",
                                            "8,000-12,000",
                                            ">12,000")) +
            
                ylab("Monthly Income (Ukrainian hryvnias)")
        }
            
        if (input$indicator_hh == "children_sum")
            p <- p + 
                geom_jitter(aes(y = as.factor(children_sum)), 
                            size = .5, 
                            alpha = .2,
                            color = "#9a53bb") +
                ylab("Number of Children")
        
        p
    })
}


# Run the application 
shinyApp(ui, server)