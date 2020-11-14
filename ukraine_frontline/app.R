library(shiny)
library(tidyverse)
library(ggforce)
library(readr)
library(shinythemes)

# Read in data

msna <- read.csv("data/MSNA.csv")
hh <- read.csv("data/HH_R2.csv")
ki <- read.csv("data/KI_R2.csv")

# Define UI for application

ui <- navbarPage(theme = shinytheme("cosmo"),
                 "Life on the Frontline",
                 tabPanel("About", 
                          titlePanel("Life on the Frontline: Impact of the Armed Conflict in Eastern Ukraine on People’s Access to Services"),
                          h3("Project Background and Motivations"),
                          p("For the last 5 years, few topics have been affecting the everyday life of Ukrainians as much as the military 
               conflict in the Donbass Region, in the East of the country. My own family has been personally impacted, as my step-father
               served in the warzone for 3 years."),
                          p("In this project, I decided to analyze the impact this military conflict has had on the people living directly around the frontline 
               I reached out to a number of international humanitarian organizations and think tanks that collect data in Eastern Ukraine."),
                          p("Currently, I am using datasets obtained from", a("the REACH Initiative", href = "https://www.reach-initiative.org/where-we-work/ukraine/"), "and", a("UN Office for the Coordination of Humanitarian Affairs", href = "https://www.unocha.org/ukraine"), ". 
               They contain data on 5000+ household interviews focusing on people's access to basic services in the 100 government controlled cities and villages along the line of contact seperating the government controlled and non-government controlled areas of the Donetsk and Luhansk oblasts of Ukraine.
               I'm planning to analyze this data to determine what the main needs of people in the region are, and how they are distributed geographically. I'm hoping to draw meaningful insights by creating visualizations 
               and maybe even to build a prediction model using machine learning/neural networks."),
                          p("Code and materials:", a("GitHub", href = "https://github.com/vladd-i/gov50-final-project.git"),
                            h3("About Me"),
                            p("My name is Vlad, I'm an international student from Ukraine studying Applied Math & Economics. 
             You can reach me at vlad_ivanchuk@college.harvard.edu or connect with me on", a("LinkedIn", href = "https://www.linkedin.com/in/vladyslav-ivanchuk/"),".")
                          )),
                 tabPanel("Data",
                          
                          fluidPage(
                              
                              # Application title
                              titlePanel("Multi-Sectoral Needs Assessment Data"),
                              
                              # setting up drop downs 
                              selectInput("x", "X variable", choices = names(msna)),
                              selectInput("y", "Y variable", choices = names(msna)),
                              selectInput("geom", "geom", c("point", "smooth", "column", "jitter")),
                              plotOutput("plot_msna")
                              
                          ),
                          
                          fluidPage(
                              
                              # Application title
                              titlePanel("Household Data by Hromada (geographic unit)"),
                              
                              # setting up drop downs
                              selectInput("indicator_hh", "Indicator", choices = names(hh)),
                              plotOutput("plot_hh")
                              
                          )),
                 tabPanel("Discussion",
                          titlePanel("Discussion Title"),
                          p("Tour of the modeling choices you made and an explanation of 
               why you made them")))



# Define server logic
server <- function(input, output) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               smooth = geom_smooth(se = TRUE, na.rm = TRUE),
               column = geom_col(),
               jitter = geom_jitter()
        )
    })
    
    output$plot_msna <- renderPlot({
        ggplot(msna, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    })
    
    output$plot_hh <- renderPlot({
        ggplot(hh, aes(hromada, .data[[input$indicator_hh]])) +
            geom_jitter()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)










