#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(primer.data)
library(janitor)
library(ggplot2)
library(gcookbook)
library(tidycensus)

    covid_data <- readRDS("covid-and-domestic-dispuits/cleandata/covid-data.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "The Effects of COVID-19 on Domestic Violence in the United States",
    tabPanel("Model",
             titlePanel("The Model"),
             h3("Domestic violence as a function of COVID-19 cases and race"),
             p("In this project, I model the effects of COVID-19 and race as a demographic 
              variable on domestic violence cases in the United States. Specifically,
              focusing on Baltimore County, Maryland, Hamilton County, Ohio, King County, Washington,
              Los Angeles County, California, Maricopa County, Arizona, Multnomah County, Oregon and 
              Orange County, Florida."),
             h4("Domestic violence as a function of COVID-19 cases and race:"),
             p("We fit this using COVID-19 data from USA Facts, as well as police data 
            from the Police Data Initiative and census data. We will also 
             be using a new package, broom.mixed, which allows us to tidy 
              regression data for plotting.")),
    tabPanel("Discussion", 
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
                an explanation of why you made them")),
    tabPanel("Covid Data Visualization",
             titlePanel("Covid-19 Data"),
             mainPanel(plotOutput("data_plot"))),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"), 
             p("In this project I will look to conduct analysis on the effects of COVID-19 on domestic violence.
                Due to the global pandemic, families and households have been forced into extended isolation. With this increased 
                isolation, I want to analyze the effects of COVID-19 on domestic violence. I will collect data on police calls for service 
                from 9 large metropolitan cities or areas: Baltimore, Maryland; Chandler, Arizona; Cincinnati, Ohio; 
                Los Angeles, California; New Orleans, Louisiana; Portland, Oregon; Orlando, Florida; Seattle, 
                Washington; and Tucson, Arizona. All of these cities except Phoenix participate in the Police Data Initiative. Of the 32 
                police agencies participating, these cities had up-to-date
                incidence data and provided adequate documentation to identify calls about domestic-violence-related incidents. I will also
                use data from USA Facts which contains data regarding US Coronavirus Cases & Deaths by State. This database has been 
                tracking COVID-19 data daily by state and county. I have data up to 3/11/21."), 
             p("Link to my github repo: https://github.com/atsotadze/milestone-6"),
             h3("About Me"),
             p("My name is Alex Tsotadze and I study Economics.
                 You can reach me at atsotadze@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$data_plot <- renderPlot(
        covid_data %>%
            ggplot(aes(x = `County Name`,
                       y = total_covid_cases)) +
            geom_point() +
            labs(title = "Total Covid Cases by County between 2020-01-22 and 2021-03-11",
                 subtitle = "The highest amount of cases were found in Los Angeles County",
                 x = " ",
                 y = "Total Covid Cases",
                 caption = "Source: USA Today COVID-19 Data") +
            scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
            coord_flip() +
            theme_classic()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
