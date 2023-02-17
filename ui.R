#gdp_app - an example application using the Shiny library
#Eric Howard, University of Washington, Seattle, WA
#Feburary 20, 2014
#Based on the shiny tutorial at http://rstudio.github.io/shiny/tutorial/

library(shiny)
library(gapminder)
library(simcf)

## Focus on 2002 data
gap2002 <- gapminder[gapminder$year==2002,]

## Load some additional data
hivData <- read.csv("http://faculty.washington.edu/cadolph/vis/hivGapminder.csv")
gap2002 <- merge(gap2002, hivData, all.x=TRUE)

mdata2 <- extractdata(lifeExp~gdpPercap + hivPrevalence15to49,
                     data=gap2002, extra=~country+pop, na.rm=TRUE)

minHiv <- floor(min(mdata2$hivPrevalence15to49))
maxHiv <- ceiling(max(mdata2$hivPrevalence15to49))

#Define UI for application that produces a line plot based on GDP
shinyUI(fluidPage(
  
  #Application title
  headerPanel("Life expectancy by GDP & HIV infection rate"), 
  
  #Sidebar with a list to select the country that you are interested in
  sidebarPanel(
    #Select the country you are intersted in from a list
    #selectInput("country", "Select a country:",
    #            choices = country.names),
    #Create a slider so that the y-axis can be rescaled dynamically
    sliderInput("hivCutoff", "Choose a threshold to separate high and low HIV cases",
                min = minHiv,
                max = maxHiv,
                value = 2,
                step = 0.5)
    ),
  
  #Show the lines plots of GDP by year, with the selected country being highligthed
  mainPanel(
    plotOutput("mainplot"))
))
