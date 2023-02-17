#gdp_app - an example application using the Shiny library
#Eric Howard, University of Washington, Seattle, WA
#Feburary 20, 2014
#Based on the shiny tutorial at http://rstudio.github.io/shiny/tutorial/

library(shiny)
library(ggplot2)      ## ggplot graphics package
library(RColorBrewer) ## for colors
library(ggrepel)      ## to avoid point overlap
library(gapminder)    ## for data
source("helper.R")

## Make some nice colors to use by name
brewer <- brewer.pal(9, "Set1")
red <- brewer[1]    
black <- "black"
nicegray <- "gray45"

pastels <- brewer.pal(9, "Pastel1")
lightred <- pastels[1]

## Focus on 2002 data
gap2002 <- gapminder[gapminder$year==2002,]

## Load some additional data
hivData <- read.csv("http://faculty.washington.edu/cadolph/vis/hivGapminder.csv")
gap2002 <- merge(gap2002, hivData, all.x=TRUE)

mdata2 <- extractdata(lifeExp~gdpPercap + hivPrevalence15to49,
                     data=gap2002, extra=~country+pop, na.rm=TRUE)

ctyLabs2 <- as.character(mdata2$country)
ctyLabs2[mdata2$pop<quantile(mdata2$pop, probs=0.75)] <- ""

popBreaks <- rev(c(3, 10, 30, 100, 300)*1000000)
xbreaks <- c(500, 1000, 2000, 5000, 10000, 20000, 50000)

shinyServer(function(input, output){
  output$mainplot <- renderPlot({ #renderPlot() - passes the plot the user interface
      p <- ggplot(data=mdata2,
                  mapping=aes(x=gdpPercap, y=lifeExp,
                              color=hivPrevalence15to49>input$hivCutoff))
      p <- p + goldenScatterCAtheme +
          geom_smooth(method=MASS::rlm, method.args=list(method="MM"),
                      size=0.5, aes(fill=hivPrevalence15to49>input$hivCutoff)) +
          geom_point(alpha=0.40, aes(size=pop)) +
          geom_text_repel(aes(label=ctyLabs2), size=2,
                          alpha=0.40, segment.size=0.2) +
          scale_x_log10(breaks=xbreaks,
                        labels=xbreaks/1000) +
          scale_color_manual(values=c(black, red), guide="none") +
          scale_fill_manual(values=c(nicegray, lightred), guide="none") +
          scale_size(breaks = popBreaks, labels = popBreaks/1000000) +
          guides(size = guide_legend(nrow = 1)) +
          labs(x="GDP per capita in $k",
               y="Life expectancy in years",
               size="Population in millions") +
          theme(legend.position="bottom") +
          annotate(geom="text", x=20000, y=54,
                   label=paste0("HIV rate\nover ", input$hivCutoff,"%"),
                   col=red, size=4) +
          annotate(geom="text", x=800, y=75,
                   label=paste0("HIV rate\nbelow ", input$hivCutoff,"%"),
                   col=black, size=4)
      
      ##width <- 7
      #ggsave(filename="ggScatterEx10_2.pdf",
      ##       plot = p,
      ##       width=width,
      ##       height=width/1.618)
      p
  })
})
