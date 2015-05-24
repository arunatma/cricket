library(shiny)
library(rCharts)
shinyUI(pageWithSidebar(
    headerPanel("Table", windowTitle = "Table"),
    sidebarPanel(
        radioButtons("radInType", "Date or Match:", selected="Match",
            c("Date" = "Date", "Match" = "Match")),

        conditionalPanel(condition="input.radInType=='Match'",
            sliderInput("matchNum", "Match Number", min = 1,
                    max = 56, value =1, step = 1,
                    animate = animationOptions(loop = FALSE, interval = 3000))
        ),
        
        conditionalPanel(condition="input.radInType=='Date'",
            dateInput("dateNum", "Date", value = "2015-04-08", min = "2015-04-08",
                    max = "2015-05-17")
        ),
        
        radioButtons("sortType", "Sort by", c("TeamNames" = "TeamNames", 
                                            "TeamPoints" = "TeamPoints"))
    ),
    mainPanel(
        wellPanel(tableOutput('dtable')),
        wellPanel(showOutput("thePlot", "nvd3"))
    )
))    

