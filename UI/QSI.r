library(shiny)
library(shinydashboard)

QSI = function(){
  tabItem(tabName = "qsi",
          titlePanel("Quadratic Spline Interpolation"),
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                fileInput("qsifile", "Choose CSV File",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                ),
                numericInput("qsiestimate", "Value of x", 1, min = 1)
              ),
              mainPanel(
                fluidRow(
                  column(9, verbatimTextOutput("funcStringList")),
                  column(1, tableOutput("QSI")),
                  column(8, textOutput("qsiEstimate"))
                )
              )
            )
          )
  )
}