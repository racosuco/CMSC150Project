library(shiny)
library(shinydashboard)

PolyReg = function(){
  tabItem(tabName = "pr",
    titlePanel("Polynomial Regression"),
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          fileInput("prfile", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          numericInput("prdegree", "Degree", 2, min = 2)
        ),
        mainPanel(
          fluidRow(
            column(8, textOutput("funcString")),
            column(2, tableOutput("PolyReg"))
          )
        )
      )
    )
  )
}