library(shiny)
library(shinydashboard)
library(rhandsontable)

Simplex = function(){
  tabItem(tabName = "spx",
          titlePanel("Simplex"),
          fluidPage(
            sidebarLayout(
              sidebarPanel = NULL,
              mainPanel = mainPanel(
                fluidRow(
                  rHandsontableOutput("compute")
                ),
                fluidRow(
                  rHandsontableOutput("shippingCost")
                ),
                fluidRow(
                  selectInput("tableau", "Select Iteration", c("Hide"))
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.tableau != 'Hide'",
                    verbatimTextOutput("tables")
                  )
                )
              )
            )
          )
  )
}