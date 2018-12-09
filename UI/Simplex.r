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
                )
              )
            )
          )
  )
}