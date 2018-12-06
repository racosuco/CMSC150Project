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
                  column(1, rHandsontableOutput("Simplex"))
                ),
                fluidRow(
                  column(1, rHandsontableOutput("Simplex2"))
                )
              )
            )
          )
  )
}