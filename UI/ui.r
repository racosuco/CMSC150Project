library(shiny)
library(shinydashboard)

source("UI/PolyReg.r")
source("UI/QSI.r")

ui <- dashboardPage(
  dashboardHeader(title = "CMSC 150 Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "pr"),
      menuItem("Quadratic Spline Interpolation", tabName = "qsi"),
      menuItem("Simplex", tabName = "spx")
    )
  ),
  dashboardBody(
    tabItems(
      PolyReg(),
      QSI()
    )
  )
)


