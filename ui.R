# Author: Yuan Yuan (danioyuan@gmail.com)

suppressPackageStartupMessages(library(googleVis))
library(rulefit)
library(shiny)

addResourcePath('plots', tempdir())

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-mini", `data-toggle` = "popover",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      tags$i(class="icon-question-sign")
    )
  )
}

# Define header panel not using the default headerPanel function
header_panel <- tagList(
  # Add message handling to switch image in need
  tags$head(
    tags$style(type="text/css", ".leftcolumn {text-align: right;}"),
    
    #    tags$script(src = 'js/highcharts.js'),
    
    tags$script(HTML('
                     Shiny.addCustomMessageHandler("model_running",
                     function(message) {
                     var headerImg = document.getElementById("header_image_div");
                     var spinImg = document.getElementById("spin_image_div");
                     if (message.spin == "yes") {
                     headerImg.style.display = "none";
                     spinImg.style.display = "";
                     } else {
                     headerImg.style.display = "";
                     spinImg.style.display = "none";
                     }
                     }
                     );
                     '))),
  
  tags$head(tags$title("Knox")),
  div(id="header_image_div", style="padding: 10px 0px;",
      tags$img(src = "images/knox.png", height = '42px')
  ),
  div(id="spin_image_div", style="padding: 10px 0px;display: none;",
      tags$img(src = "images/knox.gif", height = '42px')
  )
    )

sidebar_panel <- sidebarPanel(
  tabsetPanel(
    id = 'sidebar_tab',
    tabPanel("Data",
             h5("Choose CSV File:"),
             fileInput('input_csv_file', '',
                       accept=c('text/csv', 'text/comma-separated-values,text/plain'))
    ),
    tabPanel("Plot",
             h5("X-axis:"),
             #             helpPopup("Looking for help???", ""),
             checkboxInput("plotx_radio1",
                           "Choose from exisiting columns:", TRUE),
             uiOutput("plotx_select"),
             checkboxInput("plotx_radio2", "Define your own:", FALSE),
             textInput("plotx_define", "", ""),
             h5("Y-axis"),
             uiOutput("ploty_select"),
             tags$h4(''),
             actionButton("plot_submit", "plot")
    ),
    tabPanel("Model",
             h4("Choose model variables:"),
             h5("Response (Y):"),
             uiOutput("response_select"),
             h5("Predictors (X):"),
             uiOutput("covariates_select"),
             tags$hr(),
             HTML("<table>"),
             HTML("<tr><td class='leftcolumn'>Mode:&nbsp;</td><td>"),
             selectInput("rulefit_mode", "",
                         c("Auto" = "auto",
                           "Classification" = "class",
                           "Regression" = "regress")),
             HTML("</td></tr><tr><td class='leftcolumn'>sparse =&nbsp;</td><td>"),
             textInput("rulefit_sparse", "", value = "1"),
             HTML("</td></tr></table>"),
             h3(""),
             actionButton("model_submit", "Run")
    )
  )
)

main_panel <- mainPanel(
  uiOutput("main_tabs")
)

# Define UI
shinyUI(
  pageWithSidebar(header_panel, sidebar_panel, main_panel)
)

