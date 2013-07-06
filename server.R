shinyServer(function(input, output, session) {
  df_default <- read.table(file = 'example_data.tsv',
                           header = TRUE, sep = "\t")
  df_global <- df_default
  
  ##################################################
  # Visual components
  
  # Data tab
  
  # Plot tab
  
  # Dropdown select
  output$plotx_select <- renderUI({
    input$input_csv_file
    dropdown_items <- c("None", names(getFullData()))
    selectedVar <- dropdown_items[3]
    selectInput("plotx_select", "", choices = dropdown_items,
                selected = selectedVar)
  })
  output$ploty_select <- renderUI({
    input$input_csv_file
    dropdown_items <- c("None", names(getFullData()))
    selectedVar <- dropdown_items[2]
    selectInput("ploty_select","",choices=dropdown_items,selected=selectedVar)
  })
  
  # Use checkbox as radio buttons
  observe({
    updateCheckboxInput(session, "plotx_radio2", value = !input$plotx_radio1)
  })
  observe({
    updateCheckboxInput(session, "plotx_radio1", value = !input$plotx_radio2)
  })  
  
  # swap checkboxes according to user input
  observe({
    input$plotx_define
    updateCheckboxInput(session, "plotx_radio2", value = TRUE)
    updateCheckboxInput(session, "plotx_radio1", value = FALSE)
  })
  observe({
    input$plotx_select
    updateCheckboxInput(session, "plotx_radio1", value = TRUE)
    updateCheckboxInput(session, "plotx_radio2", value = FALSE)
  })
  
  # Model tab
  output$response_select <- renderUI({
    dropdown_items <- c("None", names(getFullData()))
    selected_var <- dropdown_items[2]
    selectInput("response", "", choices = dropdown_items, selected = selected_var)
  })
  
  output$covariates_select <- renderUI({
    dropdown_items <- names(getFullData())
    selected_var <- dropdown_items[2:4]
    selectInput("covariates", "", choices=dropdown_items,
                selected = selected_var, multiple = TRUE)
  })
  
  # Define main tabs
  tabList <- list(
    list(textOutput("data_size"),
         tableOutput("sample_data"),
         title = "Data",
         value = "1"),
    list(htmlOutput("chart_xy"),
         htmlOutput("chart_y"),
         htmlOutput("chart_x"),
         title = "Plot",
         value = "2"),
    list(title = "+", value = "+")
  )
  output$main_tabs <- renderUI({
    tabItems <- lapply(tabList, function(x) { do.call(tabPanel, x) })
    tabItems$id <- "active_tab"
    do.call(tabsetPanel, tabItems)
  })
  
  addNamedTab <- function(pos, title, value = NULL) {
    k <- length(tabList)
    if (is.null(value)) {
      value <- as.character(k)
    }
    pos <- min(pos, k + 1)
    if (pos < 0) {
      pos <- max(1, k + 2 + pos)
    }
    new_tab <- list(title = title, value = value)
    if (pos <= k) {
      tabList <<- c(tabList[1:pos], tabList[pos:k])
    }
    tabList[[pos]] <<- new_tab
    tabItems <- lapply(tabList, function(x) { do.call(tabPanel, x) })
    tabItems$id <- "active_tab"
    tabItems$selected <- as.character(k)
    output$main_tabs <- renderUI({do.call(tabsetPanel, tabItems)})
  }
  
  # Add new tab to main tabs
  observe({
    if ("active_tab" %in% names(input) && !is.null(input$active_tab) && input$active_tab == "+") {
      isolate({
        k <- length(tabList)
        addNamedTab(-2, paste("Model", k - 2, sep = ""), as.character(k))
      })
    }
  })
  
  ##################################################
  # Data components
  
  # Read input data
  observe({
    inFile <- input$input_csv_file
    if (is.null(inFile) || is.null(inFile$datapath) || inFile$datapath == "") {
    } else {
      df <- try(read.csv(inFile$datapath))
      if (!inherits(df, "try-error")) {
        df_global <<- df
      }
    }
  })
  
  getFullData <- reactive({
    input$plot_submit
    df_global
  })
  
  # Data sample
  output$sample_data <- renderTable({
    df <- getFullData()
    output$data_size <- renderText(paste(nrow(df), "rows."))
    head(df, n = 10)
  })
  
  ##################################################
  # Plot components
  
  # Get values of a column from the data frame. If the column does not exist,
  # try to see whether it is a function of existing columns and create a new
  # column if possible.
  getVarByName <- function(df, var_name) {
    print(paste("getVarByName", var_name))
    if (var_name %in% c("", "None")) {
      return(NULL)
    }
    df_var_names <- names(df)
    if (var_name %in% df_var_names) {
      return(df[, var_name])
    } else {
      var_name <- gsub(" ", "", var_name)
      if (var_name %in% df_var_names) {
        return(df[, var_name])
      } else {
        x <- try(eval(parse(text = paste("with(df,", var_name, ")"))))
        if (!inherits(x, "try-error")) {
          df_global[, var_name] <<- x
          col_names <- names(df_global)
          # HERE IS THE PROBLEM
          # Update all the select inputs to add the new column name that is
          # just created.
          sapply(c("plotx_select", "ploty_select", "response"),
                 function(inputId) {
                   updateSelectInput(session, inputId,
                                     choices = c("None", col_names),
                                     selected = input[[inputId]])
                 })
          updateSelectInput(session, "covariates", choices = col_names,
                            selected = input$covariates)
          return(x)
        }
      }
    }
    return(NULL)
  }
  
  getPlotVariables <- reactive({
    xvar_name <- ifelse(input$plotx_radio1, input$plotx_select, input$plotx_define)
    yvar_name <- input$ploty_select
    print(paste("getPlotVariables: X var: ", xvar_name, "Y var: ", yvar_name))
    df <- getFullData()
    return(list(x = getVarByName(df, xvar_name),
                xvar_name = xvar_name,
                y = getVarByName(df, yvar_name),
                yvar_name = yvar_name))
  })
  
  plotSingleVariable <- function(x, title) {
    print("plotSingleVariable")
    k <- length(unique(x))
    n <- length(x)
    options = list(width = 800,
                   height = 400,
                   hAxis = paste("{title: '", title, "'}", sep = ""),
                   vAxis = "{title:'Count'}",
                   legend = "none")
    if (k > 20) {
      hist_info <- hist(x, max(10, min(50, n / 10)), plot = FALSE)
      hist_info <- data.frame(mids = as.character(hist_info$mids), counts = hist_info$counts)
      gvisColumnChart(hist_info, "mids", "counts", options = options)
    } else {
      hist_info <- as.data.frame(table(x))
      gvisColumnChart(hist_info, "x", "Freq", options = options)
    }
  }
  
  plotPairVariables <- function(x, y, xvar_name, yvar_name) {
    options = list(width = 800,
                   height = 400,
                   pointSize = 1,
                   hAxis = paste("{title: '", xvar_name, "'}", sep = ""),
                   vAxis = paste("{title: '", yvar_name, "'}", sep = ""),
                   legend = "none")
    gvisScatterChart(data.frame(x = x, y = y), options = options)
  }
  
  # switch to plot tab when plot button is clicked
  observe({
    if (input$plot_submit == 0) {
    } else {
      isolate({
        updateTabsetPanel(session, "active_tab", "2")
      })
    }
  })
  
  output$chart_x <- renderGvis({
    if (input$plot_submit == 0) {
      return(NULL)
    } else {
      isolate({
        data <- getPlotVariables()
        if (!is.null(data$x)) {
          print("output$chart_x <- renderGvis")
          plotSingleVariable(data$x, data$xvar_name)
        }
      })
    }
  })
  
  output$chart_y <- renderGvis({
    if (input$plot_submit == 0) {
      return(NULL)
    } else {
      isolate({
        data <- getPlotVariables()
        if (!is.null(data$y)) {
          plotSingleVariable(data$y, data$yvar_name)
        }
      })
    }
  })
  
  output$chart_xy <- renderGvis({
    if (input$plot_submit == 0) {
      return(NULL)
    } else {
      isolate({
        data <- getPlotVariables()
        if (!is.null(data$x) && !is.null(data$y)) {
          plotPairVariables(data$x, data$y, data$xvar_name, data$yvar_name)
        }
      })
    }
  })
  
  ##################################################
  # Model components
  
  # Show dynamic icon when model is running, and add a new tab to main panel if
  # necessary.
  progressObserver <- observe({  
    if (input$model_submit == 0) {
    } else {
      isolate({
        session$sendCustomMessage("model_running", list(spin = "yes"))
        j <- as.numeric(input$active_tab)
        if (j < 3) {
          k <- length(tabList)
          addNamedTab(-2, paste("Model", k - 2, sep = ""), as.character(k))
        }
      })
    }
  })
  progressObserver$setPriority(10)
  
  # Run model fitting
  observe({
    if (input$model_submit == 0) {
    } else {
      isolate({
        y <- setdiff(input$response, "None")
        x <- setdiff(input$covariates, c(y, "None"))
        if (length(y) == 1 && length(x) > 0) {
          model_result <- rulefit_plot(x, y)
          k <- as.numeric(input$active_tab)
          if (k < 3) {
            k <- length(tabList) - 1
          }
          m <- length(tabList[[k]])
          imgs <- list()
          if (length(model_result$plots) > 0) {
            for (i in length(model_result$plots):1) {
              plot_id <- paste("model_plot", k, m + i - 1, sep = "_")
              imgs[[length(imgs) + 1]] <- tags$img(src = file.path('plots', basename(model_result$plots[[i]]$filename)),
                                                   height = paste(model_result$plots[[i]]$height, 'px', sep = ""))
              imgs[[length(imgs) + 1]] <- tags$hr()
            }
          }
          imgs[[length(imgs) + 1]] <- tags$hr()
          tabList[[k]] <<- c(imgs, tabList[[k]])
          tabItems <- lapply(tabList, function(x) { do.call(tabPanel, x) })
          tabItems$id <- "active_tab"
          tabItems$selected <- as.character(k)
          session$sendCustomMessage("model_running", list(spin = "no"))
          output$main_tabs <- renderUI({do.call(tabsetPanel, tabItems)})
        }
      })
    }
  })
  
  # Run rulefit model and make plots
  rulefit_plot <- function(x, y) {
    model_result <- list(plots = list(), error = NULL)
    
    if (length(y) == 1 && length(x) > 0) {
      data <- getFullData()
      y_var <- y
      data <- data[which(!is.na(data[, y_var])), ]
      y <- data[, y_var]
      model <- try(rulefit(
        data[, x, drop = FALSE], y,
        rfmode = "regress"))
      if (inherits(model, "try-error")) {
        cond <- attr(model, "condition")
        model_result$error <- paste("Error in ", cond$call, ": ",
                                    cond$message, sep = "")
        return(model_result)
      }
      outfile <- tempfile(fileext='.png')
      png(outfile, width = 800, height = 600)
      par(mar = c(5, 10, 4, 2))
      vi <- varimp(horiz = TRUE)
      dev.off()
      model_result$plots[[length(model_result$plots) + 1]] <- list(filename = outfile, height = 600)
      nplot <- 
        outfile <- tempfile(fileext='.png')
      png(outfile, width = 800, height = 500)
      singleplot(vi$ord[1:min(9, length(vi$ord))])
      dev.off()
      model_result$plots[[length(model_result$plots) + 1]] <- list(filename = outfile, height = 500)
    }
    return(model_result)
  }
})

