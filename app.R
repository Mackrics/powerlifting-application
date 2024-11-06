source("packages.R")
source("helpers.R")

ui <- fluidPage(

  theme = bslib::bs_theme(
    base_font    = "IBM Plex Mono",
    code_font    = "IBM Plex Mono",
    heading_font = "IBM Plex Mono",
    bg           = ctp_mocha[["crust"]], 
    fg           = ctp_mocha[["text"]], 
    primary      = ctp_mocha[["blue"]],
    secondary    = ctp_mocha[["blue"]],
    success      = ctp_mocha[["blue"]],
    info         = ctp_mocha[["blue"]],
    warning      = ctp_mocha[["blue"]],
    danger       = ctp_mocha[["blue"]],
  ),
  titlePanel("Powerlifting application"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", 
                     "Date range",
		     min   = "2000-01-01",
		     max   = as.character(Sys.Date()),
		     start = as.character(Sys.Date() - 90),
                     end   = as.character(Sys.Date())
    ),
    actionButton("refresh", "Refresh data"),
    actionButton("save", "Save data"),
    selectizeInput("exercise", "Select exercises", multiple = TRUE, 
		   choices = get_exercises(),
		   selected = c("squat", "bench press", "deadlift")),
    selectizeInput("group", "Select muscle group", multiple = TRUE, 
		   choices = get_groups(),
		   selected = get_groups())
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Volume", plotOutput("volume", height = "auto")),
      tabPanel("Total Volume", plotOutput("total_volume", height = "auto")),
      tabPanel("Estimated orm", plotOutput("e1rm", height = "auto")),
      tabPanel("Tabulation of max weight", gt_output("maxtab")),
      tabPanel("Meso details", gt_output("meso"))
    )

  )
)
)

server <- function(input, output, session) {

  old_data <- read_feather("./data/lifting-data.arrow")

  data <- eventReactive(input$refresh, {
    refresh_data()
  }, ignoreNULL = FALSE)

  observeEvent(input$save, {
        data() |>
          write_feather("./data/lifting-data.arrow")
  })

  output$total_volume <- renderPlot({
      if (input$refresh == 0) {
        plot_total_volume(
	  old_data,
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]]
	)
      } else {
        plot_total_volume(
	  data(),
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]]
	)
      }
  }, height = function() {
      session$clientData$output_volume_width/2
  })

  output$volume <- renderPlot({
      if (input$refresh == 0) {
        plot_volume(
	  old_data,
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  groups     = input$group
	)
      } else {
        plot_volume(
	  data(),
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  groups     = input$group
	)
      }
  }, height = function() {
      session$clientData$output_volume_width/2
  })

  output$e1rm <- renderPlot({
      if (input$refresh == 0) {
        plot_e1rm(
	  old_data,
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
	)
      } else {
        plot_e1rm(
	  data(),
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
	)
      }
  }, height = function() {
      session$clientData$output_e1rm_width/2
  })

  output$maxtab <- render_gt({
      if (input$refresh == 0) {
        max_table(
	  old_data,
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
	)
      } else {
        max_table(
	  data(),
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
      }
  })

  output$meso <- render_gt({
      if (input$refresh == 0) {
        show_meso(
	  old_data,
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
	)
      } else {
        show_meso(
	  data(),
	  start_date = input$dates[[1]],
	  end_date   = input$dates[[2]],
	  exercise   = input$exercise
	)
      }
  })
}

shinyApp(ui, server)
