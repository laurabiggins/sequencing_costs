library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(magrittr)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)
library(rclipboard)

all_run_info <- readRDS("data/all_runs.rds")
available_library_types <- unique(all_run_info$Library.Type)
available_run_types <- ""
size_to_cost <- 2.5

valid_no_of_lanes <- function(input_no){
	if(!isTruthy(input_no)) return (FALSE)
	max_no_of_lanes <- 100
	min_no_of_lanes <- 1
	if_else(input_no <= max_no_of_lanes & input_no >= min_no_of_lanes, TRUE, FALSE)
}

box_wrapper <- function(box_id, box_title=NULL, panel_tags=NULL, box_width = 12, collapsible = FALSE, collapsed=FALSE) {
	shinydashboard::box(
		id = box_id,
		title = box_title,
		width = box_width, 
		class = "plotbox",
		collapsible = collapsible,
		collapsed = collapsed,
		panel_tags)
}


# UI -----
ui <- fluidPage(
	rclipboard::rclipboardSetup(),
	shinyjs::useShinyjs(),
	shinyFeedback::useShinyFeedback(),
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
	),
	tags$head(tags$title("Sequencing costs")),
	dashboardPage(
		dashboardHeader(disable = TRUE),
		dashboardSidebar(disable = TRUE),
		dashboardBody(
			br(),
			br(),
			fluidRow(
				column(
					width = 6, offset = 3,
					h1("Storage and running costs for sequencing data"),
					br(),
					#actionButton("browser", "browser"),
					box_wrapper(
						box_id = "filter_options",
						panel_tags = tagList(
								shinyWidgets::virtualSelectInput(
									inputId  = "library_selector", 
									label    = "Library type", 
									autoSelectFirstOption = FALSE,
									choices  = available_library_types
								),
							br(),
							shinyWidgets::virtualSelectInput(
								inputId  = "run_type_selector", 
								label    = "Run type",
								autoSelectFirstOption = FALSE,
								choices  = available_run_types
							),
							br(),
							numericInput(
								inputId = "no_of_lanes",
								label = "Number of lanes",
								value = 0
							)
						)
					)
				)
			),
			br(),
			fluidRow(
				column(
					width = 6, offset = 3,
					tabsetPanel(
						id = "calculate_panel",
						type = "hidden",
						tabPanelBody(
							"select_msg",
							h3("Fill in all options above")
						),
						tabPanelBody(
							"calculate",
							fluidRow(
								column(
									width = 4,
									actionButton(inputId = "calculate_btn", label = "Calculate cost")
								),
								column(
									width = 2,
									offset = 1,
									textOutput(outputId = "cost")
								)
							),
							br(),
							br(),
							br(),
							conditionalPanel(
								condition = "input.calculate_btn > 0",
								fluidRow(
									column(width = 11, textOutput(outputId = "output_text")),
									column(width = 1, uiOutput("clip"))
								)
							)
						)
					)
				)
			)
		)
	)
)


server <- function(input, output, session) {
	
	observeEvent(input$browser, browser())
	
	output_msg <- reactiveVal("Calculation summary here")
	calculated_cost <- reactiveVal("")
	
	output$output_text <- renderText(output_msg())
	
	output$cost <- renderText(calculated_cost())
	
	validSelections <- reactive({
		
		if_else(
			(isTruthy(input$run_type_selector) & 
			isTruthy(input$library_selector)  &
			valid_no_of_lanes(input$no_of_lanes)),
			TRUE,
			FALSE
		)
	})
	
	observeEvent(validSelections(), {
		if(validSelections() == TRUE) {
			updateTabsetPanel(session, "calculate_panel", selected = "calculate")
		} else {
			updateTabsetPanel(session, "calculate_panel", selected = "select_msg")
			output_msg("")
			calculated_cost("")
			shinyjs::hide(id = "clip")
		}
	})
	
	# There actually seem to be the same run types for all the library types so this
	# isn't strictly necessary, but we might as well have this for future proofing. 
	observeEvent(input$library_selector, {
		if(isTruthy(input$library_selector)) {
			run_types <- all_run_info |>
				filter(Library.Type == input$library_selector) |>
				pull(Run.Type)
			
			updateVirtualSelect(inputId = "run_type_selector", choices = run_types)
		}
	})
	
	calculation_summary <- reactive({
		
		if(input$no_of_lanes == 1){
			lane_text <- " lane of "
		} else {
			lane_text <- " lanes of "
		}
		
		paste0("The cost of running ", input$no_of_lanes, lane_text, input$library_selector,
					 " on a ", input$run_type_selector, " and storing the data is ", calculated_cost(), ".")
	})
	
	
	observeEvent(input$calculate_btn, {
		
		size <- all_run_info %>%
			filter(Library.Type == input$library_selector) |>
			filter(Run.Type == input$run_type_selector) %>%
			pull(Practical.Size..GB.)
		
		cost <- size * input$no_of_lanes * size_to_cost
		
		calculated_cost(paste0("£", cost))
		
		output_msg(calculation_summary())
		
		shinyjs::show(id = "clip")
	})
	
	# Add clipboard buttons
	output$clip <- renderUI({
		rclipButton(
			inputId = "clipbtn",
			label = "",
			clipText = output_msg(),
			icon = icon("copy"),
			tooltip = "Copy text",
			placement = "top",
			options = list(delay = list(show = 800, hide = 100), trigger = "hover")
		)
	})
	
}

# Run the application 
shinyApp(ui = ui, server = server)
