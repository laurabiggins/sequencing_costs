library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(rclipboard)

# more info - generates x gb of data which costs x, plus extra files

all_run_info <- readRDS("data/all_run_costs.rds")
available_library_types <- unique(all_run_info$Library_Prep)
# available_run_types <- "" # populate this once library has been selected
# available_read_lengths <- "" # populate once
cost_per_unit <- 1.32
max_lanes <- 100
min_lanes <- 1

acceptable_lane_nos <- paste0("Number of lanes must be between ", min_lanes, " and ", max_lanes)


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
			## dropdowns ----
			fluidRow(
				column(
					width = 6, offset = 3,
					h1("Storage costs for sequencing data"),
					br(),
					actionButton("browser", "browser"),
					verticalLayout(
						shinyWidgets::virtualSelectInput(
							inputId = "library_selector", 
							label   = "Library type", 
							choices = ""
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "run_type_selector", 
							label   = "Run type",
							choices = ""
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "read_length_selector", 
							label   = "Read Length",
							choices = ""
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "paired_end_selector", 
							label   = "Paired End",
							#choices = list(No = FALSE, Yes = TRUE)
							choices = ""
						),
						br(),
						numericInput(
							inputId = "no_of_lanes",
							label   = "Number of lanes",
							value   = 1
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
							textOutput(outputId = "field_fill_msg")
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
	
	## reactive Vals ----
	output_msg <- reactiveVal("Calculation summary here")
	
	calculated_cost <- reactiveVal("")
	
	no_of_lanes <- reactiveVal(NULL)
	
	## outputs ----
	output$field_fill_msg <- renderText(output_msg())
	
	output$output_text <- renderText(output_msg())
	
	output$cost <- renderText(formatted_cost())
	
	observe({
		
		updateVirtualSelect(inputId = "library_type_selector", choices = valid_libraries(),    selected = chosen_lib_type())
		updateVirtualSelect(inputId = "run_type_selector",     choices = valid_runs(),         selected = chosen_run_type())
		updateVirtualSelect(inputId = "read_length_selector",  choices = valid_read_lengths(), selected = chosen_read_length())
		updateVirtualSelect(inputId = "paired_end_selector",   choices = valid_paired(),       selected = chosen_paired_type())
		
	})
	
	
	# These will return NULL if not Truthy
	chosen_lib_type <- reactive({
		if(isTruthy(input$library_selector)) input$library_selector
	})
	
	chosen_run_type <- reactive({
		if(isTruthy(input$run_type_selector)) input$run_type_selector
	})

	chosen_read_length <- reactive({
		 if(isTruthy(input$read_length_selector)) input$read_length_selector
	})
	
	chosen_paired_type <- reactive({
		if(isTruthy(input$paired_end_selector)) input$paired_end_selector
	})
	
	
	valid_libraries <- reactive({
		get_valid_libraries(
			all_run_info  = all_run_info,
			chosen_run    = chosen_run_type(),
			chosen_paired = chosen_paired_type(),
			chosen_read_length = chosen_read_length()
		)	
	})
	
	valid_runs <- reactive({
		get_valid_runs(
			all_run_info  = all_run_info,
			chosen_lib    = chosen_lib_type(), 
			chosen_paired = chosen_paired_type(),
			chosen_read_length = chosen_read_length()
		)	
	})
	
	valid_read_lengths <- reactive({
		get_valid_read_lengths(
			all_run_info  = all_run_info,
			chosen_lib    = chosen_lib_type(), 
			chosen_run    = chosen_run_type(),
			chosen_paired = chosen_paired_type()
		)	
	})
	
	valid_paired <- reactive({
		get_valid_paired(
			all_run_info  = all_run_info,
			chosen_lib    = chosen_lib_type(), 
			chosen_run    = chosen_run_type(),
			chosen_read_length = chosen_read_length()
		)	
	})
	
	
	
	## observeEvents ----
	observeEvent(input$no_of_lanes, {
		if(!isTruthy(input$no_of_lanes)) no_of_lanes(NULL)
		else {
			if(input$no_of_lanes < min_lanes | input$no_of_lanes > max_lanes){
				no_of_lanes(NULL)
			} else {
				no_of_lanes(input$no_of_lanes)
			} 
		}
	})

	 
	### validate selections ----
	# only show the calculate button if we've got valid input for each field
	observe({
		if(isTruthy(input$run_type_selector) & 
			 isTruthy(input$read_length_selector) &
			 isTruthy(input$paired_end_selector) &
			 isTruthy(input$library_selector)  &
			 isTruthy(no_of_lanes())) {
				updateTabsetPanel(session, "calculate_panel", selected = "calculate")
				output_msg("")
				calculated_cost("")
		} else {
			if(!isTruthy(no_of_lanes())) {
				output_msg(acceptable_lane_nos)
			}
			else {
				output_msg("Fill in the fields above")
			}
			calculated_cost("")
			updateTabsetPanel(session, "calculate_panel", selected = "select_msg")
			shinyjs::hide(id = "clip")
		}
	})
	

	### Calculate button ----
	
	observeEvent(input$calculate_btn, {
		
		calculated_cost(cost())
		output_msg(output_text())
		
		shinyjs::show(id = "clip")
	})
	
		
	## Output value and text ----
	
	# We'll round up to the nearest pound
	cost <- reactive({
		
		req(storage_size())
		
		ceiling(storage_size() * input$no_of_lanes * cost_per_unit)
	})
	
	# extract run size
	storage_size <- reactive({
		
		req(input$no_of_lanes, chosen_lib_type(), chosen_run_type(), chosen_paired_type(), chosen_read_length())
		
		filt <- get_filtered_table(
			all_run_info, 
			chosen_lib = chosen_lib_type(), 
			chosen_run = chosen_run_type(), 
			chosen_paired = chosen_paired_type(), 
			chosen_read_length = chosen_read_length()
		)
		
		req(nrow(filt) == 1)
		
		filt |>
			pull(practical_storage_size_gb)
	}) #|>
	#	bindEvent(input$calculate_btn)
	
	# calculated cost can be "" or a number. 
	formatted_cost <- reactive({
		if_else(
			isTruthy(calculated_cost()),
			paste0("£",calculated_cost()),
			""
		)
	})
	
	output_text <- reactive({
		
		lane_text <- format_lane_text(input$no_of_lanes)

		paste0("The cost of storing the data from", lane_text, input$library_selector, " on a ", 
					 input$run_type_selector, " is ", formatted_cost(), ".")
	})
	
	## Clipboard button ---- 
	# to enable easy copying of text
	output$clip <- renderUI({
		rclipButton(
			inputId = "clipbtn",
			label = "",
			clipText = output_text(),
			icon = icon("copy"),
			tooltip = "Copy text",
			placement = "top",
			options = list(delay = list(show = 800, hide = 100), trigger = "hover")
		)
	})
	
}

# Run the application 
shinyApp(ui = ui, server = server)
