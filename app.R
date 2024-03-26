library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(rclipboard)
library(kableExtra)

# more info - generates x gb of data which costs x, plus extra files

all_run_info <- readRDS("data/all_run_costs.rds")
available_library_types <- unique(all_run_info$Library_Prep)
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
					width = 10, offset = 1,
					h1("Storage costs for sequencing data"),
					br(),
					#actionButton("browser", "browser"),
					#verticalLayout(
					fluidRow(
						column(width = 6,
							shinyWidgets::virtualSelectInput(
								inputId = "library_selector", 
								label   = "Library type", 
								choices = ""
							)
						),
						column(width = 6,
							shinyWidgets::virtualSelectInput(
								inputId = "run_type_selector", 
								label   = "Run type",
								choices = ""
							)
						)
					),
					fluidRow(
						column(width = 6,
							shinyWidgets::virtualSelectInput(
								inputId = "read_length_selector", 
								label   = "Read Length",
								choices = ""
							)
						),
						column(width = 6,
							shinyWidgets::virtualSelectInput(
								inputId = "paired_end_selector", 
								label   = "Paired End",
								#choices = list(No = FALSE, Yes = TRUE)
								choices = ""
							)
						)
					),
					fluidRow(
						column(width = 6,
							numericInput(
								inputId = "no_of_lanes",
								label   = "Number of lanes",
								value   = 1
							)
						)
					),
					fluidRow(
						column(
							width = 12,
							#DT::dataTableOutput(outputId = "fake_table")
						)
					)
				)
			),
			br(),
			fluidRow(
				column(
					width = 10, offset = 1,
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
									width = 3,
									offset = 1,
									actionButton("reset", "reset")
								)
							),
							br(),
							br(),
							conditionalPanel(
								condition = "input.calculate_btn > 0",
								fluidRow(
									column(
										width = 12,
										DT::dataTableOutput(outputId = "summary_table")
									)
								),
								br(),
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
	
	seq_options <- reactiveValues(
		chosen_lib_type    = NULL,
		chosen_run_type    = NULL,
		chosen_read_length = NULL,
		chosen_paired_type = NULL
	)
	
	## outputs ----
	output$field_fill_msg <- renderText(output_msg())
	
	output$output_text <- renderText(output_msg())
	
	output$cost <- renderText(formatted_cost())
	
	output$summary_table <- DT::renderDataTable({
	 	
		DT::datatable(neat_table(), rownames = FALSE, options = list(dom="t")) |>
			DT::formatRound(columns = c(2,3))
	})
	
	output$fake_table <- DT::renderDataTable({
		
		DT::datatable(temp_table, rownames = FALSE, options = list(dom="t")) |>
			DT::formatRound(columns = c(2,3))
		
	})
	
	temp_table <- tibble::tibble(
			`storage type`    = c("min practical", "full"), 
			`storage size gb` = c(1000, 2500),
			`storage cost`    = c(1320, 5440)
		)
	
	observe({
		
		updateVirtualSelect(
			"library_selector", choices = valid_libraries(), selected = seq_options$chosen_lib_type
		)
		updateVirtualSelect(
			"run_type_selector", choices = valid_runs(), selected = seq_options$chosen_run_type
		)
		updateVirtualSelect(
			"read_length_selector", choices = valid_read_lengths(), selected = seq_options$chosen_read_length
		)
		updateVirtualSelect(
			"paired_end_selector", choices = valid_paired(), selected = seq_options$chosen_paired_type
		)
		
	})


	
	# These will return NULL if not Truthy
	observeEvent(input$library_selector, {
		seq_options$chosen_lib_type <- if(isTruthy(input$library_selector)) input$library_selector
	})
	
	observeEvent(input$run_type_selector, {
		seq_options$chosen_run_type <- if(isTruthy(input$run_type_selector)) input$run_type_selector
	})

	observeEvent(input$read_length_selector, {
		seq_options$chosen_read_length <- if(isTruthy(input$read_length_selector)) input$read_length_selector
	})
	
	observeEvent(input$paired_end_selector, {
		seq_options$chosen_paired_type <- if(isTruthy(input$paired_end_selector)) input$paired_end_selector
	})
	
	
	valid_libraries <- reactive({
		get_valid_libraries(all_run_info, reactiveValuesToList(seq_options))	
	})
	
	valid_runs <- reactive({
		get_valid_runs(all_run_info, reactiveValuesToList(seq_options))	
	})
	
	valid_read_lengths <- reactive({
		get_valid_read_lengths(all_run_info, reactiveValuesToList(seq_options))	
	})
	
	valid_paired <- reactive({
		get_valid_paired(all_run_info, reactiveValuesToList(seq_options))		
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
		
		ceiling(total_values()$practical_cost)
		#req(storage_size())
		#ceiling(storage_size() * input$no_of_lanes * cost_per_unit)
	})
	
	# extract run size
	filtered_table <- reactive({
		
		req(input$no_of_lanes, seq_options$chosen_lib_type, seq_options$chosen_run_type, seq_options$chosen_paired_type, seq_options$chosen_read_length)
		
		filt <- get_filtered_table(all_run_info, reactiveValuesToList(seq_options))
		
		req(nrow(filt) == 1)
		
		dplyr::select(filt, practical_storage_size_gb:full_cost)
	})
	
	total_values <- reactive({
		filtered_table() * no_of_lanes()
	})
	
	
	neat_table <- reactive({
		tibble::tibble(
			`storage type`    = c("min practical", "full"), 
			`storage size gb` = c(total_values()$practical_storage_size_gb, total_values()$full_data_size_gb),
			`storage cost £`    = c(total_values()$practical_cost, total_values()$full_cost)
		)
	})
	
	storage_size <- reactive({
		total_values()$practical_storage_size_gb
	}) 
	
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

		paste0("The minimum practical storage size for", lane_text, input$library_selector, " on a ", 
					 input$run_type_selector, " is ", total_values()$practical_storage_size_gb, "Gb. At a storage cost of £1.32 per Gb for 10 years, this comes to ", formatted_cost(), ".")
		
		# paste0("The cost of storing the data from", lane_text, input$library_selector, " on a ", 
		# 			 input$run_type_selector, " is ", formatted_cost(), ".")
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
	
	observeEvent(input$reset, {
		seq_options$chosen_lib_type <- NULL
		seq_options$chosen_run_type <- NULL
		seq_options$chosen_read_length <- NULL
		seq_options$chosen_paired_type <- NULL
		
		updateNumericInput(inputId = "no_of_lanes", value = 1)
	})
	
}

# Run the application 
shinyApp(ui = ui, server = server)
