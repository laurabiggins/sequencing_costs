library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(rclipboard)

# more info - generates x gb of data which costs x, plus extra files

all_run_info <- readRDS("data/all_run_costs.rds")
available_library_types <- unique(all_run_info$Library_Prep)
available_run_types <- "" # populate this once library has been selected
available_read_lengths <- "" # populate once 
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
							choices = available_library_types,
							autoSelectFirstOption = FALSE
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "run_type_selector", 
							label   = "Run type",
							choices = available_run_types,
							autoSelectFirstOption = FALSE
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "read_length_selector", 
							label   = "Read Length",
							choices = available_read_lengths
						),
						br(),
						shinyWidgets::virtualSelectInput(
							inputId = "paired_end_selector", 
							label   = "Paired End",
							#choices = list(No = FALSE, Yes = TRUE)
							choices = ("")
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
	
	filtered_run_info <- reactiveVal(NULL)
	
	## outputs ----
	output$field_fill_msg <- renderText(output_msg())
	
	output$output_text <- renderText(output_msg())
	
	output$cost <- renderText(formatted_cost())
	
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

	### update available run types after library selection ----

	# There actually seem to be the same run types for all the library types so this
	# isn't strictly necessary, but we probably need to amend that.
	observeEvent(input$library_selector, {
		if(isTruthy(input$library_selector)) {
			
			run_info_filt <- all_run_info |>
				filter(Library_Prep == input$library_selector)
			
			valid_run_types <- run_info_filt |>
				distinct(Run) |>
				pull(Run)

			filtered_run_info(run_info_filt)

			updateVirtualSelect(inputId = "run_type_selector", choices = valid_run_types)
		}
	})
	
	### update available read lengths ----------------
	
	observeEvent(input$run_type_selector, {
		if(isTruthy(input$run_type_selector)) {
			
			run_info_filt <- filtered_run_info() |>
				filter(Run == input$run_type_selector)
			
			valid_read_lengths <- run_info_filt |>
				distinct(read_length) |>
				pull(read_length)
			
			filtered_run_info(run_info_filt)	
			
			updateVirtualSelect(inputId = "read_length_selector", choices = valid_read_lengths)
		}
	})
	
	
	
	 observeEvent(input$read_length_selector, {
	 	if(isTruthy(input$read_length_selector)) {
	 		
	 		run_info_filt <- filtered_run_info() |>
	 			filter(read_length == input$read_length_selector)

	 		single_paired_options <- run_info_filt |>
	 			pull(paired)
	 		
	 		vec_names <- if_else(single_paired_options, "yes", "no")
	 		names(single_paired_options) <- vec_names
	 		
	 		updateVirtualSelect(inputId = "paired_end_selector", choices = single_paired_options)
	 		
	 		filtered_run_info(run_info_filt)	
	 	}
	 })
	
	 observeEvent(input$read_length_selector, {
	 	if(isTruthy(input$read_length_selector)) {
	 		
	 		run_info_filt <- filtered_run_info() |>
	 			filter(read_length == input$read_length_selector)
	 		
	 		single_paired_options <- run_info_filt |>
	 			pull(paired)
	 		
	 		vec_names <- if_else(single_paired_options, "yes", "no")
	 		names(single_paired_options) <- vec_names
	 		
	 		updateVirtualSelect(inputId = "paired_end_selector", choices = single_paired_options)
	 		
	 		filtered_run_info(run_info_filt)	
	 	}
	 })
	 
	 observeEvent(input$paired_end_selector, {
	 	if(isTruthy(input$paired_end_selector)) { # this works because it's text, not logical
	 		
	 		run_info_filt <- filtered_run_info() |>
	 			filter(paired == input$paired_end_selector) # this works because as.logical("FALSE") #> FALSE
	 		
	 		filtered_run_info(run_info_filt)
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
		
		req(input$no_of_lanes)
		
		ceiling(run_size() * input$no_of_lanes * cost_per_unit)
	})
	
	# extract run size
	run_size <- reactive({
		
		req(filtered_run_info())
		req(nrow(filtered_run_info()) == 1)
		
		filtered_run_info() |>
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
