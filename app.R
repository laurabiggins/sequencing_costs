library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(tibble)
library(rclipboard)
#library(shinyFeedback)
library(DT)

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
	#shinyFeedback::useShinyFeedback(),
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
	),
	tags$head(tags$title("Sequencing costs")),
	fluidPage(
		br(),
	#	actionButton("browser", "browser"),
		## select inputs ----
		fluidRow(
			column(
				width = 10, offset = 1,
				h1("Storage costs for sequencing data"),
				br(),
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
							label   = "Single or Paired End",
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
					),
					#### reset button ----
					column(
						width = 6,
						actionButton("reset", "reset"),
						align = "right"
					)
				)
			)
		),
		br(),
		## calculate tabset panel ----
		tabsetPanel(
			id = "calculate_panel",
			type = "hidden",
			### info msg ----
			tabPanelBody(
				"select_msg",
				fluidRow(
					column(
						width = 10,
						offset = 1,
						textOutput(outputId = "field_fill_msg")
					)
				)
			),
			### calculate button ----
			tabPanelBody(
				"calculate",
				fluidRow(
					column(
						width = 12,
						actionButton(inputId = "calculate_btn", label = "Calculate cost"),
						align = "center"
					)
				)
			),
			### cost info ----
			#### summary table ---- 
			tabPanelBody(
				"cost_info",
				fluidRow(
					column(
						width  = 10, 
						offset = 1,
						br(),
						DT::dataTableOutput(outputId = "summary_table")
					)
				),
				br(),
				#### summary text ----
				fluidRow(
					column(
						width  = 10, 
						offset = 1,
						p(id = "info_text", "The volume of processed data that is retained will affect the total storage size. The 'practical storage size' includes the raw fastq files, mapped BAM files and minimal derived data files. The 'full storage size' allows for the retention of more processed data files.")
					)
				),
				br(),
				fluidRow(
					column(
						width  = 10, 
						offset = 1,
						textOutput(outputId = "output_text_practical")
					),
					column(width = 1, uiOutput("clip1"))
				),
				br(),
				fluidRow(
					column(
						width  = 10, 
						offset = 1,
						textOutput(outputId = "output_text_full")
					),
					column(width = 1, uiOutput("clip2"))
				)
			)
		),
		br(),
		br(),
		br(),
		fluidRow(
			column(
				width = 2,
				tags$img(src = "bioinformatics_logo_small.png", width = "200", height = "71")
			),
			column(
				width = 10,
				#offset = 1,
				br(),
				br(),
				p("Any problems please email laura.biggins@babraham.ac.uk", style = "font-size:12px", align = "right")
			)  
		),
		br()
	)
)

# server ----
server <- function(input, output, session) {
	
	observeEvent(input$browser, browser())
	
	## reactive Vals ----
	# Instruction message 
	field_fill_info <- reactiveVal("")

	# dropdown options
	seq_options <- reactiveValues(
		chosen_lib_type    = NULL,
		chosen_run_type    = NULL,
		chosen_read_length = NULL,
		chosen_paired_type = NULL,
		no_of_lanes        = NULL
	)
	
	## outputs ----
	output$field_fill_msg <- renderText(field_fill_info())

	output$output_text_practical <- renderText(calc_summary_text())
	output$output_text_full <- renderText(full_storage_text())
	
	output$summary_table <- DT::renderDataTable({
		neat_table() |>
			DT::datatable(rownames = FALSE, options = list(dom="t")) |>
			DT::formatRound(columns = c(2,3), digits = 0)
	})
	
	## observeEvents for dropdown selections ----
	# The if statement will return NULL if isTruthy == FALSE
	observeEvent(input$library_selector, {
		l <- input$library_selector
		seq_options$chosen_lib_type <- if(isTruthy(l)) l
	})
	
	observeEvent(input$run_type_selector, {
		rt <- input$run_type_selector
		seq_options$chosen_run_type <- if(isTruthy(rt)) rt
	})

	observeEvent(input$read_length_selector, {
		rl <- input$read_length_selector
		seq_options$chosen_read_length <- if(isTruthy(rl)) rl
	})
	
	observeEvent(input$paired_end_selector, {
		pe <- input$paired_end_selector
		seq_options$chosen_paired_type <- if(isTruthy(pe)) pe
	})
	
	observeEvent(input$no_of_lanes, {
		n <- input$no_of_lanes
		seq_options$no_of_lanes <- if(isTruthy(n) & n >= min_lanes & n <= max_lanes) n
	})
	

	## valid dropdown options ----
	# We only want to show compatible options, so once one has been selected, the 
	# others need to be updated. 
	# The get_valid_ functions are in utils.R
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
		vec <- get_valid_paired(all_run_info, reactiveValuesToList(seq_options))	
		# this should be "" and then either or both TRUE, FALSE
		#single_paired <- unique(filt$paired)
		vec_names <- case_when(
			vec == TRUE ~ "paired", 
			vec == FALSE ~ "single",
			vec == "" ~ "  ",
			.default = "nothing"
		)
		names(vec) <- vec_names
		vec
	})
	
	## update select inputs ----
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
			"paired_end_selector",  choices = valid_paired(), selected = seq_options$chosen_paired_type
		)
		
	})

	## all_valid flag
	all_valid <- reactive({
		all(sapply(reactiveValuesToList(seq_options), isTruthy))
	})
	
	### validate selections ----
	# only show the calculate button if we've got valid input for each field
	observe({
		
		if(all_valid()){
				updateTabsetPanel(session, "calculate_panel", selected = "calculate")
				field_fill_info("")
				#calculated_cost("")
		} else {
			msg <- if_else(
				!isTruthy(seq_options$no_of_lanes), 
				acceptable_lane_nos, 
				"Fill in all fields above"
			)
			field_fill_info(msg)
			
			#calculated_cost("")
			updateTabsetPanel(session, "calculate_panel", selected = "select_msg")
		}
	})
	

	### Calculate button ----
	
	observeEvent(input$calculate_btn, {
		updateTabsetPanel(session, "calculate_panel", selected = "cost_info")
	})
	
	### filtered table ----
	# get_filtered_table is in utils.R
	filtered_table <- reactive({
		
		req(all_valid())
		filt <- get_filtered_table(all_run_info, reactiveValuesToList(seq_options))
		req(nrow(filt) == 1)
		dplyr::select(filt, practical_storage_size_gb:full_cost)
	})

	### total values ----
	# multiply storage and cost values by number of lanes
	# round the values at this point
	total_values <- reactive({
		req(seq_options$no_of_lanes)
		values <- filtered_table() * seq_options$no_of_lanes
		round(values)
	})
	
	## summary table ----
	# update the size and costs in the summary table
	neat_table <- reactive({
		tv <- total_values()
		tibble::tibble(
			`storage type`    = c("min practical", "full"),
			`storage size gb` = c(tv$practical_storage_size_gb, tv$full_data_size_gb),
			`storage cost £`  = c(tv$practical_cost, tv$full_cost)
		)
	})
	
	calc_summary_text <- reactive({
		
		lane_text <- format_lane_text(seq_options$no_of_lanes)
		format_summary_msg(
			reactiveValuesToList(seq_options),
			total_values()$practical_storage_size_gb, 
			total_values()$practical_cost, 
			cost_per_unit, 
			full=FALSE
		)
	})	

	full_storage_text <- reactive({
		
		format_summary_msg(
			reactiveValuesToList(seq_options),
			total_values()$full_data_size_gb, 
			total_values()$full_cost, 
			cost_per_unit, 
			full=TRUE
		)

	})
				 
	## Clipboard button ---- 
	# to enable easy copying of text
	output$clip1 <- renderUI({
		rclipButton(
			inputId = "clipbtn1",
			label = "",
			clipText = calc_summary_text(),
			icon = icon("copy"),
			tooltip = "Copy text",
			placement = "top",
			options = list(delay = list(show = 800, hide = 100), trigger = "hover")
		)
	})
	
	output$clip2 <- renderUI({
		rclipButton(
			inputId = "clipbtn2",
			label = "",
			clipText = full_storage_text(),
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
