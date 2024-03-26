

format_lane_text <- function(number){
	if(number == 1){
		return(" 1 lane of ")
	} else {
		return(paste0(" ", number, " lanes of "))
	}
}

get_filtered_table <- function(
		filt, 
		chosen_lib = NULL, 
		chosen_run = NULL,
		chosen_paired = NULL,
		chosen_read_length = NULL) {
	
	if(isTruthy(chosen_lib)) {
		filt <- dplyr::filter(filt, Library_Prep == chosen_lib)
	}	
	if(isTruthy(chosen_run)) {
		filt <- dplyr::filter(filt, Run == chosen_run)
	}
	if(isTruthy(chosen_read_length)) {
		filt <- dplyr::filter(filt, read_length == chosen_read_length)
	}
	if(isTruthy(chosen_paired)) { # this works because it's text, not logical
		filt <- dplyr::filter(filt, paired == chosen_paired) # this works because as.logical("FALSE") #> FALSE
	}	
	
	filt	
}


get_valid_libraries <- function(all_run_info, chosen_run, chosen_paired, chosen_read_length) {
	filt <- get_filtered_table(all_run_info, chosen_lib = NULL, chosen_run, chosen_paired, chosen_read_length)
	c("", unique(filt$Library_Prep))
}

get_valid_runs <- function(all_run_info, chosen_lib, chosen_paired, chosen_read_length) {
	filt <- get_filtered_table(all_run_info, chosen_run = NULL, chosen_lib, chosen_paired, chosen_read_length)
	c("", unique(filt$Run))
}

get_valid_read_lengths <- function(all_run_info, chosen_lib, chosen_run, chosen_paired) {
	filt <- get_filtered_table(all_run_info, chosen_read_length = NULL, chosen_lib, chosen_run, chosen_paired)
	c("", unique(filt$read_length))
}

get_valid_paired <- function(all_run_info, chosen_lib, chosen_run, chosen_read_length) {
	filt <- get_filtered_table(all_run_info, chosen_paired = NULL, chosen_lib, chosen_run,  chosen_read_length)
	c("", unique(filt$paired))
}









