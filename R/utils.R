

format_lane_text <- function(number){
	if(number == 1){
		return(" 1 lane of ")
	} else {
		return(paste0(" ", number, " lanes of "))
	}
}


get_filtered_table <- function(filt, all_seq_options) {
	
	if(isTruthy(all_seq_options$chosen_lib_type)) {
		filt <- dplyr::filter(filt, Library_Prep == all_seq_options$chosen_lib_type)
	}	
	if(isTruthy(all_seq_options$chosen_run_type)) {
		filt <- dplyr::filter(filt, Run == all_seq_options$chosen_run_type)
	}
	if(isTruthy(all_seq_options$chosen_read_length)) {
		filt <- dplyr::filter(filt, read_length == all_seq_options$chosen_read_length)
	}
	if(isTruthy(all_seq_options$chosen_paired_type)) { # this works because it's text, not logical
		filt <- dplyr::filter(filt, paired == all_seq_options$chosen_paired_type) # this works because as.logical("FALSE") #> FALSE
	}	
	
	filt	
}



get_valid_libraries <- function(all_run_info, all_options) {
	all_options$chosen_lib_type <- NULL
	filt <- get_filtered_table(all_run_info, all_options)
	c("", unique(filt$Library_Prep))
}


get_valid_runs <- function(all_run_info, all_options) {
	all_options$chosen_run_type  <- NULL
	filt <- get_filtered_table(all_run_info, all_options)
	c("", unique(filt$Run))
}

get_valid_read_lengths <- function(all_run_info, seq_options) {
	seq_options$chosen_read_length <- NULL
	filt <- get_filtered_table(all_run_info, seq_options)
	c("", unique(filt$read_length))
}

get_valid_paired <- function(all_run_info, seq_options) {
	
	seq_options$chosen_paired_type_type <- NULL
	filt <- get_filtered_table(all_run_info, seq_options)
	# single_paired <- unique(filt$paired)
	# vec_names <- if_else(single_paired_seq_options, "yes", "no")
	# names(single_paired_) <- vec_names
	
	c("", unique(filt$paired))
}





