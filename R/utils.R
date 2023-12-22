

format_lane_text <- function(number){
	if(number == 1){
		return(" 1 lane of ")
	} else {
		return(paste0(" ", number, " lanes of "))
	}
}
