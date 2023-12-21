library(tidyverse)

all_runs <- read_delim("data/All run and Library Sizes.txt") %>%
	tibble(.name_repair = "universal")

all_runs$Run.Type <- str_trim(all_runs$Run.Type)

# We'll use the "practical size" figures
all_runs <- all_runs %>%
	select(Library.Type, Run.Type, Practical.Size..GB.)

saveRDS(all_runs, file = "data/all_runs.rds")
