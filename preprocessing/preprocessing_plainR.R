## -----------------------------------------------------------------------------------------------------------
library(tidyverse)

# R code extracted from Preprocessing1.qmd
# this can be run to create a new version of all_run_costs.rds if any of the input 
# files have been updated

## -----------------------------------------------------------------------------------------------------------
# Input files
#
sequencing_run_types_file <- "sequencing_run_types.csv"
file_sizes_file <- "file_sizesL.txt"
library_types <- "library_types.csv"
# Cost updated 02/06/2025
cost_per_unit <- 1.72 

## -----------------------------------------------------------------------------------------------------------
file_sizes <- read_delim(file_sizes_file, col_names = c("bytes","file")) 

## -----------------------------------------------------------------------------------------------------------
file_sizes <- file_sizes %>%
  separate(
    file,
    into=c("read_length",NA,"mseqs",NA),
    sep="_",
    convert=TRUE
  ) 

## -----------------------------------------------------------------------------------------------------------
get_slope <- function(b,m) {
  lm(b~m) -> l
  return (l$coefficients[2])
}

file_slopes <- file_sizes %>%
  group_by(read_length) %>%
  summarise(
    slope=get_slope(bytes,mseqs)
) 

## -----------------------------------------------------------------------------------------------------------
run_types <- read_delim(sequencing_run_types_file)

## -----------------------------------------------------------------------------------------------------------
run_types <- run_types |>
	mutate(Run = stringr::str_trim(Run)) |>
	left_join(file_slopes) |>
	mutate(fastq_size_gb=((mseqs*slope)/(1024^3))*if_else(paired,2,1))

## -----------------------------------------------------------------------------------------------------------
run_types <- run_types |>
	mutate(
    split_fq_size_gb   = fastq_size_gb,
    trimmed_fq_size_gb = fastq_size_gb,
    mapped_BAM_size_gb = fastq_size_gb * 1.5
  ) %>%
  select(-slope)

## -----------------------------------------------------------------------------------------------------------
library_types <- read_csv(library_types)

sizes <- run_types |>
	crossing(library_types) |>
	mutate(
	    derived_data_size_gb = fastq_size_gb * `Derived Data`
	  ) %>%
	  select(-`Derived Data`, -Notes) %>%
	  mutate(
	    minimum_possible_size_gb = fastq_size_gb,
	    practical_storage_size_gb = split_fq_size_gb + mapped_BAM_size_gb + derived_data_size_gb,
	    full_data_size_gb = fastq_size_gb + split_fq_size_gb + trimmed_fq_size_gb + mapped_BAM_size_gb + derived_data_size_gb
	  )

## -----------------------------------------------------------------------------------------------------------
costs <- sizes |>
	mutate(
		practical_cost = practical_storage_size_gb * cost_per_unit,
		full_cost      = full_data_size_gb * cost_per_unit
	)

## ----eval=TRUE----------------------------------------------------------------------------------------------
saveRDS(costs, file = "../data/all_run_costs.rds")

