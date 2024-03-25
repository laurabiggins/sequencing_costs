# Calculating the costs of storing sequencing data

We need to take into account the size of the fastq files, along with the size of
derived files. 

The size of the fastq files will depend on:
 - the number of reads generated
 - the length of reads

The number of reads will depend on:
	- sequencing run type - i.e. the machine and run type
	- paired or single end
	
The volume of derived files will depend on:
	- the size of the fastq files
	- the library type - i.e. more files will be generated for bisulfite libraries
	
We have got a set of files of simulated data with different numbers of reads 
and read lengths. These can be used to interpolate/extrapolate for other sizes of library.

The preprocessing has been done here:
Preprocessing1.qmd

It creates the file `data/all_run_costs.rds` that is used in the Shiny app.

It relies on 3 input files that may need to be updated for new sequencing run types:
`file_sizesL.txt`
`library_types.csv`
`sequencing_run_types.csv`
