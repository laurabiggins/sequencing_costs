# Calculating the costs of storing sequencing data

This simple [Shiny
app](https://www.bioinformatics.babraham.ac.uk/shiny/sequencing_costs/ "storage costs Shiny app")
provides estimates of costs for storage of sequencing data. This covers
demultiplexed fastq files, mapped bam files, and additional derived
files if required for the library type.

The app provides 2 separate estimates - one for the practical storage
size and one for the full storage size. If the researcher envisages that
they will want to retain more intermediate files beyond a single set of
fastq and bam files, they may want to opt for the `full` option. See
[practical vs full data storage](#Practical-vs-full-data-storage) section for more details.

The data used in the app can also be found
[here](data/all_run_costs.csv)

## Updating the application

The Quarto document
[Preprocessing1.qmd](preprocessing/Preprocessing1.qmd) is run to
calculate all the storage size options. It produces a large table of
values that is exported as an .rds object for using in the app. A csv
file containing exactly the same values is also produced and available
to view [here](data/all_run_costs.csv)

A plain R script containing the same code as `Preprocessing1.qmd` is
also available if the quarto document cannot be run for any reason.
[preprocessing_plainR.R](preprocessing/preprocessing_plainR.R)

The preprocessing script uses 3 input files that may need to be updated
as necessary:

[sequencing_run_types.csv](preprocessing/sequencing_run_types.csv) -
this should contain each available run type, available read lengths,
paired/single end and the number of million sequences generated per
lane.

[library_types.csv](preprocessing/library_types.csv) - new library types
can be added to this. The main reason for having the library type
information is to assign a size factor for derived data.

[file_sizesL.txt](file_sizesL.txt) - this contains file sizes for
different read length and numbers of reads. The file sizes were derived
from the output of the `make_outputs.py` script which generates in
silico fastq files of different read lengths and numbers of reads. This
shouldn't need to be modified as any additional read lengths or numbers
of reads can be interpolated/extrapolated from the existing data.

## Calculating file sizes

The volume of storage required will depend on the size of the files.

The size of the fastq files will depend on the:

-   number of reads generated
-   length of reads

The number of reads will depend on:

-   sequencing run type i.e. the machine and run type
-   whether the run is paired or single end

The volume of derived files will depend on the library type - e.g. more
files will be generated for bisulfite libraries.

Mapped bam file sizes are estimated at 1.5x the size of fastq files.

### Practical vs full data storage

The value for practical storage size covers demultiplexed fastq files
and mapped bam files. If additional derived data is generated for the
library type, that will also be included (standard RNA-seq and similar
libraries do not assume derived data).

The value for full storage allows the retention of more intermediate
files, such as raw fastq and trimmed fastq files.

Calculations from `Preprocessing1.qmd`

```         
minimum_possible_size_gb   = fastq_size_gb,
practical_storage_size_gb  = split_fq_size_gb + mapped_BAM_size_gb + derived_data_size_gb
full_data_size_gb          = fastq_size_gb + split_fq_size_gb + trimmed_fq_size_gb + mapped_BAM_size_gb + derived_data_size_gb
```
