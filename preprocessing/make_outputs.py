#!python
import gzip

sizes = [1,2,5,10,20,50,100]
lengths = [50,75,100,150]

# Make the output files

filehandles = {}

for size in sizes:
    filehandles[size] = {}
    for length in lengths:
         filehandles[size][length] = gzip.open(f"{length}_bp_{size}_M_rep2.fq.gz","wt",4, encoding="UTF-8")



with gzip.open("ref_larger.fq.gz","rt", encoding="UTF-8") as infh:

    # Throw away the first million lines
    for i in range(1000000):
        l = infh.readline()


    # Start from here
    count = 0
    while count <= 100000000:
        if count % 250000 == 0:
            print(f"Processed {count} reads")
        count += 1
        header = infh.readline().strip()
        sequence = infh.readline().strip()
        midline = infh.readline().strip()
        qualities = infh.readline().strip()


        # Double up the sequences to make them long enough
        sequence += sequence[::-1]
        qualities += qualities[::-1]


        # Now work our way through the different files.
        for size in sizes:
            if count >= size*1000000:
                continue

            for length in lengths:
                f = filehandles[size][length]
                print(header,file=f)
                print(sequence[1:length],file=f)
                print(midline,file=f)
                print(qualities[1:length],file=f)