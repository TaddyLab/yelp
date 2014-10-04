**** details for running the yelp f/u/c code *****

Everything is designed to be run from the base directory.

*Data*

You need to create the directory (or symbolic link to some fast-read storage) yelp/data.  This is where you will unpack the yelp_training_set archive, and it is where we store cleaned and binary data extracts.

*Setup*

Python run tokenize.py to extract from the original json files into text tables.  Then build.R to get the binary objects data/meta.rda and data/x/part*.rds.

*Regression*

The R files are all designed to run in a slurm environment. For example, `code/run.sbatch -Jmn’ will run code/build.R, then execute a distributed run for code/mn-fit.R and call the combiner code/mn-combine.R, writing coefficients and projections to results/mn.  Details depend upon your computing setup.

