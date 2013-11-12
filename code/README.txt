**** details for running the yelp f/u/c code *****

Everything is designed to be run from the base directory.

*Setup*

You need to create the directory (or symbolic link) yelp/data.  This is where you will unpack the yelp_training_set archive, and it is where we store cleaned and binary data extracts.

*Data*

Python run tokenize.py to extract from the origonal json files into text tables.  Then 'R CMD BATCH --no-save code/data.R' to get the binary object data/covars.rda and data/x/part*.rds.

*Regression*

The R files are all designed to run in a slurm environment. Something like code/ir.sbatch will execute a distributed run for code/irfit.R, producing output results/B.txt and Z.rds.  Details will depend upon your computing setup.
