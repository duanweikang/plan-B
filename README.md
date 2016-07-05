# Instructions on how to use the code
## contents
This repo has three files:`multinom3.R` the code you need to **source** for use, `simulation_multinom.R` simulation code to see how model works,`test_on_data_multi.R` code for test model on real data set
## prerequisite packages
You need to download `partitions` package and install it first on R.
## Usage
This code is used to implement a nonparametric bayesian regression model for multinomial responses and continuous predictor. The model is based on [this paper](http://users.stat.umn.edu/~gmeeden/papers/sbrown.pdf).
The only function you need to use is `manysteps_multinom`, note there are several parameters need to be given as input. *y* as the response data, should be a matrix of 1 and 0 where rows equals number of categories and columns equals number of observations.  *m* is the minimum number of each chunk, *k* is the number of chunks. *R* is the length of MCMC and *ct* is the number of categories.
