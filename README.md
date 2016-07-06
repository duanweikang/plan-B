# Instructions on how to use the code
## contents
This repo has three files:

`multinom3.R` the code you need to **source** for use,

`simulation_multinom.R` simulation code to see how model works,

`test_on_data_multi.R` code for test model on real data set

## prerequisite packages
You need to download `partitions` package and install it first on R.

## Usage
This code is used to implement a nonparametric bayesian regression model for multinomial responses and continuous predictor. The model is based on [this paper](http://users.stat.umn.edu/~gmeeden/papers/sbrown.pdf).

The only function you need to use is `manysteps_multinom`, note there are several parameters need to be given as input. 

*y* as the response data, should be a matrix of 1 and 0 where rows equals number of categories and columns equals number of observations.  

*m* is the minimum number of each chunk, 

*k* is the number of chunks. 

*R* is the length of MCMC

*ct* is the number of categories.

## Example
`> dat = rmultinom(100,1,c(0.3,0.5,0.2))`

`> foo = manysteps_multinom(dat,m=5,k=10,R=1e3,ct=3)`

`> foo[[1]]`

`[1] 0.624`

`> foo[[2]][,1:4]`

`           [,1]       [,2]       [,3]       [,4]`

`[1,] 0.47418990 0.47418990 0.47418990 0.47418990`

`[2,] 0.48429465 0.48429465 0.48429465 0.48429465`

`[3,] 0.04151545 0.04151545 0.04151545 0.04151545`
