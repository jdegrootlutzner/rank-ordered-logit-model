# rank-ordered-logit-model

An unpolished R implementation of Glickman and Hennessys' [A stochastic rank ordered logit model for rating multi-competitor games and sports](http://www.glicko.net/research/multicompetitor.pdf). 

## Context

For [a side project I was working on](https://jdegrootlutzner.com/chiphoagie), I was trying to develop a way to measure the strength of NCAA cross-country runners (a sport where times are difficult to compare because of varying race courses and conditions) I found Glickman and Hennessys' paper and implemented the algorithm they described on my own dataset - a collection of over 6,500 NCAA cross-country race results and over one million individual performances. The size of the data required me to optimize and parallelize the algorithm and run the code on a high compute Google Cloud server. 

At fist, the algorithm looked promising on a small test dataset of cross-country data. Unfortunately, when I scaled the algorithm to the complete dataset the results were nearly meaningless. I suspect this is the case because of the extreme variability of cross-country running races. Glickman and Hennessy created the algorithm for Olympic level downhill skiing where the results are much more consistent than cross-country running  (because of the variability in strength of athletes, type of courses, and length of races).

In the future, I will clean up the codebase and create an R package. If you are trying to implement the algorithm yourself, please reach out.

## Overview of Codebase
For reading, exploring, manipulating, and transforming the data I used the packages [readr](https://cran.r-project.org/web/packages/readr/), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/), [dplyr](https://cran.r-project.org/web/packages/dplyr/), and [Matrix](https://cran.r-project.org/web/packages/Matrix/). For testing and optimizing the data I used packages [RUnit](https://cran.r-project.org/web/packages/RUnit/) and [lineprof](https://cran.r-project.org/web/packages/foreach/). For parallelization of the code I used parallel, [doMC](https://cran.r-project.org/web/packages/doMC/), and [foreach](https://cran.r-project.org/web/packages/doMC/).

Here is an overview of the files above in the order that I developed them:

Step One - understanding the algorithm and getting something working
- _small-test.R_ : Prototype of the algorithm on a small dataset in order to understand the paper and test its viability 

Step Two - Attempting to scale the algorithm to larger dataset

- _large-model.R_ : The controller of the process. Feed in data, preprocess, run algorithm, save results.
- _preprocess-data.R_ : First pass at a program that transform the raw data into the matrix format detailed in the paper
- _newton-raphson.R_ : First pass at the algorithm detailed in section A of the paper, which is used to find posterior mode of theta


Step Three - Optimizing and parallelizing the algorithm
- _large-model-optimize.R_ : Optimized/parallelized version of the controller
- _preprocess-data-optimize.R_ : Optimized/parallelized version of preprocess
- _newton-raphson-optimize.R_ : Optimized/parallelized version of Newton Raphson

Step Four - Unit testing the algorithm and understanding the results
- _unit-test.R_ : Test results compared to hand derived results
- _unit-test-2.R_ : Test algorithm on dummy data
- _double-check-preprocess.R_ : I don't remember what this is  :s
- _large-model-optimize.R_ : I don't remember what this is :/ 
- _parallel-test.R_ : Used to test/understand parallelization 
- _pop-explor.R_ : Used to understand results

For questions and help reach out to Julian on Twitter [@jdegrootlutzner](https://twitter.com/jdegrootlutzner). 
The project is released under the MIT license. 

