# pi-estimate

I made this for a final project for [Advanced Statistical computing](https://faculty.ucr.edu/~jflegal/206/) 

This app shows the many different ways we can estimate $\pi$ by using Statistical methods. 


## Sampling from Uniform

We know that the probability of any two integers being coprime is $6/\pi^2$ 
So we sample a pair of numbers from uniform distribution(we round the numbers in the pair to be an integer). So we have created a pseudo-random integer pairs.  

The see how many of those pairs of numbers had a GCD = 1. We then take that number and divided by total the number of  observations (pairs), N. 

We then solve for pi by setting that result equal to  $6/\pi^2 = N$
So  $\pi = (6/N)^{0.5}$

## Normal Distribution

We can approximate pi by taking many samples  a Normal Distribution N(0, pi) and taking the variances of those samples. And then taking the the mean of those variances to estimate pi.

## Monte Carlo Integration 

We know that this integral equals pi:

$$\int^0_1 -6 * (\pi/7)(x^2 - 3x) dx$$


So we will use Monte carlo integration to approximate this integral.

We sample N times from a unif(0,1) dist

We then define the function you want to integrate h.x(x)

We then find the average of the function mean(h.x(x)) 

Then we divide by the length of the interval

## Circle

The ratio of an area circle and a square  is: 
$$\frac{\pi r^2}{4 r^2} = \frac{\pi}{4 }$$

We randomly sample x and y from uniform $(-1,1)$
If  $x^2 + y ^2 <=  1$ then the point is in the circle

The ratio of points in the circle multiplied by 4  $\approx \pi$ 


# Contributing:

If you'd like to report a bug or request a new feature, please use the [Issues tab](https://github.com/JuliaClaireLee/yelp/issues) in this repository.
If you have any questions or ideas about this app please [email](mailto:julialee64@gmail.com?subject=[GitHub]%20Source%20Han%20Sans) or reach out on [Linkedin](https://www.linkedin.com/in/julia-lee-5201b0156/)



# Acknowledgments:


Thanks to the developers of these integral, open source libraries:



[library(ggplot2)](https://github.com/tidyverse/ggplot2)


[require(devtools) ](https://github.com/r-lib/devtools)


[library(tidyverse) ](https://github.com/tidyverse/tidyverse)


[library(dplyr)](https://github.com/tidyverse/dplyr)


[library(shinythemes)](https://github.com/rstudio/shinythemes)

[library(shiny)](https://github.com/rstudio/shiny)


# Attribution:

This application was built and is maintained by [Julia Claire Lee](https://github.com/JuliaClaireLee). No need to provide credit when using any of this work (but very much appreciated!)


