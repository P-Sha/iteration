writing functions
================
Purnima Sharma
11/8/2020

## Do something simple (finding z-scores\!)

``` r
x_vec = rnorm(30, mean = 5, sd = 3) # create x-vector coming from normal distr, it has 30 numbers

(x_vec - mean(x_vec))/sd(x_vec)  #calculating z-scores of x-vector values
```

    ##  [1] -0.7663771 -0.9146124  0.7517391 -0.4534905  0.2985650  0.9060625
    ##  [7]  0.1511931 -2.0200236  0.7891832 -0.9291110  1.0309369 -0.5641638
    ## [13] -0.8070768 -0.2705842 -0.4793699  0.2999074 -1.5141219  0.1925645
    ## [19]  0.1062379  1.0584523  0.1330974 -1.1526260  0.2476893 -1.6475876
    ## [25]  0.8334079  1.8648714  1.9046338  0.4614052  1.2849200 -0.7957221

Using a function to compute z-scores instead.

``` r
z_scores = function(x){     #creating function called z-scores, that has input x.
  
  z = (x - mean(x)) / sd(x)    # (body of the function) solve function using input x. Call the solved value z.
  
  return(z)
  
}

z_scores(x_vec)     #operate z-scores function on x-vector(created above:line31). So input is x_vec
```

    ##  [1] -0.7663771 -0.9146124  0.7517391 -0.4534905  0.2985650  0.9060625
    ##  [7]  0.1511931 -2.0200236  0.7891832 -0.9291110  1.0309369 -0.5641638
    ## [13] -0.8070768 -0.2705842 -0.4793699  0.2999074 -1.5141219  0.1925645
    ## [19]  0.1062379  1.0584523  0.1330974 -1.1526260  0.2476893 -1.6475876
    ## [25]  0.8334079  1.8648714  1.9046338  0.4614052  1.2849200 -0.7957221

Updating function so it works on valid inputs only.

``` r
z_scores = function(x){
  
  if (!is.numeric(x)) {              # if x-input is not numeric, stop
    stop("input must be numeric")
  }
  
  if (length(x) < 3) {              # input should have alteast 3-values, if not, stop.
    stop("input must have atleast 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)    
  
  return(z)
  
}

z_scores(x_vec)    
```

    ##  [1] -0.7663771 -0.9146124  0.7517391 -0.4534905  0.2985650  0.9060625
    ##  [7]  0.1511931 -2.0200236  0.7891832 -0.9291110  1.0309369 -0.5641638
    ## [13] -0.8070768 -0.2705842 -0.4793699  0.2999074 -1.5141219  0.1925645
    ## [19]  0.1062379  1.0584523  0.1330974 -1.1526260  0.2476893 -1.6475876
    ## [25]  0.8334079  1.8648714  1.9046338  0.4614052  1.2849200 -0.7957221

## Trying a more complex function: Multiple outputs (want to calculate mean & sd too)

``` r
mean_and_sd = function(x){
  
  if (!is.numeric(x)) {             
    stop("input must be numeric")
  }
  
  if (length(x) < 3) {             
    stop("input must have atleast 3 numbers")
  }
  
  mean_x = mean(x)     # find mean of x
  sd_x = sd(x)         # fidn sd of x   
  
  tibble(               # organize outputs in a table. Could have used "list" too.  
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Check that the function works.

``` r
mean_and_sd(x_vec)    # use function mean_and_sd with input values of x-vector.
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.50  2.86

Checking with new x-vector

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec) 
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0459  1.02

Multiple inputs (writing a function that will ask for sample size, mean
and sd) This is NOT a function\! (function below, 2nd code)

``` r
sim_data =  #create simulated dataset, that is a table, that has an x-column w/values:
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>%   #summarize simulated dataset. Mean & sd should be close to above inputs!
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.92  2.92

This Is a function(same calculatons as above using a function):

``` r
sim_mean_sd = function(sample_size, mu, sigma) {    #input these values,from row142
  
  sim_data =    
    tibble(
      x = rnorm(n = sample_size, mean = mu, sd = sigma) #inputs plugged here, as table
  )

  sim_data %>%   
    summarize(                            #table values used to find mean,sd of n=100 of normal distr, around given mean, sd          
      mean = mean(x),
      sd = sd(x)
)
  
}

sim_mean_sd(100, 4, 3)               # inputting values into function: row 127. Each time output will be diff, cause diff normal distr values will be generated.
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.09  2.93

``` r
#OR
sim_mean_sd(sample_size = 100, mu = 4, sigma = 3)  #name-mtching input, vs position-matching (above row 142)  
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.49  3.03
