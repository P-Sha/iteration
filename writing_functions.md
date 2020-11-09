writing functions
================
Purnima Sharma
11/8/2020

## Do something simple (finding z-scores\!)

``` r
x_vec = rnorm(30, mean = 5, sd = 3) # create x-vector coming from normal distr, it has 30 numbers

(x_vec - mean(x_vec))/sd(x_vec)  #calculating z-scores of x-vector values
```

    ##  [1] -0.28165782  0.11584430 -0.08881903  2.18615726  1.55976103 -0.84366992
    ##  [7] -0.52707967  0.54991913 -1.85989127  0.65367870 -0.24790524 -0.84238992
    ## [13]  0.39895184 -0.52343241  1.29374714 -0.09266517 -1.64852354 -0.42538181
    ## [19]  1.13667737  0.51685609  1.17964770  0.67709578 -0.76560458  0.88765373
    ## [25] -1.99899105  0.49947283 -0.22640237  0.42061501 -0.76400741 -0.93965667

Using a function to compute z-scores instead.

``` r
z_scores = function(x){     #creating function called z-scores, that has input x.
  
  z = (x - mean(x)) / sd(x)    # (body of the function) solve function using input x. Call the solved value z.
  
  return(z)
  
}

z_scores(x_vec)     #operate z-scores function on x-vector(created above:line31). So input is x_vec
```

    ##  [1] -0.28165782  0.11584430 -0.08881903  2.18615726  1.55976103 -0.84366992
    ##  [7] -0.52707967  0.54991913 -1.85989127  0.65367870 -0.24790524 -0.84238992
    ## [13]  0.39895184 -0.52343241  1.29374714 -0.09266517 -1.64852354 -0.42538181
    ## [19]  1.13667737  0.51685609  1.17964770  0.67709578 -0.76560458  0.88765373
    ## [25] -1.99899105  0.49947283 -0.22640237  0.42061501 -0.76400741 -0.93965667

Updating function so it works on valid inputs only.

``` r
z_scores = function(x){
  
  if (!is.numeric(x)){              # if x-input is not numeric, stop
    stop("input must be numeric")
  }
  
  if (length(x) < 3){              # input should have alteast 3-values, if not, stop.
    stop("input must have atleast 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)    
  
  return(z)
  
}

z_scores(x_vec)    
```

    ##  [1] -0.28165782  0.11584430 -0.08881903  2.18615726  1.55976103 -0.84366992
    ##  [7] -0.52707967  0.54991913 -1.85989127  0.65367870 -0.24790524 -0.84238992
    ## [13]  0.39895184 -0.52343241  1.29374714 -0.09266517 -1.64852354 -0.42538181
    ## [19]  1.13667737  0.51685609  1.17964770  0.67709578 -0.76560458  0.88765373
    ## [25] -1.99899105  0.49947283 -0.22640237  0.42061501 -0.76400741 -0.93965667
