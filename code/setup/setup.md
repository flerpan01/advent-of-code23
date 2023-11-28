
# Setup

Here we use a [previous puzzle](https://adventofcode.com/2022/day/1) to
set up the environment

## Import dataset

Get the puzzle input inside R. Save as `day1.txt` inside `inputs/`:
`inputs/day1.txt`. To use the same paths as me, start `R` from inside
`code/` or `setwd()` manually.

``` r
# save the input from https://adventofcode.com/2022/day/1/input
input <- "../../inputs/day1.txt"
input <- readLines(input)
```

## Inspect the dataset

``` r
length(input)
```

    [1] 2237

``` r
input[1:30]
```

     [1] "5118"  "5554"  "4186"  "4729"  "1242"  "4360"  "1427"  "5312"  "6012" 
    [10] "1017"  "5581"  "5203"  "3811"  "4945"  "3960"  ""      "3812"  "7757" 
    [19] "4448"  "2205"  "15715" ""      "4164"  "6482"  "4479"  "3061"  "4082" 
    [28] "2474"  "1175"  "1918" 

Here the blank elements `""` in the array serves as delimiter for the
blocks to sum up. Lets change the elements to `numeric` (from
`"character"`). This function transforms `""` into `NA`.

``` r
# transfrom from characters to numeric
input <- as.numeric(input)
```

``` r
input[1:30]
```

     [1]  5118  5554  4186  4729  1242  4360  1427  5312  6012  1017  5581  5203
    [13]  3811  4945  3960    NA  3812  7757  4448  2205 15715    NA  4164  6482
    [25]  4479  3061  4082  2474  1175  1918

## Solving the puzzle

To complete the puzzle we need to sum up the values between each “block”
of `NA`’s. Now this is not an R problem perse, but rather a
computational problem. The most straight forward way is to make a for
loop that iterates over the elements, if not `NA`, keep value, if `NA`,
sum up values so far, save.

``` r
l <- list() # empty list
running_sum <- 0 # numeric

# loop through the inputs
for (i in seq_along(input)){
  value <- input[i]
  
  # if no NA, keep summing up
  if (!is.na(value)){
    running_sum <- sum(running_sum, value)
  }

  # if NA, save running sum into current index, reset running_sum = 0
  if (is.na(value)){
    l[[i]] <- running_sum
    running_sum <- 0
  }
}

# transform the list into an array
sums <- Reduce(function(x,y) rbind(x,y), l)
max(sums) # find the max value
```

    [1] 72602

This script is computationally costly. Our input was only `2237` long so
we do not notice it. We can stress test our script with different
lengths of of inputs.

``` r
foo <- function(input, verbose = TRUE){
  l <- list() # empty list
  running_sum <- 0 # numeric

  # loop through the inputs
  for (i in seq_along(input)){
    value <- input[i]
    
    # if no NA, keep summing up
    if (!is.na(value)){
      running_sum <- sum(running_sum, value)
    }

    # if NA, save running sum into current index, reset running_sum = 0
    if (is.na(value)){
      l[[i]] <- running_sum
      running_sum <- 0
    }
  }

  # transform the list into an array
  sums <- Reduce(function(x,y) rbind(x,y), l)
  if (verbose) print(max(sums)) # print max
}
```

``` r
length(input)
```

    [1] 2237

``` r
system.time(foo(input))
```

    [1] 72602

       user  system elapsed 
      0.012   0.000   0.013 

Now lets increase the size of `input`. We do this but sampling values
from our population (`input`) with
`sample(input, size = how_many_times, replace = TRUE)`

``` r
dataset <- sample(input, size = 10000, replace = TRUE)
length(dataset)
```

    [1] 10000

For effectiveness, lets wrap this up in a function

``` r
stresstest <- function(size){
  dataset <- sample(input, size = size, replace = TRUE)
  
  system.time(foo(dataset, verbose = FALSE))
}
```

``` r
test <- stresstest(30000)
test[["elapsed"]] # time elapsed
```

    [1] 1.024

Lets for fun do a benchmark, so we can predict how long time various
sizes will take

``` r
set.seed(1337)
#n <- sample(1:10000, 20, replace = TRUE)
n <- floor(runif(10, 2000, 200000))

size <- numeric()
time <- numeric()
for (i in n){
  output <- stresstest(n)
  
  size <- append(size, i)
  time <- append(time, output[["elapsed"]])
}
```

``` r
library(ggplot2)
data <- data.frame(time = round(time, 3), size)

ggplot(data, aes(size, time)) +
  geom_point()
```

![](setup_files/figure-commonmark/unnamed-chunk-11-1.png)

## Optimising the algorithm

R is not super good for *for loops* and performs better with vectors and
matrix. Below is an approach which index’s each `NA` and then finds the
`start` and `end` index of each block to sum up

``` r
# make index table, with 4 columns: start, end, break, sum
mat <- matrix(
  nrow = sum(is.na(input)),
  ncol = 4
)

# break = all NAs
mat[, 3] <- which(is.na(input))

# end = one element prior to break
mat[, 2] <- mat[, 3] - 1

# start = one element prior to previous break
# remove last value
mat[, 1] <- c(1, mat[, 3] + 1)[1:nrow(mat)]

# sum the values, use start and end from matrix
mat[, 4] <- apply(mat[, 1:2], 1, function(x) {
  start <- x[1]
  end <- x[2]

  sum(input[start:end])
})

colnames(mat) <- c("start", "end", "NA", "sum")
head(mat) 
```

         start end NA   sum
    [1,]     1  15 16 62457
    [2,]    17  21 22 33937
    [3,]    23  31 32 32590
    [4,]    33  39 40 49641
    [5,]    41  53 54 52982
    [6,]    55  61 62 41006

``` r
max(mat[,4]) # solution
```

    [1] 72602

Lets wrap this into a function and benchmark the performance

``` r
index_function <- function(input, verbose = TRUE){
  # make index table, with 4 columns: start, end, break, sum
  mat <- matrix(
    nrow = sum(is.na(input)),
    ncol = 4
  )

  # break = all NAs
  mat[, 3] <- which(is.na(input))

  # end = one element prior to break
  mat[, 2] <- mat[, 3] - 1

  # start = one element prior to previous break
  # remove last value
  mat[, 1] <- c(1, mat[, 3] + 1)[1:nrow(mat)]

  # sum the values, use start and end from matrix
  mat[, 4] <- apply(mat[, 1:2], 1, function(x) {
    start <- x[1]
    end <- x[2]

    sum(input[start:end])
  })

  colnames(mat) <- c("start", "end", "NA", "sum")
  #head(mat) 
  if (verbose) print(max(mat[,4])) # solution
}
```

Also, we need to update the `stresstest` function to take in a function
of our liking

``` r
stresstest <- function(size, foo){
  dataset <- sample(input, size = size, replace = TRUE)
  
  system.time(foo(dataset, verbose = FALSE))
}
```

``` r
size <- numeric()
time <- numeric()
for (i in n){
  output <- stresstest(n, index_function)
  
  size <- append(size, i)
  time <- append(time, output[["elapsed"]])
}

data$label <- "for_loop"
data <- rbind(data, data.frame(time = round(time, 3), size, label = "index_func"))

ggplot(data, aes(size, time, fill = label)) +
  geom_point(shape = 21, size = 4) +
  theme_bw()
```

![](setup_files/figure-commonmark/unnamed-chunk-15-1.png)
