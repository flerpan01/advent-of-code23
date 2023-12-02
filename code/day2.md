
From the input below, pull out the number in `blue`, `green` and `red`.
`;` separation should be stacked and counted towards the same game

``` r
dat <- readLines("../inputs/day2.txt")

head(dat)
```

    [1] "Game 1: 1 green, 2 blue; 13 red, 2 blue, 3 green; 4 green, 14 red"                                                  
    [2] "Game 2: 2 blue, 11 green; 4 blue, 12 red, 4 green; 7 red, 1 blue, 9 green; 10 green, 12 red, 6 blue"                
    [3] "Game 3: 1 blue, 12 green, 2 red; 9 red, 16 green; 1 red, 10 green, 1 blue; 1 red, 14 green"                         
    [4] "Game 4: 8 green, 18 blue; 4 green, 14 blue, 2 red; 3 blue, 5 green, 11 red"                                         
    [5] "Game 5: 7 red, 15 blue, 1 green; 13 blue; 18 red, 2 green, 9 blue; 19 blue, 5 green, 10 red; 9 green, 2 blue, 7 red"
    [6] "Game 6: 1 red, 8 blue, 2 green; 1 blue, 3 red, 5 green; 2 green, 3 red, 2 blue; 1 blue, 4 green"                    

``` r
foo <- function(x){
  x <- strsplit(x, ":")[[1]]
  #id <- as.numeric(sub("Game ", "", x[1]))
  draws <- strsplit(x[2], ";")[[1]]

  bar <- function(draw){
    draw <- strsplit(draw, ",")[[1]]
    
    blue <- draw[grepl("blue", draw)]
    green <- draw[grepl("green", draw)]
    red <- draw[grepl("red", draw)]

    # \\D match everything up to non-digit, \\1 return digit
    blue <- as.numeric(sub(".*\\D(\\d+).*", "\\1", blue))
    green <- as.numeric(sub(".*\\D(\\d+).*", "\\1", green))
    red <- as.numeric(sub(".*\\D(\\d+).*", "\\1", red))

    blue <- ifelse(length(blue) == 0, 0, blue)
    green <- ifelse(length(green) == 0, 0, green)
    red <- ifelse(length(red) == 0, 0, red)

    return(c(blue, green, red))
  }

  t(sapply(draws, bar))
}

# list
l1 <- lapply(dat, foo)

print(l1[1:2])
```

    [[1]]
                             [,1] [,2] [,3]
     1 green, 2 blue            2    1    0
     13 red, 2 blue, 3 green    2    3   13
     4 green, 14 red            0    4   14

    [[2]]
                              [,1] [,2] [,3]
     2 blue, 11 green            2   11    0
     4 blue, 12 red, 4 green     4    4   12
     7 red, 1 blue, 9 green      1    9    7
     10 green, 12 red, 6 blue    6   10   12

``` r
# max: blue = 14, green = 13, red = 12
# identify rows which > these values

val <- sapply(l1, function(row){
  any(row[,1] > 14 | row[,2] > 13 | row[,3] > 12)
})

rows <- 1:100

# Answer, which games are POSSIBLE? Sum up
sum(rows[!val])
```

    [1] 2541

Find the largest value of each column (in each game), multiply these
three numbers. Sum everything up.

``` r
val <- sapply(l1, function(row){
  max(row[,1]) * max(row[,2]) * max(row[,3])
})
sum(val)
```

    [1] 66016
