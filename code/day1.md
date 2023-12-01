
In a string of text, identify all digits. Combine the first and the last
digit to make a two digit number. If only 1 digit exist in the string,
combine it with it self to form a two digit number. Sum all the
identified numbers

``` r
dat <- readLines("../inputs/day1.txt")

numbers <- sapply(dat, function(x){
  row <- strsplit(x, "")[[1]]
  idx <- grepl("[0-9]", row)
  val <- row[idx]
  if (length(val) > 1){
    as.numeric(paste(c(val[1], val[length(val)]), collapse = ""))
  }else{
    as.numeric(paste(c(val, val), collapse = ""))
  }
})

sum(numbers)
```

    [1] 54953

The string of text can actually contain a number in written form.
Replace these and calculate the sum of the first and last combined
digits again. Note, combinations such as `eightwo` can exist, thus,
replace only the middle part to not disrupt the other numbers.

``` r
# replace letters of numbers with the numeric number in the middle
foo <- function(string){
  string <- gsub("one", "o1e", string)
  string <- gsub("two", "t2o", string)
  string <- gsub("three", "th3ee", string)
  string <- gsub("four", "f4ur", string)
  string <- gsub("five", "f5ve", string)
  string <- gsub("six", "s6x", string)
  string <- gsub("seven", "se7en", string)
  string <- gsub("eight", "ei8ht", string)
  string <- gsub("nine", "n9ne", string)
  return(string)
}

# format the first list
dat <- sapply(dat, foo)

numbers <- sapply(dat, function(x){
  row <- strsplit(x, "")[[1]]
  idx <- grepl("[0-9]", row)
  val <- row[idx]
  if (length(val) > 1){
    as.numeric(paste(c(val[1], val[length(val)]), collapse = ""))
  }else{
    as.numeric(paste(c(val, val), collapse = ""))
  }
})
sum(numbers)
```

    [1] 53868
