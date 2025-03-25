R: tips on structure, shortcuts and tidyverse
================

# Load libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.4.0     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.2.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

# Learning resources

Note that in R, there are usually multiple ways to tackle the same
problem. Subsetting, filtering, renaming, etc., can also be achieved via
base R.

-   [R for data science (Wickham&Grolemund,
    2017)](https://r4ds.had.co.nz/): a good guide to the tidyverse.
    Equally importantly, it can teach you about the phylosophy of tidy
    data, and the general “know-how” of data processing and analysis.
    The book has plenty of exercises for you to build “coding muscle”.
    You can find unofficial solutions tot he exercises
    [here](https://jrnold.github.io/r4ds-exercise-solutions/).
-   Cheatsheets covering tidyverse topics, which you can find
    [online](https://www.rstudio.com/resources/cheatsheets/), or in
    RStudio itself (Help \> Cheatsheets).
-   [Crawley
    (2013)](https://www.cs.upc.edu/~robert/teaching/estadistica/TheRBook.pdf):
    complete reference for basic coding, mathematics, statistics, etc.

# Workflow of the script

## Dataset: a modified mtcars

I will demonstrate most of the tidyverse functions using the `mtcars`
dataset, containing 11 aspects of automobile design and performance for
32 automobiles. Type `?mtcars` if you want to learn more about this data
frame.

``` r
summary(mtcars)
```

    ##       mpg             cyl             disp             hp       
    ##  Min.   :10.40   Min.   :4.000   Min.   : 71.1   Min.   : 52.0  
    ##  1st Qu.:15.43   1st Qu.:4.000   1st Qu.:120.8   1st Qu.: 96.5  
    ##  Median :19.20   Median :6.000   Median :196.3   Median :123.0  
    ##  Mean   :20.09   Mean   :6.188   Mean   :230.7   Mean   :146.7  
    ##  3rd Qu.:22.80   3rd Qu.:8.000   3rd Qu.:326.0   3rd Qu.:180.0  
    ##  Max.   :33.90   Max.   :8.000   Max.   :472.0   Max.   :335.0  
    ##       drat             wt             qsec             vs        
    ##  Min.   :2.760   Min.   :1.513   Min.   :14.50   Min.   :0.0000  
    ##  1st Qu.:3.080   1st Qu.:2.581   1st Qu.:16.89   1st Qu.:0.0000  
    ##  Median :3.695   Median :3.325   Median :17.71   Median :0.0000  
    ##  Mean   :3.597   Mean   :3.217   Mean   :17.85   Mean   :0.4375  
    ##  3rd Qu.:3.920   3rd Qu.:3.610   3rd Qu.:18.90   3rd Qu.:1.0000  
    ##  Max.   :4.930   Max.   :5.424   Max.   :22.90   Max.   :1.0000  
    ##        am              gear            carb      
    ##  Min.   :0.0000   Min.   :3.000   Min.   :1.000  
    ##  1st Qu.:0.0000   1st Qu.:3.000   1st Qu.:2.000  
    ##  Median :0.0000   Median :4.000   Median :2.000  
    ##  Mean   :0.4062   Mean   :3.688   Mean   :2.812  
    ##  3rd Qu.:1.0000   3rd Qu.:4.000   3rd Qu.:4.000  
    ##  Max.   :1.0000   Max.   :5.000   Max.   :8.000

Note that the `mtcars` dataset is a well-behaved, tidy dataset. In the
real world, datasets are usually messy (sometimes through the fault of
others, sometimes through our own). They might contain missing cases
and/or duplicated observations and/or weird formats and/or whatever
problem you can imagine.

Since untidy datasets are so prevalent, we will work with a more
realistic dataset: I will add `NA`, `NaN` and `Inf`. In order to
demonstrate column selection based on the type of information stored in
the column, I add a column with characters (the original dataset
contains only columns with numbers).

``` r
# For simplicity and clarity, let's keep only a few columns, and a few rows
data <- mutate(mtcars, something = "a") %>%
  dplyr::select(mpg, cyl, disp, carb, something) %>% 
  dplyr::slice(1:10)
data[c(1:2, 5), 1] <- NA
data[c(2:3, 6), 2] <- NaN
data[4:6, 3] <- Inf
data
```

    ##                    mpg cyl  disp carb something
    ## Mazda RX4           NA   6 160.0    4         a
    ## Mazda RX4 Wag       NA NaN 160.0    4         a
    ## Datsun 710        22.8 NaN 108.0    1         a
    ## Hornet 4 Drive    21.4   6   Inf    1         a
    ## Hornet Sportabout   NA   8   Inf    2         a
    ## Valiant           18.1 NaN   Inf    1         a
    ## Duster 360        14.3   8 360.0    4         a
    ## Merc 240D         24.4   4 146.7    2         a
    ## Merc 230          22.8   4 140.8    2         a
    ## Merc 280          19.2   6 167.6    4         a

## The pipe `%>%`

I will demonstrate functions using pipes: `%>%`. The pipe `%>%` passes
the object on the left to the first argument of the function on the
right. The dot `.` can be used to specify the argument to which the
object is passed. Check the `pull()` function lower in the script for an
example of the use of the dot as placeholder.

Throughout the script, I will demonstrate the effect of the different
functions by using the `structure()` function, or `str()` for short.
Instead of writing: `str(data)`, I will use the pipe:

``` r
data %>% 
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

Also note, the following functions will be demonstrated with a data
frame, and there are additional formats for tabular data, such as tibble
and data.table.

# Tidyverse: a few tips focusing on column operations

## Tidy selection of variables (`dplyr::select` + selection helpers)

The `dplyr::select()` takes a data frame and returns a data frame (or
takes a tibble and returns a tibble, etc), allowing to select columns by
name and/or numerical position. If you select a singe column, you will
get a data frame with a single column, not a vector (see the `pull()`
function below to get vectors from data frames). Note that the order in
which variables are selected is preserved in the resulting data frame.

``` r
data %>%
  dplyr::select(mpg) %>%
  str()
```

    ## 'data.frame':    10 obs. of  1 variable:
    ##  $ mpg: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2

``` r
# use the minus sign to remove variables
data %>%
  dplyr::select(- mpg) %>%
  str()
```

    ## 'data.frame':    10 obs. of  4 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# use c() to (de)select multiple columns
data %>%
  dplyr::select(-c(mpg, cyl)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# Use numerical position of columns. 1:3 is interpreted as range of consecutive values: from 1 to 3 including all integers in between
data %>%
  dplyr::select(1:3) %>%
  str()
```

    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ mpg : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp: num  160 160 108 Inf Inf ...

``` r
# Remove columns base on position
data %>%
  dplyr::select(-c(1:3)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# Combine numbers and names
data %>%
  dplyr::select(1, disp) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ mpg : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ disp: num  160 160 108 Inf Inf ...

### Selection helpers

The use of `dplyr::select()` can be enhanced through selection helpers,
of which you can see a non-exhaustive selection below. To learn more
about selection of variables: `?dplyr_tidy_select` Note that the default
setting is `ignore.case = TRUE`, which means the default behavior is not
case-sensitive.

``` r
data %>%
  dplyr::select(everything()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
data %>%
  dplyr::select(starts_with("c")) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ cyl : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4

``` r
data %>%
  dplyr::select(ends_with("c")) %>%
  str()
```

    ## 'data.frame':    10 obs. of  0 variables

``` r
data %>%
  dplyr::select(contains("c")) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ cyl : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4

``` r
data %>%
  dplyr::select(contains("C")) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ cyl : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4

``` r
# use the ignore.case argument to specify a case-sensitive behaviour
data %>%
  dplyr::select(contains("C", ignore.case = FALSE)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  0 variables

### Selection from a character vector (`all_of`, `any_of`)

In some cases, you might store the variables of interest in a character
vector. The functions `all_of()` to `any_of()`, get the work done, but
have slighlty different properties. The `all_of()` throws error if at
least one element in the character vector is absent from the dataset

``` r
my_selection <- c("mpg", "disp")
my_other_selection <- c("mpg", "gangman_style")

# all_of() for strict selection
data %>%
  dplyr::select(all_of(my_selection)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ mpg : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ disp: num  160 160 108 Inf Inf ...

``` r
# use the minus sign to remove a selection
data %>%
  dplyr::select(-all_of(my_selection)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  3 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# we get an error message if the character vector contains names that are absent from the column names
data %>%
  dplyr::select(-all_of(my_other_selection)) %>%
  str()
```

    ## Error in `dplyr::select()`:
    ## ! Can't subset columns that don't exist.
    ## ✖ Column `gangman_style` doesn't exist.

On the other hand, `any_of()` doesn’t check for missing variables.

``` r
data %>% 
  dplyr::select(any_of(my_selection)) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ mpg : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ disp: num  160 160 108 Inf Inf ...

``` r
# Won't give error message if a name is missing
data %>%
  dplyr::select(any_of(my_other_selection)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  1 variable:
    ##  $ mpg: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2

``` r
# use minus sign to remove variables
data %>%
  dplyr::select(-any_of(my_other_selection)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  4 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

### Selection based on logical operations (`where`)

You can also select columns using functions: the `where()` selection
helper selects the variables for which a function returns `TRUE`.
Specifically, `where()` takes as argument a function that returns `TRUE`
or `FALSE`, or purr-like formula (read the documentation to find out
more). Mind that since we are dealing with logical operations, you
should use the exclamation mark `!` rather than the minus `-` if you
want to use `where()` to remove (= de-select) columns. You could use
that to select only columns belonging to a certain type, or that have a
mean above a threshold (see documentation for an example).

``` r
# keep only columns of type character
data %>% 
  dplyr::select(where(is.character)) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  1 variable:
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# keep columns that are not character
data %>% 
  dplyr::select(!where(is.character)) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  4 variables:
    ##  $ mpg : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp: num  160 160 108 Inf Inf ...
    ##  $ carb: num  4 4 1 1 2 1 4 2 2 4

``` r
# keep columns that are character and another variable (selected by name in this instance)
data %>% 
  dplyr::select(where(is.character), mpg) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ something: chr  "a" "a" "a" "a" ...
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2

``` r
# Note that if we reverse the order of the selection, the order of the columns changes
data %>% 
  dplyr::select(mpg, where(is.character)) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  2 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ something: chr  "a" "a" "a" "a" ...

## Extract a single column as a vector (`pull`)

While the `dplyr::select()` function takes a data frame and returns a
data frame, the `pull()` function takes a data frame and returns a
vector. This makes it quite similar to the dollar operator `$`. You can
also use the dollar operator in pipes, relying on the dot `.` as
placeholder. Remember, in R, the same thing can be achieved in many
different ways.

``` r
# 4 different ways to get the same vector from a data.frame

data %>%
  pull(mpg)
```

    ##  [1]   NA   NA 22.8 21.4   NA 18.1 14.3 24.4 22.8 19.2

``` r
data$mpg
```

    ##  [1]   NA   NA 22.8 21.4   NA 18.1 14.3 24.4 22.8 19.2

``` r
data %>%
  .$mpg
```

    ##  [1]   NA   NA 22.8 21.4   NA 18.1 14.3 24.4 22.8 19.2

``` r
data %>% 
  dplyr::select(mpg) %>%
  unlist() %>%
  as.numeric()
```

    ##  [1]   NA   NA 22.8 21.4   NA 18.1 14.3 24.4 22.8 19.2

## Create new variables (`mutate`)

The mutate function will be one of your best friends. To be brief, it
creates new variables (i.e. columns). To be less brief, it takes a data
frame, and returns a data frame with columns that are transformed in
some way. Note that the examples below are concerned with creating new
variables operating on the columns, to create new variables operating on
rows (such as creating summaries per group such as the mean or the
number of observations, among others), you might want to look into
`group_by`, `summarise` (`summarize` is a synonym), `rowwise`, etc.

``` r
# Sum numerical variables
data %>%
  mutate(pointless_sum = mpg + cyl + disp + carb) %>%
  print()
```

    ##                    mpg cyl  disp carb something pointless_sum
    ## Mazda RX4           NA   6 160.0    4         a            NA
    ## Mazda RX4 Wag       NA NaN 160.0    4         a           NaN
    ## Datsun 710        22.8 NaN 108.0    1         a           NaN
    ## Hornet 4 Drive    21.4   6   Inf    1         a           Inf
    ## Hornet Sportabout   NA   8   Inf    2         a            NA
    ## Valiant           18.1 NaN   Inf    1         a           NaN
    ## Duster 360        14.3   8 360.0    4         a         386.3
    ## Merc 240D         24.4   4 146.7    2         a         177.1
    ## Merc 230          22.8   4 140.8    2         a         169.6
    ## Merc 280          19.2   6 167.6    4         a         196.8

``` r
# Don't be shy to put transformations in the sum!
data %>%
  mutate(even_more_pointless_sum = mpg + log10(cyl) + sqrt(disp)) %>%
  print()
```

    ##                    mpg cyl  disp carb something even_more_pointless_sum
    ## Mazda RX4           NA   6 160.0    4         a                      NA
    ## Mazda RX4 Wag       NA NaN 160.0    4         a                     NaN
    ## Datsun 710        22.8 NaN 108.0    1         a                     NaN
    ## Hornet 4 Drive    21.4   6   Inf    1         a                     Inf
    ## Hornet Sportabout   NA   8   Inf    2         a                      NA
    ## Valiant           18.1 NaN   Inf    1         a                     NaN
    ## Duster 360        14.3   8 360.0    4         a                34.17676
    ## Merc 240D         24.4   4 146.7    2         a                37.11404
    ## Merc 230          22.8   4 140.8    2         a                35.26798
    ## Merc 280          19.2   6 167.6    4         a                32.92419

``` r
# Apply a function, such as a logarithm
data %>%
  mutate(mpg_log10 = log10(disp)) %>%
  print()
```

    ##                    mpg cyl  disp carb something mpg_log10
    ## Mazda RX4           NA   6 160.0    4         a  2.204120
    ## Mazda RX4 Wag       NA NaN 160.0    4         a  2.204120
    ## Datsun 710        22.8 NaN 108.0    1         a  2.033424
    ## Hornet 4 Drive    21.4   6   Inf    1         a       Inf
    ## Hornet Sportabout   NA   8   Inf    2         a       Inf
    ## Valiant           18.1 NaN   Inf    1         a       Inf
    ## Duster 360        14.3   8 360.0    4         a  2.556303
    ## Merc 240D         24.4   4 146.7    2         a  2.166430
    ## Merc 230          22.8   4 140.8    2         a  2.148603
    ## Merc 280          19.2   6 167.6    4         a  2.224274

``` r
# Change the type of variable
data %>%
  mutate(mpg = as.character(mpg)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : chr  NA NA "22.8" "21.4" ...
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# Add row numbers
data %>%
  mutate(row.n = row_number()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  6 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...
    ##  $ row.n    : int  1 2 3 4 5 6 7 8 9 10

### Mutate multiple columns at the same time (`across`)

But what if we wanted to apply a function to many columns at once?
`across(.cols = everything(), .fns)` to the rescue! You can combine with
the helpers for tidy selection you have seen in the previous section.

``` r
# apply a function to all columns (default behavior)
data %>%
  mutate(across(.fns = as.character)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : chr  NA NA "22.8" "21.4" ...
    ##  $ cyl      : chr  "6" "NaN" "NaN" "6" ...
    ##  $ disp     : chr  "160" "160" "108" "Inf" ...
    ##  $ carb     : chr  "4" "4" "1" "1" ...
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# apply a function to the columns whose name meets a condition
data %>%
  mutate(across(.cols = starts_with("c"), .fns = as.character)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : chr  "6" "NaN" "NaN" "6" ...
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : chr  "4" "4" "1" "1" ...
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# apply a function to columns selected by name
data %>%
  mutate(across(.cols = c(mpg, cyl, disp), .fns = sqrt)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 4.77 4.63 NA ...
    ##  $ cyl      : num  2.45 NaN NaN 2.45 2.83 ...
    ##  $ disp     : num  12.6 12.6 10.4 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# write a function yourself
data %>%
  mutate(across(.cols = !where(is.character), .fns = function(x) x/10 + 5)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 7.28 7.14 NA 6.81 6.43 7.44 7.28 6.92
    ##  $ cyl      : num  5.6 NaN NaN 5.6 5.8 NaN 5.8 5.4 5.4 5.6
    ##  $ disp     : num  21 21 15.8 Inf Inf ...
    ##  $ carb     : num  5.4 5.4 5.1 5.1 5.2 5.1 5.4 5.2 5.2 5.4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
data %>%
  mutate(across(.cols = !where(is.character), .fns = function(x) round(x/10 + 5, digits = 0))) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 7 7 NA 7 6 7 7 7
    ##  $ cyl      : num  6 NaN NaN 6 6 NaN 6 5 5 6
    ##  $ disp     : num  21 21 16 Inf Inf ...
    ##  $ carb     : num  5 5 5 5 5 5 5 5 5 5
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# apply a function and provide a regular name for the resulting variables
data %>%
  mutate(across(.cols = c(mpg, cyl, disp), .fns = sqrt, .names = "{.col}.sqrt")) %>%
  str()
```

    ## 'data.frame':    10 obs. of  8 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...
    ##  $ mpg.sqrt : num  NA NA 4.77 4.63 NA ...
    ##  $ cyl.sqrt : num  2.45 NaN NaN 2.45 2.83 ...
    ##  $ disp.sqrt: num  12.6 12.6 10.4 Inf Inf ...

### Adding and removing `NA`

Sometimes we might wish to transform a certain value into `NA`. You may
use `na_if(x, y)` for that, which comes with two arguments: a vector to
modify (`x`) and the value to replace with `NA` (`y`).

``` r
# check the variables cyl and carb: the number 4 has been replaced by NA
# the second argument to na_if (y) is separated by a comma, although you could avoid that by writing it as a function
data %>%
  mutate(across(.cols = starts_with("c"), .fns = na_if, y = 4)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 NA NA 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  NA NA 1 1 2 1 NA 2 2 NA
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
data %>%
  mutate(across(.cols = starts_with("c"), .fns = function(whatev) na_if(x = whatev, y = 4))) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 NA NA 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  NA NA 1 1 2 1 NA 2 2 NA
    ##  $ something: chr  "a" "a" "a" "a" ...

In some other cases, we might want to replace `NA` with a certain value.
You may use `na_replace(data, replace)` for that, which comes with two
arguments: data frame or vector (`data`) and a list of values or a
single value to replace the `NA`, depending on whether the input is a
data frame or vector (`replace`).

``` r
# note that there might be easier, more elegant, and more tidy-versy ways to calculate how many values are NA per column
data %>%
  apply(2, is.na) %>%
  apply(2, sum)
```

    ##       mpg       cyl      disp      carb something 
    ##         3         3         0         0         0

``` r
# replace those NA with 9000. Note the second argument is separated by a comma
data %>%
  mutate(across(.cols = 1:3, .fns = replace_na, 9000)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  9000 9000 22.8 21.4 9000 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 9000 9000 6 8 9000 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

## Sorting a data frame (`arrange()`, `desc()`)

The `arrange()` function takes a data frame, and returns a data frame in
which the rows are sorted in ascending order (from small to big) based
on some column(s). To sort in descending order (from big to small),
combine `arrange()` with the `desc()` function:
`arrange(data, desc(column))`.

Note that `NA` are sorted to the end!

The base function `sort()` has a slightly different behavior

``` r
# sort data frame in ascending order of miles per gallon (mpg)
data %>%
  arrange(mpg)
```

    ##                    mpg cyl  disp carb something
    ## Duster 360        14.3   8 360.0    4         a
    ## Valiant           18.1 NaN   Inf    1         a
    ## Merc 280          19.2   6 167.6    4         a
    ## Hornet 4 Drive    21.4   6   Inf    1         a
    ## Datsun 710        22.8 NaN 108.0    1         a
    ## Merc 230          22.8   4 140.8    2         a
    ## Merc 240D         24.4   4 146.7    2         a
    ## Mazda RX4           NA   6 160.0    4         a
    ## Mazda RX4 Wag       NA NaN 160.0    4         a
    ## Hornet Sportabout   NA   8   Inf    2         a

``` r
# sort data frame in descending order of miles per gallon (mpg)
data %>%
  arrange(desc(mpg))
```

    ##                    mpg cyl  disp carb something
    ## Merc 240D         24.4   4 146.7    2         a
    ## Datsun 710        22.8 NaN 108.0    1         a
    ## Merc 230          22.8   4 140.8    2         a
    ## Hornet 4 Drive    21.4   6   Inf    1         a
    ## Merc 280          19.2   6 167.6    4         a
    ## Valiant           18.1 NaN   Inf    1         a
    ## Duster 360        14.3   8 360.0    4         a
    ## Mazda RX4           NA   6 160.0    4         a
    ## Mazda RX4 Wag       NA NaN 160.0    4         a
    ## Hornet Sportabout   NA   8   Inf    2         a

## Change column order (`relocate`)

The `dplyr::relocate()` function takes a data frame, and returns a data
frame with reordered columns. Use either `.before` or `.after` arguments
to indicate the position (relative to some column) where you want to
relocate your column(s). Combine `.before` or `.after` with `last_col()`
to specify a relocated position relative to the last column. You can use
tidy selection functions introduced in previous sections.

``` r
data %>%
  dplyr::relocate(mpg, .after = cyl) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# Note the use of last_col() and the difference between .after and .before
data %>%
  dplyr::relocate(mpg, .after = last_col()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2

``` r
data %>%
  dplyr::relocate(mpg, .before = last_col()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# combine this with tidy selection
data %>%
  dplyr::relocate(starts_with("c"), .after = last_col()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ something: chr  "a" "a" "a" "a" ...
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ carb     : num  4 4 1 1 2 1 4 2 2 4

## Change column name (`rename` or `select`)

The `rename()` takes a data frame, and returns a data frame with changed
column names, like this: `new_name = old_name`.

``` r
data %>%
  rename(miles.per.gallon = mpg) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ miles.per.gallon: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl             : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp            : num  160 160 108 Inf Inf ...
    ##  $ carb            : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something       : chr  "a" "a" "a" "a" ...

You may use quotations to handle names that contain blank spaces,
although it is not advisable to include blank spaces in variables names

``` r
data %>%
  rename("miles per gallon" = mpg) %>%
  str()

# this won't work
data %>%
  rename(miles per gallon = mpg) %>%
  str()
```

    ## Error: <text>:7:16: unexpected symbol
    ## 6: data %>%
    ## 7:   rename(miles per
    ##                   ^

This can be enhanced with `rename_with(.cols = everything(), .fn)` and
tidy selection, and applying regular expressions for the manipulation of
strings.

``` r
# change all names to uppercase, .cols defaults to everything()
data %>%
  rename_with(.fn = toupper) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ MPG      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ CYL      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ DISP     : num  160 160 108 Inf Inf ...
    ##  $ CARB     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ SOMETHING: chr  "a" "a" "a" "a" ...

``` r
# this alternative syntax returns the same result
data %>%
  rename_with(.fn = ~toupper(.)) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ MPG      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ CYL      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ DISP     : num  160 160 108 Inf Inf ...
    ##  $ CARB     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ SOMETHING: chr  "a" "a" "a" "a" ...

``` r
# change a few selected names to uppercase
data %>%
  rename_with(.cols = starts_with("c"), .fn = toupper) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ CYL      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ CARB     : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# use regular expressions
data %>% 
  rename_with(.cols = everything(), .fn = ~ str_replace(string = ., pattern = "a", replacement = "AAA")) %>% 
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ cAAArb   : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

``` r
# alternative syntax with the same result
data %>% 
  rename_with(.cols = everything(), .fn = str_replace, pattern = "a", replacement = "AAA") %>% 
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ mpg      : num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl      : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp     : num  160 160 108 Inf Inf ...
    ##  $ cAAArb   : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something: chr  "a" "a" "a" "a" ...

The `select()` function can also be used to change the name of selected
columns, similarly to `rename`, specifying `new_name = old_name`.

``` r
data %>%
  dplyr::select(miles.per.galon = mpg) %>%
  str()
```

    ## 'data.frame':    10 obs. of  1 variable:
    ##  $ miles.per.galon: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2

``` r
data %>%
  dplyr::select(miles.per.galon = mpg, everything()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ miles.per.galon: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl            : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp           : num  160 160 108 Inf Inf ...
    ##  $ carb           : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something      : chr  "a" "a" "a" "a" ...

``` r
data %>%
  dplyr::select("miles per galon" = mpg, everything()) %>%
  str()
```

    ## 'data.frame':    10 obs. of  5 variables:
    ##  $ miles per galon: num  NA NA 22.8 21.4 NA 18.1 14.3 24.4 22.8 19.2
    ##  $ cyl            : num  6 NaN NaN 6 8 NaN 8 4 4 6
    ##  $ disp           : num  160 160 108 Inf Inf ...
    ##  $ carb           : num  4 4 1 1 2 1 4 2 2 4
    ##  $ something      : chr  "a" "a" "a" "a" ...

# Shortcuts

For a complete list of shortcuts: Tools \> Keyboard Shortcuts Help. From
the whole range of shortcuts, I highlight the following:

| Shortcut               | Result                                  |
|------------------------|-----------------------------------------|
| Control + Enter        | Run code (line or selection)            |
| Control + Shift + r    | Create and name section                 |
| Control + Shift + a    | Format code (line or selection)         |
| Control + Shift + c    | Comment code (line or selection)        |
| Control + Shift + m    | Write pipe `%>%`                        |
| ts + Tabulator + Enter | Create a section with the date and hour |

Here’s an extra tip: when you create an object, wrap the line in
parentheses to print the created object. This removes the need to use an
additional line of code to `print()`the object, since objects are not
printed when they are created.

``` r
sample.df <- data.frame(Var1 = c(1, 2, 3), Var2 = c(1, 2, 3))
```

``` r
(sample.df <- data.frame(Var1 = c(1, 2, 3), Var2 = c(1, 2, 3)))
```

    ##   Var1 Var2
    ## 1    1    1
    ## 2    2    2
    ## 3    3    3

# Structuring your code

I like to start my scripts indicating when the script was created, as
well as adding timestamps if the code has been revised substantially.
Moreover, creating numbered sections with informative titles helps a lot
to give order and order to the code.

-   Create time stamps (`ts + Tabulator + Enter`). This will create a
    section with the date and hour
-   Create section (`Control + Shift + r`). Note that sections can be
    created by writing 5 or more \#

Moreover, it can be a good idea to follow a consistent order in all
scripts. For instance, start the script by loading libraries and files.
In addition, you may store the libraries, and even paths to the
different files, in a separate scripts, and run them at the beginning of
the code through the `source()` function.
