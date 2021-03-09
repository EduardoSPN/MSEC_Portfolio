---
title: "Homework 4"
author: '[Group names]'
date: "9/26/2019"
output: 
  html_document:
    keep_md: yes
    df_print: paged
---



## Packages


```r
library(tidyverse)
```

## Data


```r
nasa <- readRDS(file = "nasa.rds")
```



## Task 1

The data is in the form of a deeply nested lists, so the first step will be to transform each entry into an atomic character vector with named elements.

```r
# Flattens a sample into an atomic vector, recursively
# applying a flattening function to each variable within it
flatten.sample <- function(sample) {
  
  return ((unlist(map(sample, flatten.variable))))
}

# Given a variable entry for one sample, flattens out all sub-elements
# If a variable is stored as a list, recurses down and flattens
flatten.variable = function(var) {
  
  if (typeof(var) == "list") {
    return (unlist(map(var,
                       ~str_c(as.character(.x), collapse = ", ")))) 
  }
  else {
    return (as.character(var))
  }
}
nasa1 <- map(nasa, flatten.sample)
```

Once this is done, we must also make sure each sample has the same name and number of variables before the list can be converted to a dataframe.

```r
# Each sample may have slightly different number of variables
map_dbl(nasa1, length)[1:100]
```

```
  [1] 12 12 12 12 14 14 12 12 12 12 12 12 12 14 12 12 12 12 12 12 12 12 11
 [24] 12 12 12 12 12 12 12 12 12 12 14 12 12 12 12 14 12 14 12 12 12 14 12
 [47] 12 12 14 12 12 12 12 12 12 12 12 12 12 12 12 12 14 14 12 12 12 13 12
 [70] 14 12 14 12 12 12 12 12 12 12 11 12  8 12 12 12 12 12 12 12 12 12 12
 [93] 12 12 14 12 12 12 11 11
```

```r
# Get the unique variable names across all samples
unique.names <- unique(unlist(map(nasa1, names)))
unique.names
```

```
 [1] "name"                        "id"                         
 [3] "nametype"                    "recclass"                   
 [5] "mass"                        "fall"                       
 [7] "year"                        "reclat"                     
 [9] "reclong"                     "geolocation.type"           
[11] "geolocation.coordinates"     "features"                   
[13] ":@computed_region_cbhk_fwbd" ":@computed_region_nnqa_25f4"
```

```r
# Given a potential variable name, adds it to a sample if necessary
# Returns the modified sample vector
add.variable <- function(sample, var.name) {
  
  if (!(var.name %in% names(sample))) {
    sample[var.name] <- NA
  }
  return (sample)
}

nasa2 <- nasa1
for (var.name in unique.names) {
  nasa2 <- map(nasa2, add.variable, var.name)
}
# See how the number of variables in each sample has changed
map_dbl(nasa2, length)[1:100]
```

```
  [1] 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
 [24] 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
 [47] 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
 [70] 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14 14
 [93] 14 14 14 14 14 14 14 14
```

Now, all that is needed is to row-bind each sample together.  However, since row binding requires a list of lists, the samples must first be converted back to lists in order to combine them into a data frame.

```r
# Convert each sample back to a list so that row-binding works properly
nasa.df <- map_dfr(nasa2, as.list)
```


```r
change_names <- function(data, old_names, new_names){
  change_single_name <- function(names, old_name, new_name){
    names[names == old_name] <- new_name
    return(names)
  }
  tmp <- accumulate2(old_names, new_names, change_single_name, .init = colnames(data))
  colnames(data) <- tmp[[length(tmp)]]
  return (data)
}
```


```r
nasa.df <- nasa.df %>% change_names(c("geolocation.type", "geolocation.coordinates"),
                         c("geo_type", "geo_coord"))
head(nasa.df)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["id"],"name":[2],"type":["chr"],"align":["left"]},{"label":["nametype"],"name":[3],"type":["chr"],"align":["left"]},{"label":["recclass"],"name":[4],"type":["chr"],"align":["left"]},{"label":["mass"],"name":[5],"type":["chr"],"align":["left"]},{"label":["fall"],"name":[6],"type":["chr"],"align":["left"]},{"label":["year"],"name":[7],"type":["chr"],"align":["left"]},{"label":["reclat"],"name":[8],"type":["chr"],"align":["left"]},{"label":["reclong"],"name":[9],"type":["chr"],"align":["left"]},{"label":["geo_type"],"name":[10],"type":["chr"],"align":["left"]},{"label":["geo_coord"],"name":[11],"type":["chr"],"align":["left"]},{"label":["features"],"name":[12],"type":["chr"],"align":["left"]},{"label":[":@computed_region_cbhk_fwbd"],"name":[13],"type":["chr"],"align":["left"]},{"label":[":@computed_region_nnqa_25f4"],"name":[14],"type":["chr"],"align":["left"]}],"data":[{"1":"Djermaia","2":"7656","3":"Valid","4":"H","5":"3950","6":"Fell","7":"1925-02-03 06:59:59","8":"12.733330","9":"15.050000","10":"Point","11":"15.05, 12.73333","12":"grey, some holes, smooth, irregular, thin lines, black, high in iron and nickel, shiny","13":"NA","14":"NA"},{"1":"Elbogen","2":"7823","3":"Valid","4":"Iron, IID","5":"107000","6":"Fell","7":"1400-01-01TTT00:00:00.000","8":"50.183330","9":"12.733330","10":"Point","11":"12.73333, 50.18333","12":"black, smooth, thin lines, shiny, irregular, some holes","13":"NA","14":"NA"},{"1":"Pacula","2":"18068","3":"Valid","4":"L6","5":"3400","6":"Fell","7":"1925-02-03 07:35:03","8":"21.050000","9":"-99.300000","10":"Point","11":"-99.3, 21.05","12":"high in iron and nickel","13":"NA","14":"NA"},{"1":"Bali","2":"4928","3":"Valid","4":"CV3","5":"1000","6":"Fell","7":"1907-01-01TTT00:00:00.000","8":"5.383330","9":"16.383330","10":"Point","11":"16.38333, 5.38333","12":"__NA__","13":"NA","14":"NA"},{"1":"Benld","2":"5021","3":"Valid","4":"H6","5":"1770.5","6":"Fell","7":"1925-02-03 07:15:37","8":"39.083330","9":"-89.150000","10":"Point","11":"-89.15, 39.08333","12":"grey, smooth, thin lines","13":"34","14":"1869"},{"1":"Lumpkin","2":"14753","3":"Valid","4":"L6","5":"-340","6":"Fell","7":"1869-01-01T00:00:00.000","8":"32.033330","9":"-84.766670","10":"Point","11":"-84.76667, 32.03333","12":"black, high in iron and nickel, smooth, some holes, grey, thin lines, shiny, irregular","13":"31","14":"1567"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Task 2

Now that we have an actual dataframe, we can look at each of the columns individually and see what specific concerns need addressed in each.
<!-- explore data -->

```r
#name has spaces on the left
print(head(nasa.df)$name)
```

```
[1] "            Djermaia" "             Elbogen" "              Pacula"
[4] "                Bali" "               Benld" "             Lumpkin"
```

```r
#nametype - only Valid observation
summary(factor(nasa.df$nametype))
```

```
Valid 
 1003 
```

```r
#recclass - keep it
#mass - NA 28s - ++(have to remove ++)
mass <- as.numeric(nasa.df$mass)
nasa.df$mass[is.na(mass)]
```

```
 [1] NA        NA        NA        NA        NA        NA        NA       
 [8] NA        NA        NA        NA        NA        NA        NA       
[15] NA        NA        NA        NA        NA        NA        NA       
[22] "++56000" NA        NA        NA        NA        NA        NA       
[29] "++780"   NA       
```

```r
#fall - Fell 999, Found 4
summary(factor(nasa.df$fall))
```

```
 Fell Found 
  999     4 
```

```r
#find everywhere where geo_coord has an X included
a <- nasa.df$geo_coord %>% str_split(pattern = ", ") %>% lapply(as.numeric)
index <- sapply(lapply(a, is.na),sum) == 2
nasa.df[index,"geo_coord"]
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["geo_coord"],"name":[1],"type":["chr"],"align":["left"]}],"data":[{"1":"X-90.23333, X38.7"},{"1":"xx-11.34133, xx17.17493"},{"1":"xx84.78333, xx26.75"},{"1":"xx-74, xx40.25"},{"1":"xx-7.33333, xx53.53333"},{"1":"X82.05, X20.93694"},{"1":"xx-86.16667, xx38.25"},{"1":"X5.36667, X47.71667"},{"1":"xx8.91667, xx50.3"},{"1":"xx-104.9, xx40.35"},{"1":"xx-8.03333, xx52.55"},{"1":"X59.685, X41.98444"},{"1":"X4, X51.75"},{"1":"xx111.75, xx29.08333"},{"1":"xx-13.1, xx24.3"},{"1":"X1.06667, X47.7"},{"1":"xx131.56667, xx34.2"},{"1":"xx7.53333, xx49.98333"},{"1":"X-100.81667, X19.86667"},{"1":"xx73.11528, xx26.50833"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Below are some helper functions that can be used to clean the data based on this analysis.
<!-- - How about using str_remove instead of str_replace -->

```r
remove_blanks <- function(vec){
  remove_blanks_str <- function(str) str_replace(str, "^\\s*", "")
  return (sapply(vec, remove_blanks_str))
}
```


```r
clear_na <- function(vec){
  na_reps <- c("NA", "unknown", "n/a")
  return (ifelse(vec %in% na_reps, NA, vec))
}
```


```r
in_range <- function(vec, min = -Inf, max = Inf){
  return (ifelse(vec < min | vec > max, NA, vec))
}
```

Since some observations included obvious errors, we cleaned the data using the information gained from exploring the data above.


```r
nasa.temp.df <- nasa.df %>%
  #remove blank before names
  mutate(name = name %>% remove_blanks) %>% 
  #remove + sign included in mass and convert negative mass into NA
  mutate(mass = str_remove_all(mass,"[\\+]+") %>% in_range(min = 0)) %>%
  #remove X,x sign included in geo_coord
  mutate(geo_coord = str_remove_all(geo_coord,"[xX]+")) %>% 
  #convert various expression represent missing value into NA
  mutate(features = features %>% clear_na) %>%
  #remove 0.000 in second
  mutate(year = str_remove_all(year,"\\...."))

head(nasa.temp.df)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["id"],"name":[2],"type":["chr"],"align":["left"]},{"label":["nametype"],"name":[3],"type":["chr"],"align":["left"]},{"label":["recclass"],"name":[4],"type":["chr"],"align":["left"]},{"label":["mass"],"name":[5],"type":["chr"],"align":["left"]},{"label":["fall"],"name":[6],"type":["chr"],"align":["left"]},{"label":["year"],"name":[7],"type":["chr"],"align":["left"]},{"label":["reclat"],"name":[8],"type":["chr"],"align":["left"]},{"label":["reclong"],"name":[9],"type":["chr"],"align":["left"]},{"label":["geo_type"],"name":[10],"type":["chr"],"align":["left"]},{"label":["geo_coord"],"name":[11],"type":["chr"],"align":["left"]},{"label":["features"],"name":[12],"type":["chr"],"align":["left"]},{"label":[":@computed_region_cbhk_fwbd"],"name":[13],"type":["chr"],"align":["left"]},{"label":[":@computed_region_nnqa_25f4"],"name":[14],"type":["chr"],"align":["left"]}],"data":[{"1":"Djermaia","2":"7656","3":"Valid","4":"H","5":"3950","6":"Fell","7":"1925-02-03 06:59:59","8":"12.733330","9":"15.050000","10":"Point","11":"15.05, 12.73333","12":"grey, some holes, smooth, irregular, thin lines, black, high in iron and nickel, shiny","13":"NA","14":"NA","_row":"            Djermaia"},{"1":"Elbogen","2":"7823","3":"Valid","4":"Iron, IID","5":"107000","6":"Fell","7":"1400-01-01TTT00:00:00","8":"50.183330","9":"12.733330","10":"Point","11":"12.73333, 50.18333","12":"black, smooth, thin lines, shiny, irregular, some holes","13":"NA","14":"NA","_row":"             Elbogen"},{"1":"Pacula","2":"18068","3":"Valid","4":"L6","5":"3400","6":"Fell","7":"1925-02-03 07:35:03","8":"21.050000","9":"-99.300000","10":"Point","11":"-99.3, 21.05","12":"high in iron and nickel","13":"NA","14":"NA","_row":"              Pacula"},{"1":"Bali","2":"4928","3":"Valid","4":"CV3","5":"1000","6":"Fell","7":"1907-01-01TTT00:00:00","8":"5.383330","9":"16.383330","10":"Point","11":"16.38333, 5.38333","12":"NA","13":"NA","14":"NA","_row":"                Bali"},{"1":"Benld","2":"5021","3":"Valid","4":"H6","5":"1770.5","6":"Fell","7":"1925-02-03 07:15:37","8":"39.083330","9":"-89.150000","10":"Point","11":"-89.15, 39.08333","12":"grey, smooth, thin lines","13":"34","14":"1869","_row":"               Benld"},{"1":"Lumpkin","2":"14753","3":"Valid","4":"L6","5":"NA","6":"Fell","7":"1869-01-01T00:00:00","8":"32.033330","9":"-84.766670","10":"Point","11":"-84.76667, 32.03333","12":"black, high in iron and nickel, smooth, some holes, grey, thin lines, shiny, irregular","13":"31","14":"1567","_row":"             Lumpkin"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

With this dataframe, we can use `dplyr` to remove any extraneous columns.  Additionally, we can use the `tidyr` function `separate()` to split the `year` variable into day, month, year, hour, minute, and second components.  Several different regular expressions are needed:


```r
nasa.new.df <- nasa.temp.df %>%
  #Remove all unnecessary variables
  select(-geo_type, -':@computed_region_cbhk_fwbd', -':@computed_region_nnqa_25f4') %>%
  separate(col = year, into = c("date", "time"), sep = "(\\s|T)+") %>%
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>%
  separate(col = time, into = c("hour", "minute", "second"), sep = ":") %>%
  #Convert all numeric variables to doubles
  mutate_at(vars(id, mass, reclat, reclong,
                 year, month, day, hour, minute, second), as.numeric) %>%
  # Convert all categorical variables to factors
  mutate_at(vars(nametype, recclass, fall), as.factor) %>%
  # Convert concatenated strings (from Task 1) into lists
  mutate(features = features %>% strsplit(split = ", ")) %>%
  mutate(geo_coord = geo_coord %>% strsplit(split = ", ") %>% lapply(as.numeric))

head(nasa.new.df)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["id"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["nametype"],"name":[3],"type":["fctr"],"align":["left"]},{"label":["recclass"],"name":[4],"type":["fctr"],"align":["left"]},{"label":["mass"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["fall"],"name":[6],"type":["fctr"],"align":["left"]},{"label":["year"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["month"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["day"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["hour"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["minute"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["second"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["reclat"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["reclong"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["geo_coord"],"name":[15],"type":["list"],"align":["right"]},{"label":["features"],"name":[16],"type":["list"],"align":["right"]}],"data":[{"1":"Djermaia","2":"7656","3":"Valid","4":"H","5":"3950.0","6":"Fell","7":"1925","8":"2","9":"3","10":"6","11":"59","12":"59","13":"12.73333","14":"15.05000","15":"<dbl [2]>","16":"<chr [8]>"},{"1":"Elbogen","2":"7823","3":"Valid","4":"Iron, IID","5":"107000.0","6":"Fell","7":"1400","8":"1","9":"1","10":"0","11":"0","12":"0","13":"50.18333","14":"12.73333","15":"<dbl [2]>","16":"<chr [6]>"},{"1":"Pacula","2":"18068","3":"Valid","4":"L6","5":"3400.0","6":"Fell","7":"1925","8":"2","9":"3","10":"7","11":"35","12":"3","13":"21.05000","14":"-99.30000","15":"<dbl [2]>","16":"<chr [1]>"},{"1":"Bali","2":"4928","3":"Valid","4":"CV3","5":"1000.0","6":"Fell","7":"1907","8":"1","9":"1","10":"0","11":"0","12":"0","13":"5.38333","14":"16.38333","15":"<dbl [2]>","16":"<chr [1]>"},{"1":"Benld","2":"5021","3":"Valid","4":"H6","5":"1770.5","6":"Fell","7":"1925","8":"2","9":"3","10":"7","11":"15","12":"37","13":"39.08333","14":"-89.15000","15":"<dbl [2]>","16":"<chr [3]>"},{"1":"Lumpkin","2":"14753","3":"Valid","4":"L6","5":"NA","6":"Fell","7":"1869","8":"1","9":"1","10":"0","11":"0","12":"0","13":"32.03333","14":"-84.76667","15":"<dbl [2]>","16":"<chr [8]>"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>



## Task 3

__In Task 1, we decided to flatten the many nested lists into one list, filled with atomic character vectors with named elements (one character vector per meteorite). List variables such as geolocation or features were unlisted, and any data stored as a vector was converted into a single string by pasting commas between all elements. As a result, ater the first step, we had a single list of character vectors -- one per meteorite. However, the length of each vector was different from one another. Thus, we had to equalize the length of each vector by finding all possible variable names among the samples, and then for each vector, adding a variable with an NA value where one did not already exist with that name. Having consistent variable names across all samples allowed for an easy conversion to dataframe, and gave us the option to easily remove irrelevant variables later.  Once each vector had equal length, we row-bound the vectors (after first converting the vectors back to lists, to conform to the requirements of `bind_rows`).  That gave us a data.frame named `nasa.df`.__ 

__In Task 2, we explored data as our first step. Some observations in each of the columns had obvious errors that needed addressing. Firstly, it seems that the `name` component was stored originally as a fixed-length string, so there are spaces padding the left side; we just removed these. Secondly, the `mass` variable had some values preceded by "++" (which does not convert well to a numeric type) and some negative values (obviously not possible for the mass of a meteorite). We concluded that "++" is probably a typo which still represented a positive value of mass and simply removed the "++" so conversion would be successful. However, we treated observation which had negative values as NAs, because we were not sure that whether these were typos. Thirdly, we found some observations included "x" or "X" characters in the `geo_coord` variable, so we decided to remove these characters. Lastly, we removed the trailing ".000" which had remained after the second component in `year` to unify formatting.  We found no issues with the range of the numeric variables.  There are a few samples with mass above 1,000,000, but we do not know if this is a normal observation or not-- this type of thinking is best left for the data analysis phase.__

__After cleaning the data, we converted `id`, `mass`, `reclat`, `reclong`, `geo_coord`, and all the time-related variables into numeric types; and we converted the `nametype`, `recclass`, and `fall` variables into factors. Also, we removed redundant variables `:@computed_region_cbhk_fwbd`, `:@computed_region_nnqa_25f4` and `geo_type` that remained from Task 1. Then, we divided the `year` variable into a temporary "date" and "time" variable, separating based on whitespace or the use of "T" characters (the way date-times were formatted in this dataset was inconsistent, but these seemed to be the only delimiters). Moreover, we divided "date" into `year`, `month`, `day` by splitting on `-` and divided "time" into `hour`, `minutes`, and `second` by splitting on `:` (these all did have consistent delimiters). Finally, we took the variables that held multiple values per row (`geo_coord` and `features`) and converted each value to a list of distinct items (this is necessary because Task 1 concatenated these individual elements into a single string).  In the end, we were left with a tidy dataframe with proper data and the requested subset of variables.__




