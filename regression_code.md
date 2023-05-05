Regression project
================
Andriani Panagi
Natalia Korai
Waqar Aziz Sulaiman
2023-05-05

## Import the data set

``` r
df <- read.csv("C:/Users/ntbna/Downloads/Spring Semester 2023/DSC532/Regression PROJECT/melb_data.csv")
head(df) # 1st six observations
```

    ##       Suburb          Address Rooms Type   Price Method SellerG      Date
    ## 1 Abbotsford     85 Turner St     2    h 1480000      S  Biggin 3/12/2016
    ## 2 Abbotsford  25 Bloomburg St     2    h 1035000      S  Biggin 4/02/2016
    ## 3 Abbotsford     5 Charles St     3    h 1465000     SP  Biggin 4/03/2017
    ## 4 Abbotsford 40 Federation La     3    h  850000     PI  Biggin 4/03/2017
    ## 5 Abbotsford      55a Park St     4    h 1600000     VB  Nelson 4/06/2016
    ## 6 Abbotsford   129 Charles St     2    h  941000      S  Jellis 7/05/2016
    ##   Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea YearBuilt
    ## 1      2.5     3067        2        1   1      202           NA        NA
    ## 2      2.5     3067        2        1   0      156           79      1900
    ## 3      2.5     3067        3        2   0      134          150      1900
    ## 4      2.5     3067        3        2   1       94           NA        NA
    ## 5      2.5     3067        3        1   2      120          142      2014
    ## 6      2.5     3067        2        1   0      181           NA        NA
    ##   CouncilArea Lattitude Longtitude            Regionname Propertycount
    ## 1       Yarra  -37.7996   144.9984 Northern Metropolitan          4019
    ## 2       Yarra  -37.8079   144.9934 Northern Metropolitan          4019
    ## 3       Yarra  -37.8093   144.9944 Northern Metropolitan          4019
    ## 4       Yarra  -37.7969   144.9969 Northern Metropolitan          4019
    ## 5       Yarra  -37.8072   144.9941 Northern Metropolitan          4019
    ## 6       Yarra  -37.8041   144.9953 Northern Metropolitan          4019

``` r
# Preprocessing
cat('Rows:', nrow(df), 'Columns:', ncol(df),'\n') # number of rows and columns
```

    ## Rows: 13580 Columns: 21

``` r
cat('Duplicates:', nrow(df[duplicated(df), ])) # check for duplicates rows
```

    ## Duplicates: 0

``` r
library(dplyr) # package for %>%
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr) # package for fill
```

## Missing Values

``` r
sapply(df, function(x) sum(is.na(x))) # number of missing values for each column
```

    ##        Suburb       Address         Rooms          Type         Price 
    ##             0             0             0             0             0 
    ##        Method       SellerG          Date      Distance      Postcode 
    ##             0             0             0             0             0 
    ##      Bedroom2      Bathroom           Car      Landsize  BuildingArea 
    ##             0             0            62             0          6450 
    ##     YearBuilt   CouncilArea     Lattitude    Longtitude    Regionname 
    ##          5375             0             0             0             0 
    ## Propertycount 
    ##             0

``` r
library('VIM')
```

    ## Warning: package 'VIM' was built under R version 4.2.3

    ## Loading required package: colorspace

    ## Loading required package: grid

    ## VIM is ready to use.

    ## Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues

    ## 
    ## Attaching package: 'VIM'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     sleep

``` r
library(dplyr)
#for the column car fill the missing values with the median for the column car/ with downup for the YearBuilt
df$Car[is.na(df$Car)] <- median(df$Car, na.rm = T)
df <- df %>% fill(YearBuilt, .direction = 'downup')
boxplot(df$Landsize, na.rm = TRUE)
```

![](regression_code_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
boxplot(df$BuildingArea, na.rm = TRUE)
```

![](regression_code_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Identify row number of extreme values
extreme_row <- which(df$Landsize > 25000) 
# Remove extreme values from the Landsize column
df <- df[-c(extreme_row), ]

# Identify row number of extreme values
extreme_row <- which(df$BuildingArea > 1800) 
# Remove extreme value from the BuildingArea column
df <- df[-c(extreme_row), ]

##Column LANDSIZE
#set as nan values the values that are less than 3 in landsize
df$Landsize[df$Landsize <= 3] <- NA 
# fill the nan values with knn
df <- kNN(df,variable = 'Landsize',dist_var = c('Rooms', 'Bathroom', 'Bedroom2', 'BuildingArea','Price','Type'),k=5)

##BUILDLING AREA
#set as nan values the values that are less than 3 in buildingarea
df$BuildingArea[df$BuildingArea <= 3] <- NA 
#fill the nan values with knn
df <- kNN(df,variable = 'BuildingArea',dist_var = c('Rooms', 'Bathroom', 'Bedroom2','BuildingArea','Price','Landsize'),k=5)
```

## Type of the columns

``` r
str(df) # check the type of each column
```

    ## 'data.frame':    13568 obs. of  23 variables:
    ##  $ Suburb          : chr  "Abbotsford" "Abbotsford" "Abbotsford" "Abbotsford" ...
    ##  $ Address         : chr  "85 Turner St" "25 Bloomburg St" "5 Charles St" "40 Federation La" ...
    ##  $ Rooms           : int  2 2 3 3 4 2 3 2 1 2 ...
    ##  $ Type            : chr  "h" "h" "h" "h" ...
    ##  $ Price           : num  1480000 1035000 1465000 850000 1600000 ...
    ##  $ Method          : chr  "S" "S" "SP" "PI" ...
    ##  $ SellerG         : chr  "Biggin" "Biggin" "Biggin" "Biggin" ...
    ##  $ Date            : chr  "3/12/2016" "4/02/2016" "4/03/2017" "4/03/2017" ...
    ##  $ Distance        : num  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 ...
    ##  $ Postcode        : num  3067 3067 3067 3067 3067 ...
    ##  $ Bedroom2        : num  2 2 3 3 3 2 4 2 1 3 ...
    ##  $ Bathroom        : num  1 1 2 2 1 1 2 1 1 1 ...
    ##  $ Car             : num  1 0 0 1 2 0 0 2 1 2 ...
    ##  $ Landsize        : num  202 156 134 94 120 181 245 256 811 220 ...
    ##  $ BuildingArea    : num  95 79 150 122 142 94 210 107 36 75 ...
    ##  $ YearBuilt       : num  1900 1900 1900 1900 2014 ...
    ##  $ CouncilArea     : chr  "Yarra" "Yarra" "Yarra" "Yarra" ...
    ##  $ Lattitude       : num  -37.8 -37.8 -37.8 -37.8 -37.8 ...
    ##  $ Longtitude      : num  145 145 145 145 145 ...
    ##  $ Regionname      : chr  "Northern Metropolitan" "Northern Metropolitan" "Northern Metropolitan" "Northern Metropolitan" ...
    ##  $ Propertycount   : num  4019 4019 4019 4019 4019 ...
    ##  $ Landsize_imp    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ BuildingArea_imp: logi  TRUE FALSE FALSE TRUE FALSE TRUE ...

## Summary statistics

``` r
# summary for the numerical features
summary(df[,-c(1,2,4,6,7,8,17,20)])
```

    ##      Rooms            Price            Distance        Postcode   
    ##  Min.   : 1.000   Min.   :  85000   Min.   : 0.00   Min.   :3000  
    ##  1st Qu.: 2.000   1st Qu.: 650000   1st Qu.: 6.10   1st Qu.:3044  
    ##  Median : 3.000   Median : 902750   Median : 9.20   Median :3084  
    ##  Mean   : 2.938   Mean   :1075494   Mean   :10.13   Mean   :3105  
    ##  3rd Qu.: 3.000   3rd Qu.:1330000   3rd Qu.:13.00   3rd Qu.:3148  
    ##  Max.   :10.000   Max.   :9000000   Max.   :47.40   Max.   :3977  
    ##     Bedroom2         Bathroom          Car            Landsize      
    ##  Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :    5.0  
    ##  1st Qu.: 2.000   1st Qu.:1.000   1st Qu.: 1.000   1st Qu.:  256.0  
    ##  Median : 3.000   Median :1.000   Median : 2.000   Median :  532.0  
    ##  Mean   : 2.914   Mean   :1.534   Mean   : 1.611   Mean   :  596.4  
    ##  3rd Qu.: 3.000   3rd Qu.:2.000   3rd Qu.: 2.000   3rd Qu.:  691.0  
    ##  Max.   :20.000   Max.   :8.000   Max.   :10.000   Max.   :21715.0  
    ##   BuildingArea      YearBuilt      Lattitude        Longtitude   
    ##  Min.   :   4.0   Min.   :1196   Min.   :-38.18   Min.   :144.4  
    ##  1st Qu.:  96.0   1st Qu.:1945   1st Qu.:-37.86   1st Qu.:144.9  
    ##  Median : 125.0   Median :1970   Median :-37.80   Median :145.0  
    ##  Mean   : 139.6   Mean   :1965   Mean   :-37.81   Mean   :145.0  
    ##  3rd Qu.: 166.0   3rd Qu.:1999   3rd Qu.:-37.76   3rd Qu.:145.1  
    ##  Max.   :1561.0   Max.   :2018   Max.   :-37.41   Max.   :145.5  
    ##  Propertycount   Landsize_imp    BuildingArea_imp
    ##  Min.   :  249   Mode :logical   Mode :logical   
    ##  1st Qu.: 4380   FALSE:11625     FALSE:7061      
    ##  Median : 6567   TRUE :1943      TRUE :6507      
    ##  Mean   : 7456                                   
    ##  3rd Qu.:10331                                   
    ##  Max.   :21650

Some columns have quite large range; BuildingArea (size of building) has
44515 range, YearBuilt’s one is 822, Landsize has 433014, the
Propertycount has 21401, as well as price has minimum 85,000 and maximum
value 9,000,000.

``` r
df <- df[,-c(22,23)] #Drop Logical Vectors
```

## Unique Values

``` r
#how many unique values each column has
number_uniques = c()
for (i in 1:21){
  number_uniques[i] = length(unique(df[,i]))
}
number_uniques = as.data.frame(number_uniques)
number_uniques$col = colnames(df)
number_uniques$index = 1:21
number_uniques
```

    ##    number_uniques           col index
    ## 1             312        Suburb     1
    ## 2           13366       Address     2
    ## 3               9         Rooms     3
    ## 4               3          Type     4
    ## 5            2203         Price     5
    ## 6               5        Method     6
    ## 7             268       SellerG     7
    ## 8              58          Date     8
    ## 9             200      Distance     9
    ## 10            196      Postcode    10
    ## 11             12      Bedroom2    11
    ## 12              9      Bathroom    12
    ## 13             11           Car    13
    ## 14           1435      Landsize    14
    ## 15            594  BuildingArea    15
    ## 16            144     YearBuilt    16
    ## 17             34   CouncilArea    17
    ## 18           6497     Lattitude    18
    ## 19           7057    Longtitude    19
    ## 20              8    Regionname    20
    ## 21            309 Propertycount    21

Suburb, Address, SellerG, CoouncilArea are categorical columns which
cannot be transformed into dummy variables, since the number of their
unique values is large. Thus, we will either drop them or make into
further processing in order to manipulate them later in the analysis.

## Outliers

``` r
# check for outliers for numerical (excluding those which has small number of unique) 
find_outliers <- function(x){
  H=1.5 * IQR(x)
  number <- sum(x < (quantile(x)[2]-H)) + sum(x > (quantile(x)[4]+H))
  number
}

df_continuous <- df[,c(5, 9, 10, 14, 15, 16, 18, 19, 21)]
outliers=c()
for (i in 1:9){
  outliers[i]=find_outliers(df_continuous[1:13568,i])
}

# percentage of outliers for each of the above columns
outliers_tois100=as.data.frame(round(((outliers/13568)*100),2)) 
# Add the name of each column
outliers_tois100$col=colnames(df_continuous) 
outliers_tois100
```

    ##   round(((outliers/13568) * 100), 2)           col
    ## 1                               4.50         Price
    ## 2                               3.00      Distance
    ## 3                               1.50      Postcode
    ## 4                               4.56      Landsize
    ## 5                               4.89  BuildingArea
    ## 6                               0.18     YearBuilt
    ## 7                               1.90     Lattitude
    ## 8                               2.98    Longtitude
    ## 9                               2.64 Propertycount

Although the outliers are less than 5% of the overall data we cannot say
if they can be considered negligible, since they are may located far
from the whiskers and generally from the other data points. However, we
could drop the very extreme ones (either the maximum or minimum ones, in
order to make data a bit more stable) and not affect much the
performance of the machine learning algorithms.

``` r
par(mfrow=c(3,3))

# Creation of boxplots for all the numerical variables
boxplot(x=df_continuous[,1],main = "Price",col = "red")
boxplot(x=df_continuous[,2],main = "Distance",col = "red")
boxplot(x=df_continuous[,3],main = "Postcode",col = "red")
boxplot(x=df_continuous[,4],main = "Landsize",col = "red")
boxplot(x=df_continuous[,5],main = "Buldingarea",col = "red")
boxplot(x=df_continuous[,6],main = "Year of Built",col = "red")
boxplot(x=df_continuous[,7],main = "Lattidute",col = "red")
boxplot(x=df_continuous[,8],main = "Longtitute",col = "red")
boxplot(x=df_continuous[,9],main = "Property count",col = "red")
```

![](regression_code_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
# Bar plot for the column car
barplot(table(df$Car), ylab= 'Number of carspots')
```

![](regression_code_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

We verify that these columns have some outliers. There are cases where
there are outliers, either much greater than the upper whisker, or much
lower than the lower whisker. Specifically, the maximum of the columns
Landsize and BuldingArea is too high, and the minimum of the YearBuilt
is quite lower than the other values. Thus, we will investigate the
behaviour of these features without these extreme values.

``` r
# boxplots without the most extreme values of the columns LandSize, BuildingArea, and YearBuilt
par(mfrow=c(1,3))
boxplot(df$Landsize[df$Landsize != max(df$Landsize)], ylab='Land size')
boxplot(df$BuildingArea[df$BuildingArea != max(df$BuildingArea)], ylab='Building Size')
boxplot(df$YearBuilt[df$YearBuilt != min(df$YearBuilt)], ylab='Year Built')
```

![](regression_code_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

However there are many outliers, there is some progress in the range of
data of these columns, especially for the column YearBuilt. We decided
to drop the rows where the Landsize is greater than or equal to 1000, or
BuildingArea is more than or equal to 600 or YearBuilt is equal to its
maximum.

``` r
# drop the most of the extreme outlies of the columns BuildingArea, Landsize, and YearBuilt
df <- df[(df$BuildingArea < 600) & (df$Landsize < 1000) & (df$YearBuilt) != min(df$YearBuilt),]
summary(df[,c(14, 15, 16)])
```

    ##     Landsize      BuildingArea     YearBuilt   
    ##  Min.   :  5.0   Min.   :  4.0   Min.   :1850  
    ##  1st Qu.:240.0   1st Qu.:100.0   1st Qu.:1940  
    ##  Median :481.0   Median :127.0   Median :1970  
    ##  Mean   :461.4   Mean   :140.3   Mean   :1964  
    ##  3rd Qu.:650.0   3rd Qu.:167.0   3rd Qu.:1997  
    ##  Max.   :999.0   Max.   :594.0   Max.   :2018

BuldingArea (size of building) has now 590 range (whereas it had 44515
before), YearBuilt has 168 (822 before) and Landsize has 994 (433014
before).

``` r
cat('Rows after dropping very extreme values:', nrow(df))
```

    ## Rows after dropping very extreme values: 12495

## Distribution of the Price

The price is positively skewed

``` r
# The histogram of the price
hist(df_continuous[,1],main="Price Frequency",xlab="Price",breaks=20)
```

![](regression_code_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# For the price QQplot
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,1], main = "Q-Q plot of price")
qqline(y = df_continuous[,1])
```

![](regression_code_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

## Distributions of the numerical columns

``` r
# histogram for the distance for the distance
par(mfrow=c(1,2))
h <- hist(df_continuous[,2],breaks = "scott",main = "Distribution ",xlab = "Distance",col = "grey",freq = TRUE)
h <- hist(df_continuous[,2],breaks = "scott",main = "Distribution ",xlab = "Distance",col = "grey",freq = FALSE)
lines(density(df_continuous[,2]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# qqplot for the distance 
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,2], main = "Q-Q plot of distance")
qqline(y = df_continuous[,2])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

``` r
# histogram for the landsize
par(mfrow=c(1,2))
h <- hist(df_continuous[,4],breaks = "scott",main = "Distribution ",xlab = "Land size",col = "grey",freq = TRUE)
h <- hist(df_continuous[,4],breaks = "scott",main = "Distribution ",xlab = "Land size",col = "grey",freq = FALSE)
lines(density(df_continuous[,4]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-3.png)<!-- -->

``` r
#qqplot for the landsize 
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,4], main = "Q-Q plot of landsize")
qqline(y = df_continuous[,4])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-4.png)<!-- -->

``` r
# histogram for the bulding area
par(mfrow=c(1,2))
h <- hist(df_continuous[,5],breaks = "scott",main = "Distribution ",xlab = "Building area",col = "grey",freq = TRUE)
h <- hist(df_continuous[,5],breaks = "scott",main = "Distribution ",xlab = "Building area",col = "grey",freq = FALSE)
lines(density(df_continuous[,5]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-5.png)<!-- -->

``` r
#qqplot for the builiding area 
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,5], main = "Q-Q plot of building area")
qqline(y = df_continuous[,5])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-6.png)<!-- -->

``` r
#histogram for year of built
par(mfrow=c(1,2))
h <- hist(df_continuous[,6],main = "Distribution ",xlab = "Year of Built",col = "grey",freq = TRUE)
h <- hist(df_continuous[,6],main = "Distribution ",xlab = "Year of Built",col = "grey",freq = FALSE)
lines(density(df_continuous[,6]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-7.png)<!-- -->

``` r
# qqplot for year of built 
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,6], main = "Q-Q plot of Year of Built")
qqline(y = df_continuous[,6])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-8.png)<!-- -->

``` r
# histogram for longtitute
par(mfrow=c(1,2))
h <- hist(df_continuous[,8],main = "Distribution ",xlab = "Longtidute",col = "grey",freq = TRUE)
h <- hist(df_continuous[,8],main = "Distribution ",xlab = "Longtidute",col = "grey",freq = FALSE)
lines(density(df_continuous[,8]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-9.png)<!-- -->

``` r
# qqplot for longtitute
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,8], main = "Q-Q plot of Longtitute")
qqline(y = df_continuous[,8])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-10.png)<!-- -->

``` r
# histogram for lattidute
par(mfrow=c(1,2))
h <- hist(df_continuous[,7],main = "Distribution ",xlab = "Lattidute",col = "grey",freq = TRUE)
h <- hist(df_continuous[,7],main = "Distribution ",xlab = "Lattidute",col = "grey",freq = FALSE)
lines(density(df_continuous[,7]), col = 2, lwd = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-11.png)<!-- -->

``` r
#qqplot for lattitute
par(mfrow=c(1,1))
qqnorm(y =df_continuous[,7], main = "Q-Q plot of Lattitute")
qqline(y = df_continuous[,7])
```

![](regression_code_files/figure-gfm/unnamed-chunk-16-12.png)<!-- -->

## Investigate the column Address

``` r
# the Address has data either 3 or 4 strings
library(stringr)
df[,22:25] <- str_split_fixed(df$Address, ' ', 4)
names(df)
```

    ##  [1] "Suburb"        "Address"       "Rooms"         "Type"         
    ##  [5] "Price"         "Method"        "SellerG"       "Date"         
    ##  [9] "Distance"      "Postcode"      "Bedroom2"      "Bathroom"     
    ## [13] "Car"           "Landsize"      "BuildingArea"  "YearBuilt"    
    ## [17] "CouncilArea"   "Lattitude"     "Longtitude"    "Regionname"   
    ## [21] "Propertycount" "V22"           "V23"           "V24"          
    ## [25] "V25"

``` r
# check for any missing values in the useful column, which is the name of Address (excluding the number and St/Rd etc)
nrow(df[!is.na(df$V23),])
```

    ## [1] 12495

``` r
df <- df[-c(2,22,24,25)]
colnames(df)[colnames(df) == 'V23'] <- 'Address'
length(unique(df$Address)) 
```

    ## [1] 4011

Since we have a very large number of unique addresses we cannot
visualize the data based on this variable.

## Transform the column Date (date sold)

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
df$Date <- strptime(as.character(df$Date), "%d/%m/%Y")
df$Day <- weekdays(df$Date)
df$Year <- year(df$Date)
df$Month <- month(df$Date, label = T)
df <- subset(df, select = -c(Date))
```

## Drop categorical columns which contain many unique values

``` r
df <- subset(df, select = -c(Suburb, SellerG, CouncilArea, Address))
```

## Correlation between variables

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
corrplot(cor(df[,-c(2,4,15,17,19)]))
```

![](regression_code_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

- Rooms correlated with:
  - Price
  - Distance
  - Bedroom2 (strongly)
  - Bathroom
  - Car
  - Landsize
  - BuildingAre (strongly)
- Price correlated with:
  - Distance (negatively)
  - Bedroom2
  - Bathroom
  - Car
  - Landsize
  - BuildingArea (strongly)
  - YearBuilt (negatively)
  - Lattitude (negatively)
  - Longtitude
- Distance is correlated with:
  - Postcode
  - Bedroom2
  - Car
  - Landsize
  - YearBuilt
  - Longtitude
  - Year
- Postcode is correlated with:
  - Lattitude (negatively)
  - Longtitude
- Bedroom2 is correlated with:
  - Bathroom
  - Car
  - Landsize
  - BuildingArea (strongly)
- Bathroom is correlated with:
  - Car
  - Landsize
  - BuildingArea (strongly)
- Car
  - Landsize
  - BuildingArea
- Landsize is correlated with:
  - BuildingArea
- Lattitude and Longtitude are negatively correlated.

``` r
pairs(df[,-c(2,4,15,17,19)])
```

![](regression_code_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
pairs(~ Price +Rooms +Car +Distance +Bathroom +Landsize +BuildingArea +YearBuilt +Lattitude +Longtitude +Propertycount, data = df)
```

![](regression_code_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

- We can observe that the price increases while the number of rooms
  increases as well.
- The price of accomodations which are located near the Central Business
  District (CBD) varies, whereas those which are far from the CBD cost
  much less.
- In 2017 the only houses which are sold are located near the CBD.
- There is no evidence to prompt us to make any transformation on some
  of the predictors.

``` r
#apply log transformation on the price 
pairs(~ log(Price) +Rooms +Car +Distance +Bathroom +Landsize +BuildingArea +YearBuilt, data = df)
```

![](regression_code_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# combine features into multiple plots

library(ggplot2)
qplot(Distance, data = df, geom = "histogram", fill = Type)
```

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](regression_code_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
qplot(Distance, Price,  data = df, color = Type)
```

![](regression_code_files/figure-gfm/unnamed-chunk-23-2.png)<!-- -->

``` r
qplot(Distance, Price,  data = df, color = Method)
```

![](regression_code_files/figure-gfm/unnamed-chunk-23-3.png)<!-- -->

``` r
qplot(Distance, Price,  data = df, color = Regionname)
```

![](regression_code_files/figure-gfm/unnamed-chunk-23-4.png)<!-- -->

- The histogram shows that the most of accommodations of the dataset are
  houses, cottages, villas, semi, or terraces (type h). Also, few ones
  of every type are far from CBD.
- Observing the plots of Price and Distance we can say that there is a
  pattern, which it represents that accommodations located far from the
  CBD have low price, whereas the houses near the CBD have a range of
  price, from very low to very high amount.
- Based on type of the accommodation, houses, cottages, villas, semi, or
  terraces (type h) are the most expensive and the distance from the CBD
  varies, while the other 2 types, units, or duplexes (type u) and
  townhouses (type t) have low prices and they are near the CBD.

``` r
# see how many values there are in each category of the two columns: method and regionname
meth <- sort(table(df$Method), decreasing = T)
reg <- sort(table(df$Regionname), decreasing = T)
```

``` r
# bar plots of the frequency for the columns method and regionname

library("RColorBrewer")
barplot(sort(table(df$Method), decreasing = T), col= brewer.pal(n = 5, name = "GnBu"), names = F, main = 'Frequency of house sells based on method')
legend("center", legend = names(meth), fill =brewer.pal(n = 5, name = "GnBu"))
```

![](regression_code_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
barplot(sort(table(df$Regionname), decreasing = T), col= brewer.pal(n = 8, name = "GnBu"), names = F, main ='Frequency of house sells in each region' )
legend("topright", legend = names(reg), fill =brewer.pal(n = 8, name = "GnBu"))
```

![](regression_code_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

- Most of the houses have been sold conventionally (method S) and very
  few accommodations have been sold after auction.
- Most of the accommodations of the dataset are located to Southern
  Metropolitan, followed by Northern Metropolitan and Western
  Metropolitan.

``` r
# see how many values there are in each day and month
days <- sort(table(df$Day), decreasing = T)
months <- sort(table(df$Month), decreasing = T)
```

``` r
# bar plots of the frequency for the columns day and month

barplot(months, col= brewer.pal(n = 12, name = "Set3"), names = F, main = 'Frequency of house sells for each month')
legend("topright", legend = names(months), fill =brewer.pal(n = 12, name = "Set3"))
```

![](regression_code_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
barplot(days, col= brewer.pal(n = 8, name = "Set3"), names = F, main = 'Frequency of house sells for each day of week')
legend("topright", legend = names(days), fill =brewer.pal(n = 5, name = "Set3"), cex = 1.3)
```

![](regression_code_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

- The most of accommodations were sold during May, September, and months
  of the summer. We can “predict” that January is not significant for
  the model.
- Saturdays were the days when all almost of the accommodations were
  sold. Wednesdays and Fridays are the days which are not appeared at
  all in data. We can “predict” that the Day is not significant for the
  regression.

## Heat Maps

``` r
# heat maps for the categorical variables

df %>% count(Type,Method ) %>% group_by(Type) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = Type, y = Method)) +
  geom_tile(mapping = aes(fill = prop))
```

![](regression_code_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
df %>% count(Type,Regionname ) %>% group_by(Type) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = Type, y = Regionname)) +
  geom_tile(mapping = aes(fill = prop))
```

![](regression_code_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

``` r
df %>% count(Method,Regionname ) %>% group_by(Method) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = Method, y = Regionname)) +
  geom_tile(mapping = aes(fill = prop))
```

![](regression_code_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

## Dummy variables for columns Type, Method, Regionname, Month, Day

``` r
# type (the type 't' will have 0 in both columns 'h' and 'u')
df$Type_h <- ifelse(df$Type == "h", 1, 0)
df$Type_u <- ifelse(df$Type == "u", 1, 0) 

# method
df$Method_S <- ifelse(df$Method == 'S', 1, 0)
df$Method_SP <- ifelse(df$Method == 'SP', 1, 0)
df$Method_PI <- ifelse(df$Method == 'PI', 1, 0)
df$Method_VB <- ifelse(df$Method == 'VB', 1, 0)

# regionname
df$Regionname_SM <- ifelse(df$Regionname == 'Southern Metropolitan', 1, 0)
df$Regionname_NM <- ifelse(df$Regionname == 'Northern Metropolitan', 1, 0)
df$Regionname_WM <- ifelse(df$Regionname == 'Western Metropolitan', 1, 0)
df$Regionname_EM <- ifelse(df$Regionname == 'Eastern Metropolitan', 1, 0)
df$Regionname_SEM <- ifelse(df$Regionname == 'South-Eastern Metropolitan', 1, 0)
df$Regionname_EV <- ifelse(df$Regionname == 'Eastern Victoria', 1, 0)
df$Regionname_NV <- ifelse(df$Regionname == 'Northern Victoria', 1, 0)

# Month
df$Month_JAN <- ifelse(df$Month == 'Jan', 1, 0)
df$Month_FEB <- ifelse(df$Month == 'Feb', 1, 0)
df$Month_MAR <- ifelse(df$Month == 'Mar', 1, 0)
df$Month_APR <- ifelse(df$Month == 'Apr', 1, 0)
df$Month_MAY <- ifelse(df$Month == 'May', 1, 0)
df$Month_JUN <- ifelse(df$Month == 'Jun', 1, 0)
df$Month_JUL <- ifelse(df$Month == 'Jul', 1, 0)
df$Month_AUG <- ifelse(df$Month == 'Aug', 1, 0)
df$Month_SEP <- ifelse(df$Month == 'Sep', 1, 0)
df$Month_OCT <- ifelse(df$Month == 'Oct', 1, 0)
df$Month_NOV <- ifelse(df$Month == 'Nov', 1, 0)

# Day
df$Day_SAT <- ifelse(df$Day == 'Saturday', 1, 0)
df$Day_SUN <- ifelse(df$Day == 'Sunday', 1, 0)
df$Day_MON <- ifelse(df$Day == 'Monday', 1, 0)
df$Day_TUE <- ifelse(df$Day == 'Tuesday', 1, 0)
```

``` r
#drop the Type, Method, Regionname, Day, and Month since are now dummy variables and also Lattitude and Longtitude since we have the name of the regions in the column regionname
df <- subset(df, select = -c(Type, Method, Regionname, Day, Month, Lattitude,Longtitude))
```

## Transforming the Price

``` r
#using the logarithmic transformation on the price 
df$Price<-log(df$Price)
```

## Scaling the features

The range of the variables varies hence we have to check if it is needed
to scale (standardize) them, in order to make machine learning
algorithms perform better.

``` r
# normalize only not binary columns and Year (they have only 2 unique values)
df_scaled <- df
for (i in 1:11){
  if (length(unique(df[,i])) != 2){
    df_scaled[,i] <- scale(df_scaled[,i], center =  T, scale = T)
  }
}
#head(df_scaled)
summary(df_scaled)
```

    ##       Rooms.V1            Price.V1           Distance.V1    
    ##  Min.   :-2.166828   Min.   :-4.728444   Min.   :-1.803298  
    ##  1st Qu.:-1.075554   1st Qu.:-0.683383   1st Qu.:-0.680183  
    ##  Median : 0.015721   Median :-0.062121   Median :-0.101077  
    ##  Mean   : 0.000000   Mean   : 0.000000   Mean   : 0.000000  
    ##  3rd Qu.: 1.106995   3rd Qu.: 0.674647   3rd Qu.: 0.478029  
    ##  Max.   : 7.654640   Max.   : 4.406093   Max.   : 6.497223  
    ##      Postcode.V1         Bedroom2.V1         Bathroom.V1    
    ##  Min.   :-1.157077   Min.   :-3.190549   Min.   :-2.237528  
    ##  1st Qu.:-0.667075   1st Qu.:-1.034936   1st Qu.:-0.777661  
    ##  Median :-0.232755   Median : 0.042871   Median :-0.777661  
    ##  Mean   : 0.000000   Mean   : 0.000000   Mean   : 0.000000  
    ##  3rd Qu.: 0.479975   3rd Qu.: 0.042871   3rd Qu.: 0.682206  
    ##  Max.   : 9.723192   Max.   :18.365584   Max.   : 9.441406  
    ##        Car.V1            Landsize.V1        BuildingArea.V1  
    ##  Min.   :-1.701192   Min.   :-1.9433073   Min.   :-2.107905  
    ##  1st Qu.:-0.653046   1st Qu.:-0.9426585   1st Qu.:-0.622802  
    ##  Median : 0.395099   Median : 0.0835388   Median :-0.205117  
    ##  Mean   : 0.000000   Mean   : 0.0000000   Mean   : 0.000000  
    ##  3rd Qu.: 0.395099   3rd Qu.: 0.8031543   3rd Qu.: 0.413676  
    ##  Max.   : 8.780264   Max.   : 2.2892243   Max.   : 7.019290  
    ##      YearBuilt.V1      Propertycount.V1        Year          Type_h      
    ##  Min.   :-3.1532321   Min.   :-1.598931   Min.   :2016   Min.   :0.0000  
    ##  1st Qu.:-0.6658099   1st Qu.:-0.712680   1st Qu.:2016   1st Qu.:0.0000  
    ##  Median : 0.1633309   Median :-0.197237   Median :2017   Median :1.0000  
    ##  Mean   : 0.0000000   Mean   : 0.000000   Mean   :2017   Mean   :0.7295  
    ##  3rd Qu.: 0.9095576   3rd Qu.: 0.630021   3rd Qu.:2017   3rd Qu.:1.0000  
    ##  Max.   : 1.4899561   Max.   : 3.243676   Max.   :2017   Max.   :1.0000  
    ##      Type_u          Method_S        Method_SP        Method_PI     
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :1.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.1838   Mean   :0.6753   Mean   :0.1223   Mean   :0.1125  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##    Method_VB       Regionname_SM    Regionname_NM    Regionname_WM   
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.00000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.08299   Mean   :0.3349   Mean   :0.2884   Mean   :0.2262  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##  Regionname_EM    Regionname_SEM    Regionname_EV      Regionname_NV     
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.000000   Min.   :0.000000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:0.000000  
    ##  Median :0.0000   Median :0.00000   Median :0.000000   Median :0.000000  
    ##  Mean   :0.1079   Mean   :0.03449   Mean   :0.003281   Mean   :0.002481  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:0.000000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.000000   Max.   :1.000000  
    ##    Month_JAN           Month_FEB         Month_MAR         Month_APR      
    ##  Min.   :0.0000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.:0.0000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
    ##  Median :0.0000000   Median :0.00000   Median :0.00000   Median :0.00000  
    ##  Mean   :0.0001601   Mean   :0.03217   Mean   :0.04906   Mean   :0.06875  
    ##  3rd Qu.:0.0000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
    ##  Max.   :1.0000000   Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
    ##    Month_MAY        Month_JUN        Month_JUL        Month_AUG     
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.1509   Mean   :0.1331   Mean   :0.1453   Mean   :0.1142  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##    Month_SEP        Month_OCT         Month_NOV          Day_SAT      
    ##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:1.0000  
    ##  Median :0.0000   Median :0.00000   Median :0.00000   Median :1.0000  
    ##  Mean   :0.1379   Mean   :0.04106   Mean   :0.08259   Mean   :0.8656  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.0000  
    ##     Day_SUN           Day_MON          Day_TUE       
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.00000  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.00000  
    ##  Median :0.00000   Median :0.0000   Median :0.00000  
    ##  Mean   :0.07803   Mean   :0.0441   Mean   :0.01016  
    ##  3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.00000  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :1.00000

# Regression Analysis

## Linear Regression

``` r
# 1st exploration to see which variables are the most significant provided that all predictors are included
lm.fit_full <- lm(Price ~ ., data = df)
summary(lm.fit_full)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ ., data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.48217 -0.16178  0.00173  0.16198  2.38224 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -1.673e+02  1.252e+01 -13.365  < 2e-16 ***
    ## Rooms           7.635e-02  7.924e-03   9.635  < 2e-16 ***
    ## Distance       -4.175e-02  6.469e-04 -64.546  < 2e-16 ***
    ## Postcode        3.186e-04  4.010e-05   7.945 2.11e-15 ***
    ## Bedroom2        1.465e-02  7.441e-03   1.969 0.048989 *  
    ## Bathroom        3.427e-02  5.035e-03   6.806 1.05e-11 ***
    ## Car             2.291e-02  2.843e-03   8.057 8.53e-16 ***
    ## Landsize        5.644e-05  1.221e-05   4.622 3.84e-06 ***
    ## BuildingArea    2.408e-03  6.125e-05  39.320  < 2e-16 ***
    ## YearBuilt      -1.366e-03  7.315e-05 -18.671  < 2e-16 ***
    ## Propertycount  -7.541e-07  5.795e-07  -1.301 0.193186    
    ## Year            9.035e-02  6.214e-03  14.540  < 2e-16 ***
    ## Type_h          1.582e-01  9.520e-03  16.620  < 2e-16 ***
    ## Type_u         -3.158e-01  1.048e-02 -30.134  < 2e-16 ***
    ## Method_S        5.701e-02  2.860e-02   1.993 0.046295 *  
    ## Method_SP       6.537e-03  2.927e-02   0.223 0.823261    
    ## Method_PI      -4.821e-02  2.934e-02  -1.643 0.100349    
    ## Method_VB      -4.521e-02  2.965e-02  -1.525 0.127268    
    ## Regionname_SM   4.560e-01  4.991e-02   9.136  < 2e-16 ***
    ## Regionname_NM   1.038e-01  5.032e-02   2.063 0.039136 *  
    ## Regionname_WM   7.331e-02  5.031e-02   1.457 0.145054    
    ## Regionname_EM   3.343e-01  4.987e-02   6.704 2.12e-11 ***
    ## Regionname_SEM  4.992e-01  5.002e-02   9.980  < 2e-16 ***
    ## Regionname_EV   5.308e-01  6.395e-02   8.300  < 2e-16 ***
    ## Regionname_NV   1.910e-01  6.757e-02   2.827 0.004702 ** 
    ## Month_JAN       2.505e-02  1.947e-01   0.129 0.897653    
    ## Month_FEB      -7.072e-02  1.860e-02  -3.803 0.000144 ***
    ## Month_MAR      -4.215e-02  1.662e-02  -2.536 0.011218 *  
    ## Month_APR      -6.680e-02  1.489e-02  -4.487 7.30e-06 ***
    ## Month_MAY      -5.464e-02  1.320e-02  -4.139 3.51e-05 ***
    ## Month_JUN      -4.861e-02  1.356e-02  -3.586 0.000338 ***
    ## Month_JUL      -8.799e-02  1.383e-02  -6.361 2.08e-10 ***
    ## Month_AUG      -3.011e-02  1.386e-02  -2.172 0.029887 *  
    ## Month_SEP      -3.434e-02  1.325e-02  -2.591 0.009582 ** 
    ## Month_OCT      -1.251e-02  1.610e-02  -0.777 0.437050    
    ## Month_NOV      -5.141e-04  1.441e-02  -0.036 0.971537    
    ## Day_SAT        -6.347e-02  5.581e-02  -1.137 0.255502    
    ## Day_SUN        -6.146e-02  5.647e-02  -1.089 0.276393    
    ## Day_MON        -7.644e-02  5.690e-02  -1.343 0.179174    
    ## Day_TUE        -4.306e-02  6.059e-02  -0.711 0.477336    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2631 on 12455 degrees of freedom
    ## Multiple R-squared:  0.735,  Adjusted R-squared:  0.7342 
    ## F-statistic: 885.9 on 39 and 12455 DF,  p-value: < 2.2e-16

``` r
# explore how scaling affects the fitting of linear regression
lm.fit_full <- lm(Price ~ ., data = df_scaled)
summary(lm.fit_full)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ ., data = df_scaled)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8631 -0.3170  0.0034  0.3174  4.6673 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -3.574e+02  2.453e+01 -14.568  < 2e-16 ***
    ## Rooms           1.371e-01  1.423e-02   9.635  < 2e-16 ***
    ## Distance       -4.662e-01  7.222e-03 -64.546  < 2e-16 ***
    ## Postcode        5.604e-02  7.054e-03   7.945 2.11e-15 ***
    ## Bedroom2        2.663e-02  1.353e-02   1.969 0.048989 *  
    ## Bathroom        4.599e-02  6.757e-03   6.806 1.05e-11 ***
    ## Car             4.282e-02  5.315e-03   8.057 8.53e-16 ***
    ## Landsize        2.597e-02  5.618e-03   4.622 3.84e-06 ***
    ## BuildingArea    3.050e-01  7.757e-03  39.320  < 2e-16 ***
    ## YearBuilt      -9.682e-02  5.186e-03 -18.671  < 2e-16 ***
    ## Propertycount  -6.486e-03  4.985e-03  -1.301 0.193186    
    ## Year            1.770e-01  1.218e-02  14.540  < 2e-16 ***
    ## Type_h          3.100e-01  1.865e-02  16.620  < 2e-16 ***
    ## Type_u         -6.187e-01  2.053e-02 -30.134  < 2e-16 ***
    ## Method_S        1.117e-01  5.604e-02   1.993 0.046295 *  
    ## Method_SP       1.281e-02  5.734e-02   0.223 0.823261    
    ## Method_PI      -9.445e-02  5.747e-02  -1.643 0.100349    
    ## Method_VB      -8.858e-02  5.809e-02  -1.525 0.127268    
    ## Regionname_SM   8.934e-01  9.779e-02   9.136  < 2e-16 ***
    ## Regionname_NM   2.034e-01  9.858e-02   2.063 0.039136 *  
    ## Regionname_WM   1.436e-01  9.856e-02   1.457 0.145054    
    ## Regionname_EM   6.550e-01  9.772e-02   6.704 2.12e-11 ***
    ## Regionname_SEM  9.781e-01  9.801e-02   9.980  < 2e-16 ***
    ## Regionname_EV   1.040e+00  1.253e-01   8.300  < 2e-16 ***
    ## Regionname_NV   3.743e-01  1.324e-01   2.827 0.004702 ** 
    ## Month_JAN       4.907e-02  3.815e-01   0.129 0.897653    
    ## Month_FEB      -1.386e-01  3.644e-02  -3.803 0.000144 ***
    ## Month_MAR      -8.258e-02  3.256e-02  -2.536 0.011218 *  
    ## Month_APR      -1.309e-01  2.917e-02  -4.487 7.30e-06 ***
    ## Month_MAY      -1.071e-01  2.586e-02  -4.139 3.51e-05 ***
    ## Month_JUN      -9.525e-02  2.656e-02  -3.586 0.000338 ***
    ## Month_JUL      -1.724e-01  2.710e-02  -6.361 2.08e-10 ***
    ## Month_AUG      -5.898e-02  2.716e-02  -2.172 0.029887 *  
    ## Month_SEP      -6.727e-02  2.596e-02  -2.591 0.009582 ** 
    ## Month_OCT      -2.451e-02  3.154e-02  -0.777 0.437050    
    ## Month_NOV      -1.007e-03  2.823e-02  -0.036 0.971537    
    ## Day_SAT        -1.243e-01  1.094e-01  -1.137 0.255502    
    ## Day_SUN        -1.204e-01  1.106e-01  -1.089 0.276393    
    ## Day_MON        -1.498e-01  1.115e-01  -1.343 0.179174    
    ## Day_TUE        -8.436e-02  1.187e-01  -0.711 0.477336    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5156 on 12455 degrees of freedom
    ## Multiple R-squared:  0.735,  Adjusted R-squared:  0.7342 
    ## F-statistic: 885.9 on 39 and 12455 DF,  p-value: < 2.2e-16

- The p-values do not seem to be affected from scaling, hence we can
  move on with either the original data or the scaled ones. However,
  original data may help us to conclude to more clear and direct
  interpretations.
- Since the p-value of the F-statistic is very small we reject the null
  hypothesis which is the statement where there is no linearity at all
  between the price and the rest columns (at least 1 of the columns) at
  a level of significance 0.05.
- As we had said Day is not important at all, since the p-values of its
  dummy variables are too high. Also, we can forecast quite clearly, due
  to the very large corresponding p-values, that Month_JAN, Month_NOV,
  Propertycount and Method_SP are useless for our regression analysis.

``` r
# do not include variables which had very large p-values
df_reduced1 <- subset(df_scaled, select = -c(Month_JAN, Propertycount, Method_SP, Month_NOV, Day_SAT, Day_SUN, Day_MON, Day_TUE))
lm.fit1 <- lm(Price ~ ., data = df_reduced1)
summary(lm.fit1)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ ., data = df_reduced1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8634 -0.3175  0.0030  0.3175  4.6634 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -3.546e+02  2.322e+01 -15.271  < 2e-16 ***
    ## Rooms           1.367e-01  1.422e-02   9.613  < 2e-16 ***
    ## Distance       -4.664e-01  7.212e-03 -64.676  < 2e-16 ***
    ## Postcode        5.556e-02  7.046e-03   7.885 3.40e-15 ***
    ## Bedroom2        2.712e-02  1.352e-02   2.007 0.044787 *  
    ## Bathroom        4.608e-02  6.753e-03   6.824 9.28e-12 ***
    ## Car             4.289e-02  5.311e-03   8.075 7.35e-16 ***
    ## Landsize        2.577e-02  5.616e-03   4.590 4.49e-06 ***
    ## BuildingArea    3.049e-01  7.753e-03  39.328  < 2e-16 ***
    ## YearBuilt      -9.704e-02  5.181e-03 -18.731  < 2e-16 ***
    ## Year            1.756e-01  1.152e-02  15.243  < 2e-16 ***
    ## Type_h          3.096e-01  1.865e-02  16.606  < 2e-16 ***
    ## Type_u         -6.208e-01  2.050e-02 -30.289  < 2e-16 ***
    ## Method_S        9.875e-02  1.413e-02   6.987 2.96e-12 ***
    ## Method_PI      -1.079e-01  1.906e-02  -5.663 1.52e-08 ***
    ## Method_VB      -1.009e-01  2.085e-02  -4.838 1.33e-06 ***
    ## Regionname_SM   8.866e-01  9.760e-02   9.084  < 2e-16 ***
    ## Regionname_NM   1.925e-01  9.821e-02   1.960 0.050014 .  
    ## Regionname_WM   1.387e-01  9.847e-02   1.408 0.159102    
    ## Regionname_EM   6.507e-01  9.763e-02   6.666 2.75e-11 ***
    ## Regionname_SEM  9.745e-01  9.793e-02   9.951  < 2e-16 ***
    ## Regionname_EV   1.033e+00  1.251e-01   8.252  < 2e-16 ***
    ## Regionname_NV   3.735e-01  1.324e-01   2.822 0.004781 ** 
    ## Month_FEB      -1.271e-01  3.076e-02  -4.131 3.63e-05 ***
    ## Month_MAR      -7.856e-02  2.713e-02  -2.896 0.003786 ** 
    ## Month_APR      -1.272e-01  2.314e-02  -5.494 4.00e-08 ***
    ## Month_MAY      -1.033e-01  1.867e-02  -5.532 3.22e-08 ***
    ## Month_JUN      -9.480e-02  1.929e-02  -4.914 9.05e-07 ***
    ## Month_JUL      -1.664e-01  1.979e-02  -8.405  < 2e-16 ***
    ## Month_AUG      -5.862e-02  1.974e-02  -2.969 0.002992 ** 
    ## Month_SEP      -6.400e-02  1.880e-02  -3.404 0.000667 ***
    ## Month_OCT      -2.179e-02  2.619e-02  -0.832 0.405493    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5155 on 12463 degrees of freedom
    ## Multiple R-squared:  0.7349, Adjusted R-squared:  0.7343 
    ## F-statistic:  1115 on 31 and 12463 DF,  p-value: < 2.2e-16

``` r
anova(lm.fit1, lm.fit_full)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Year + Type_h + Type_u + 
    ##     Method_S + Method_PI + Method_VB + Regionname_SM + Regionname_NM + 
    ##     Regionname_WM + Regionname_EM + Regionname_SEM + Regionname_EV + 
    ##     Regionname_NV + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT
    ## Model 2: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Propertycount + Year + 
    ##     Type_h + Type_u + Method_S + Method_SP + Method_PI + Method_VB + 
    ##     Regionname_SM + Regionname_NM + Regionname_WM + Regionname_EM + 
    ##     Regionname_SEM + Regionname_EV + Regionname_NV + Month_JAN + 
    ##     Month_FEB + Month_MAR + Month_APR + Month_MAY + Month_JUN + 
    ##     Month_JUL + Month_AUG + Month_SEP + Month_OCT + Month_NOV + 
    ##     Day_SAT + Day_SUN + Day_MON + Day_TUE
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1  12463 3311.9                           
    ## 2  12455 3310.5  8    1.4173 0.6665 0.7215

- Since the p-value is larger than 0.05 in anova test we cannot reject
  the null hypothesis hence the two models are quite similar in terms of
  the performance. Thus, we can select the simpler model, meaning the
  reduced one which does not include Month_JAN, Month_NOV,
  Propertycount, Method_SP, Day_SAT, Day_SUN, Day_MON and Day_TUE.
- Now, with the reduced df we have that Method_PI, Method_VB are
  significant, whereas before they were not. Regionname_WM, Month_OCT
  are still not important.

``` r
# do not include variables which are still not significant
df_reduced2 <- subset(df_reduced1, select = -c(Regionname_WM, Month_OCT))
lm.fit2 <- lm(Price ~ ., data = df_reduced2)
summary(lm.fit2)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ ., data = df_reduced2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8635 -0.3166  0.0015  0.3171  4.6618 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -3.542e+02  2.322e+01 -15.252  < 2e-16 ***
    ## Rooms           1.366e-01  1.422e-02   9.605  < 2e-16 ***
    ## Distance       -4.679e-01  7.137e-03 -65.560  < 2e-16 ***
    ## Postcode        5.403e-02  6.962e-03   7.761 9.06e-15 ***
    ## Bedroom2        2.734e-02  1.351e-02   2.023 0.043069 *  
    ## Bathroom        4.607e-02  6.753e-03   6.823 9.35e-12 ***
    ## Car             4.304e-02  5.309e-03   8.106 5.71e-16 ***
    ## Landsize        2.579e-02  5.616e-03   4.592 4.43e-06 ***
    ## BuildingArea    3.051e-01  7.751e-03  39.364  < 2e-16 ***
    ## YearBuilt      -9.685e-02  5.180e-03 -18.699  < 2e-16 ***
    ## Year            1.754e-01  1.152e-02  15.229  < 2e-16 ***
    ## Type_h          3.095e-01  1.865e-02  16.596  < 2e-16 ***
    ## Type_u         -6.208e-01  2.050e-02 -30.291  < 2e-16 ***
    ## Method_S        9.859e-02  1.413e-02   6.976 3.19e-12 ***
    ## Method_PI      -1.081e-01  1.906e-02  -5.670 1.46e-08 ***
    ## Method_VB      -1.012e-01  2.085e-02  -4.852 1.24e-06 ***
    ## Regionname_SM   7.513e-01  1.656e-02  45.369  < 2e-16 ***
    ## Regionname_NM   5.553e-02  1.366e-02   4.066 4.81e-05 ***
    ## Regionname_EM   5.159e-01  1.824e-02  28.293  < 2e-16 ***
    ## Regionname_SEM  8.439e-01  3.139e-02  26.884  < 2e-16 ***
    ## Regionname_EV   9.104e-01  9.014e-02  10.100  < 2e-16 ***
    ## Regionname_NV   2.484e-01  9.812e-02   2.532 0.011367 *  
    ## Month_FEB      -1.216e-01  3.008e-02  -4.041 5.34e-05 ***
    ## Month_MAR      -7.314e-02  2.636e-02  -2.775 0.005531 ** 
    ## Month_APR      -1.217e-01  2.223e-02  -5.473 4.51e-08 ***
    ## Month_MAY      -9.799e-02  1.754e-02  -5.587 2.36e-08 ***
    ## Month_JUN      -8.970e-02  1.821e-02  -4.927 8.45e-07 ***
    ## Month_JUL      -1.612e-01  1.874e-02  -8.602  < 2e-16 ***
    ## Month_AUG      -5.314e-02  1.869e-02  -2.844 0.004465 ** 
    ## Month_SEP      -5.859e-02  1.769e-02  -3.313 0.000926 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5155 on 12465 degrees of freedom
    ## Multiple R-squared:  0.7349, Adjusted R-squared:  0.7342 
    ## F-statistic:  1191 on 29 and 12465 DF,  p-value: < 2.2e-16

``` r
anova(lm.fit1, lm.fit2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Year + Type_h + Type_u + 
    ##     Method_S + Method_PI + Method_VB + Regionname_SM + Regionname_NM + 
    ##     Regionname_WM + Regionname_EM + Regionname_SEM + Regionname_EV + 
    ##     Regionname_NV + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT
    ## Model 2: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Year + Type_h + Type_u + 
    ##     Method_S + Method_PI + Method_VB + Regionname_SM + Regionname_NM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_FEB + Month_MAR + Month_APR + Month_MAY + Month_JUN + 
    ##     Month_JUL + Month_AUG + Month_SEP
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1  12463 3311.9                           
    ## 2  12465 3312.7 -2  -0.71097 1.3377 0.2625

Similarly, the anova test indicates that the models provide similar
performance, hence we can select the model which does not include also
Regionname_WM, Month_OCT.

``` r
# see the plots of the linear regression model

par(mfrow=c(2,2))
plot(lm.fit2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

From the plot of predictions and residuals we can see that the reduced
model fits well, since the standardized residuals follow a normal
distribution, as well as we do not have heteroscedasticity. However, we
can observe high leverage points hence we should investigate more and
maybe drop them.

``` r
# leverage statistic
plot(hatvalues(lm.fit_full))
```

![](regression_code_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
hats <- as.data.frame(hatvalues(lm.fit_full))
hats[order(-hats['hatvalues(lm.fit_full)']), ][1:20]
```

    ## Warning in xtfrm.data.frame(x): cannot xtfrm data frames

    ##  [1] 0.50022993 0.50022993 0.23548833 0.05306354 0.04590398 0.04481650
    ##  [7] 0.04480969 0.04410590 0.04372324 0.04341014 0.04339875 0.04337497
    ## [13] 0.04322606 0.04317757 0.04311036 0.04310581 0.04292368 0.04285143
    ## [19] 0.04282857 0.04281078

We have 3 points having much higher leverage statistic, hence we should
drop them to proceed to the analysis.

``` r
high_lev_stat <- which(hats['hatvalues(lm.fit_full)'] > 0.1) 
df_scaled <- df_scaled[-c(high_lev_stat), ]
```

``` r
library(boot)
# original reduced data 
glm.fit <- glm(Price ~ ., data = df_reduced2)
set.seed(1)
round(cv.glm(df_reduced2, glm.fit, K = 10)$delta[1],3)
```

    ## [1] 0.267

``` r
# estimate the error for the models of df_scaled using the 10-fold cross validation to compare the scaled errors

glm.fit <- glm(Price ~ ., data = df_scaled)
set.seed(1)
cat('CV error for full scaled df:', round(cv.glm(df_scaled, glm.fit, K = 10)$delta[1],3))
```

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
    ## prediction from a rank-deficient fit may be misleading

    ## CV error for full scaled df: 0.267

``` r
glm.fit <- glm(Price ~ . - Month_JAN -Propertycount- Method_PI- Day_SAT- Day_SUN- Day_MON- Day_TUE, data = df_scaled)
set.seed(1)
cat('\nCV error for df without the 7 most not significant predictors:', round(cv.glm(df_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## CV error for df without the 7 most not significant predictors: 0.267

``` r
glm.fit <- glm(Price ~ . - Month_JAN -Propertycount- Method_PI- Day_SAT- Day_SUN- Day_MON- Day_TUE - Regionname_NV- Month_OCT- Month_NOV, data = df_scaled)
set.seed(1)
cat('\nCV error for scaled df without all the not significant predictors:', round(cv.glm(df_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## CV error for scaled df without all the not significant predictors: 0.267

Before we proceed to the algorithms for choosing the best model we could
explore if polynomial regression fits better on our data. Based on the
pairplots we can try to use polynomial of some orders for the column
Distance and check if they provide better performance for our data.

``` r
# use poly
lm.fit_poly <- lm(Price ~ .  -Distance + poly(Distance, 2), data = df_scaled)
summary(lm.fit_poly)
```

    ## 
    ## Call:
    ## lm(formula = Price ~ . - Distance + poly(Distance, 2), data = df_scaled)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.8656 -0.3144  0.0014  0.3161  4.7832 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -3.482e+02  2.449e+01 -14.217  < 2e-16 ***
    ## Rooms               1.432e-01  1.563e-02   9.166  < 2e-16 ***
    ## Postcode            2.911e-02  7.445e-03   3.910 9.28e-05 ***
    ## Bedroom2            2.589e-02  1.538e-02   1.683 0.092348 .  
    ## Bathroom            4.282e-02  6.740e-03   6.353 2.18e-10 ***
    ## Car                 4.762e-02  5.309e-03   8.969  < 2e-16 ***
    ## Landsize            3.049e-02  5.609e-03   5.436 5.54e-08 ***
    ## BuildingArea        3.040e-01  7.722e-03  39.372  < 2e-16 ***
    ## YearBuilt          -9.117e-02  5.188e-03 -17.572  < 2e-16 ***
    ## Propertycount      -1.032e-02  4.974e-03  -2.075 0.037989 *  
    ## Year                1.723e-01  1.215e-02  14.179  < 2e-16 ***
    ## Type_h              3.024e-01  1.858e-02  16.276  < 2e-16 ***
    ## Type_u             -6.301e-01  2.047e-02 -30.787  < 2e-16 ***
    ## Method_S            1.197e-01  5.579e-02   2.145 0.031975 *  
    ## Method_SP           2.039e-02  5.708e-02   0.357 0.720940    
    ## Method_PI          -8.487e-02  5.722e-02  -1.483 0.138025    
    ## Method_VB          -8.904e-02  5.782e-02  -1.540 0.123584    
    ## Regionname_SM       1.144e+00  1.000e-01  11.440  < 2e-16 ***
    ## Regionname_NM       4.152e-01  1.000e-01   4.150 3.34e-05 ***
    ## Regionname_WM       3.561e-01  1.000e-01   3.560 0.000372 ***
    ## Regionname_EM       8.932e-01  9.969e-02   8.960  < 2e-16 ***
    ## Regionname_SEM      1.104e+00  9.824e-02  11.238  < 2e-16 ***
    ## Regionname_EV       9.312e-01  1.251e-01   7.444 1.04e-13 ***
    ## Regionname_NV       3.398e-01  1.318e-01   2.578 0.009955 ** 
    ## Month_JAN                  NA         NA      NA       NA    
    ## Month_FEB          -1.329e-01  3.628e-02  -3.664 0.000249 ***
    ## Month_MAR          -8.093e-02  3.242e-02  -2.496 0.012568 *  
    ## Month_APR          -1.286e-01  2.905e-02  -4.427 9.62e-06 ***
    ## Month_MAY          -1.083e-01  2.576e-02  -4.206 2.62e-05 ***
    ## Month_JUN          -9.735e-02  2.645e-02  -3.680 0.000234 ***
    ## Month_JUL          -1.774e-01  2.699e-02  -6.571 5.20e-11 ***
    ## Month_AUG          -6.243e-02  2.706e-02  -2.307 0.021043 *  
    ## Month_SEP          -6.934e-02  2.585e-02  -2.682 0.007330 ** 
    ## Month_OCT          -2.545e-02  3.140e-02  -0.811 0.417630    
    ## Month_NOV          -2.319e-03  2.810e-02  -0.083 0.934233    
    ## Day_SAT            -1.251e-01  1.088e-01  -1.149 0.250543    
    ## Day_SUN            -1.191e-01  1.101e-01  -1.082 0.279421    
    ## Day_MON            -1.494e-01  1.110e-01  -1.346 0.178217    
    ## Day_TUE            -7.445e-02  1.182e-01  -0.630 0.528643    
    ## poly(Distance, 2)1 -4.946e+01  8.394e-01 -58.928  < 2e-16 ***
    ## poly(Distance, 2)2  7.393e+00  6.788e-01  10.891  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5132 on 12452 degrees of freedom
    ## Multiple R-squared:  0.7375, Adjusted R-squared:  0.7367 
    ## F-statistic: 897.1 on 39 and 12452 DF,  p-value: < 2.2e-16

- Including powers of the distance column, we can see that the function
  lm suggests different predictors. In particular, assuming that we have
  all the columns in the model, in case of the data included 2nd power
  of column Distance, ‘lm’ provides that Bedroom2, Regionname_WM,
  Regionname_EV are significant at level of significance 0.05 whereas in
  the case of initial data they were not.
- The new column which is created of is very significant.

``` r
lm.fit <- lm(Price ~ ., data = df_scaled)
anova(lm.fit, lm.fit_poly)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Propertycount + Year + 
    ##     Type_h + Type_u + Method_S + Method_SP + Method_PI + Method_VB + 
    ##     Regionname_SM + Regionname_NM + Regionname_WM + Regionname_EM + 
    ##     Regionname_SEM + Regionname_EV + Regionname_NV + Month_JAN + 
    ##     Month_FEB + Month_MAR + Month_APR + Month_MAY + Month_JUN + 
    ##     Month_JUL + Month_AUG + Month_SEP + Month_OCT + Month_NOV + 
    ##     Day_SAT + Day_SUN + Day_MON + Day_TUE
    ## Model 2: Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
    ##     Car + Landsize + BuildingArea + YearBuilt + Propertycount + 
    ##     Year + Type_h + Type_u + Method_S + Method_SP + Method_PI + 
    ##     Method_VB + Regionname_SM + Regionname_NM + Regionname_WM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_JAN + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT + 
    ##     Month_NOV + Day_SAT + Day_SUN + Day_MON + Day_TUE) - Distance + 
    ##     poly(Distance, 2)
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1  12453 3310.3                                  
    ## 2  12452 3279.1  1    31.234 118.61 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Consequently, from the anova test we reject the null hypothesis (Null
hypothesis: the models are similar) at level of significance 0.05, hence
the above polynomial model is better than the initial one.

We could create more powers and check if they provide better
performance.

``` r
fit.1 <- lm(Price ~ ., data = df_scaled)
fit.2 <- lm(Price ~ .  -Distance + poly(Distance, 2), data = df_scaled)
fit.3 <- lm(Price ~ .  -Distance + poly(Distance, 3), data = df_scaled)
fit.4 <- lm(Price ~ .  -Distance + poly(Distance, 4), data = df_scaled)
fit.5 <- lm(Price ~ .  -Distance + poly(Distance, 5), data = df_scaled)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
    ##     Landsize + BuildingArea + YearBuilt + Propertycount + Year + 
    ##     Type_h + Type_u + Method_S + Method_SP + Method_PI + Method_VB + 
    ##     Regionname_SM + Regionname_NM + Regionname_WM + Regionname_EM + 
    ##     Regionname_SEM + Regionname_EV + Regionname_NV + Month_JAN + 
    ##     Month_FEB + Month_MAR + Month_APR + Month_MAY + Month_JUN + 
    ##     Month_JUL + Month_AUG + Month_SEP + Month_OCT + Month_NOV + 
    ##     Day_SAT + Day_SUN + Day_MON + Day_TUE
    ## Model 2: Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
    ##     Car + Landsize + BuildingArea + YearBuilt + Propertycount + 
    ##     Year + Type_h + Type_u + Method_S + Method_SP + Method_PI + 
    ##     Method_VB + Regionname_SM + Regionname_NM + Regionname_WM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_JAN + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT + 
    ##     Month_NOV + Day_SAT + Day_SUN + Day_MON + Day_TUE) - Distance + 
    ##     poly(Distance, 2)
    ## Model 3: Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
    ##     Car + Landsize + BuildingArea + YearBuilt + Propertycount + 
    ##     Year + Type_h + Type_u + Method_S + Method_SP + Method_PI + 
    ##     Method_VB + Regionname_SM + Regionname_NM + Regionname_WM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_JAN + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT + 
    ##     Month_NOV + Day_SAT + Day_SUN + Day_MON + Day_TUE) - Distance + 
    ##     poly(Distance, 3)
    ## Model 4: Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
    ##     Car + Landsize + BuildingArea + YearBuilt + Propertycount + 
    ##     Year + Type_h + Type_u + Method_S + Method_SP + Method_PI + 
    ##     Method_VB + Regionname_SM + Regionname_NM + Regionname_WM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_JAN + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT + 
    ##     Month_NOV + Day_SAT + Day_SUN + Day_MON + Day_TUE) - Distance + 
    ##     poly(Distance, 4)
    ## Model 5: Price ~ (Rooms + Distance + Postcode + Bedroom2 + Bathroom + 
    ##     Car + Landsize + BuildingArea + YearBuilt + Propertycount + 
    ##     Year + Type_h + Type_u + Method_S + Method_SP + Method_PI + 
    ##     Method_VB + Regionname_SM + Regionname_NM + Regionname_WM + 
    ##     Regionname_EM + Regionname_SEM + Regionname_EV + Regionname_NV + 
    ##     Month_JAN + Month_FEB + Month_MAR + Month_APR + Month_MAY + 
    ##     Month_JUN + Month_JUL + Month_AUG + Month_SEP + Month_OCT + 
    ##     Month_NOV + Day_SAT + Day_SUN + Day_MON + Day_TUE) - Distance + 
    ##     poly(Distance, 5)
    ##   Res.Df    RSS Df Sum of Sq        F    Pr(>F)    
    ## 1  12453 3310.3                                    
    ## 2  12452 3279.1  1   31.2339 118.7943 < 2.2e-16 ***
    ## 3  12451 3276.3  1    2.8129  10.6984  0.001075 ** 
    ## 4  12450 3273.9  1    2.3986   9.1227  0.002530 ** 
    ## 5  12449 3273.1  1    0.7240   2.7537  0.097053 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The above test indicates that we should use the polynomial of 4th order.

## Forward/Backward selection of the features

### Best Subset Selection

``` r
# just to explore the behaviour of the regsubsets between already reduced and initial data
df_new <- subset(df, select = -c(Bedroom2, Month_JAN, Propertycount, Method_SP, Day_SAT, Day_SUN, Day_MON, Day_TUE, Bedroom2, Regionname_WM, Month_OCT, Month_NOV))
df_scaled_new <- subset(df_scaled, select = -c(Bedroom2, Month_JAN, Propertycount, Method_SP, Day_SAT, Day_SUN, Day_MON, Day_TUE, Bedroom2, Regionname_WM, Month_OCT, Month_NOV))

# INCLUDE ONLY THE MOST SIGNIFICANT COLUMNS 29 columns == 28 predictors and the target column
library(leaps)
```

    ## Warning: package 'leaps' was built under R version 4.2.3

``` r
regfit_new <- regsubsets(Price ~ ., data = df_scaled_new, nvmax = 28)
reg.summary_new <- summary(regfit_new)
reg.summary_new
```

    ## Subset selection object
    ## Call: regsubsets.formula(Price ~ ., data = df_scaled_new, nvmax = 28)
    ## 28 Variables  (and intercept)
    ##                Forced in Forced out
    ## Rooms              FALSE      FALSE
    ## Distance           FALSE      FALSE
    ## Postcode           FALSE      FALSE
    ## Bathroom           FALSE      FALSE
    ## Car                FALSE      FALSE
    ## Landsize           FALSE      FALSE
    ## BuildingArea       FALSE      FALSE
    ## YearBuilt          FALSE      FALSE
    ## Year               FALSE      FALSE
    ## Type_h             FALSE      FALSE
    ## Type_u             FALSE      FALSE
    ## Method_S           FALSE      FALSE
    ## Method_PI          FALSE      FALSE
    ## Method_VB          FALSE      FALSE
    ## Regionname_SM      FALSE      FALSE
    ## Regionname_NM      FALSE      FALSE
    ## Regionname_EM      FALSE      FALSE
    ## Regionname_SEM     FALSE      FALSE
    ## Regionname_EV      FALSE      FALSE
    ## Regionname_NV      FALSE      FALSE
    ## Month_FEB          FALSE      FALSE
    ## Month_MAR          FALSE      FALSE
    ## Month_APR          FALSE      FALSE
    ## Month_MAY          FALSE      FALSE
    ## Month_JUN          FALSE      FALSE
    ## Month_JUL          FALSE      FALSE
    ## Month_AUG          FALSE      FALSE
    ## Month_SEP          FALSE      FALSE
    ## 1 subsets of each size up to 28
    ## Selection Algorithm: exhaustive
    ##           Rooms Distance Postcode Bathroom Car Landsize BuildingArea YearBuilt
    ## 1  ( 1 )  " "   " "      " "      " "      " " " "      "*"          " "      
    ## 2  ( 1 )  " "   "*"      " "      " "      " " " "      "*"          " "      
    ## 3  ( 1 )  " "   " "      " "      " "      " " " "      "*"          " "      
    ## 4  ( 1 )  " "   "*"      " "      " "      " " " "      "*"          " "      
    ## 5  ( 1 )  " "   "*"      " "      " "      " " " "      "*"          "*"      
    ## 6  ( 1 )  " "   "*"      " "      " "      " " " "      "*"          " "      
    ## 7  ( 1 )  " "   "*"      " "      " "      " " " "      "*"          " "      
    ## 8  ( 1 )  "*"   "*"      " "      " "      " " " "      "*"          " "      
    ## 9  ( 1 )  "*"   "*"      " "      " "      " " " "      "*"          "*"      
    ## 10  ( 1 ) "*"   "*"      " "      " "      " " " "      "*"          "*"      
    ## 11  ( 1 ) "*"   "*"      " "      " "      " " " "      "*"          "*"      
    ## 12  ( 1 ) "*"   "*"      " "      " "      " " " "      "*"          "*"      
    ## 13  ( 1 ) "*"   "*"      "*"      " "      "*" " "      "*"          "*"      
    ## 14  ( 1 ) "*"   "*"      "*"      " "      "*" " "      "*"          "*"      
    ## 15  ( 1 ) "*"   "*"      "*"      " "      "*" " "      "*"          "*"      
    ## 16  ( 1 ) "*"   "*"      "*"      "*"      "*" " "      "*"          "*"      
    ## 17  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 18  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 19  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 20  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 21  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 22  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 23  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 24  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 25  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 26  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 27  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ## 28  ( 1 ) "*"   "*"      "*"      "*"      "*" "*"      "*"          "*"      
    ##           Year Type_h Type_u Method_S Method_PI Method_VB Regionname_SM
    ## 1  ( 1 )  " "  " "    " "    " "      " "       " "       " "          
    ## 2  ( 1 )  " "  " "    " "    " "      " "       " "       " "          
    ## 3  ( 1 )  " "  " "    "*"    " "      " "       " "       "*"          
    ## 4  ( 1 )  " "  " "    "*"    " "      " "       " "       "*"          
    ## 5  ( 1 )  " "  " "    "*"    " "      " "       " "       "*"          
    ## 6  ( 1 )  " "  " "    "*"    " "      " "       " "       "*"          
    ## 7  ( 1 )  " "  "*"    "*"    " "      " "       " "       "*"          
    ## 8  ( 1 )  " "  "*"    "*"    " "      " "       " "       "*"          
    ## 9  ( 1 )  " "  "*"    "*"    " "      " "       " "       "*"          
    ## 10  ( 1 ) " "  "*"    "*"    "*"      " "       " "       "*"          
    ## 11  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 12  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 13  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 14  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 15  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 16  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 17  ( 1 ) "*"  "*"    "*"    "*"      " "       " "       "*"          
    ## 18  ( 1 ) "*"  "*"    "*"    "*"      "*"       " "       "*"          
    ## 19  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 20  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 21  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 22  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 23  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 24  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 25  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 26  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 27  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ## 28  ( 1 ) "*"  "*"    "*"    "*"      "*"       "*"       "*"          
    ##           Regionname_NM Regionname_EM Regionname_SEM Regionname_EV
    ## 1  ( 1 )  " "           " "           " "            " "          
    ## 2  ( 1 )  " "           " "           " "            " "          
    ## 3  ( 1 )  " "           " "           " "            " "          
    ## 4  ( 1 )  " "           " "           " "            " "          
    ## 5  ( 1 )  " "           " "           " "            " "          
    ## 6  ( 1 )  " "           "*"           "*"            " "          
    ## 7  ( 1 )  " "           "*"           "*"            " "          
    ## 8  ( 1 )  " "           "*"           "*"            " "          
    ## 9  ( 1 )  " "           "*"           "*"            " "          
    ## 10  ( 1 ) " "           "*"           "*"            " "          
    ## 11  ( 1 ) " "           "*"           "*"            " "          
    ## 12  ( 1 ) " "           "*"           "*"            "*"          
    ## 13  ( 1 ) " "           "*"           "*"            " "          
    ## 14  ( 1 ) " "           "*"           "*"            "*"          
    ## 15  ( 1 ) " "           "*"           "*"            "*"          
    ## 16  ( 1 ) " "           "*"           "*"            "*"          
    ## 17  ( 1 ) " "           "*"           "*"            "*"          
    ## 18  ( 1 ) " "           "*"           "*"            "*"          
    ## 19  ( 1 ) " "           "*"           "*"            "*"          
    ## 20  ( 1 ) "*"           "*"           "*"            "*"          
    ## 21  ( 1 ) "*"           "*"           "*"            "*"          
    ## 22  ( 1 ) "*"           "*"           "*"            "*"          
    ## 23  ( 1 ) "*"           "*"           "*"            "*"          
    ## 24  ( 1 ) "*"           "*"           "*"            "*"          
    ## 25  ( 1 ) "*"           "*"           "*"            "*"          
    ## 26  ( 1 ) "*"           "*"           "*"            "*"          
    ## 27  ( 1 ) "*"           "*"           "*"            "*"          
    ## 28  ( 1 ) "*"           "*"           "*"            "*"          
    ##           Regionname_NV Month_FEB Month_MAR Month_APR Month_MAY Month_JUN
    ## 1  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 2  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 3  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 4  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 5  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 6  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 7  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 8  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 9  ( 1 )  " "           " "       " "       " "       " "       " "      
    ## 10  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 11  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 12  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 13  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 14  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 15  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 16  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 17  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 18  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 19  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 20  ( 1 ) " "           " "       " "       " "       " "       " "      
    ## 21  ( 1 ) " "           " "       " "       "*"       " "       " "      
    ## 22  ( 1 ) " "           " "       " "       "*"       "*"       " "      
    ## 23  ( 1 ) " "           " "       " "       "*"       "*"       "*"      
    ## 24  ( 1 ) " "           "*"       " "       "*"       "*"       "*"      
    ## 25  ( 1 ) "*"           "*"       " "       "*"       "*"       "*"      
    ## 26  ( 1 ) "*"           "*"       " "       "*"       "*"       "*"      
    ## 27  ( 1 ) " "           "*"       "*"       "*"       "*"       "*"      
    ## 28  ( 1 ) "*"           "*"       "*"       "*"       "*"       "*"      
    ##           Month_JUL Month_AUG Month_SEP
    ## 1  ( 1 )  " "       " "       " "      
    ## 2  ( 1 )  " "       " "       " "      
    ## 3  ( 1 )  " "       " "       " "      
    ## 4  ( 1 )  " "       " "       " "      
    ## 5  ( 1 )  " "       " "       " "      
    ## 6  ( 1 )  " "       " "       " "      
    ## 7  ( 1 )  " "       " "       " "      
    ## 8  ( 1 )  " "       " "       " "      
    ## 9  ( 1 )  " "       " "       " "      
    ## 10  ( 1 ) " "       " "       " "      
    ## 11  ( 1 ) " "       " "       " "      
    ## 12  ( 1 ) " "       " "       " "      
    ## 13  ( 1 ) " "       " "       " "      
    ## 14  ( 1 ) " "       " "       " "      
    ## 15  ( 1 ) "*"       " "       " "      
    ## 16  ( 1 ) "*"       " "       " "      
    ## 17  ( 1 ) "*"       " "       " "      
    ## 18  ( 1 ) "*"       " "       " "      
    ## 19  ( 1 ) "*"       " "       " "      
    ## 20  ( 1 ) "*"       " "       " "      
    ## 21  ( 1 ) "*"       " "       " "      
    ## 22  ( 1 ) "*"       " "       " "      
    ## 23  ( 1 ) "*"       " "       " "      
    ## 24  ( 1 ) "*"       " "       " "      
    ## 25  ( 1 ) "*"       " "       " "      
    ## 26  ( 1 ) "*"       " "       "*"      
    ## 27  ( 1 ) "*"       "*"       "*"      
    ## 28  ( 1 ) "*"       "*"       "*"

``` r
data.frame(
  Adj.R2 = which.max(reg.summary_new$adjr2),
  CP = which.min(reg.summary_new$cp),
  BIC = which.min(reg.summary_new$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     28 28  22

Cp and BIC give different suggestions for the best model. We have to
investigate the evaluation metrics of the models and decide later on
what we pursue through the model, either simpler one or the one which
provides lower error.

``` r
dim(df_scaled)
```

    ## [1] 12492    40

``` r
# for better results we should investigate the initial data by using this method and proceed then to conclusions
regfit.full <- regsubsets(Price ~ ., data = df, nvmax = 39)
reg.summary <- summary(regfit.full)
reg.summary
```

    ## Subset selection object
    ## Call: regsubsets.formula(Price ~ ., data = df, nvmax = 39)
    ## 39 Variables  (and intercept)
    ##                Forced in Forced out
    ## Rooms              FALSE      FALSE
    ## Distance           FALSE      FALSE
    ## Postcode           FALSE      FALSE
    ## Bedroom2           FALSE      FALSE
    ## Bathroom           FALSE      FALSE
    ## Car                FALSE      FALSE
    ## Landsize           FALSE      FALSE
    ## BuildingArea       FALSE      FALSE
    ## YearBuilt          FALSE      FALSE
    ## Propertycount      FALSE      FALSE
    ## Year               FALSE      FALSE
    ## Type_h             FALSE      FALSE
    ## Type_u             FALSE      FALSE
    ## Method_S           FALSE      FALSE
    ## Method_SP          FALSE      FALSE
    ## Method_PI          FALSE      FALSE
    ## Method_VB          FALSE      FALSE
    ## Regionname_SM      FALSE      FALSE
    ## Regionname_NM      FALSE      FALSE
    ## Regionname_WM      FALSE      FALSE
    ## Regionname_EM      FALSE      FALSE
    ## Regionname_SEM     FALSE      FALSE
    ## Regionname_EV      FALSE      FALSE
    ## Regionname_NV      FALSE      FALSE
    ## Month_JAN          FALSE      FALSE
    ## Month_FEB          FALSE      FALSE
    ## Month_MAR          FALSE      FALSE
    ## Month_APR          FALSE      FALSE
    ## Month_MAY          FALSE      FALSE
    ## Month_JUN          FALSE      FALSE
    ## Month_JUL          FALSE      FALSE
    ## Month_AUG          FALSE      FALSE
    ## Month_SEP          FALSE      FALSE
    ## Month_OCT          FALSE      FALSE
    ## Month_NOV          FALSE      FALSE
    ## Day_SAT            FALSE      FALSE
    ## Day_SUN            FALSE      FALSE
    ## Day_MON            FALSE      FALSE
    ## Day_TUE            FALSE      FALSE
    ## 1 subsets of each size up to 39
    ## Selection Algorithm: exhaustive
    ##           Rooms Distance Postcode Bedroom2 Bathroom Car Landsize BuildingArea
    ## 1  ( 1 )  " "   " "      " "      " "      " "      " " " "      "*"         
    ## 2  ( 1 )  " "   "*"      " "      " "      " "      " " " "      "*"         
    ## 3  ( 1 )  " "   " "      " "      " "      " "      " " " "      "*"         
    ## 4  ( 1 )  " "   "*"      " "      " "      " "      " " " "      "*"         
    ## 5  ( 1 )  " "   "*"      " "      " "      " "      " " " "      "*"         
    ## 6  ( 1 )  " "   "*"      " "      " "      " "      " " " "      "*"         
    ## 7  ( 1 )  "*"   "*"      " "      " "      " "      " " " "      "*"         
    ## 8  ( 1 )  "*"   "*"      " "      " "      " "      " " " "      "*"         
    ## 9  ( 1 )  "*"   "*"      " "      " "      " "      " " " "      "*"         
    ## 10  ( 1 ) "*"   "*"      " "      " "      " "      " " " "      "*"         
    ## 11  ( 1 ) "*"   "*"      " "      " "      " "      " " " "      "*"         
    ## 12  ( 1 ) "*"   "*"      " "      " "      " "      "*" " "      "*"         
    ## 13  ( 1 ) "*"   "*"      "*"      " "      " "      "*" " "      "*"         
    ## 14  ( 1 ) "*"   "*"      "*"      " "      " "      "*" " "      "*"         
    ## 15  ( 1 ) "*"   "*"      "*"      " "      " "      "*" " "      "*"         
    ## 16  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" " "      "*"         
    ## 17  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" " "      "*"         
    ## 18  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 19  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 20  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 21  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 22  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 23  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 24  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 25  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 26  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 27  ( 1 ) "*"   "*"      "*"      " "      "*"      "*" "*"      "*"         
    ## 28  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 29  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 30  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 31  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 32  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 33  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 34  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 35  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 36  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 37  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 38  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ## 39  ( 1 ) "*"   "*"      "*"      "*"      "*"      "*" "*"      "*"         
    ##           YearBuilt Propertycount Year Type_h Type_u Method_S Method_SP
    ## 1  ( 1 )  " "       " "           " "  " "    " "    " "      " "      
    ## 2  ( 1 )  " "       " "           " "  " "    " "    " "      " "      
    ## 3  ( 1 )  " "       " "           " "  " "    "*"    " "      " "      
    ## 4  ( 1 )  " "       " "           " "  " "    "*"    " "      " "      
    ## 5  ( 1 )  " "       " "           " "  " "    "*"    " "      " "      
    ## 6  ( 1 )  " "       " "           " "  "*"    "*"    " "      " "      
    ## 7  ( 1 )  "*"       " "           " "  " "    "*"    " "      " "      
    ## 8  ( 1 )  "*"       " "           " "  "*"    "*"    " "      " "      
    ## 9  ( 1 )  "*"       " "           " "  "*"    "*"    " "      " "      
    ## 10  ( 1 ) "*"       " "           " "  "*"    "*"    "*"      " "      
    ## 11  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 12  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 13  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 14  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 15  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 16  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 17  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 18  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 19  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 20  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 21  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 22  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 23  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 24  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 25  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 26  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 27  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 28  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      "*"      
    ## 29  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 30  ( 1 ) "*"       " "           "*"  "*"    "*"    "*"      " "      
    ## 31  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 32  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 33  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 34  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 35  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 36  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      " "      
    ## 37  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      "*"      
    ## 38  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      "*"      
    ## 39  ( 1 ) "*"       "*"           "*"  "*"    "*"    "*"      "*"      
    ##           Method_PI Method_VB Regionname_SM Regionname_NM Regionname_WM
    ## 1  ( 1 )  " "       " "       " "           " "           " "          
    ## 2  ( 1 )  " "       " "       " "           " "           " "          
    ## 3  ( 1 )  " "       " "       "*"           " "           " "          
    ## 4  ( 1 )  " "       " "       "*"           " "           " "          
    ## 5  ( 1 )  " "       " "       " "           "*"           "*"          
    ## 6  ( 1 )  " "       " "       " "           "*"           "*"          
    ## 7  ( 1 )  " "       " "       " "           "*"           "*"          
    ## 8  ( 1 )  " "       " "       " "           "*"           "*"          
    ## 9  ( 1 )  " "       " "       " "           "*"           "*"          
    ## 10  ( 1 ) " "       " "       " "           "*"           "*"          
    ## 11  ( 1 ) " "       " "       " "           "*"           "*"          
    ## 12  ( 1 ) " "       " "       " "           "*"           "*"          
    ## 13  ( 1 ) " "       " "       "*"           " "           " "          
    ## 14  ( 1 ) " "       " "       "*"           " "           " "          
    ## 15  ( 1 ) " "       " "       "*"           " "           " "          
    ## 16  ( 1 ) " "       " "       "*"           " "           " "          
    ## 17  ( 1 ) " "       " "       "*"           " "           " "          
    ## 18  ( 1 ) " "       " "       "*"           " "           " "          
    ## 19  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 20  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 21  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 22  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 23  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 24  ( 1 ) " "       " "       "*"           " "           "*"          
    ## 25  ( 1 ) " "       " "       "*"           "*"           " "          
    ## 26  ( 1 ) " "       " "       "*"           "*"           " "          
    ## 27  ( 1 ) " "       " "       "*"           "*"           " "          
    ## 28  ( 1 ) " "       " "       "*"           "*"           " "          
    ## 29  ( 1 ) "*"       "*"       "*"           "*"           " "          
    ## 30  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 31  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 32  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 33  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 34  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 35  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 36  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 37  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 38  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ## 39  ( 1 ) "*"       "*"       "*"           "*"           "*"          
    ##           Regionname_EM Regionname_SEM Regionname_EV Regionname_NV Month_JAN
    ## 1  ( 1 )  " "           " "            " "           " "           " "      
    ## 2  ( 1 )  " "           " "            " "           " "           " "      
    ## 3  ( 1 )  " "           " "            " "           " "           " "      
    ## 4  ( 1 )  " "           " "            " "           " "           " "      
    ## 5  ( 1 )  " "           " "            " "           " "           " "      
    ## 6  ( 1 )  " "           " "            " "           " "           " "      
    ## 7  ( 1 )  " "           " "            " "           " "           " "      
    ## 8  ( 1 )  " "           " "            " "           " "           " "      
    ## 9  ( 1 )  "*"           " "            " "           " "           " "      
    ## 10  ( 1 ) "*"           " "            " "           " "           " "      
    ## 11  ( 1 ) "*"           " "            " "           " "           " "      
    ## 12  ( 1 ) "*"           " "            " "           " "           " "      
    ## 13  ( 1 ) "*"           "*"            " "           " "           " "      
    ## 14  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 15  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 16  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 17  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 18  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 19  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 20  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 21  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 22  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 23  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 24  ( 1 ) "*"           "*"            "*"           " "           " "      
    ## 25  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 26  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 27  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 28  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 29  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 30  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 31  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 32  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 33  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 34  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 35  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 36  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 37  ( 1 ) "*"           "*"            "*"           "*"           " "      
    ## 38  ( 1 ) "*"           "*"            "*"           "*"           "*"      
    ## 39  ( 1 ) "*"           "*"            "*"           "*"           "*"      
    ##           Month_FEB Month_MAR Month_APR Month_MAY Month_JUN Month_JUL Month_AUG
    ## 1  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 2  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 3  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 4  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 5  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 6  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 7  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 8  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 9  ( 1 )  " "       " "       " "       " "       " "       " "       " "      
    ## 10  ( 1 ) " "       " "       " "       " "       " "       " "       " "      
    ## 11  ( 1 ) " "       " "       " "       " "       " "       " "       " "      
    ## 12  ( 1 ) " "       " "       " "       " "       " "       " "       " "      
    ## 13  ( 1 ) " "       " "       " "       " "       " "       " "       " "      
    ## 14  ( 1 ) " "       " "       " "       " "       " "       " "       " "      
    ## 15  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 16  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 17  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 18  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 19  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 20  ( 1 ) " "       " "       " "       " "       " "       "*"       " "      
    ## 21  ( 1 ) " "       " "       "*"       " "       " "       "*"       " "      
    ## 22  ( 1 ) " "       " "       "*"       "*"       " "       "*"       " "      
    ## 23  ( 1 ) "*"       " "       "*"       "*"       "*"       "*"       " "      
    ## 24  ( 1 ) "*"       " "       "*"       "*"       "*"       "*"       " "      
    ## 25  ( 1 ) "*"       " "       "*"       "*"       "*"       "*"       " "      
    ## 26  ( 1 ) "*"       " "       "*"       "*"       "*"       "*"       " "      
    ## 27  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 28  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 29  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 30  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 31  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 32  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 33  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 34  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 35  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 36  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 37  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 38  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ## 39  ( 1 ) "*"       "*"       "*"       "*"       "*"       "*"       "*"      
    ##           Month_SEP Month_OCT Month_NOV Day_SAT Day_SUN Day_MON Day_TUE
    ## 1  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 2  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 3  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 4  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 5  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 6  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 7  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 8  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 9  ( 1 )  " "       " "       " "       " "     " "     " "     " "    
    ## 10  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 11  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 12  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 13  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 14  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 15  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 16  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 17  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 18  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 19  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 20  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 21  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 22  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 23  ( 1 ) " "       " "       " "       " "     " "     " "     " "    
    ## 24  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 25  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 26  ( 1 ) " "       " "       "*"       " "     " "     " "     " "    
    ## 27  ( 1 ) "*"       " "       " "       " "     " "     " "     " "    
    ## 28  ( 1 ) "*"       " "       " "       " "     " "     " "     " "    
    ## 29  ( 1 ) "*"       " "       " "       " "     " "     " "     " "    
    ## 30  ( 1 ) "*"       " "       " "       " "     " "     " "     " "    
    ## 31  ( 1 ) "*"       " "       " "       " "     " "     " "     " "    
    ## 32  ( 1 ) "*"       " "       " "       " "     " "     "*"     " "    
    ## 33  ( 1 ) "*"       "*"       " "       " "     " "     "*"     " "    
    ## 34  ( 1 ) "*"       " "       " "       "*"     "*"     "*"     " "    
    ## 35  ( 1 ) "*"       "*"       " "       "*"     "*"     "*"     " "    
    ## 36  ( 1 ) "*"       "*"       " "       "*"     "*"     "*"     "*"    
    ## 37  ( 1 ) "*"       "*"       " "       "*"     "*"     "*"     "*"    
    ## 38  ( 1 ) "*"       "*"       " "       "*"     "*"     "*"     "*"    
    ## 39  ( 1 ) "*"       "*"       "*"       "*"     "*"     "*"     "*"

``` r
data.frame(
  Adj.R2 = which.max(reg.summary$adjr2),
  CP = which.min(reg.summary$cp),
  BIC = which.min(reg.summary$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     32 29  20

``` r
cat('Best model by using reduced data, based on the BIC\n')
```

    ## Best model by using reduced data, based on the BIC

``` r
cat('--------------------------------------------------\n')
```

    ## --------------------------------------------------

``` r
for (i in 1:28){
  if (reg.summary_new$which[20,i] == TRUE){
    print(colnames(reg.summary_new$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"

``` r
cat('Best model by using initial data, based on the BIC\n')
```

    ## Best model by using initial data, based on the BIC

``` r
cat('--------------------------------------------------\n')
```

    ## --------------------------------------------------

``` r
for (i in 1:39){
  if (reg.summary$which[20,i] == TRUE){
    print(colnames(reg.summary$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_SP"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_WM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"
    ## [1] "Month_NOV"

We can observe that although the method Best Subset Selection suggests
the same number of predictors for either the initial or reduced data
they are not the same ones; Method_SP, Regionname_WM and Month_NOV,
which had not been significant from previous algorithm, Best Subset
Selection suggests to be included to the best model.

### Forward Stepwise Selection

``` r
regfit.fwd <- regsubsets(Price ~ ., data = df_scaled,
    nvmax = 39, method = "forward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Reordering variables and trying again:

    ## Warning in rval$lopt[] <- rval$vorder[rval$lopt]: number of items to replace is
    ## not a multiple of replacement length

``` r
regfwd.summary <- summary(regfit.fwd)
data.frame(
  Adj.R2 = which.max(regfwd.summary$adjr2),
  CP = which.min(regfwd.summary$cp),
  BIC = which.min(regfwd.summary$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     36 32  20

``` r
cat('Best model (Forward Selection) by using initial data, based on the BIC\n')
```

    ## Best model (Forward Selection) by using initial data, based on the BIC

``` r
cat('----------------------------------------------------------------------\n')
```

    ## ----------------------------------------------------------------------

``` r
for (i in 1:39){
  if (regfwd.summary$which[20,i] == TRUE){
    print(colnames(regfwd.summary$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_SP"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_WM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"
    ## [1] "Month_NOV"

``` r
# using the reduced data
regfit.fwd_new <- regsubsets(Price ~ ., data = df_scaled_new,
    nvmax = 28, method = "forward")
regfwd.summary_new <- summary(regfit.fwd_new)
data.frame(
  Adj.R2 = which.max(regfwd.summary_new$adjr2),
  CP = which.min(regfwd.summary_new$cp),
  BIC = which.min(regfwd.summary_new$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     28 28  22

``` r
cat('Best model (Forward Selection) by using reduced data, based on the BIC\n')
```

    ## Best model (Forward Selection) by using reduced data, based on the BIC

``` r
cat('----------------------------------------------------------------------\n')
```

    ## ----------------------------------------------------------------------

``` r
for (i in 1:28){
  if (regfwd.summary_new$which[20,i] == TRUE){
    print(colnames(regfwd.summary_new$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"

We can see that the Best Subset Selection and Forward Selection suggests
the same predictors by using as input both the reduced dataframe and
initial one.

### Backward Stepwise Selection

``` r
regfit.bwd <- regsubsets(Price ~ ., data = df_scaled,
    nvmax = 39, method = "backward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Reordering variables and trying again:

    ## Warning in rval$lopt[] <- rval$vorder[rval$lopt]: number of items to replace is
    ## not a multiple of replacement length

``` r
regbwd.summary <- summary(regfit.bwd)

data.frame(
  Adj.R2 = which.max(regbwd.summary$adjr2),
  CP = which.min(regbwd.summary$cp),
  BIC = which.min(regbwd.summary$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     32 29  22

``` r
cat('Best model (Backward Selection) by using initial data, based on the BIC\n')
```

    ## Best model (Backward Selection) by using initial data, based on the BIC

``` r
cat('-----------------------------------------------------------------------\n')
```

    ## -----------------------------------------------------------------------

``` r
for (i in 1:39){
  if (regbwd.summary$which[20,i] == TRUE){
    print(colnames(regbwd.summary$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"

``` r
# using the reduced data
regfit.bwd_new <- regsubsets(Price ~ ., data = df_scaled_new,
    nvmax = 28, method = "backward")
regbwd.summary_new <- summary(regfit.bwd_new)
data.frame(
  Adj.R2 = which.max(regbwd.summary_new$adjr2),
  CP = which.min(regbwd.summary_new$cp),
  BIC = which.min(regbwd.summary_new$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     28 28  22

``` r
cat('Best model (Backward Selection) by using reduced data, based on the BIC\n')
```

    ## Best model (Backward Selection) by using reduced data, based on the BIC

``` r
cat('-----------------------------------------------------------------------\n')
```

    ## -----------------------------------------------------------------------

``` r
for (i in 1:28){
  if (regbwd.summary_new$which[20,i] == TRUE){
    print(colnames(regbwd.summary_new$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Distance"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"

- Similarly, the Backward method using the reduced dataframe suggests
  the same predictors with the other 2 ones.
- Although Best Subset Selection recommends same number with the Best
  and Backward the predictors differ.

``` r
# best subset
regfit_poly <- regsubsets(Price ~ . -Distance + poly(Distance, 4), data = df_scaled,
    nvmax = 42)
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Reordering variables and trying again:

``` r
reg.summary_poly <- summary(regfit_poly)

data.frame(
  Adj.R2 = which.max(reg.summary_poly$adjr2),
  CP = which.min(reg.summary_poly$cp),
  BIC = which.min(reg.summary_poly$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     37 34  22

``` r
cat('Best polynomial model (Best Subset Selection) based on the BIC\n')
```

    ## Best polynomial model (Best Subset Selection) based on the BIC

``` r
cat('--------------------------------------------------------------\n')
```

    ## --------------------------------------------------------------

``` r
for (i in 1:42){
  if (reg.summary_poly$which[22,i] == TRUE){
    print(colnames(reg.summary_poly$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_SP"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"
    ## [1] "Month_NOV"
    ## [1] "poly(Distance, 4)1"
    ## [1] "poly(Distance, 4)2"
    ## [1] "poly(Distance, 4)3"

``` r
# forward
regfit.fwd_poly <- regsubsets(Price ~ . -Distance + poly(Distance, 4), data = df_scaled,
    nvmax = 42, method = "forward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Reordering variables and trying again:

    ## Warning in rval$lopt[] <- rval$vorder[rval$lopt]: number of items to replace is
    ## not a multiple of replacement length

``` r
regfwd.summary_poly <- summary(regfit.fwd_poly)

data.frame(
  Adj.R2 = which.max(regfwd.summary_poly$adjr2),
  CP = which.min(regfwd.summary_poly$cp),
  BIC = which.min(regfwd.summary_poly$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     35 35  22

``` r
cat('Best polynomial model (Forward Selection) based on the BIC\n')
```

    ## Best polynomial model (Forward Selection) based on the BIC

``` r
cat('--------------------------------------------------------------\n')
```

    ## --------------------------------------------------------------

``` r
for (i in 1:42){
  if (regfwd.summary_poly$which[22,i] == TRUE){
    print(colnames(regfwd.summary_poly$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_SP"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_JUL"
    ## [1] "Month_NOV"
    ## [1] "poly(Distance, 4)1"
    ## [1] "poly(Distance, 4)2"
    ## [1] "poly(Distance, 4)3"

``` r
# backward 
regfit.bwd_poly <- regsubsets(Price ~ . -Distance + poly(Distance, 4), data = df_scaled,
    nvmax = 42, method = "backward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in =
    ## force.in, : 1 linear dependencies found

    ## Reordering variables and trying again:

    ## Warning in rval$lopt[] <- rval$vorder[rval$lopt]: number of items to replace is
    ## not a multiple of replacement length

``` r
regbwd.summary_poly <- summary(regfit.bwd_poly)

data.frame(
  Adj.R2 = which.max(regbwd.summary_poly$adjr2),
  CP = which.min(regbwd.summary_poly$cp),
  BIC = which.min(regbwd.summary_poly$bic)
)
```

    ##   Adj.R2 CP BIC
    ## 1     37 34  24

``` r
cat('Best polynomial model (Backward Selection) based on the BIC\n')
```

    ## Best polynomial model (Backward Selection) based on the BIC

``` r
cat('--------------------------------------------------------------\n')
```

    ## --------------------------------------------------------------

``` r
for (i in 1:42){
  if (regbwd.summary_poly$which[24,i] == TRUE){
    print(colnames(regbwd.summary_poly$which)[i])
  }
}
```

    ## [1] "(Intercept)"
    ## [1] "Rooms"
    ## [1] "Postcode"
    ## [1] "Bathroom"
    ## [1] "Car"
    ## [1] "Landsize"
    ## [1] "BuildingArea"
    ## [1] "YearBuilt"
    ## [1] "Year"
    ## [1] "Type_h"
    ## [1] "Type_u"
    ## [1] "Method_S"
    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Regionname_SM"
    ## [1] "Regionname_NM"
    ## [1] "Regionname_EM"
    ## [1] "Regionname_SEM"
    ## [1] "Regionname_EV"
    ## [1] "Month_APR"
    ## [1] "Month_MAY"
    ## [1] "Month_JUL"
    ## [1] "poly(Distance, 4)1"
    ## [1] "poly(Distance, 4)2"
    ## [1] "poly(Distance, 4)3"

Best Subset and Forward Selection methods suggest the same model,
whereas the Backward Selection proposes a different one with 2 more
predictors.

### Compare the suggested best models

#### BASED ON BIC

``` r
# scaled data
df_best_fw_scaled <- subset(df_scaled, select = c(Price, Rooms,Distance,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_SP,Regionname_SM, Regionname_WM,
Regionname_EM,Regionname_SEM,Regionname_EV,Month_JUL,Month_NOV))


df_bw_scaled <- subset(df_scaled, select = c(Price,Rooms,Distance,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_PI,Method_VB,Regionname_SM, Regionname_NM,Regionname_EM,
Regionname_SEM,Regionname_EV,Month_JUL))

df_scaled$Distance2 <- I(df_scaled$Distance^2)
df_scaled$Distance3 <- I(df_scaled$Distance^3)

df_best_fw_poly_scaled <-subset(df_scaled, select = c(Price,Rooms,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_SP,Regionname_SM, Regionname_NM,Regionname_EM,Regionname_SEM,Regionname_EV,
Month_NOV,Month_JUL, Distance2, Distance3))

df_bw_poly_scaled <- subset(df_scaled, select = c(Price,Rooms,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u, 
Method_S,Method_PI,Method_VB,Regionname_SM, Regionname_NM,Regionname_EM,Regionname_SEM,Regionname_EV,
Month_APR,Month_JUL, Month_MAY,Distance2, Distance3))
```

``` r
# Scaled data
# estimate the error of the above models using the 10-fold cross validation to compare them

cat('By using scaled data')
```

    ## By using scaled data

``` r
cat('\n------------------')
```

    ## 
    ## ------------------

``` r
cat('\n---Based on BIC---')
```

    ## 
    ## ---Based on BIC---

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the initial data')
```

    ## 
    ## CV error in case of the initial data

``` r
cat('\n------------------------------------')
```

    ## 
    ## ------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_fw_scaled)
set.seed(1)
cat('\nBest Subset and Forward Stepwise Selection:', round(cv.glm(df_best_fw_scaled, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Best Subset and Forward Stepwise Selection: 0.267

``` r
glm.fit <- glm(Price ~ ., data = df_bw_scaled)
set.seed(1)
cat('\nBackward Stepwise Selection::', round(cv.glm(df_bw_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Backward Stepwise Selection:: 0.267

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the polynomial data')
```

    ## 
    ## CV error in case of the polynomial data

``` r
cat('\n---------------------------------------')
```

    ## 
    ## ---------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_fw_poly_scaled)
set.seed(1)
cat('\nBest Subset Selection and Forward Stepwise Selection:', round(cv.glm(df_best_fw_poly_scaled, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Best Subset Selection and Forward Stepwise Selection: 0.351

``` r
glm.fit <- glm(Price ~ ., data = df_bw_poly_scaled)
set.seed(1)
cat('\nBackward Stepwise Selection::', round(cv.glm(df_bw_poly_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Backward Stepwise Selection:: 0.351

``` r
#using the unscaled data set

df_best_fw <- subset(df, select = c(Price, Rooms,Distance,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_SP,Regionname_SM, Regionname_WM,
Regionname_EM,Regionname_SEM,Regionname_EV,Month_JUL,Month_NOV))


df_bw <- subset(df, select = c(Price,Rooms,Distance,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_PI,Method_VB,Regionname_SM, Regionname_NM,Regionname_EM,
Regionname_SEM,Regionname_EV,Month_JUL))

df$Distance2 <- I(df$Distance^2)
df$Distance3 <- I(df$Distance^3)

df_best_fw_poly <-subset(df, select = c(Price,Rooms,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u,
Method_S,Method_SP,Regionname_SM, Regionname_NM,Regionname_EM,Regionname_SEM,Regionname_EV,
Month_NOV,Month_JUL, Distance2, Distance3))

df_bw_poly <- subset(df, select = c(Price,Rooms,Postcode,Bathroom,Car,Landsize,BuildingArea,
YearBuilt, Year,Type_h,Type_u, 
Method_S,Method_PI,Method_VB,Regionname_SM, Regionname_NM,Regionname_EM,Regionname_SEM,Regionname_EV,
Month_APR,Month_JUL, Month_MAY,Distance2, Distance3))
```

``` r
# estimate the error of the above models using the 10-fold cross validation to compare them

cat('By using unscaled data')
```

    ## By using unscaled data

``` r
cat('\n-------------------')
```

    ## 
    ## -------------------

``` r
cat('\n---Based on BIC----')
```

    ## 
    ## ---Based on BIC----

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the initial data')
```

    ## 
    ## CV error in case of the initial data

``` r
cat('\n------------------------------------')
```

    ## 
    ## ------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_fw)
set.seed(1)
cat('\nBest Subset and Forward Stepwise Selection:', format(round(cv.glm(df_best_fw, glm.fit, K = 10)$delta[1],3),big.mark=","))
```

    ## 
    ## Best Subset and Forward Stepwise Selection: 0.07

``` r
glm.fit <- glm(Price ~ ., data = df_bw)
set.seed(1)
cat('\nBackward Stepwise Selection:', format(round(cv.glm(df_bw, glm.fit, K = 10)$delta[1], 3),big.mark=","))
```

    ## 
    ## Backward Stepwise Selection: 0.07

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the polynomial data')
```

    ## 
    ## CV error in case of the polynomial data

``` r
cat('\n---------------------------------------')
```

    ## 
    ## ---------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_fw_poly)
set.seed(1)
cat('\nBest Subset Selection and Forward Stepwise Selection:', format(round(cv.glm(df_best_fw_poly, glm.fit, K = 10)$delta[1],3),big.mark=","))
```

    ## 
    ## Best Subset Selection and Forward Stepwise Selection: 0.07

``` r
glm.fit <- glm(Price ~ ., data = df_bw_poly)
set.seed(1)
cat('\nBackward Stepwise Selection::', format(round(cv.glm(df_bw_poly, glm.fit, K = 10)$delta[1], 3),big.mark=","))
```

    ## 
    ## Backward Stepwise Selection:: 0.07

It is clear that the methods provide very similar MSE. Overall so far,
the best model is the one which is suggests either from Best and Forward
Stepwise Selection using initial data, or the Backward Stepwise
Selection using initial data.

Although the polynomial data fits better on data (based on the anova) it
seems that it does not provide better MSE.

#### BASED ON Cp

``` r
# using initial data
cat('Predictors that are not included in best model by using initial data, based on the Cp\n')
```

    ## Predictors that are not included in best model by using initial data, based on the Cp

``` r
cat('-------------------------------------------------------------------------------------\n')
```

    ## -------------------------------------------------------------------------------------

``` r
for (i in 1:40){
  if (reg.summary$which[29,i] == FALSE){
    print(colnames(reg.summary$which)[i])
  }
}
```

    ## [1] "Propertycount"
    ## [1] "Method_SP"
    ## [1] "Regionname_WM"
    ## [1] "Month_JAN"
    ## [1] "Month_OCT"
    ## [1] "Month_NOV"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_MON"
    ## [1] "Day_TUE"

``` r
cat('\n')
```

``` r
cat('Predictors that are not included in best model (Forward Selection) by using initial data, based on the Cp\n')
```

    ## Predictors that are not included in best model (Forward Selection) by using initial data, based on the Cp

``` r
cat('---------------------------------------------------------------------------------------------------------\n')
```

    ## ---------------------------------------------------------------------------------------------------------

``` r
for (i in 1:40){
  if (regfwd.summary$which[31,i] == FALSE){
    print(colnames(regfwd.summary$which)[i])
  }
}
```

    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Month_JAN"
    ## [1] "Month_AUG"
    ## [1] "Month_OCT"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_TUE"

``` r
cat('\n')
```

``` r
cat('Predictors that are not included in best model (Backward Selection) by using initial data, based on the Cp\n')
```

    ## Predictors that are not included in best model (Backward Selection) by using initial data, based on the Cp

``` r
cat('----------------------------------------------------------------------------------------------------------\n')
```

    ## ----------------------------------------------------------------------------------------------------------

``` r
for (i in 1:40){
  if (regbwd.summary$which[30,i] == FALSE){
    print(colnames(regbwd.summary$which)[i])
  }
}
```

    ## [1] "Propertycount"
    ## [1] "Method_SP"
    ## [1] "Month_JAN"
    ## [1] "Month_OCT"
    ## [1] "Month_NOV"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_MON"
    ## [1] "Day_TUE"

``` r
# polynomial data
cat('Predictors that are not included in best polynomial model (Best Subset Selection) based on the Cp\n')
```

    ## Predictors that are not included in best polynomial model (Best Subset Selection) based on the Cp

``` r
cat('-------------------------------------------------------------------------------------------------\n')
```

    ## -------------------------------------------------------------------------------------------------

``` r
for (i in 1:43){
  if (reg.summary_poly$which[34,i] == FALSE){
    print(colnames(reg.summary_poly$which)[i])
  }
}
```

    ## [1] "Method_SP"
    ## [1] "Month_JAN"
    ## [1] "Month_OCT"
    ## [1] "Month_NOV"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_MON"
    ## [1] "Day_TUE"

``` r
cat('Predictors that are not included in best polynomial model (Forward Selection) based on the Cp\n')
```

    ## Predictors that are not included in best polynomial model (Forward Selection) based on the Cp

``` r
cat('---------------------------------------------------------------------------------------------\n')
```

    ## ---------------------------------------------------------------------------------------------

``` r
for (i in 1:43){
  if (regfwd.summary_poly$which[35,i] == FALSE){
    print(colnames(regfwd.summary_poly$which)[i])
  }
}
```

    ## [1] "Method_PI"
    ## [1] "Method_VB"
    ## [1] "Month_JAN"
    ## [1] "Month_OCT"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_TUE"

``` r
cat('Predictors that are not included in best polynomial model (Backward Selection) based on the Cp\n')
```

    ## Predictors that are not included in best polynomial model (Backward Selection) based on the Cp

``` r
cat('----------------------------------------------------------------------------------------------\n')
```

    ## ----------------------------------------------------------------------------------------------

``` r
for (i in 1:43){
  if (regbwd.summary_poly$which[34,i] == FALSE){
    print(colnames(regbwd.summary_poly$which)[i])
  }
}
```

    ## [1] "Method_SP"
    ## [1] "Month_JAN"
    ## [1] "Month_OCT"
    ## [1] "Month_NOV"
    ## [1] "Day_SAT"
    ## [1] "Day_SUN"
    ## [1] "Day_MON"
    ## [1] "Day_TUE"

``` r
# scaled data

df_best_scaled <- subset(df_scaled, select = -c(Bedroom2,Propertycount,Method_SP,
Month_JAN,Month_OCT,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE,Distance2,Distance3))

df_fw_scaled <- subset(df_scaled, select = -c(Propertycount,Method_PI,Method_VB,
Month_JAN,Month_OCT,Day_SAT,Day_SUN,Day_TUE,Distance2,Distance3))

df_bw_scaled <- subset(df_scaled, select = -c(Bedroom2,Propertycount,Method_PI,
Regionname_NV,Month_JAN,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE,Distance2,Distance3))


df_scaled$Distance4 <- I(df_scaled$Distance^4)

df_best_bw_poly_scaled <- subset(df_scaled, select = -c(Method_SP,Month_JAN,Month_OCT,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE))

df_fw_poly_scaled <-subset(df_scaled, select = -c(Method_PI,Method_VB,Month_JAN,Month_OCT,Day_SAT,Day_SUN,Day_TUE))
```

``` r
# Scaled data
# estimate the error of the above models using the 10-fold cross validation to compare them

cat('By using scaled data')
```

    ## By using scaled data

``` r
cat('\n------------------')
```

    ## 
    ## ------------------

``` r
cat('\n---Based on Cp----')
```

    ## 
    ## ---Based on Cp----

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the initial data')
```

    ## 
    ## CV error in case of the initial data

``` r
cat('\n------------------------------------')
```

    ## 
    ## ------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_scaled)
set.seed(1)
cat('\nBest Subset Selection:', round(cv.glm(df_best_scaled, glm.fit, K = 10)$delta[1],3)) 
```

    ## 
    ## Best Subset Selection: 0.267

``` r
glm.fit <- glm(Price ~ ., data = df_fw_scaled)
set.seed(1)
cat('\nForward Stepwise Selection:', round(cv.glm(df_fw_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Forward Stepwise Selection: 0.267

``` r
glm.fit <- glm(Price ~ ., data = df_bw_scaled)
set.seed(1)
cat('\nBackward Stepwise Selection:', round(cv.glm(df_bw_scaled, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Backward Stepwise Selection: 0.267

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the polynomial data')
```

    ## 
    ## CV error in case of the polynomial data

``` r
cat('\n---------------------------------------')
```

    ## 
    ## ---------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_bw_poly_scaled)
set.seed(1)
cat('\nBest Subset and Backward Stepwise Selection:', round(cv.glm(df_best_bw_poly_scaled, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Best Subset and Backward Stepwise Selection: 0.264

``` r
glm.fit <- glm(Price ~ ., data = df_fw_poly_scaled)
set.seed(1)
cat('\nForward Stepwise Selection:', round(cv.glm(df_fw_poly_scaled, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Forward Stepwise Selection: 0.264

``` r
# unscaled data

df_best <- subset(df, select = -c(Bedroom2,Propertycount,Method_SP,
Month_JAN,Month_OCT,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE,Distance2,Distance3))

df_fw <- subset(df, select = -c(Propertycount,Method_PI,Method_VB,
Month_JAN,Month_OCT,Day_SAT,Day_SUN,Day_TUE,Distance2,Distance3))

df_bw <- subset(df, select = -c(Bedroom2,Propertycount,Method_PI,
Regionname_NV,Month_JAN,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE,Distance2,Distance3))


df$Distance4 <- I(df$Distance^4)

df_best_bw_poly <- subset(df, select = -c(Method_SP,Month_JAN,Month_OCT,Month_NOV,Day_SAT,Day_SUN,Day_MON,Day_TUE))

df_fw_poly <-subset(df, select = -c(Method_PI,Method_VB,Month_JAN,Month_OCT,Day_SAT,Day_SUN,Day_TUE))
```

``` r
# unscaled data
# estimate the error of the above models using the 10-fold cross validation to compare them

cat('By using unscaled data')
```

    ## By using unscaled data

``` r
cat('\n-------------------')
```

    ## 
    ## -------------------

``` r
cat('\n----Based on Cp----')
```

    ## 
    ## ----Based on Cp----

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the initial data')
```

    ## 
    ## CV error in case of the initial data

``` r
cat('\n------------------------------------')
```

    ## 
    ## ------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best)
set.seed(1)
cat('\nBest Subset Selection:', round(cv.glm(df_best, glm.fit, K = 10)$delta[1],3)) 
```

    ## 
    ## Best Subset Selection: 0.069

``` r
glm.fit <- glm(Price ~ ., data = df_fw)
set.seed(1)
cat('\nForward Stepwise Selection:', round(cv.glm(df_fw, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Forward Stepwise Selection: 0.07

``` r
glm.fit <- glm(Price ~ ., data = df_bw)
set.seed(1)
cat('\nBackward Stepwise Selection:', round(cv.glm(df_bw, glm.fit, K = 10)$delta[1], 3))
```

    ## 
    ## Backward Stepwise Selection: 0.07

``` r
cat('\n')
```

``` r
cat('\nCV error in case of the polynomial data')
```

    ## 
    ## CV error in case of the polynomial data

``` r
cat('\n---------------------------------------')
```

    ## 
    ## ---------------------------------------

``` r
glm.fit <- glm(Price ~ ., data = df_best_bw_poly)
set.seed(1)
cat('\nBest Subset and Backward Stepwise Selection:', round(cv.glm(df_best_bw_poly, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Best Subset and Backward Stepwise Selection: 0.069

``` r
glm.fit <- glm(Price ~ ., data = df_fw_poly)
set.seed(1)
cat('\nForward Stepwise Selection:', round(cv.glm(df_fw_poly, glm.fit, K = 10)$delta[1],3))
```

    ## 
    ## Forward Stepwise Selection: 0.069

Thus, the best models, so far, are the polynomial ones which are
suggested based on Cp and they provide about 0.264 error in scaled data.
Specifically the models have either 34 or 35 features, hence we could
select the simpler one from Best Subset and Backward Stepwise Selection.

``` r
model <- data.frame()
for (i in 1:43){
  if (reg.summary_poly$which[34,i] == TRUE){
    model[nrow(model)+1,1] <- colnames(reg.summary_poly$which)[i]
  }
}
colnames(model)[1] <- 'Predictors'
model
```

    ##            Predictors
    ## 1         (Intercept)
    ## 2               Rooms
    ## 3            Postcode
    ## 4            Bedroom2
    ## 5            Bathroom
    ## 6                 Car
    ## 7            Landsize
    ## 8        BuildingArea
    ## 9           YearBuilt
    ## 10      Propertycount
    ## 11               Year
    ## 12             Type_h
    ## 13             Type_u
    ## 14           Method_S
    ## 15          Method_PI
    ## 16          Method_VB
    ## 17      Regionname_SM
    ## 18      Regionname_NM
    ## 19      Regionname_WM
    ## 20      Regionname_EM
    ## 21     Regionname_SEM
    ## 22      Regionname_EV
    ## 23      Regionname_NV
    ## 24          Month_FEB
    ## 25          Month_MAR
    ## 26          Month_APR
    ## 27          Month_MAY
    ## 28          Month_JUN
    ## 29          Month_JUL
    ## 30          Month_AUG
    ## 31          Month_SEP
    ## 32 poly(Distance, 4)1
    ## 33 poly(Distance, 4)2
    ## 34 poly(Distance, 4)3
    ## 35 poly(Distance, 4)4

## Other Regression models

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 4.2.3

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(lattice)
```

    ## 
    ## Attaching package: 'lattice'

    ## The following object is masked from 'package:boot':
    ## 
    ##     melanoma

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.3

``` r
#train and test sets for the scaled dataset
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(df_scaled), replace=TRUE, prob=c(0.8,0.2))
train <- df_scaled[sample,]
test <- df_scaled[!sample,]

#Random Forest
rf <- randomForest(Price~., data=train, proximity=TRUE) 
rf_predict <- predict(rf, test)

print(rf)
```

    ## 
    ## Call:
    ##  randomForest(formula = Price ~ ., data = train, proximity = TRUE) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 14
    ## 
    ##           Mean of squared residuals: 0.1261871
    ##                     % Var explained: 87.26

``` r
library("Metrics")
```

    ## Warning: package 'Metrics' was built under R version 4.2.3

    ## 
    ## Attaching package: 'Metrics'

    ## The following objects are masked from 'package:caret':
    ## 
    ##     precision, recall

``` r
#RMSE
RMSE <-rmse(rf_predict, test$Price)

#mse
(RMSE)^2
```

    ## [1] 0.1328644

``` r
# R2
cor(test$Price, rf_predict) ^ 2
```

    ##           [,1]
    ## [1,] 0.8747969

As we expected, the MSE (at about 0.13) is less than the one of the
suggested models from the above models.

\###**Boosted Trees–Stochastic Gradient Boosting**

After getting good results with random forest we decide to implement
Stochastic Gradient Boosting

``` r
df_gdb <- df
library(gbm)
```

    ## Warning: package 'gbm' was built under R version 4.2.3

    ## Loaded gbm 2.1.8.1

``` r
library(lattice)
library(caret)


#model <- gbm(Price ~ ., data = df_gdb) #Simple Model
#model

#Without Preprocessing (Centering, Scaling)
set.seed(1)
df_gdb <- df_gdb[, -c(26)] # remove column 26 (Month_JAN)
model <- train(
  Price ~ .,
  data = df_gdb,
  method = 'gbm',
  verbose = FALSE
)
model
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 12495 samples
    ##    41 predictor
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 12495, 12495, 12495, 12495, 12495, 12495, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      0.3129171  0.6972817  0.2443213
    ##   1                  100      0.2668049  0.7507588  0.2058276
    ##   1                  150      0.2497209  0.7705377  0.1919288
    ##   2                   50      0.2538142  0.7792333  0.1932973
    ##   2                  100      0.2231452  0.8147276  0.1684555
    ##   2                  150      0.2126072  0.8285176  0.1599502
    ##   3                   50      0.2361466  0.7984773  0.1786949
    ##   3                  100      0.2132520  0.8274786  0.1603712
    ##   3                  150      0.2050918  0.8393906  0.1538620
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

Try with Preprocessing (Model2)

``` r
set.seed(1)

model2 <- train(
  Price ~ .,
  data = df_gdb,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)
model2
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 12495 samples
    ##    41 predictor
    ## 
    ## Pre-processing: centered (41), scaled (41) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 12495, 12495, 12495, 12495, 12495, 12495, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      0.3129159  0.6972864  0.2443207
    ##   1                  100      0.2668038  0.7507613  0.2058270
    ##   1                  150      0.2497198  0.7705398  0.1919280
    ##   2                   50      0.2538147  0.7792324  0.1932976
    ##   2                  100      0.2231478  0.8147238  0.1684565
    ##   2                  150      0.2126127  0.8285089  0.1599522
    ##   3                   50      0.2361490  0.7984745  0.1786974
    ##   3                  100      0.2132586  0.8274661  0.1603707
    ##   3                  150      0.2050728  0.8394228  0.1538422
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

Training& Testing (Model_3)

``` r
set.seed(1)
#80/20Split

inTraining <- createDataPartition(df_gdb$Price, p = .80, list = FALSE)
training <- df_gdb[inTraining,]
testing  <- df_gdb[-inTraining,]

#fit our model again using only the training data
set.seed(1)
model3 <- train(
  Price ~ .,
  data = training,
  method = 'gbm',
  preProcess = c("center", "scale"),
  verbose = FALSE
)
model3
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 9998 samples
    ##   41 predictor
    ## 
    ## Pre-processing: centered (41), scaled (41) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 9998, 9998, 9998, 9998, 9998, 9998, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      0.3126148  0.6997374  0.2442565
    ##   1                  100      0.2664774  0.7523227  0.2059453
    ##   1                  150      0.2491639  0.7724177  0.1917659
    ##   2                   50      0.2536907  0.7804250  0.1933376
    ##   2                  100      0.2230986  0.8151642  0.1684656
    ##   2                  150      0.2128560  0.8284470  0.1599645
    ##   3                   50      0.2355477  0.8005909  0.1782712
    ##   3                  100      0.2127052  0.8288552  0.1599087
    ##   3                  150      0.2049135  0.8400971  0.1535956
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
#predict using the model3
predictions = predict(model3, newdata = testing)

# RMSE
sqrt(mean((testing$Price - predictions)^2))
```

    ## [1] 0.1979884

``` r
# R2
cor(testing$Price, predictions) ^ 2
```

    ## [1] 0.8478059

We try with CV (Model 4)

``` r
set.seed(1) #10 Folds CV

ctrl <- trainControl(
  method = "cv",
  number = 10
)

# set.seed(1)
model4 <- train(
  Price ~ .,
  data = training,
  method = 'gbm',
  preProcess = c("center", "scale"),
  trControl = ctrl,
  verbose = FALSE
)
model4
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 9998 samples
    ##   41 predictor
    ## 
    ## Pre-processing: centered (41), scaled (41) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 8998, 8998, 8998, 8998, 8999, 8999, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      0.3117796  0.6985388  0.2432722
    ##   1                  100      0.2657137  0.7526631  0.2050383
    ##   1                  150      0.2486154  0.7726854  0.1908905
    ##   2                   50      0.2536462  0.7799667  0.1933857
    ##   2                  100      0.2227742  0.8152515  0.1680376
    ##   2                  150      0.2120451  0.8293732  0.1591975
    ##   3                   50      0.2351002  0.8002591  0.1779113
    ##   3                  100      0.2117280  0.8299928  0.1592134
    ##   3                  150      0.2038730  0.8414228  0.1527811
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
plot(model4) #small improvement 
```

![](regression_code_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

``` r
predictions = predict(model4, newdata = testing)

# RMSE
sqrt(mean((testing$Price - predictions)^2))
```

    ## [1] 0.1959339

``` r
# R2
cor(testing$Price, predictions) ^ 2
```

    ## [1] 0.8512639

Tuning Hyperparamters (Model5)

``` r
set.seed(1)

tuneGrid <- expand.grid(
 n.trees = c(50, 100),
 interaction.depth = c(1, 2),
 shrinkage = 0.1,
 n.minobsinnode = 10
)

model5 <- train(
  Price ~ .,
  data = df_gdb,
  method = 'gbm',
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = FALSE
)
model5
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 12495 samples
    ##    41 predictor
    ## 
    ## Pre-processing: centered (41), scaled (41) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 11246, 11246, 11246, 11244, 11246, 11246, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE       Rsquared   MAE      
    ##   1                   50      0.3111960  0.6996665  0.2433703
    ##   1                  100      0.2648443  0.7543224  0.2047024
    ##   2                   50      0.2517005  0.7827523  0.1918360
    ##   2                  100      0.2211586  0.8175277  0.1671673
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 100, interaction.depth =
    ##  2, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
predictions = predict(model5, newdata = testing)

# RMSE
sqrt(mean((testing$Price - predictions)^2))
```

    ## [1] 0.2125467

``` r
# R2
cor(testing$Price, predictions) ^ 2
```

    ## [1] 0.8314218

``` r
library(gridExtra)
```

    ## Warning: package 'gridExtra' was built under R version 4.2.3

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
plot2 <- plot(model2)
plot3 <- plot(model3)
plot4 <- plot(model4)
plot5 <- plot(model5)

# assuming you have 4 plots saved as p1, p2, p3, and p4
grid.arrange(plot2, plot3, plot4, plot5, ncol = 2)
```

![](regression_code_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

We can conclude that stochastic gradient boosting give the best results
so far

## Principal Components Regression

``` r
library(pls)
```

    ## Warning: package 'pls' was built under R version 4.2.3

    ## 
    ## Attaching package: 'pls'

    ## The following object is masked from 'package:caret':
    ## 
    ##     R2

    ## The following object is masked from 'package:corrplot':
    ## 
    ##     corrplot

    ## The following object is masked from 'package:stats':
    ## 
    ##     loadings

``` r
x <- model.matrix(Price ~ .,df_scaled)[, -1]
y <- df_scaled$Price

set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

pcr.fit <- pcr(Price ~ ., data = df_scaled, subset = train, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
```

![](regression_code_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

``` r
summary(pcr.fit)
```

    ## Data:    X dimension: 6246 42 
    ##  Y dimension: 6246 1
    ## Fit method: svdpc
    ## Number of components considered: 42
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV           1.005   0.9983   0.9939   0.7865   0.6988   0.6949   0.6933
    ## adjCV        1.005   0.9983   0.9938   0.7863   0.6984   0.6948   0.6933
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV      0.6742   0.6743   0.6743    0.6741    0.6610    0.6561    0.6324
    ## adjCV   0.6740   0.6741   0.6740    0.6739    0.6604    0.6558    0.6321
    ##        14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps
    ## CV       0.6194    0.5958    0.5785    0.5719    0.5716    0.5678    0.5670
    ## adjCV    0.6191    0.5955    0.5782    0.5715    0.5718    0.5676    0.5667
    ##        21 comps  22 comps  23 comps  24 comps  25 comps  26 comps  27 comps
    ## CV       0.5542    0.5531    0.5503    0.5328    0.5237    0.5236    0.5237
    ## adjCV    0.5538    0.5528    0.5500    0.5325    0.5235    0.5235    0.5235
    ##        28 comps  29 comps  30 comps  31 comps  32 comps  33 comps  34 comps
    ## CV       0.5237    0.5237    0.5237    0.5238    0.5223    0.5219    0.5168
    ## adjCV    0.5235    0.5235    0.5235    0.5236    0.5215    0.5216    0.5166
    ##        35 comps  36 comps  37 comps  38 comps  39 comps  40 comps  41 comps
    ## CV       0.5166    0.5165    0.5166    0.5165    0.5167    0.5168    0.5151
    ## adjCV    0.5164    0.5162    0.5164    0.5162    0.5165    0.5166    0.5149
    ##        42 comps
    ## CV       0.5153
    ## adjCV    0.5149
    ## 
    ## TRAINING: % variance explained
    ##        1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
    ## X       99.453   99.685    99.78    99.81    99.83    99.86    99.88    99.90
    ## Price    1.315    2.162    39.29    51.89    52.43    52.66    55.46    55.47
    ##        9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
    ## X        99.91     99.92     99.93     99.94     99.95     99.95     99.96
    ## Price    55.49     55.54     57.31     58.00     61.05     62.56     65.24
    ##        16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
    ## X         99.96     99.96     99.97     99.97     99.98     99.98     99.98
    ## Price     67.30     68.14     68.14     68.60     68.66     69.98     70.12
    ##        23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
    ## X         99.98     99.99     99.99     99.99     99.99     99.99    100.00
    ## Price     70.43     72.21     73.14     73.16     73.16     73.17     73.17
    ##        30 comps  31 comps  32 comps  33 comps  34 comps  35 comps  36 comps
    ## X        100.00    100.00    100.00     100.0    100.00    100.00    100.00
    ## Price     73.18     73.19     73.36      73.4     73.92     73.94     73.97
    ##        37 comps  38 comps  39 comps  40 comps  41 comps  42 comps
    ## X        100.00    100.00    100.00    100.00    100.00    100.00
    ## Price     73.97     74.01     74.03     74.03     74.17     74.18

``` r
# we could choose the number of components being equal to 41
pcr.pred <- predict(pcr.fit, x[test, ], ncomp=41)
mse = mean((pcr.pred - y.test)^2)
mse
```

    ## [1] 0.2654098

``` r
# compare the error of PCR using the best number of components with the error of previous Cp best model

mse - 0.264
```

    ## [1] 0.001409817

We conclude that the best model so far (apart from the random forest
method) is the previous one, from linear (polynomial) model based on Cp.

# Partial Least Squares

``` r
set.seed(1)
pls.fit <- plsr(Price ~ ., data = df_scaled, subset = train, validation = "CV")
summary(pls.fit)
```

    ## Data:    X dimension: 6246 42 
    ##  Y dimension: 6246 1
    ## Fit method: kernelpls
    ## Number of components considered: 42
    ## 
    ## VALIDATION: RMSEP
    ## Cross-validated using 10 random segments.
    ##        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
    ## CV           1.005   0.9983   0.7362   0.6846   0.6109   0.5441   0.5309
    ## adjCV        1.005   0.9983   0.7359   0.6860   0.6108   0.5438   0.5306
    ##        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
    ## CV      0.5217   0.5204   0.5191    0.5185    0.5181    0.5178    0.5176
    ## adjCV   0.5215   0.5202   0.5189    0.5183    0.5179    0.5175    0.5174
    ##        14 comps  15 comps  16 comps  17 comps  18 comps  19 comps  20 comps
    ## CV       0.5175    0.5176    0.5175    0.5175    0.5176    0.5178    0.5180
    ## adjCV    0.5172    0.5173    0.5172    0.5172    0.5173    0.5175    0.5176
    ##        21 comps  22 comps  23 comps  24 comps  25 comps  26 comps  27 comps
    ## CV       0.5182    0.5182    0.5183    0.5183    0.5183    0.5184    0.5185
    ## adjCV    0.5178    0.5178    0.5179    0.5179    0.5179    0.5180    0.5181
    ##        28 comps  29 comps  30 comps  31 comps  32 comps  33 comps  34 comps
    ## CV       0.5185    0.5186    0.5185    0.5184    0.5178    0.5174    0.5172
    ## adjCV    0.5181    0.5181    0.5180    0.5179    0.5174    0.5169    0.5168
    ##        35 comps  36 comps  37 comps  38 comps  39 comps  40 comps  41 comps
    ## CV       0.5172    0.5172    0.5172    0.5172    0.5172    0.5172    0.5172
    ## adjCV    0.5168    0.5168    0.5168    0.5168    0.5168    0.5168    0.5168
    ##        42 comps
    ## CV       0.5172
    ## adjCV    0.5168
    ## 
    ## TRAINING: % variance explained
    ##        1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
    ## X       99.450    99.56    99.76    99.80    99.81    99.83    99.84    99.86
    ## Price    1.399    47.79    53.61    63.48    71.21    72.58    73.43    73.58
    ##        9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
    ## X        99.87     99.88     99.90     99.91     99.92     99.93     99.94
    ## Price    73.74     73.82     73.87     73.93     73.96     73.97     73.97
    ##        16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
    ## X         99.95     99.95     99.96     99.96     99.96     99.97     99.97
    ## Price     73.98     73.99     74.02     74.03     74.04     74.05     74.06
    ##        23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
    ## X         99.97     99.97     99.98     99.98     99.98     99.98     99.98
    ## Price     74.07     74.08     74.09     74.10     74.11     74.12     74.14
    ##        30 comps  31 comps  32 comps  33 comps  34 comps  35 comps  36 comps
    ## X         99.99     99.99     99.99     99.99     99.99     99.99    100.00
    ## Price     74.14     74.14     74.16     74.17     74.17     74.17     74.17
    ##        37 comps  38 comps  39 comps  40 comps  41 comps  42 comps
    ## X        100.00    100.00    100.00    100.00    100.00    100.70
    ## Price     74.17     74.17     74.17     74.17     74.17     74.17

``` r
validationplot(pls.fit, val.type = "MSEP")
```

![](regression_code_files/figure-gfm/unnamed-chunk-91-1.png)<!-- -->

``` r
pls.pred <- predict(pls.fit, x[test,])
err.pls <- mean((pls.pred-y.test)^2)
err.pls
```

    ## [1] 0.2967832

``` r
#with 34 components 
pls.fit <- plsr(Price ~ ., data = df_scaled,ncomp = 34)
summary(pls.fit)
```

    ## Data:    X dimension: 12492 42 
    ##  Y dimension: 12492 1
    ## Fit method: kernelpls
    ## Number of components considered: 34
    ## TRAINING: % variance explained
    ##        1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
    ## X       99.453    99.56    99.75    99.80    99.81    99.83    99.84    99.86
    ## Price    1.316    48.65    54.13    63.55    70.74    72.15    73.02    73.21
    ##        9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
    ## X        99.87     99.88     99.90     99.91     99.92     99.93     99.94
    ## Price    73.37     73.46     73.52     73.57     73.60     73.61     73.62
    ##        16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
    ## X         99.95     99.95     99.96     99.96     99.96     99.97     99.97
    ## Price     73.62     73.64     73.67     73.68     73.69     73.70     73.70
    ##        23 comps  24 comps  25 comps  26 comps  27 comps  28 comps  29 comps
    ## X         99.97     99.97     99.98     99.98     99.98     99.98     99.98
    ## Price     73.71     73.71     73.72     73.73     73.74     73.75     73.77
    ##        30 comps  31 comps  32 comps  33 comps  34 comps
    ## X         99.99     99.99     99.99     99.99     99.99
    ## Price     73.77     73.78     73.78     73.79     73.79

``` r
pls.pred <- predict(pls.fit, x[test,], ncomp=34)
err.pls <- mean((pls.pred-y.test)^2)
err.pls
```

    ## [1] 0.262689

# Lasso and Ridge Regression

``` r
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.2.3

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Loaded glmnet 4.1-7

``` r
# using the scaled data set
x <- model.matrix(Price ~ ., df_scaled)[, -1]
y <- df_scaled$Price
```

``` r
# RIDGE
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
```

    ## [1]  43 100

``` r
# train and test
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
```

``` r
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 2, newx = x[test, ])
mean((ridge.pred - y.test)^2)
```

    ## [1] 0.4497056

``` r
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
```

![](regression_code_files/figure-gfm/unnamed-chunk-97-1.png)<!-- -->

``` r
#best lambda proposed is:
bestlam <- cv.out$lambda.min
bestlam
```

    ## [1] 0.06100229

``` r
#using the best lambda
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)
```

    ## [1] 0.2679489

``` r
bestlam1se <- cv.out$lambda.1se
bestlam1se
```

    ## [1] 0.0971329

``` r
# the best lambda according to one standard error rule give a worst test MSE
ridge.pred1se <- predict(ridge.mod, s = bestlam1se,
    newx = x[test, ])
mean((ridge.pred1se - y.test)^2)
```

    ## [1] 0.2707966

``` r
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:40, ]
```

    ##    (Intercept)          Rooms       Distance       Postcode       Bedroom2 
    ##  -2.817026e+02   1.107787e-01  -4.240738e-01   3.241468e-02   5.959845e-02 
    ##       Bathroom            Car       Landsize   BuildingArea      YearBuilt 
    ##   6.162080e-02   4.120553e-02   2.347728e-02   2.803493e-01  -1.001523e-01 
    ##  Propertycount           Year         Type_h         Type_u       Method_S 
    ##  -9.639946e-03   1.396046e-01   3.228618e-01  -5.664008e-01   9.240988e-02 
    ##      Method_SP      Method_PI      Method_VB  Regionname_SM  Regionname_NM 
    ##  -1.155899e-02  -9.840270e-02  -9.057237e-02   4.409882e-01  -2.360973e-01 
    ##  Regionname_WM  Regionname_EM Regionname_SEM  Regionname_EV  Regionname_NV 
    ##  -3.107040e-01   1.742661e-01   3.830657e-01   2.402693e-01  -3.331010e-01 
    ##      Month_JAN      Month_FEB      Month_MAR      Month_APR      Month_MAY 
    ##   0.000000e+00  -5.110705e-02  -5.662424e-03  -6.500763e-02  -4.516920e-02 
    ##      Month_JUN      Month_JUL      Month_AUG      Month_SEP      Month_OCT 
    ##  -3.688831e-02  -1.217490e-01  -2.973334e-03  -1.186099e-02   1.564277e-02 
    ##      Month_NOV        Day_SAT        Day_SUN        Day_MON        Day_TUE 
    ##   4.137162e-02  -3.205746e-03  -4.260870e-03  -4.162670e-02   2.402002e-02

``` r
# LASSO

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)
```

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm):
    ## collapsing to unique 'x' values

![](regression_code_files/figure-gfm/unnamed-chunk-101-1.png)<!-- -->

``` r
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
```

![](regression_code_files/figure-gfm/unnamed-chunk-102-1.png)<!-- -->

``` r
#using the best lambda
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)
```

    ## [1] 0.2685977

``` r
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:40, ]
lasso.coef
```

    ##    (Intercept)          Rooms       Distance       Postcode       Bedroom2 
    ##  -2.387420e+02   1.299360e-01  -4.410490e-01   2.924114e-02   3.112311e-02 
    ##       Bathroom            Car       Landsize   BuildingArea      YearBuilt 
    ##   3.997500e-02   3.629236e-02   1.923496e-02   3.106644e-01  -9.060625e-02 
    ##  Propertycount           Year         Type_h         Type_u       Method_S 
    ##   0.000000e+00   1.182455e-01   3.077788e-01  -6.031532e-01   1.074515e-01 
    ##      Method_SP      Method_PI      Method_VB  Regionname_SM  Regionname_NM 
    ##   0.000000e+00  -5.705138e-02  -4.377827e-02   5.197435e-01  -1.660632e-01 
    ##  Regionname_WM  Regionname_EM Regionname_SEM  Regionname_EV  Regionname_NV 
    ##  -2.312759e-01   2.465542e-01   4.720797e-01   2.332893e-01   0.000000e+00 
    ##      Month_JAN      Month_FEB      Month_MAR      Month_APR      Month_MAY 
    ##   0.000000e+00   0.000000e+00   0.000000e+00  -4.110005e-03   0.000000e+00 
    ##      Month_JUN      Month_JUL      Month_AUG      Month_SEP      Month_OCT 
    ##   0.000000e+00  -6.200853e-02   0.000000e+00   0.000000e+00   0.000000e+00 
    ##      Month_NOV        Day_SAT        Day_SUN        Day_MON        Day_TUE 
    ##   1.672631e-02   0.000000e+00   0.000000e+00   0.000000e+00   0.000000e+00

``` r
#17 coefficients are equal to 0, here 25 are printed
lasso.coef[lasso.coef != 0]
```

    ##    (Intercept)          Rooms       Distance       Postcode       Bedroom2 
    ##  -2.387420e+02   1.299360e-01  -4.410490e-01   2.924114e-02   3.112311e-02 
    ##       Bathroom            Car       Landsize   BuildingArea      YearBuilt 
    ##   3.997500e-02   3.629236e-02   1.923496e-02   3.106644e-01  -9.060625e-02 
    ##           Year         Type_h         Type_u       Method_S      Method_PI 
    ##   1.182455e-01   3.077788e-01  -6.031532e-01   1.074515e-01  -5.705138e-02 
    ##      Method_VB  Regionname_SM  Regionname_NM  Regionname_WM  Regionname_EM 
    ##  -4.377827e-02   5.197435e-01  -1.660632e-01  -2.312759e-01   2.465542e-01 
    ## Regionname_SEM  Regionname_EV      Month_APR      Month_JUL      Month_NOV 
    ##   4.720797e-01   2.332893e-01  -4.110005e-03  -6.200853e-02   1.672631e-02
