# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
library(scales)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(knitr)


raw_data <- read.csv("activity.csv")
act_data <- raw_data %>%
                    tbl_df() %>%
                    mutate(date=as.Date(date),
                           day=factor(wday(date,label = TRUE, abbr = TRUE)),
                           weekend=factor(ifelse(day %in% c('Sun','Sat'),'weekend','weekday')),
                           month=factor(month(date,label = TRUE, abbr = TRUE)),
                           interval=parse_date_time(sprintf("%04d", interval),'hM') #convert intervals to time
                           ) 
summary(act_data)
```

```
##      steps             date               interval               
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :0-01-01 00:00:00  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.:0-01-01 05:58:45  
##  Median :  0.00   Median :2012-10-31   Median :0-01-01 11:57:30  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :0-01-01 11:57:30  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:0-01-01 17:56:15  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :0-01-01 23:55:00  
##  NA's   :2304                                                    
##     day          weekend      month     
##  Sun  :2304   weekday:12960   Oct:8928  
##  Mon  :2592   weekend: 4608   Nov:8640  
##  Tues :2592                             
##  Wed  :2592                             
##  Thurs:2592                             
##  Fri  :2592                             
##  Sat  :2304
```


## What is mean total number of steps taken per day?

```r
act_data %>%
  na.exclude() %>%
  group_by(date) %>%
  summarise(day_steps=sum(steps,na.rm=TRUE)) -> day_act
qplot(day_steps,data=day_act, geom='histogram',binwidth=2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_per_day <- mean(day_act$day_steps)
median_per_day <- median(day_act$day_steps)
```

The mean total number of steps taken per day was 1.0766189\times 10^{4}. Median was 10765.


## What is the average daily activity pattern?

```r
act_data %>%
  na.exclude() %>%
  group_by(interval) %>%
  summarise(mean=mean(steps)) -> daily_act
p <- ggplot(act_data,aes(interval,steps))
p + geom_smooth()+
    geom_line(aes(interval,mean),data=daily_act,col='red')+
    scale_x_datetime("",labels=date_format("%H:%M"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
act_data %>%
  group_by(date) %>%
  summarise(na_sum=100* sum(is.na(steps))/n() ) -> nas
  kable(data.frame(table(nas$na_sum)),
        format = "markdown",
        col.names=c("per day NA's proportion,%","Frequency"))
```



|per day NA's proportion,% | Frequency|
|:-------------------------|---------:|
|0                         |        53|
|100                       |         8|

First we looking when NA's values have occured. So we calculate a proportion of NA values for each day. We see in 53, 8,There are 0 whole days of missing data and 8 of partially missing days. Its make values interpolation impossible and the best way is to through them out from data analisis.


```r
act_data<-na.exclude(act_data)
```



## Are there differences in activity patterns between weekdays and weekends?

```r
p <- ggplot(act_data,aes(interval,steps))
p +facet_grid(weekend~.)+
  stat_smooth(size=3,colour='black')+
  stat_smooth(aes(colour=factor(day)),se=F,alpha=0.5)+
  scale_x_datetime("",labels=date_format("%H:%M"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

