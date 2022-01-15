discover correlation in customer personality analysis data
================
Sascha Siegmund
2022-01-15

## purpose of notebook

-   \[ \]

## insights

-   

## load packages

``` r
library(tidyverse) # tidy data frame
library(ggthemes) # for extra plot themes
library(plotly) # make ggplots interactive

# individual libraries are in the according cell
```

## import data

``` r
df <- read_csv('../data/heart_attack_classification/heart.csv')
```

## overview

``` r
head(df)
```

    ## # A tibble: 6 x 14
    ##     age   sex    cp trtbps  chol   fbs restecg thalachh  exng oldpeak   slp
    ##   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl>    <dbl> <dbl>   <dbl> <dbl>
    ## 1    63     1     3    145   233     1       0      150     0     2.3     0
    ## 2    37     1     2    130   250     0       1      187     0     3.5     0
    ## 3    41     0     1    130   204     0       0      172     0     1.4     2
    ## 4    56     1     1    120   236     0       1      178     0     0.8     2
    ## 5    57     0     0    120   354     0       1      163     1     0.6     2
    ## 6    57     1     0    140   192     0       1      148     0     0.4     1
    ## # ... with 3 more variables: caa <dbl>, thall <dbl>, output <dbl>

``` r
summary(df)
```

    ##       age             sex               cp            trtbps     
    ##  Min.   :29.00   Min.   :0.0000   Min.   :0.000   Min.   : 94.0  
    ##  1st Qu.:47.50   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:120.0  
    ##  Median :55.00   Median :1.0000   Median :1.000   Median :130.0  
    ##  Mean   :54.37   Mean   :0.6832   Mean   :0.967   Mean   :131.6  
    ##  3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:140.0  
    ##  Max.   :77.00   Max.   :1.0000   Max.   :3.000   Max.   :200.0  
    ##       chol            fbs            restecg          thalachh    
    ##  Min.   :126.0   Min.   :0.0000   Min.   :0.0000   Min.   : 71.0  
    ##  1st Qu.:211.0   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:133.5  
    ##  Median :240.0   Median :0.0000   Median :1.0000   Median :153.0  
    ##  Mean   :246.3   Mean   :0.1485   Mean   :0.5281   Mean   :149.6  
    ##  3rd Qu.:274.5   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:166.0  
    ##  Max.   :564.0   Max.   :1.0000   Max.   :2.0000   Max.   :202.0  
    ##       exng           oldpeak          slp             caa        
    ##  Min.   :0.0000   Min.   :0.00   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:1.000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.80   Median :1.000   Median :0.0000  
    ##  Mean   :0.3267   Mean   :1.04   Mean   :1.399   Mean   :0.7294  
    ##  3rd Qu.:1.0000   3rd Qu.:1.60   3rd Qu.:2.000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :6.20   Max.   :2.000   Max.   :4.0000  
    ##      thall           output      
    ##  Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:2.000   1st Qu.:0.0000  
    ##  Median :2.000   Median :1.0000  
    ##  Mean   :2.314   Mean   :0.5446  
    ##  3rd Qu.:3.000   3rd Qu.:1.0000  
    ##  Max.   :3.000   Max.   :1.0000

## correlation plot

-   

``` r
name = c('')
tmp_df <- df %>% select(-output) %>% 
  na.omit()

cor <- cor(tmp_df)
cor_mtest <- corrplot::cor.mtest(tmp_df, conf.level = 0.99) # combining correlogram with significance test
corrplot::corrplot(cor, method = "number", order = 'hclust', addrect = 3, p.mat = cor_mtest$p, insig = "pch") 
```

![](nb_figs/corr_unnamed-chunk-5-1.png)<!-- -->

``` r
corrplot::corrplot(cor, p.mat = cor_mtest$p, low = cor_mtest$lowCI, upp = cor_mtest$uppCI, 
                   order = 'hclust', sig.level = 0.01, tl.pos = 'd', addrect = 3, rect.col = 'navy', 
                   plotC = 'rect', cl.pos = 'n', insig = "p-value")
```

![](nb_figs/corr_unnamed-chunk-5-2.png)<!-- -->

## parallel coordinate plot

-   

``` r
name = c('')
tmp_df <- df %>% select(output, everything()) %>% 
  mutate(output = factor(output)) %>% 
  na.omit() 

# https://r-charts.com/ranking/parallel-coordinates-ggplot2/
parcoord_plot <- GGally::ggparcoord(tmp_df,
           columns = 2:ncol(tmp_df), groupColumn = 1,
           scale='center', # scaling: center, std, robust, 
           showPoints = FALSE,
           order = 'allClass', # scagnostics available
           title = name,
           alphaLines = 0.2) +
  theme_minimal() 
parcoord_plot <- ggplotly(parcoord_plot) %>% layout(autosize=T)

parcoord_plot
```

![](nb_figs/corr_unnamed-chunk-6-1.png)<!-- -->

``` r
name = c('')
tmp_df <- df %>% select(everything()) %>% 
  na.omit()


outlier <- outliers::scores(tmp_df)
outlier <- outlier > 3 | outlier < -3 # more than 3 std away from mean is outlier
outlier <- outlier %>% as_tibble %>% mutate(outlier = rowMeans(.) > 0.2)
tmp_df <- bind_cols(tmp_df, outlier = outlier$outlier) %>% mutate(outlier = as.factor(outlier))

# https://r-charts.com/ranking/parallel-coordinates-ggplot2/
parcoord_plot <- GGally::ggparcoord(tmp_df,
           columns = 1:(ncol(tmp_df)-1), groupColumn = ncol(tmp_df),
           scale='center', # scaling: standardize and center variables
           showPoints = FALSE,
           # order = 'Outlying', # Ordering methods
           title = name,
           alphaLines = 0.1) +
      scale_color_manual(values = c("black", "red")) +
      theme_minimal() 
parcoord_plot <- ggplotly(parcoord_plot) %>% layout(autosize=T)

parcoord_plot
```

![](nb_figs/corr_unnamed-chunk-7-1.png)<!-- -->

``` r
name = c('age', 'output')
tmp_df <- df %>% rename(x = name[1], y = name[2]) %>% select(x, y) %>% na.omit


tmp_df <- as_tibble(scale(tmp_df)) # scaling data required to align parcoord and other plots

outlier <- outliers::scores(tmp_df)
outlier <- outlier > 3 | outlier < -3 # more than 3 std away from mean is outlier
outlier <- outlier %>% as_tibble %>% mutate(outlier = x | y)
tmp_df <- bind_cols(tmp_df, outlier = outlier$outlier) %>% mutate(outlier = as.factor(outlier))

# https://r-charts.com/ranking/parallel-coordinates-ggplot2/
p1 <- GGally::ggparcoord(tmp_df,
           columns = 1:(ncol(tmp_df)-1), groupColumn = ncol(tmp_df),
           scale='globalminmax', # data already scaled
           showPoints = FALSE,
           splineFactor = TRUE,
           alphaLines = 0.1) +
    scale_color_manual(values = c("black", "red")) +
    theme_minimal() +
    ggtitle(paste("correlation of", name[1], "and", name[2], sep=" "))
p1 <- ggplotly(p1) %>% layout(yaxis = list(title = ''), xaxis = list(title = '')) %>%
  add_annotations(text = name[1], align = 'left', yref='paper', yref = 'paper', x = 1, y = 0, showarrow=FALSE) %>%
  add_annotations(text = name[2], align = 'left', yref='paper', yref = 'paper', x = 2, y = 0, showarrow=FALSE)

p2 <- tmp_df %>%
  ggplot(aes(x = y)) +
    stat_density(geom="line", trim = TRUE) + 
    scale_x_continuous(limits = c(min(tmp_df$x, tmp_df$y), max(tmp_df$x, tmp_df$y))) +
    coord_flip() +
    theme_minimal()
p2 <- ggplotly(p2) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

p3 <- tmp_df %>%
  ggplot(aes(x = 1, y = y)) +
    geom_boxplot() +
    theme_minimal() 
p3 <- ggplotly(p3) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

p4 <- tmp_df %>%
  ggplot(aes(x = x)) +
    stat_density(geom="line", trim = TRUE) + 
    scale_x_continuous(limits = c(min(tmp_df$x, tmp_df$y), max(tmp_df$x, tmp_df$y))) +
    coord_flip() +
    theme_minimal()
p4 <- ggplotly(p4) %>% layout(yaxis = list(showticklabels = TRUE, showgrid = TRUE, title = ''),
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = '', 
                                           autorange = "reversed"))

p5 <- tmp_df %>%
  ggplot(aes(x = 1, y = x)) +
    geom_boxplot() +
    theme_minimal() 
p5 <- ggplotly(p5) %>% layout(yaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''), 
                              xaxis = list(showticklabels = FALSE, showgrid = FALSE, title = ''))

# merge figures into one plot, via subplots, https://plotly-r.com/arranging-views.html
fig <- subplot(p4, p5, p1, p3, p2,  
               margin = 0, widths = c(0.05, 0.02, 0.86, 0.02, 0.05), 
               shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>% layout()

fig
```

![](nb_figs/corr_unnamed-chunk-8-1.png)<!-- -->
