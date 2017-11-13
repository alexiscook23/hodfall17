Multiple Models
================
Will Doyle

Introduction
------------

The essence of prediction is discovering the extent to which our models can predict outcomes for data that does not come from our sample. Many times this process is temporal. We fit a model to data from one time period, then take predictors from a subsequent time period to come up with a prediction in the future. For instance, we might use data on team performance to predict the likely winners and losers for upcoming soccer games.

This process does not have to be temporal. We can also have data that is out of sample because it hadn't yet been collected when our first data was collected, or we can also have data that is out of sample because we designated it as out of sample.

The data that is used to generate our predictions is known as *training* data. The idea is that this is the data used to train our model, to let it know what the relationship is between our predictors and our outcome. So far, we have worked mostly with training data.

That data that is used to validate our predictions is known as *testing* data. With testing data, we take our trained model and see how good it is at predicting outcomes using out of sample data.

One very simple approach to this would be to cut our data in half. This is what we've done so far. We could then train our model on half the data, then test it on the other half. This would tell us whether our measure of model fit (e.g. rmse, auc) is similar or different when we apply our model to out of sample data.

But this would only be a "one-shot" approach. It would be better to do this multiple times, cutting the data into two parts: training and testing, then fitting the model to the training data, and then checking its predictions against the testing data. That way, we could generate a large number of rmse's to see how well the model fits on lots of different possible out-of-sample predictions.

This process is called *cross-fold validation*, and it involves two important decisions: first, how will the data be cut, and how many times will the validation run.

We start by getting a new package `modelr` which has nice facilities for creating cross fold validation-ready datasets.

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(modelr)
```

Next we load the quickfacts data, which contains county-by-county information. We're going to create a simple model that predicts median home values in the county as a function of education, home ownership and income.

``` r
load("quickfacts.Rdata")
qf<-qf%>%
  select(median_home_val,median_hh_inc,coll_grad_pc,homeown_rate,per_capita_inc)%>%
       mutate_all(funs(as.numeric))%>%tbl_df()
```

A quick look at this outcome lets us know it needs to be logged.

``` r
gg<-ggplot(data=qf,aes(median_home_val))
gg<-gg+geom_density()
gg
```

![](13-multiple_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-3-1.png)

We can run this model on the full dataset, but we're not taking advantage of the idea of cross-validation.

``` r
## Define the model
mod1_formula<-formula("log(median_home_val+1)~
              log(median_hh_inc+1)+
              homeown_rate+
              coll_grad_pc")
           
## Run the model against all of the data
basic.mod<-lm(mod1_formula,
              data=qf); summary(basic.mod)
```

    ## 
    ## Call:
    ## lm(formula = mod1_formula, data = qf)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.8120  -0.1734   0.0068   0.1814   1.3031 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             2.1088598  0.3825071   5.513  3.8e-08 ***
    ## log(median_hh_inc + 1)  0.8735845  0.0388893  22.463  < 2e-16 ***
    ## homeown_rate           -0.0026253  0.0009254  -2.837  0.00458 ** 
    ## coll_grad_pc            0.0204320  0.0011213  18.221  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3713 on 3191 degrees of freedom
    ## Multiple R-squared:  0.4864, Adjusted R-squared:  0.4859 
    ## F-statistic:  1007 on 3 and 3191 DF,  p-value: < 2.2e-16

The `crossv_kfold` command creates a list of datasets from our original dataset, each of which contains a testing and training dataset. The proportion of cases held out for testing is determined by the number of folds: 10 folds would indicate 1/10 of the data to be held out.

``` r
qf_cf<-qf%>%
  crossv_kfold(10)
qf_cf
```

    ## # A tibble: 10 x 3
    ##             train           test   .id
    ##            <list>         <list> <chr>
    ##  1 <S3: resample> <S3: resample>    01
    ##  2 <S3: resample> <S3: resample>    02
    ##  3 <S3: resample> <S3: resample>    03
    ##  4 <S3: resample> <S3: resample>    04
    ##  5 <S3: resample> <S3: resample>    05
    ##  6 <S3: resample> <S3: resample>    06
    ##  7 <S3: resample> <S3: resample>    07
    ##  8 <S3: resample> <S3: resample>    08
    ##  9 <S3: resample> <S3: resample>    09
    ## 10 <S3: resample> <S3: resample>    10

The `qf_cf` dataset is now a nested dataset, as described in (Chapter 25)\[<http://r4ds.had.co.nz/many-models.html>\] of the Wickham 4ds book.

The next bit of code is key. It starts by converting all of the individual training datasets to tibbles. Then the model is run on each training dataset. Then apply the predictions from the model to each testing dataset, and finally pull the rmse from each of the testing datasets.

``` r
rmse_mod1<-qf_cf %>% 
  mutate(train = map(train, as_tibble)) %>% ## Convert to tibbles
  mutate(model = map(train, ~ lm(mod1_formula,
                                 data = .))) %>%
  mutate(rmse = map2_dbl(model, test, rmse)) %>% ## apply model, get rmse
  select(.id, rmse) ## pull just id and rmse 
```

The resulting dataset includes the id for the cross validation and the rmse. We can summarize and plot this new data frame to see what our likely range of rmse happens to be.

``` r
summary(rmse_mod1$rmse)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2801  0.2861  0.2953  0.3439  0.3023  0.7920

``` r
gg<-ggplot(rmse_mod1,aes(rmse))
gg<-gg+geom_histogram(bins=50)
gg
```

![](13-multiple_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

As this shows, the rmse for the crossfold validations goes from a minimum of 0.28 to a maximum of 0.79, with a median of 0.3.

*Quick Exercise* Run the crossfold command again, but this time only 5 times. Then run it again, but 20 times. What happens to the RMSE?

Full Cross Validation
---------------------

The `crossv_mc` command provides for a generalization of the crossfold command. For this command, we can specify the proportion to be randomly held out in each iteration, via `test=p` where `p` is the proportion to be held out.

``` r
qf_cv<-qf%>%
  crossv_mc(n=1000,test=.2)
qf_cv
```

    ## # A tibble: 1,000 x 3
    ##             train           test   .id
    ##            <list>         <list> <chr>
    ##  1 <S3: resample> <S3: resample>  0001
    ##  2 <S3: resample> <S3: resample>  0002
    ##  3 <S3: resample> <S3: resample>  0003
    ##  4 <S3: resample> <S3: resample>  0004
    ##  5 <S3: resample> <S3: resample>  0005
    ##  6 <S3: resample> <S3: resample>  0006
    ##  7 <S3: resample> <S3: resample>  0007
    ##  8 <S3: resample> <S3: resample>  0008
    ##  9 <S3: resample> <S3: resample>  0009
    ## 10 <S3: resample> <S3: resample>  0010
    ## # ... with 990 more rows

The `qf_cv` dataset is a dataset of 1000x2 datasets, with each row containing a training and testing dataset. The testing dataset is .2 of the sample, but it's different each time.

Now we use the same approach, but with the MUCH larger qf\_cv dataset.

``` r
mod1_rmse_cv<-qf_cv %>% 
  mutate(train = map(train, as_tibble)) %>% ## Convert to tibbles
  mutate(model = map(train, ~ lm(mod1_formula, data = .)))%>%
  mutate(rmse = map2_dbl(model, test, rmse))%>% 
  select(.id, rmse) ## pull just id and rmse 

mod1_rmse_cv
```

    ## # A tibble: 1,000 x 2
    ##      .id      rmse
    ##    <chr>     <dbl>
    ##  1  0001 0.2963750
    ##  2  0002 0.2889695
    ##  3  0003 0.2910074
    ##  4  0004 0.2959773
    ##  5  0005 0.2911588
    ##  6  0006 0.2938672
    ##  7  0007 0.2958140
    ##  8  0008 0.6064119
    ##  9  0009 0.2901191
    ## 10  0010 0.2991592
    ## # ... with 990 more rows

``` r
summary(mod1_rmse_cv$rmse)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2684  0.2905  0.2979  0.3538  0.3097  0.6084

``` r
gg<-ggplot(mod1_rmse_cv,aes(rmse))
gg<-gg+geom_histogram(bins=50)
gg
```

![](13-multiple_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)

Comparing Performance of Models
-------------------------------

It's the comparison between two different cross-validated models that we're really interested in. We want to know which model will perform best in predicting the future.

``` r
## Define the model
mod2_formula<-formula("log(median_home_val+1)~
              log(median_hh_inc+1)+
              homeown_rate+
              coll_grad_pc+
              per_capita_inc")


mod2_rmse_cv<-qf_cv %>% 
  mutate(train = map(train, as_tibble)) %>% ## Convert to tibbles
  mutate(model = map(train, ~ lm(mod2_formula, data = .)))%>%
  mutate(rmse = map2_dbl(model, test, rmse))%>% 
  select(.id, rmse) ## pull just id and rmse 

mod2_rmse_cv      
```

    ## # A tibble: 1,000 x 2
    ##      .id      rmse
    ##    <chr>     <dbl>
    ##  1  0001 0.2969821
    ##  2  0002 0.2904207
    ##  3  0003 0.2965008
    ##  4  0004 0.2976711
    ##  5  0005 0.2928369
    ##  6  0006 0.2969187
    ##  7  0007 0.2961977
    ##  8  0008 0.6082246
    ##  9  0009 0.2925141
    ## 10  0010 0.3039671
    ## # ... with 990 more rows

``` r
summary(mod2_rmse_cv$rmse)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2687  0.2932  0.3009  0.3562  0.3130  0.6117

``` r
summary(mod1_rmse_cv$rmse)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2684  0.2905  0.2979  0.3538  0.3097  0.6084

``` r
gg<-ggplot(mod2_rmse_cv,aes(x=rmse))
gg<-gg+geom_density()
gg<-gg+geom_density(data=mod1_rmse_cv,aes(x=rmse),color="blue")
gg
```

![](13-multiple_models_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-11-1.png)
