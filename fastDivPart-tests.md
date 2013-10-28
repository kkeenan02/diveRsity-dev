Testing divPart versions
==============================

### Kevin Keenan


#### 2013




##### Make sure dev version of `diveRsity` is installed

```r
# library(devtools)

# install_github("diveRsity-dev", "kkeenan02")

library(diveRsity)
```

```
Loading required package: plotrix Loading required package: shiny Loading
required package: ggplot2 Loading required package: qgraph
```


#### check pairwise results

```r
# use Big_data from diveRsity
data(Big_data)
```



```r
# new divPart
system.time({
  nwpw <- fastDivPart(Big_data, pairwise = TRUE, parallel = TRUE,
                      gp = 2, WC_Fst = TRUE)
})
```

```
   user  system elapsed 
  48.51    0.29   49.19 
```



```r
# old divPart
system.time({
  oldpw <- divPart(Big_data, pairwise = TRUE, parallel = TRUE,
                   gp = 2, WC_Fst = TRUE)
})
```

```
   user  system elapsed 
  16.63    1.06   87.62 
```

#### Check element names

```r
# Element names
names(nwpw)
```

```
[1] "standard"     "estimate"     "pairwise"     "meanPairwise"
```

```r
names(oldpw)
```

```
[1] "standard"     "estimate"     "pairwise"     "meanPairwise"
```

#### Check the first five rows of standard

```r
# new
nwpw$standard[1:5, ]
```

```
        H_st   D_st   G_st G_hed_st D_jost
loc-1 0.8192 0.7808 0.9435   0.9907 0.8359
loc-2 0.8296 0.8021 0.9604   0.9939 0.8465
loc-3 0.8446 0.7859 0.9186   0.9888 0.8619
loc-4 0.7676 0.7431 0.9588   0.9911 0.7832
loc-5 0.7214 0.6814 0.9247   0.9801 0.7362
```

```r
# old
oldpw$standard[1:5,]
```

```
        H_st   D_st   G_st G_hed_st D_jost
loc-1 0.8192 0.7808 0.9435   0.9907 0.8359
loc-2 0.8296 0.8021 0.9604   0.9939 0.8465
loc-3 0.8446 0.7859 0.9186   0.9888 0.8619
loc-4 0.7676 0.7431 0.9588   0.9911 0.7832
loc-5 0.7214 0.6814 0.9247   0.9801 0.7362
```

#### Check the first five rows of estimate

```r
# new
nwpw$estimate[1:5, ]
```

```
      Harmonic_N H_st_est D_st_est G_st_est G_hed_st_est D_Jost_est Fst_WC
loc-1         50   0.8191   0.7804   0.9429       0.9906     0.8359 0.9434
loc-2         50   0.8295   0.8018   0.9600       0.9939     0.8464 0.9603
loc-3         50   0.8446   0.7852   0.9178       0.9886     0.8618 0.9185
loc-4         50   0.7675   0.7428   0.9585       0.9910     0.7832 0.9588
loc-5         50   0.7213   0.6808   0.9239       0.9799     0.7360 0.9246
      Fit_WC
loc-1 0.9441
loc-2 0.9568
loc-3 0.9225
loc-4 0.9555
loc-5 0.9244
```

```r
# old
oldpw$estimate[1:5,]
```

```
      Harmonic_N H_st_est D_st_est G_st_est G_hed_st_est D_Jost_est Fst_WC
loc-1         50   0.8191   0.7804   0.9429       0.9906     0.8359 0.9434
loc-2         50   0.8295   0.8018   0.9600       0.9939     0.8464 0.9603
loc-3         50   0.8446   0.7852   0.9178       0.9886     0.8618 0.9185
loc-4         50   0.7675   0.7428   0.9585       0.9910     0.7832 0.9588
loc-5         50   0.7213   0.6808   0.9239       0.9799     0.7360 0.9246
      Fit_WC
loc-1 0.9441
loc-2 0.9568
loc-3 0.9225
loc-4 0.9555
loc-5 0.9244
```

#### Check output structure of pairwise

```r
# new
str(nwpw$pairwise)
```

```
List of 4
 $ gstEst   : num [1:50, 1:50] NA 0.791 0.605 0.776 0.678 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ gstEstHed: num [1:50, 1:50] NA 0.841 0.666 0.845 0.751 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ djostEst : num [1:50, 1:50] NA 0.0584 0.0299 0.1019 0.0661 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ thetaWC  : num [1:50, 1:50] NA 0.882 0.752 0.873 0.806 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
```

```r
# old
str(oldpw$pairwise)
```

```
List of 8
 $ Gst         : num [1:50, 1:50] NA 0.793 0.608 0.778 0.68 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ G_hed_st    : num [1:50, 1:50] NA 0.842 0.668 0.847 0.754 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ D_Jost      : num [1:50, 1:50] NA 0.236 0.154 0.308 0.23 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ Gst_est     : num [1:50, 1:50] NA 0.792 0.605 0.776 0.678 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ G_hed_st_est: num [1:50, 1:50] NA 0.841 0.666 0.845 0.752 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ D_Jost_est  : num [1:50, 1:50] NA 0.0584 0.0299 0.1019 0.0661 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ Fst_WC      : num [1:50, 1:50] NA 0.882 0.752 0.873 0.806 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
 $ Fit_WC      : num [1:50, 1:50] NA 0.89 0.759 0.877 0.817 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
  .. ..$ : chr [1:50] "1" "2" "3" "4" ...
```

We can see that the old version of `divPart` returns eight matrices in this instance (i.e. when `WC_Fst` is `TRUE`), while `fastDivPart` only returns four. The reason for this difference is that `fastDivPart` only outputs the estimators of $G_{ST}$, $G'_{ST}$, $D_{Jost}$ and $latex \theta $.

#### $latex G_{ST}$

```r
# check gst (10 rows and 10 cols)
# new
nwpw$pairwise$gstEst[1:10, 1:10]
```

```
        1      2      3      4      5      6      7     8      9 10
1      NA     NA     NA     NA     NA     NA     NA    NA     NA NA
2  0.7914     NA     NA     NA     NA     NA     NA    NA     NA NA
3  0.6049 0.8782     NA     NA     NA     NA     NA    NA     NA NA
4  0.7762 0.9278 0.8184     NA     NA     NA     NA    NA     NA NA
5  0.6776 0.8742 0.6935 0.7904     NA     NA     NA    NA     NA NA
6  0.7332 0.8854 0.7163 0.7895 0.7658     NA     NA    NA     NA NA
7  0.6940 0.8292 0.7707 0.7833 0.7666 0.7600     NA    NA     NA NA
8  0.7154 0.8522 0.7570 0.7817 0.7460 0.7525 0.7147    NA     NA NA
9  0.7074 0.8405 0.7728 0.8463 0.7797 0.8027 0.7473 0.795     NA NA
10 0.5123 0.6690 0.5956 0.6650 0.6232 0.6125 0.4968 0.629 0.6049 NA
```

```r
# old
oldpw$pairwise$Gst_est[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.7917     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.6051 0.8779     NA     NA     NA     NA     NA     NA     NA NA
4  0.7765 0.9278 0.8182     NA     NA     NA     NA     NA     NA NA
5  0.6777 0.8741 0.6938 0.7902     NA     NA     NA     NA     NA NA
6  0.7331 0.8850 0.7165 0.7892 0.7658     NA     NA     NA     NA NA
7  0.6938 0.8294 0.7706 0.7836 0.7666 0.7598     NA     NA     NA NA
8  0.7154 0.8520 0.7571 0.7816 0.7461 0.7526 0.7147     NA     NA NA
9  0.7073 0.8406 0.7727 0.8465 0.7797 0.8025 0.7475 0.7950     NA NA
10 0.5124 0.6687 0.5960 0.6650 0.6234 0.6128 0.4968 0.6292 0.6049 NA
```


#### $latex G'_{ST}$

```r
# check Hedrick's Gst (10 rows and 10 cols)
# new
nwpw$pairwise$gstEstHed[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.8405     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.6655 0.9096     NA     NA     NA     NA     NA     NA     NA NA
4  0.8450 0.9509 0.8688     NA     NA     NA     NA     NA     NA NA
5  0.7515 0.9128 0.7500 0.8459     NA     NA     NA     NA     NA NA
6  0.8051 0.9152 0.7669 0.8365 0.8264     NA     NA     NA     NA NA
7  0.7810 0.8785 0.8457 0.8506 0.8479 0.8322     NA     NA     NA NA
8  0.8003 0.8975 0.8258 0.8438 0.8203 0.8192 0.7974     NA     NA NA
9  0.7821 0.8748 0.8332 0.9029 0.8474 0.8636 0.8241 0.8715     NA NA
10 0.5930 0.7289 0.6723 0.7427 0.7090 0.6899 0.5735 0.7219 0.6861 NA
```

```r
# old
oldpw$pairwise$G_hed_st_est[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.8408     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.6657 0.9094     NA     NA     NA     NA     NA     NA     NA NA
4  0.8453 0.9509 0.8687     NA     NA     NA     NA     NA     NA NA
5  0.7516 0.9127 0.7503 0.8457     NA     NA     NA     NA     NA NA
6  0.8049 0.9149 0.7671 0.8362 0.8265     NA     NA     NA     NA NA
7  0.7808 0.8786 0.8456 0.8508 0.8480 0.8321     NA     NA     NA NA
8  0.8003 0.8973 0.8258 0.8437 0.8203 0.8193 0.7974     NA     NA NA
9  0.7821 0.8749 0.8331 0.9030 0.8474 0.8635 0.8242 0.8715     NA NA
10 0.5931 0.7287 0.6726 0.7427 0.7092 0.6902 0.5735 0.7221 0.6861 NA
```


#### $latex D_{Jost}$

```r
# check Jost's D (10 rows and 10 cols)
# new
nwpw$pairwise$djostEst[1:10, 1:10]
```

```
         1       2       3       4       5       6       7       8       9
1       NA      NA      NA      NA      NA      NA      NA      NA      NA
2  0.05837      NA      NA      NA      NA      NA      NA      NA      NA
3  0.02987 0.07303      NA      NA      NA      NA      NA      NA      NA
4  0.10193 0.10355 0.08252      NA      NA      NA      NA      NA      NA
5  0.06606 0.10167 0.04210 0.07469      NA      NA      NA      NA      NA
6  0.08069 0.07183 0.03602 0.05646 0.07866      NA      NA      NA      NA
7  0.10360 0.09158 0.12509 0.11007 0.15042 0.10294      NA      NA      NA
8  0.10112 0.09945 0.09459 0.08783 0.10187 0.09009 0.09717      NA      NA
9  0.07472 0.05087 0.08043 0.14675 0.10922 0.10538 0.10730 0.15290      NA
10 0.03214 0.03886 0.04765 0.06716 0.06478 0.05285 0.04139 0.07844 0.05269
   10
1  NA
2  NA
3  NA
4  NA
5  NA
6  NA
7  NA
8  NA
9  NA
10 NA
```

```r
# old
oldpw$pairwise$D_Jost_est[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.0584     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.0299 0.0730     NA     NA     NA     NA     NA     NA     NA NA
4  0.1019 0.1035 0.0825     NA     NA     NA     NA     NA     NA NA
5  0.0661 0.1017 0.0421 0.0747     NA     NA     NA     NA     NA NA
6  0.0807 0.0718 0.0360 0.0565 0.0787     NA     NA     NA     NA NA
7  0.1036 0.0916 0.1251 0.1101 0.1504 0.1029     NA     NA     NA NA
8  0.1011 0.0994 0.0946 0.0878 0.1019 0.0901 0.0972     NA     NA NA
9  0.0747 0.0509 0.0804 0.1468 0.1092 0.1054 0.1073 0.1529     NA NA
10 0.0321 0.0389 0.0477 0.0672 0.0648 0.0529 0.0414 0.0784 0.0527 NA
```


#### $latex \theta $

```r
# check Weir & Cockerham's theta (10 rows and 10 cols)
# new
nwpw$pairwise$thetaWC[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.8825     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.7519 0.9345     NA     NA     NA     NA     NA     NA     NA NA
4  0.8729 0.9622 0.8992     NA     NA     NA     NA     NA     NA NA
5  0.8062 0.9322 0.8175 0.8819     NA     NA     NA     NA     NA NA
6  0.8447 0.9386 0.8333 0.8813 0.8662     NA     NA     NA     NA NA
7  0.8179 0.9058 0.8694 0.8774 0.8667 0.8624     NA     NA     NA NA
8  0.8327 0.9195 0.8605 0.8764 0.8532 0.8576 0.8322     NA     NA NA
9  0.8272 0.9125 0.8707 0.9160 0.8751 0.8895 0.8541 0.8848     NA NA
10 0.6752 0.8000 0.7447 0.7972 0.7660 0.7578 0.6616 0.7705 0.7519 NA
```

```r
# old
oldpw$pairwise$Fst_WC[1:10, 1:10]
```

```
        1      2      3      4      5      6      7      8      9 10
1      NA     NA     NA     NA     NA     NA     NA     NA     NA NA
2  0.8825     NA     NA     NA     NA     NA     NA     NA     NA NA
3  0.7519 0.9345     NA     NA     NA     NA     NA     NA     NA NA
4  0.8729 0.9622 0.8992     NA     NA     NA     NA     NA     NA NA
5  0.8062 0.9322 0.8175 0.8819     NA     NA     NA     NA     NA NA
6  0.8447 0.9386 0.8333 0.8813 0.8662     NA     NA     NA     NA NA
7  0.8179 0.9058 0.8694 0.8774 0.8667 0.8624     NA     NA     NA NA
8  0.8327 0.9195 0.8605 0.8764 0.8532 0.8576 0.8322     NA     NA NA
9  0.8272 0.9125 0.8707 0.9160 0.8751 0.8895 0.8541 0.8848     NA NA
10 0.6752 0.8000 0.7447 0.7972 0.7660 0.7578 0.6616 0.7705 0.7519 NA
```


It is clear that the results from each of these function versions are virtually identical. Any differences are trivial, and due to different calculation methods which round values at different stages.

When the `WC_Fst` argument is set to `TRUE`, the speed difference is only about 2X. This is because the calculation of Weir & Cockerham's (1984) $latex \theta $ is acting as a computational bottleneck. The nature of the calculations for this statistic preclude more efficient code. Let's look at the speed difference when `WC_Fst = FALSE`.


```r
# new
system.time({
  nwpwfstoff <- fastDivPart(Big_data, pairwise = TRUE, parallel = TRUE,
                            gp = 2, WC_Fst = FALSE)
})
```

```
   user  system elapsed 
   4.67    0.00    4.85 
```



```r
# new
system.time({
  oldpwfstoff <- divPart(Big_data, pairwise = TRUE, parallel = TRUE,
                         gp = 2, WC_Fst = FALSE)
})
```

```
   user  system elapsed 
  16.27    0.73   82.52 
```


Now we can see that the speed up is > 17X.

## Pairwise bootstrapping
Pairwise bootstrapping is the most computationally intensive process that the `diveRsity` package carries out. The new function `fastDivPart` is intended to reduce the time taken. Below is a example of how `fastDivPart` performs relative to `divPart`.


```r
# Use Test_data for illustration
data(Test_data)

system.time({
  nwpwbs <- fastDivPart(Test_data, bs_pairwise = TRUE, bootstraps = 1000,
                        parallel = TRUE)
})
```

```
   user  system elapsed 
   2.59    1.39   94.49 
```


```r
system.time({
  oldpwbs <- divPart(Test_data, bs_pairwise = TRUE, bootstraps = 1000,
                     parallel = TRUE)
})
```

```
   user  system elapsed 
  10.95    7.31  634.35 
```


##### Checking results output

```r
# new
str(nwpwbs$bs_pairwise)
```

```
List of 3
 $ gstEst   : num [1:15, 1:3] 0.00829 0.03788 0.03273 0.04768 0.03388 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "mean" "Lower_95%CI" "Upper_95%CI"
 $ gstEstHed: num [1:15, 1:3] 0.0545 0.2583 0.2251 0.3209 0.2339 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "mean" "Lower_95%CI" "Upper_95%CI"
 $ djostEst : num [1:15, 1:3] 0.0361 0.2157 0.1774 0.2767 0.1872 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "mean" "Lower_95%CI" "Upper_95%CI"
```

```r
# old
str(oldpwbs$bs_pairwise)
```

```
List of 6
 $ Gst         : num [1:15, 1:3] 0.014 0.0465 0.0404 0.0569 0.0413 0.0419 0.0366 0.0523 0.0389 0.015 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
 $ G_hed_st    : num [1:15, 1:3] 0.0849 0.2853 0.2538 0.3494 0.2655 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
 $ D_Jost      : num [1:15, 1:3] 0.072 0.25 0.222 0.31 0.234 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
 $ Gst_est     : num [1:15, 1:3] 0.0081 0.0404 0.0352 0.051 0.0354 0.0355 0.031 0.0461 0.0327 0.0092 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
 $ G_hed_st_est: num [1:15, 1:3] 0.0511 0.2576 0.2282 0.3245 0.2365 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
 $ D_Jost_est  : num [1:15, 1:3] 0.0318 0.209 0.1774 0.2751 0.1855 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:15] "pop1, vs. pop2," "pop1, vs. pop3," "pop1, vs. pop4," "pop1, vs. pop5," ...
  .. ..$ : chr [1:3] "Mean" "Lower_CI" "Upper_CI"
```


Again we can see that `fastDivPart` does not return the biased versions of $G_{ST}$, $G'_{ST}$ or $D_{Jost}$. Lets make sure the results are as expected.

#### $G_{ST}$

```r
# new Gst
nwpwbs$bs_pairwise$gstEst
```

```
                    mean Lower_95%CI Upper_95%CI
pop1, vs. pop2, 0.008287    0.005480     0.01144
pop1, vs. pop3, 0.037879    0.032820     0.04369
pop1, vs. pop4, 0.032732    0.028342     0.03755
pop1, vs. pop5, 0.047677    0.041571     0.05447
pop1, vs. pop6, 0.033876    0.028966     0.03910
pop2, vs. pop3, 0.032710    0.028309     0.03775
pop2, vs. pop4, 0.029337    0.025671     0.03356
pop2, vs. pop5, 0.042989    0.037522     0.04938
pop2, vs. pop6, 0.031626    0.027627     0.03632
pop3, vs. pop4, 0.009979    0.006581     0.01393
pop3, vs. pop5, 0.032313    0.027208     0.03782
pop3, vs. pop6, 0.019136    0.015339     0.02371
pop4, vs. pop5, 0.028183    0.023812     0.03355
pop4, vs. pop6, 0.017020    0.013471     0.02108
pop5, vs. pop6, 0.026920    0.022228     0.03220
```

```r
# old Gst
oldpwbs$bs_pairwise$Gst_est
```

```
                  Mean Lower_CI Upper_CI
pop1, vs. pop2, 0.0081   0.0053   0.0116
pop1, vs. pop3, 0.0404   0.0349   0.0470
pop1, vs. pop4, 0.0352   0.0306   0.0401
pop1, vs. pop5, 0.0510   0.0446   0.0578
pop1, vs. pop6, 0.0354   0.0308   0.0410
pop2, vs. pop3, 0.0355   0.0306   0.0414
pop2, vs. pop4, 0.0310   0.0269   0.0355
pop2, vs. pop5, 0.0461   0.0400   0.0526
pop2, vs. pop6, 0.0327   0.0286   0.0373
pop3, vs. pop4, 0.0092   0.0061   0.0131
pop3, vs. pop5, 0.0354   0.0301   0.0413
pop3, vs. pop6, 0.0192   0.0156   0.0234
pop4, vs. pop5, 0.0300   0.0249   0.0358
pop4, vs. pop6, 0.0173   0.0137   0.0214
pop5, vs. pop6, 0.0283   0.0235   0.0340
```

#### $G'_{ST}$

```r
# new G'st
nwpwbs$bs_pairwise$gstEstHed
```

```
                   mean Lower_95%CI Upper_95%CI
pop1, vs. pop2, 0.05453     0.03659     0.07419
pop1, vs. pop3, 0.25833     0.22754     0.29240
pop1, vs. pop4, 0.22508     0.20028     0.25312
pop1, vs. pop5, 0.32085     0.28569     0.35433
pop1, vs. pop6, 0.23389     0.20556     0.26416
pop2, vs. pop3, 0.22826     0.19942     0.26073
pop2, vs. pop4, 0.20648     0.18319     0.23350
pop2, vs. pop5, 0.29599     0.26324     0.32930
pop2, vs. pop6, 0.22353     0.19637     0.25407
pop3, vs. pop4, 0.07285     0.04891     0.10050
pop3, vs. pop5, 0.23063     0.20000     0.26468
pop3, vs. pop6, 0.14035     0.11265     0.17385
pop4, vs. pop5, 0.20289     0.17435     0.23506
pop4, vs. pop6, 0.12582     0.10080     0.15287
pop5, vs. pop6, 0.19463     0.16411     0.22795
```

```r
# old G'st
oldpwbs$bs_pairwise$G_hed_st_est
```

```
                  Mean Lower_CI Upper_CI
pop1, vs. pop2, 0.0511   0.0337   0.0724
pop1, vs. pop3, 0.2576   0.2259   0.2925
pop1, vs. pop4, 0.2282   0.2012   0.2556
pop1, vs. pop5, 0.3245   0.2907   0.3585
pop1, vs. pop6, 0.2365   0.2093   0.2678
pop2, vs. pop3, 0.2313   0.2013   0.2634
pop2, vs. pop4, 0.2058   0.1800   0.2334
pop2, vs. pop5, 0.2993   0.2654   0.3342
pop2, vs. pop6, 0.2231   0.1966   0.2522
pop3, vs. pop4, 0.0617   0.0411   0.0860
pop3, vs. pop5, 0.2336   0.2029   0.2660
pop3, vs. pop6, 0.1328   0.1093   0.1589
pop4, vs. pop5, 0.2015   0.1705   0.2354
pop4, vs. pop6, 0.1220   0.0976   0.1485
pop5, vs. pop6, 0.1952   0.1659   0.2303
```

#### $D_{Jost}$

```r
# new G'st
nwpwbs$bs_pairwise$djostEst
```

```
                   mean Lower_95%CI Upper_95%CI
pop1, vs. pop2, 0.03610     0.02050     0.05539
pop1, vs. pop3, 0.21573     0.18639     0.24824
pop1, vs. pop4, 0.17739     0.15114     0.20628
pop1, vs. pop5, 0.27668     0.24355     0.30755
pop1, vs. pop6, 0.18725     0.15969     0.21951
pop2, vs. pop3, 0.19102     0.16243     0.22329
pop2, vs. pop4, 0.16386     0.13861     0.19192
pop2, vs. pop5, 0.25293     0.22160     0.28659
pop2, vs. pop6, 0.19395     0.16694     0.22294
pop3, vs. pop4, 0.04822     0.02848     0.07415
pop3, vs. pop5, 0.18188     0.15106     0.21841
pop3, vs. pop6, 0.11017     0.08413     0.14120
pop4, vs. pop5, 0.15825     0.12968     0.18973
pop4, vs. pop6, 0.09376     0.07141     0.11983
pop5, vs. pop6, 0.15587     0.12495     0.18983
```

```r
# old G'st
oldpwbs$bs_pairwise$D_Jost_est
```

```
                  Mean Lower_CI Upper_CI
pop1, vs. pop2, 0.0318   0.0165   0.0504
pop1, vs. pop3, 0.2090   0.1783   0.2407
pop1, vs. pop4, 0.1774   0.1523   0.2051
pop1, vs. pop5, 0.2751   0.2439   0.3051
pop1, vs. pop6, 0.1855   0.1571   0.2168
pop2, vs. pop3, 0.1890   0.1610   0.2196
pop2, vs. pop4, 0.1608   0.1358   0.1877
pop2, vs. pop5, 0.2523   0.2200   0.2851
pop2, vs. pop6, 0.1887   0.1633   0.2181
pop3, vs. pop4, 0.0395   0.0231   0.0604
pop3, vs. pop5, 0.1810   0.1497   0.2140
pop3, vs. pop6, 0.1005   0.0797   0.1247
pop4, vs. pop5, 0.1529   0.1238   0.1841
pop4, vs. pop6, 0.0899   0.0673   0.1157
pop5, vs. pop6, 0.1528   0.1245   0.1863
```


## Conclusions

In general, we can see that `fastDivPart` provides significant timing improvements over `divPart`, although the extent of these improvements are dependent on the particular options used. The calculation of Weir & Cockerham's $latex \theta $, in all instances, results in a smaller time saving. There also seems to be an interaction between the number of loci and number of population samples used. The new function `fastDivPart` has been optimized to improve speed for many pairwise comparisons, however, data sets with many loci may result in smaller time saving because looping over loci has not been optimized.

## Reproducibility

```
R version 3.0.2 (2013-09-25)
Platform: x86_64-w64-mingw32/x64 (64-bit)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] diveRsity_1.6.0 qgraph_1.2.3    ggplot2_0.9.3.1 shiny_0.7.0    
[5] plotrix_3.5-1   knitr_1.4.1    

loaded via a namespace (and not attached):
 [1] bitops_1.0-6       caTools_1.14       cluster_1.14.4    
 [4] colorspace_1.2-2   dichromat_2.0-0    digest_0.6.3      
 [7] ellipse_0.3-8      evaluate_0.4.7     formatR_0.9       
[10] grid_3.0.2         gtable_0.1.2       Hmisc_3.12-2      
[13] httpuv_1.1.0       igraph_0.6.5-2     jpeg_0.1-6        
[16] labeling_0.2       lattice_0.20-23    lavaan_0.5-14     
[19] MASS_7.3-29        munsell_0.4.2      plyr_1.8          
[22] png_0.1-6          proto_0.3-10       psych_1.3.10.12   
[25] RColorBrewer_1.0-5 reshape2_1.2.2     RJSONIO_1.0-3     
[28] rpart_4.1-3        scales_0.2.3       sem_3.1-3         
[31] stats4_3.0.2       stringr_0.6.2      tools_3.0.2       
[34] xtable_1.7-1      
```

