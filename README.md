# s2fa
Simple-Supervised Factor Analysis - an R package

**Factor Analysis** is used for *dimensionality reduction*, an *unsupervised* task. With some changes, Factor Analysis (**FA**) can be also be used for [*supervised*] single-output/multi-output regression (**S2FA**), for *semi-supervised* single-output/multi-output regression (**S3FA**) and for imputing *missing values* in input or output (**MS3FA**). 

In the training phase, S2FA uses an *analytic solution* in *matrix* form, S3FA uses the *EM* algorithm in *matrix* form, and MS3FA uses the *EM* algorithm. For the same task, MS3FA would be *slower* than FA, S2FA, and S3FA. MS3FA can be used to *learn a model with missing data* and to *impute missing values*.

The R package can be installed via:

```
devtools::install_github("aciobanusebi/s2fa")
```

More info on the algorithms: https://github.com/aciobanusebi/msc_dissertation
