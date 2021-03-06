Gender Wage Gap
================
Bismark Adomako
April 17, 2020

## Goals

Here we ask and answer the following question: What is the difference in
predicted wages between men and women with the same job-relevant
characteristics?

## Content of Repo

1.  data/codebook.rtf contains the description of worker job-relevant
    worker characteristics.
2.  data/pay.descrimination.Rdata: the CPS (2012) data on wages and
    job-relevant worker characteristics, suc as experience exp, gender,
    education, etc.
3.  gender\_wage\_gap.R predicts the difference in predicted wages
    between men and women with the same job-relevant characteristics
    using linear model with linear specificaions.

## Data Summary

``` 
       male averages female averages
female     0.0000000       1.0000000
cg         0.3548387       0.4061135
sc         0.3019713       0.3543356
hsg        0.3431900       0.2395508
mw         0.2849462       0.2913288
so         0.2352151       0.2551466
we         0.2213262       0.1983780
ne         0.2585125       0.2551466
exp1      13.5801971      13.0371179
exp2       2.5865883       2.4494526
exp3       5.9649377       5.5992975
wage      16.1174580      14.7200584
```

## Specifications

We estimate the linear regression model:

``` 
    Y = β1D + βr W + ε
    
```

D is the indicator of being a female (1 if female and 0 otherwise). W’s
are controls. Basic model: W’s consist of education and regional
indicators, experience, experience squared, and experience cubed.
Flexible model: W’s consist of controls in the basic model plus all of
their two-way interactions.

## Results

``` 
           Estimate Standard Error Lower Conf. Bound Upper Conf. Bound
basic reg -1.826397      0.4245163         -2.658697        -0.9940968
flex reg  -1.880013      0.4247438         -2.712761        -1.0472652
```

The estimated gender gap in hourly wage is about $−2.0 with a confidence
interval that ranges from about $−2.7 to $−1. This means that women get
paid $2 less per hour on average than men, controlling for experience,
education, and geographicalregion.

## Conclusion

To sum it up… The gender wage gap may partly reflect genuine
discrimination against women in the labormarket…. It may also partly
reflect the so-called selection effect, namely that women are more
likely to end up in occupations that pay somewhat less (for example,
school teachers).
