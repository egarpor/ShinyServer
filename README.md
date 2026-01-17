ShinyServer
===========

[![License](https://img.shields.io/badge/license-CC_BY--NC--ND_4.0-blue.svg)](https://creativecommons.org/licenses/by-nc-nd/4.0/)

## List of contents

### Shiny apps

* Simple linear regression
	* [Least squares and distance choice](https://myshiny.duckdns.org/least-squares)
	* [Randomness of the regression line](https://myshiny.duckdns.org/lm-random)
	* [Coverage of confidence intervals](https://myshiny.duckdns.org/ci-random)
	* [Confidence intervals for prediction](https://myshiny.duckdns.org/ci-prediction)
	* [ANOVA decomposition](https://myshiny.duckdns.org/anova)
	* [Nonlinear relationships](https://myshiny.duckdns.org/non-linear)
* Multiple linear regression
	* [Least squares and distance choice](https://myshiny.duckdns.org/least-squares-3D)
	* [Assumptions of the linear model](https://myshiny.duckdns.org/assump-lm-3D)
	* [ANOVA decomposition](https://myshiny.duckdns.org/anova-3D)
	* [Nonlinear relationships](https://myshiny.duckdns.org/mult-non-linear)
	* [Partial least squares and principal component regression](https://myshiny.duckdns.org/plsr)
* Logistic regression
	* [Logistic curve and maximum likelihood](https://myshiny.duckdns.org/log-maximum-likelihood)
	* [Randomness of the logistic regression curve](https://myshiny.duckdns.org/log-random)
	* [Confidence intervals for prediction](https://myshiny.duckdns.org/log-ci-prediction)
	* [Nonlinear relationships](https://myshiny.duckdns.org/log-non-linear)
* Nonparametric density estimation
	* [Bias and variance of the moving histogram](https://myshiny.duckdns.org/bias-var-movhist/)
	* [Kernel density estimator](https://myshiny.duckdns.org/kde/)
	* [Bandwidth selection in kernel density estimation](https://myshiny.duckdns.org/kde-bwd/)
	* [Transformed kernel density estimator](https://myshiny.duckdns.org/kde-transf/)
* Nonparametric regression estimation
	* [Local polynomial regression estimator](https://myshiny.duckdns.org/kreg/)
	* [Local likelihood estimator](https://myshiny.duckdns.org/loclik/)
* Other
	* [Overfitting in regression](https://myshiny.duckdns.org/over-fitting/)
	* [Nonparametric vs parametric estimation](https://myshiny.duckdns.org/dist-mse/)

### `Rmd` documents

* [A 10 minute-ish introduction to linear regression](https://myshiny.duckdns.org/10min-lin-reg/)

## Required packages

To run the applications in this repository, install the following R packages:

```r
install.packages(c("shiny", "pls", "mvtnorm", "viridis", "nor1mix", "rgl",
                   "plot3Drgl", "plot3D", "ks"))
```

## License

All the material in this repository is licensed under [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).
