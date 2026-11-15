ShinyServer
===========

[![](https://img.shields.io/badge/license-CC_BY--NC--ND_4.0-blue.svg)](https://creativecommons.org/licenses/by-nc-nd/4.0/)

A served-oriented repository of Shiny apps and `Rmd` documents for teaching purposes, mainly focused on regression analysis. The apps run on a Shiny server at [shiny.egarpor.es](https://shiny.egarpor.es/).

## List of contents

### Shiny apps

* Simple linear regression
	* [Least squares and distance choice](https://shiny.egarpor.es/least-squares)
	* [Linear model assumptions](https://shiny.egarpor.es/lm-population)
	* [Randomness of the regression line](https://shiny.egarpor.es/lm-random)
	* [Coverage of confidence intervals](https://shiny.egarpor.es/ci-random)
	* [Confidence intervals for prediction](https://shiny.egarpor.es/ci-prediction)
	* [ANOVA decomposition](https://shiny.egarpor.es/anova)
	* [Nonlinear relationships](https://shiny.egarpor.es/non-linear)
* Multiple linear regression
	* [Least squares and distance choice](https://shiny.egarpor.es/least-squares-3D)
	* [Assumptions of the linear model](https://shiny.egarpor.es/assump-lm-3D)
	* [ANOVA decomposition](https://shiny.egarpor.es/anova-3D)
	* [Nonlinear relationships](https://shiny.egarpor.es/mult-non-linear)
	* [Partial least squares and principal component regression](https://shiny.egarpor.es/plsr)
* Logistic regression
	* [Logistic curve and maximum likelihood](https://shiny.egarpor.es/log-maximum-likelihood)
	* [Randomness of the logistic regression curve](https://shiny.egarpor.es/log-random)
	* [Confidence intervals for prediction](https://shiny.egarpor.es/log-ci-prediction)
	* [Nonlinear relationships](https://shiny.egarpor.es/log-non-linear)
* Nonparametric density estimation
	* [Bias and variance of the moving histogram](https://shiny.egarpor.es/bias-var-movhist/)
	* [Kernel density estimator](https://shiny.egarpor.es/kde/)
	* [Bandwidth selection in kernel density estimation](https://shiny.egarpor.es/kde-bwd/)
	* [Transformed kernel density estimator](https://shiny.egarpor.es/kde-transf/)
* Nonparametric regression estimation
	* [Local polynomial regression estimator](https://shiny.egarpor.es/kreg/)
	* [Local likelihood estimator](https://shiny.egarpor.es/loclik/)
* Other
	* [Overfitting in regression](https://shiny.egarpor.es/over-fitting/)
	* [Nonparametric vs parametric estimation](https://shiny.egarpor.es/dist-mse/)

### `Rmd` documents

* [A 10 minute-ish introduction to linear regression](https://shiny.egarpor.es/10min-lin-reg/)

## Required packages

To run the applications in this repository, install the following R packages:

```r
install.packages(c("shiny", "pls", "mvtnorm", "viridis", "nor1mix", "rgl",
                   "plot3Drgl", "plot3D", "plotly", "ks"))
```

## License

All the material in this repository is licensed under [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).
