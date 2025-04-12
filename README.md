
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📦 SplitWise R Package

<!-- badges: start -->
<!-- badges: end -->

SplitWise is a hybrid stepwise regression package that intelligently transforms numeric predictors using single- or double-split dummy encoding. Each variable can be retained as a continuous feature or transformed into a binary indicator based on model fit, evaluated using AIC or BIC.

By default, SplitWise uses an **iterative transformation mode**, which evaluates each variable in the context of others — enabling more accurate and interpretable models by capturing feature synergies. For faster execution on large datasets, a simpler **univariate mode** is also available, which transforms each variable independently (though it may lead to less optimal results).

## 🔧 Installation

To install from GitHub:

```r
# if not already installed:
install.packages("devtools")
devtools::install_github("mtkurbucz/SplitWise")
```

## 🚀 Usage

``` r
library(SplitWise)
data(mtcars)

# Univariate transformation (backward selection using AIC)
model_uni <- splitwise(
  mpg ~ .,
  data = mtcars,
  transformation_mode = "univariate",
  direction = "backward",
  criterion = "AIC",
  trace = 0
)
summary(model_uni)

# Iterative transformation (forward selection using BIC)
model_iter <- splitwise(
  mpg ~ .,
  data = mtcars,
  transformation_mode = "iterative",
  direction = "forward",
  criterion = "BIC",
  k = log(nrow(mtcars)), # BIC penalty
  trace = 0
)
print(model_iter)
```

## 📘 Features

- Automatic dummy transformation: Converts numeric variables into binary (0/1) indicators via data-driven thresholds using rpart.
- Stepwise model selection: Works with stats::step() to support forward, backward, or both directions.
- Iterative synergy detection: Evaluates transformations in the context of other predictors.
- Model selection criteria: Choose between AIC (default) or BIC via criterion argument.
- Exclude variables: Prevent specific predictors from being split using exclude_vars.
- Custom summary and print methods: Clearly shows transformation logic and model quality.

## 📄 Documentation

- Function help: see `?splitwise` and other `?` functions
- Vignette: `vignette("Using the SplitWise Package with the mtcars Dataset")`
- Manual files: `man/` folder
- Additional examples: see `tests/` and `inst/doc/`

## 📜 License

This package is licensed under the GPL-3 License.
