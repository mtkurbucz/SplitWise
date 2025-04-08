
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 📦 SplitWise R Package

<!-- badges: start -->
<!-- badges: end -->

**SplitWise** implements a hybrid stepwise regression approach with single-split dummy encoding. It allows numeric variables to be represented either as continuous predictors or converted into binary (0/1) indicators via data-driven splitting. The package also includes an *iterative mode* to detect partial synergies among predictors, enhancing model interpretability and performance.


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

# Simple (univariate) transformation
model_uni <- splitwise(
  mpg ~ .,
  data = mtcars,
  transformation_mode = "univariate",
  direction = "backward",
  trace = 0
)
summary(model_uni)

# Iterative (forward selection with synergy detection)
model_iter <- splitwise(
  mpg ~ .,
  data = mtcars,
  transformation_mode = "iterative",
  direction = "forward",
  trace = 0
)
print(model_iter)
```

## 📘 Features

- Converts numeric variables into optimal binary dummies (0/1) using rpart
- Supports forward, backward, or both stepwise selection (stats::step)
- Offers univariate and iterative transformation strategies
- Custom print() and summary() methods for easy inspection

## 📄 Documentation

- Function manual: see `man/`
- Vignette: usage example provided in `vignettes/` and `inst/doc/`

## 📜 License

This package is licensed under the GPL-3 License.
