# leoRtools

Personal R tools for data analysis and teaching:

- `assumptions()`: residual plots and assumption tests for a fitted model. It detects the class of the model:
  - linear models (`lm`, `aov`): standardized residuals, Levene and Shapiro-Wilk tests (Levene's test is skipped automatically when the model has quantitative predictors);
  - `nlme` models (`gls`, `lme`): Pearson residuals and Shapiro-Wilk test.
- `shapiro.modif.test()`: Rahman & Govindarajulu (1997) modification of the Shapiro-Wilk normality test, with a Monte Carlo p-value.
- `datos_trigo`: simulated wheat dataset (genotype, yield, phosphorus) for teaching.

## Installation

```r
install.packages("remotes")                            # only once
remotes::install_github("LeonardoGualano/leoRtools")
```

## Usage

```r
library(leoRtools)

# Linear model with a categorical predictor
m = lm(weight ~ group, data = PlantGrowth)
assumptions(m)

# Model with a variance structure
library(nlme)
g = gls(weight ~ group, data = PlantGrowth,
        weights = varIdent(form = ~ 1 | group))
assumptions(g)

# Modified Shapiro-Wilk test on the residuals
shapiro.modif.test(resid(m), seed = 123)
```

## Reference

Rahman, M. M., & Govindarajulu, Z. (1997). A modification of the test of Shapiro and Wilk for normality. *Journal of Applied Statistics*, 24(2), 219-236.
