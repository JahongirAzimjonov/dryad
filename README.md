# Marketing Mix Model with bootstrap estimates
---
### Status: beta
<img src='R/data/logo.png' align="right" height="169px" />

## Introduction

 **What is dryad MMM**: dryad is a fork of Robyn, the semi-automated and open-sourced Marketing Mix Modeling (MMM) package from Meta Marketing Science. 
Robyn uses various machine learning techniques (Ridge regression, multi-objective evolutionary algorithm for hyperparameter optimization, time-series decomposition for trend and season, gradient-based optimization for budget allocation etc.) to define media channel efficiency and effectivity, explore adstock rates and saturation curves. 

Dryad extends Robyn by performing bootstrapped ridge regressions for the same models to estimate penalized (added bias to reduce variance) least squares (PLS) model coefficients, which are more optimal, stable estimates that are more likely to validate in new samples.

Dryad further extends Robyn by adding diagnostics, visuals, augmented numerical outputs, and a forecasting function. To help uncover causal interdependencies from observational data (one of the great challenges of nonlinear time series analysis!), we use the popular Shannon-entropy based Transfer Entropy, which represents a prominent tool for assessing directed information flow between joint processes.
  
## The Interpretability-Flexibility tradeoff

The econometric modeller is generally presented with observational data rather than experimental data. There are two major consequences for empirical work. Because realisations of random (IID) samples are uncommon with observational data, the modeller must first hone their abilities in testing the model's assumptions. Second, the modeller has to look closely at the type and structure of the data since the data collector and the data analyzer are no longer working together.

'Learning from data' about stochastic observable events using statistical models is the overarching goal of empirical modelling, which encompasses the process, techniques, and tactics based on statistical modelling and inference. When data generated by either passive observation or intentional testing display random regularity patterns, we say that the underlying real-world phenomenon is 'stochastic,' making it accessible to statistical modelling. 

A model is a stylized mathematical representation of the processes that we believe give birth to the observations in a data collection. If the dependent variables can be determined from the independent ones, then the model is said to be deterministic. This is obviously impossible in many real-life situations. Probabilistic distribution evaluation is used by statistical (or stochastic) models to get close to the correct answer. 

There exists no universally best model. Not only that, but there is evidence that a set of assumptions that works well in one domain may work poorly in another. This is the so-called 'no free lunch' theorem. In general, statistics provides a catalogue of different models and algorithms from which we try to find the one that best fits our data. Due to the no free lunch theorem, we may need to test several models, algorithms, and hyperparameter combinations before settling on the most effective one in terms of prediction accuracy, speed of implementation, and level of complexity. The optimal mix of data, models, and algorithms for optimal out of sample performance is the focus of our current MMM modelling framework.


| Property        | Statistical inference             | Supervised machine learning            |
|-----------------|-----------------------------------|----------------------------------------|
| Goal            | Causal models with explanatory power               | Prediction performance,  often with limited explanatory power              |
| Data            | The data is generated by a model  | The data generation process is unknown |
| Framework       | Probabilistic                     | Algorithmic and Probabilistic          |
| Expressibility  | Typically linear                  | Non-linear                             |
| Model selection | Based on information criteria     | Numerical optimization                 |
| Scalability     | Limited to lower-dimensional data | Scales to high-dimensional input data  |
| Robustness      | Prone to over-fitting             | Designed for out-of-sample performance |
| Diagnostics     | Extensive                         | Limited                                |
  
## Machine learning vs econometric modelling

Machine learning is frequently an algorithmic form of statistical model estimation in which the data production process is viewed as an unknown. Model selection and inference are automated, with an emphasis on large volumes of data processing to construct robust models. It can be thought of as a high-efficiency data compression approach developed to give predictors in complicated situations with non-linear relationships between input and output variables and typically high-dimensional input space. Machine learners tend to balance data filtering with the objective of creating accurate and robust conclusions, which are frequently discrete and categorical functions of input data. This is fundamentally different from maximum likelihood estimators used in standard econometric models, which presume that the data was created by the model. These generally have over-fitting issues, particularly when used to high-dimensional datasets. Given the complexity of current datasets, whether limit order books or high-dimensional financial time series, it is becoming increasingly difficult to make inferences based on a known data generating process. Even if an economic interpretation of the data creation process can be provided, it is a legitimate assumption that the precise form cannot be known all of the time.

As a result, the paradigm for data analysis provided by machine learning differs significantly from the standard econometric modelling and testing framework. Out-of-sample performance and comprehending the bias-variance tradeoff replace traditional fit measures like as $R^2$, t-values, p-values, and the concept of statistical significance. 

It is sometimes convenient to contrast maximum likelihood estimates with supervised machine learning. This comparison is slightly exaggerated. Rather, the two approaches should be considered as opposite extremities of a methodological continuum. Linear regression approaches like Lasso and ridge regression, as well as hybrids like Elastic Net, sit somewhere in the centre, delivering some mix of maximum likelihood estimation's explanatory capacity while keeping out-of-sample predictive performance on high-dimensional datasets.
  

  
 
