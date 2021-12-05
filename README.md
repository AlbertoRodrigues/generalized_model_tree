# Generalized Linear Tree

#### This is my undergraduate degree thesis in statistics at [Federal University of Ceará](https://www.ufc.br/)

# Introduction

The basic idea of ​​a Generalized Linear Tree is that similar data subsets have a regression model that better predicts compared to some regression model fitted with all the data.

The idea that motivated the development of the Generalized Linear Tree is the same idea as a common decision/regression tree: divide the entire dataset into smaller subsets in order to improve prediction through means or medians, right?

For example, imagine that we want to predict a person's salary based on age and years of education. So, we could have the following regression tree:

![](https://www.ufc.br/) age figure

Therefore, we can think of the same structure as a tree, but instead of predicting the same value for the same subset, the prediction will be provided by a regression equation so that the prediction will be different for observations from the same subset of the tree.

So the question we can ask is: how do we obtain this regression equation? One possible choice is to always use a multiple linear regression on each subset, which **is a generalized linear model with normal distribution and identity linkage function!**

For example, we could have the following relationship between the explanatory variable and the response variable:

![](https://www.ufc.br/)scatter plot

So suggesting some linear modeling by parts. Resulting in the following tree below:

linear tree figure

Well, despite the model mentioned above being extremely more powerful in terms of predictive power than a common tree, it has a significant disadvantage: it always uses the same model and therefore the same structure for the prediction of an observation, an estimation equation linear and will always be estimated based on the Normal distribution using the maximum likelihood estimator.

In practice, this can happen, but it is complicated with most data sets due to the complexity and number of observations.

Therefore, the idea of ​​a Generalized Linear Tree is that instead of always using the Normal distribution with identity link function for the target variable, we test several distributions with different link functions and see which one best fits a particular subset of the tree.

In particular, the developed model tests only for positive continuous target variables, so the most common distributions used in MLGs are Normal, Gamma, and Inverse Normal together with the identity, logarithmic and inverse linkage functions, totaling nine different models.

# How the method works

To understand how subset models are chosen, we need to understand how the divisions of the tree are chosen. First, let us get to know a little better how divisions are carried out and the reasons behind the idea of ​​separation.

The idea of ​​separation is to determine homogeneous subsets according to the variability of the target variable. This can also be seen to improve the prediction of each particular model of a subset. More specifically, subsets are generated using the following formula:

![](https://www.ufc.br/)figure formula

The objective of a good division is to minimize the standard deviation of the target variable of each subset taking into account the quantity of observations. Therefore, we want to maximize this expression to obtain subsets with standard deviation of the target variable smaller than the standard deviation of the target of the entire set and, consequently, improve the estimation of the models of each subset.

For each division, several feature values ​​are tested, and the feature value that maximizes the expression is chosen, and the division is performed. This process is carried out until at least the stop hyperparameter is triggered, such as the minimum number of observations for the subset of the maximum tree depth, classic hyperparameters of a regression tree.

With the tree built, now it is time to choose the best generalized linear model to predict our target variable for a given subset. For each subset, some GLMs are trained, and then it is verified which model had the smallest training error, for example, the mean squared error or the mean absolute error, in such a way that this model will be chosen for each subset.

This process is carried out recursively until the final subsets of the leaves that will be responsible for predicting the observations.

An important note is that not all MLGs are tested as the tree grows, but only some that get the best results provided by the previous subsets, this is done to avoid computational cost and make learning faster.

# Avoiding overfitting

After choosing the models for each subset of the tree, the pruning process is carried out, which is the main component of the model that avoids overfitting. This is necessary because it can happen that a tree fits a very specific subset and doesn't understand the pattern of the data, thus occasionally overfitting.

There are two main types of pruning, in both pruning, a process is carried out from the bottom to the top of the tree to check whether the division carried out for the leaves has actually suffered from overfitting or not.

In the first one, the training error itself is used to verify whether the predictions made in the leaf nodes were overfitted. The training errors for the leaf nodes and also for the parent node (a subset above the leaf nodes) are calculated. With the three errors calculated, they are compared and it is checked if the training error of the parent node is less than a weighting of the training errors of the leaf nodes, if that happens, the division is "pruned", that is, it ceases to exist, and the new leaf node will be assigned to the parent node. This process is performed recursively to the top of the tree.

An important note is that it is not exactly the training error used, but the training error with a fix. This is done because the training error is usually underestimated, so this fix tries to solve this problem.

In the second type of pruning, almost the same process explained above is done, but with the difference that instead of using the training error, the pruning set error is used, which is a validation set specific to the tree, which it is not used in the training process before pruning.

With that, we finish the learning process and we have our Generalized Linear Tree to make the predictions for the observations we want!

An example is given below:

![](https://www.ufc.br/)Figure Example Generalized Linear Tree


In this case, we have three subsets and for each one we have a specific GLM. The first equation (from left to right) is a simple linear regression, which is a MLG with normal distribution and identity link function for observations that are less than or equal to 1.55, the second equation given by a MLG with distribution gamma and inverse quadratic link function for observations less than or equal to 1.95 and greater than 1.55 and the third equation is provided by an MLG with inverse normal distribution and logarithmic link function for observations greater than 1.95.

Prediction equations are provided through the inverse function of the link function, but do not worry, this is a specific component of MLG. Despite not being visualized in the tree, the probability distributions for each subset can be verified in the LINK developed code of all training in the R programming language.

# Predicting new observations

Now is the time to predict observations that the model did not see. For this purpose, we can simply check in which final subset the observation to be predicted belongs based on the prediction equation of the chosen MLG for a given subset.

For example, imagine that we have the following Generalized Linear Tree already trained with our data, and we want to make some predictions. With our feature denoted by $x1$.

![](https://www.ufc.br/) Tree figure with equations

We may want to make predictions for some feature values ​​like 2.4, 1.75, 3.31 and 1.55. Therefore, we just need to look at which subset these observations belong to and perform the prediction. Predictions are given in the table below.

![](https://www.ufc.br/) examples predictions

We can note that we have the prediction for each subset based on the specific GLM.

## Important links

1. [Original paper](https://research.latinxinai.org/papers/icml/2021/pdf/paper_35.pdf) 
2. [Artice at) Medium (in portuguese)](https://medium.com/turing-talks/%C3%A1rvore-linear-generalizada-uma-extens%C3%A3o-da-%C3%A1rvore-de-regress%C3%A3o-que-voc%C3%AA-deve-conhecer-53f9658f828c) 








