# Comparing Classification Algorithms Using Hypothesis Testing
## Context
This is a problem assigned as homework for CSCI 312A Computational Statistics at Houghton College under Professor Wei Hu. 
## Task
Using hypothesis testing, compare the from-scratch and sklearn versions of binary and multi-class classification algorithms. We will be comparing logistic regression to itself in the two different implementations and comparing the from-scratch softmax regression with the multinomial version of logistic regression implemented in sklearn.
## Solution
I have enhanced testing methods in the from scratch blogs and implemented algorithm comparisons using a basic k-fold cross validation due to problem limitations. I have still given an example of the paired t-test with 5x2 cross validation. While using built-in package implementations of these two performance measurements, I have also written a from scratch k-fold cross validation function.
## Resources
Code has been adapted from the following sources to accomplish this task:
- Logistic Regression From Scratch: https://rickwierenga.com/blog/ml-fundamentals/logistic-regression.html
- Logistic Regression sklearn: https://chrisalbon.com/machine_learning/logistic_regression/logistic_regression_with_l1_regularization/
- Softmax Regression From Scratch: https://rickwierenga.com/blog/ml-fundamentals/softmax.html
- Multinomial Logistic Regression in sklearn: https://scikit-learn.org/stable/auto_examples/linear_model/plot_logistic_multinomial.html#sphx-glr-auto-examples-linear-model-plot-logistic-multinomial-py
- Comparing Algorithms: https://machinelearningmastery.com/hypothesis-test-for-comparing-machine-learning-algorithms/