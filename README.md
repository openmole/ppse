

# PPSE


## Frame

The objective of this project is to extend the idea proposed by the PSE model exploration method. PPSE objective is to discover all the patterns that a model can produce and to associate a probability to each pattern.

### Pattern probability

The probability of a pattern is defined as the probability of reaching this zone of the output space when one sample the input space at random using a uniform distribution.

### Stochastic models

The pattern probability extends trivially to stochastic model. The probability of a given pattern is the sum of probabilities of reaching this pattern among all the output distribution computed using replication while sampling the input space uniformly.

### Naive approach

A naive approach is to sample uniformly in the input space and count the reached pattern. This work but is highly inneficient as shown in the previous work achieved by PSE. The intuition, used in PSE, that sampling more intensively near inputs leading to rare patterns is an efficient way of finding other rare patterns should be used as well in PPSE. 

## Prior work

### Importance sampling

The importance sampling is used to be compute statistic estimators of f(x) when x is drawn in a distribution p(x) and sampling directly in p(x) is hard.

It doesn't seem to be a solution to this problem since the distribution that is sampled in this method is a uniform distribution which is simple to sample.