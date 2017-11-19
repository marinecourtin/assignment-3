# Difference in the proportion of cases with a specific value between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
# target_value: the value of var that will be counted
#
# RETURN VALUE:
# The percentage of cases in which `var` was equal to `target_value` for the first group,
# minus the percentage of cases in which `var` was equal to `target_value` for the
# second group.
#
# EXPLANATION
# UQ(as.name(var)) allows us to give the column name as a string and un-string it
difference_in_proportion <- function(d, var, grouping_var, group1, group2,
                                     target_value) {
  result_1 <- nrow(dplyr::filter(d, UQ(as.name(grouping_var))==group1, UQ(as.name(var))==target_value)) / nrow(dplyr::filter(d, UQ(as.name(grouping_var))==group1))
  result_2 <- nrow(dplyr::filter(d, UQ(as.name(grouping_var))==group2, UQ(as.name(var))==target_value)) / nrow(dplyr::filter(d, UQ(as.name(grouping_var))==group2))
  return(result_1-result_2)
}


# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize, provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  # d[[var]] contains our species for each dataline
  # we randomize its content, each dataline will be attributed one of the speciestags
  # present in the original d[[var]]
  d[[var]] <- sample(d[[var]], nrow(d))
  d
  # this will result in a new dataframe where we can observe what our data would
  # look like if there was no systematic pattern between species / other variables
  # such as sepal width
  return(d)
}

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999, ...) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2, ...)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # we create a permutation using the randomize function
    permutation <- randomize(d, var)
    # we store the result of our statistic function on the permutation
    # at index i in the vector permutation_statistics
    permutation_statistics[i] <- statistic(permutation, var, grouping_var, group1, group2, ...)
  }
  # we return a list for easy access to results for both observation on our
  # original data and on permutations
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}



