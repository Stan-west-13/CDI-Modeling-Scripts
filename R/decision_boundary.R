#' Compute decision boundary of model for each group in terms of nproduced
#'
#' @param m A generalize linear model (glm) fit as \code{y ~ nproduced * group}
#'   where group is treatment coded with non-autistic as the reference group.
#' @parm crit_p The probability threshold
#' @return The number of words for each group where the vocabulary has a 50/50
#'   chance of containing the modeled word.
#' @details
#' This is hard coded for a model with two regressors (group and nproduced) and
#' group has two levels (ASD, TD).
#'
#' The objective is to solve for the value of `nproduced` where the probability
#' of the word being in a child's vocabulary of that size and group member ship
#' is 50/50. The linear prediction of a logistic model is called a "logit", and
#' it is interpreted as the log odds of an outcome. To convert between a probability and log odds involves two steps:
#' 1. Compute the ratio (i.e., odds) corresponding to the probability: `r <- p / (1 - p)`.
#' 2. Take the natural log of that ratio: `log(r)`.
#'
#' Note that the log-odds of p = .5 is 0, so:
#'     `0 = B0 + B1*n + B2*g + B3*(n*g)`
#'     `-B1*n - B3*(n*g) = B0 + B2*g`
#'     `-n(B1 + B3*g) = B0 + B2*g`
#'     `-n = (B0 + B2*g) / (B1 + B3*g)`
#'     `n = -[(B0 + B2*g) / (B1 + B3*g)]`
#'
#' Where `n` is the `nproduced` (vocabulary size) and `g` is the group `TD=0`,
#' `ASD=1`.
#'
#' We can substitute L, the logit value corresponding with some other `crit_p`, and obtain:
#'
#'     `n = -[(B0 + B2*g - L) / (B1 + B3*g)]`
#'
decision_boundary <- function(m, crit_p = .5) {
    L <- log(crit_p / (1 - crit_p))
    g <- contrasts(model.frame(m)$group)[,1]
    b <- coef(m)
    return(-(b["(Intercept)"] + (b["groupASD"] * g) - L) / (b["nproduced"] + (b["nproduced:groupASD"] * g)))
}
