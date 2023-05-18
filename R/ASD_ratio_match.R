#' DOCUMENT!
solveTD_F <- function(ASD_M, ASD_F, TD_M) {
  TD_F <- (((ASD_M+ASD_F)*TD_M) / ASD_M) - TD_M
  return(TD_F)
}
ASD_ratio_match <- function(d, ASD_M, ASD_F, group = "NA") {
  n_M <- xtabs(~sex+group, data = d)["M",group]
  n_F <- xtabs(~sex+group, data = d)["F",group]

  # Define new variable that will serve as a filter for selecting subjects to respect the gender ratio in ASD group
  d$ASD_SexRatioMatch <- FALSE

  # Beause ASD is male-dominant, we will retain all males.
  z <- rep(TRUE, n_M)
  isMale <- d$sex == 'M'
  d$ASD_SexRatioMatch[isMale] <- z

  # We will select a number of females at random to obtain the intended ratio
  # Note! This involves random sampling, so the solution may change each time!

  n_F_RatioMatch <- solveTD_F(ASD_M,ASD_F,n_M)
  ix <- sample(n_F, size = round(n_F_RatioMatch), replace = FALSE)
  z <- rep(FALSE, n_F)
  z[ix] <- TRUE
  isFemale <- d$sex == 'F'
  d$ASD_SexRatioMatch[isFemale] <- z
  print(xtabs(~sex+ASD_SexRatioMatch, data = d))
  return(d)
}
