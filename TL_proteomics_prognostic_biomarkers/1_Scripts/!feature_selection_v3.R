library(dplyr)

# - drop the remove the second part log_prob_unselected from 
"return(log_prob_selected + log_prob_unselected)"

# memo from Qiran

#Dave wants to add this component to properly model a series of iid Bernoulli distributions,
#which in theory is true and should work well if you have less than 10 features. However,
#given you have over 300 features, that component will definitely go to 0 in probability. 
#For example, 0.9^ 100 = 2.65614e-05; 0.001^100 = 1e-300. They are all values close to 0 
#even though the ratio between them is huge. Thus, this component negatively affects 
#the numerical stability while not adding too much information.

# Function to compute softmax using log-sum-exp trick
softmax <- function(log_probs) {
  max_log <- max(log_probs) # for numerical stability
  exp_probs <- exp(log_probs - max_log)
  exp_probs / sum(exp_probs)
}

compute_prior_probs <- function(p_values) {
  P <- length(p_values)  # Total number of features
  # Generate all univariate models (size 1)
  univariate_models <- matrix(1:P, nrow = 1)
  # Generate all bivariate models (size 2)
  bivariate_models <- combn(P, 2)
  # Combine univariate and bivariate models
  univariate_models <- rbind(univariate_models, rep(0, ncol(univariate_models)))
  models <- cbind(univariate_models, bivariate_models)
  # Compute log probabilities for inclusion (log(1-p)) and exclusion (log(p))
  log_include_p <- log(1 - p_values)
  # Compute log probabilities for each model
  prior_log_probs <- apply(models, 2, function(model) {
    selected_features <- model[model > 0]
    # Log probability of selected features being included
    log_prob_selected <- sum(log_include_p[selected_features])
    return(log_prob_selected)
  })
  # Normalize log probabilities to probabilities (softmax)
  prior_probs <- softmax(prior_log_probs)
  return(list(models = models, prior_probs = prior_probs))
}


# Function to compute likelihood p(D | M_k) using BIC
compute_likelihood <- function(data, models) {
  ft_names <- names(data)[-1] # remove outcome col
  results <- apply(models, 2, function(pair) {
    i <- pair[1]
    j <- pair[2]
    formula <- as.formula(paste("outcome ~", paste(ft_names[c(i, j)], collapse = " + ")))
    model <- glm(formula, data = data, family = binomial)
    bic <- BIC(model) #−2log-likelihood+kN_PAR, the lower the better fit
    likelihood <- exp(-0.5 * bic) # Convert BIC to p(D | M_k)
    list(likelihood = likelihood, indices = paste0(ft_names[c(i, j)], collapse = " + "
                                                ))
  })
  
  # Separate the results into two lists for easier handling
  likelihoods <- sapply(results, function(x) x$likelihood)
  indices <- lapply(results, function(x) x$indices)
  
  return(list(likelihoods = likelihoods, indices = indices))
}


# Function to compute posterior model probabilities p(M_k | D)
compute_posterior_probs <- function(prior_probs, likelihoods) {
  # Compute log-priors and log-likelihoods
  log_prior_probs <- log(prior_probs)
  log_likelihoods <- log(likelihoods)
  
  # Compute log-numerators for posterior probabilities
  log_numerators <- log_prior_probs + log_likelihoods
  
  # Apply log-sum-exp trick to compute the denominator
  max_log_numerator <- max(log_numerators) # for numerical stability
  log_denominator <- max_log_numerator + log(sum(exp(log_numerators - max_log_numerator)))
  
  # Compute log posterior probabilities
  log_posterior_probs <- log_numerators - log_denominator
  
  # Convert log posterior probabilities back to probabilities
  posterior_probs <- exp(log_posterior_probs)
  
  return(posterior_probs)
}
# Function to compute PPI for each protein
compute_ppi <- function(models, posterior_probs, P) {
  ppi <- numeric(P)
  for (i in seq_len(P)) {
    indices <- which(apply(models, 2, function(pair) i %in% pair))
    ppi[i] <- sum(posterior_probs[indices])
  }
  return(ppi)
}
# Function to compute bayes factor for each protein
compute_bf <- function(ppi,p_values) {
  log_1_minus_p <- log(1 - p_values)
  max_log <- max(log_1_minus_p)
  sum_term <- sum(exp(log_1_minus_p - max_log)) # summation term
  denominator <- exp(max_log + log(sum(exp(log_1_minus_p - max_log)))) # denominator
  p_protein <- exp(log_1_minus_p) / denominator # prior prob of each protein
  bf <- (ppi / (1 - ppi)) / (p_protein /(1- p_protein))
  return(bf)
}

# Main function for modified BMA
modified_bma <- function(data, p_values) {
  # Step 1: Compute prior probabilities p(M_k)
  prior_info <- compute_prior_probs(p_values)
  models <- prior_info$models
  prior_probs <- prior_info$prior_probs
  
  # Step 2: Compute likelihood p(D | M_k)
  likelihoods_info <- compute_likelihood(data, models)
  likelihoods <- likelihoods_info$likelihoods
  indices <- likelihoods_info$indices
  
  # Step 3: Compute posterior model probabilities p(M_k | D)
  posterior_probs <- compute_posterior_probs(prior_probs, likelihoods)
  print(sum(posterior_probs))  
  # Step 4: Compute PPI and bayes factor for each protein
  P <- length(p_values)
  ppi <- compute_ppi(models, posterior_probs, P)
  names(ppi) <- names(p_values)
  bf <- compute_bf(ppi,p_values)
  return(list(ppi = ppi, bf = bf, posterior_probs = posterior_probs,
              likelihoods = likelihoods,prior_probs = prior_probs,
              indices = indices))
}

# Example usage
# Simulated data
set.seed(123)
P <- 50 # Number of proteins
data <- data.frame(matrix(rnorm(100 * P), ncol = P))
colnames(data) <- paste0("protein_", seq_len(P))
data$outcome <- sample(c(0, 1), 100, replace = TRUE)
data <- data %>% select(outcome, everything())
p_values <- runif(P, min = 6.924900e-203, max = 0.95) # Simulated p-values

# Run the modified BMA
#The columns in the data representing the proteins should correspond to the same proteins in the p_values vector.
result <- modified_bma(data, p_values)

# Output PPI for each protein
ppi <- result$ppi
bf <- result$bf
protein_results <- data.frame(ppi = ppi, bf = bf)
