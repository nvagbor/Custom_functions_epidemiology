
# ------------------------------------------------------------
# Required package
# ------------------------------------------------------------
if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("Package 'MASS' is required.")
}

get_interim_simulation_inputs <- function(data,
                                          event_var = "events",
                                          trt_var = "trt",
                                          pt_var = "pt",
                                          covariates = NULL) {
  
  # Build negative binomial analysis model formula
  nb_rhs_terms <- c(trt_var, covariates)
  nb_rhs_text  <- paste(nb_rhs_terms, collapse = " + ")
  
  nb_formula_text <- paste0(
    event_var, " ~ ", nb_rhs_text,
    " + offset(log(", pt_var, "))"
  )
  nb_formula <- stats::as.formula(nb_formula_text)
  
  # Fit interim NB model
  nb_fit <- MASS::glm.nb(
    formula = nb_formula,
    data = data,
    control = stats::glm.control(maxit = 100)
  )
  
  # Extract treatment effect and dispersion
  beta_hat <- stats::coef(nb_fit)[trt_var]
  se_beta  <- sqrt(stats::vcov(nb_fit)[trt_var, trt_var])
  irr_hat  <- exp(beta_hat)
  theta_hat <- nb_fit$theta
  
  # Split by treatment arm for empirical summaries
  control_data <- data[data[[trt_var]] == 0, , drop = FALSE]
  treated_data <- data[data[[trt_var]] == 1, , drop = FALSE]
  
  if (nrow(control_data) == 0 || nrow(treated_data) == 0) {
    stop("Both treatment groups must be present in the interim data.")
  }
  
  # Empirical summaries used in simpler simulation mode
  control_rate <- sum(control_data[[event_var]]) / sum(control_data[[pt_var]])
  mean_pt_control <- mean(control_data[[pt_var]])
  mean_pt_treated <- mean(treated_data[[pt_var]])
  treated_fraction <- mean(data[[trt_var]] == 1)
  
  # Person-time model is only fitted if covariates are specified.
  # This allows future person-time to depend on treatment + covariates.
  pt_fit <- NULL
  pt_sigma <- NULL
  pt_formula <- NULL
  
  if (!is.null(covariates) && length(covariates) > 0) {
    pt_rhs_terms <- c(trt_var, covariates)
    pt_rhs_text  <- paste(pt_rhs_terms, collapse = " + ")
    
    pt_formula_text <- paste0("log(", pt_var, ") ~ ", pt_rhs_text)
    pt_formula <- as.formula(pt_formula_text)
    
    # Simple Gaussian model for log person-time
    pt_fit <- lm(pt_formula, data = data)
    
    # Residual SD used when simulating future person-time
    pt_sigma <- summary(pt_fit)$sigma
  }
  
  list(
    nb_fit            = nb_fit,
    nb_formula        = nb_formula,
    beta_hat          = unname(beta_hat),
    se_beta           = unname(se_beta),
    irr_hat           = unname(irr_hat),
    theta_hat         = unname(theta_hat),
    control_rate      = control_rate,
    mean_pt_control   = mean_pt_control,
    mean_pt_treated   = mean_pt_treated,
    treated_fraction  = treated_fraction,
    pt_fit            = pt_fit,
    pt_formula        = pt_formula,
    pt_sigma          = pt_sigma
  )
}


simulate_future_data <- function(interim_data,
                                 sim_inputs,
                                 n_new,
                                 trt_var = "trt",
                                 pt_var = "pt",
                                 event_var = "events",
                                 covariates = NULL) {
  
  # Determine how many future participants go to each arm
  n_treated <- round(n_new * sim_inputs$treated_fraction)
  n_control <- n_new - n_treated
  
  # Split interim data by arm
  control_data <- interim_data[interim_data[[trt_var]] == 0, , drop = FALSE]
  treated_data <- interim_data[interim_data[[trt_var]] == 1, , drop = FALSE]
  
  # Create future treatment assignments
  future_control <- data.frame(trt = rep(0, n_control))
  future_treated <- data.frame(trt = rep(1, n_treated))
  names(future_control)[1] <- trt_var
  names(future_treated)[1] <- trt_var
  
  # ----------------------------------------------------------
  # resample covariates only if covariates were supplied
  # ----------------------------------------------------------
  if (!is.null(covariates) && length(covariates) > 0) {
    resampled_control <- control_data[
      sample(seq_len(nrow(control_data)), n_control, replace = TRUE),
      covariates,
      drop = FALSE
    ]
    
    resampled_treated <- treated_data[
      sample(seq_len(nrow(treated_data)), n_treated, replace = TRUE),
      covariates,
      drop = FALSE
    ]
    
    future_control <- cbind(future_control, resampled_control)
    future_treated <- cbind(future_treated, resampled_treated)
  }
  
  # Combine both arms into one future dataset
  future_data <- rbind(future_control, future_treated)
  
  # ----------------------------------------------------------
  # simulate future person-time
  # ----------------------------------------------------------
  if (is.null(covariates) || length(covariates) == 0) {
    # Simpler mode:
    # resample observed person-time within arm
    # This keeps simulation transparent when no covariates are used.
    pt_control <- sample(control_data[[pt_var]], n_control, replace = TRUE)
    pt_treated <- sample(treated_data[[pt_var]], n_treated, replace = TRUE)
    
    future_data[[pt_var]] <- c(pt_control, pt_treated)
    
  } else {
    # Covariate-conditional mode:
    # simulate log person-time from the fitted person-time model,
    # then exponentiate to obtain positive person-time.
    lp_pt <- stats::predict(
      sim_inputs$pt_fit,
      newdata = future_data,
      type = "response"
    )
    
    log_pt_sim <- stats::rnorm(
      n = nrow(future_data),
      mean = lp_pt,
      sd = sim_inputs$pt_sigma
    )
    
    future_data[[pt_var]] <- exp(log_pt_sim)
  }
  
  # ----------------------------------------------------------
  # simulate future event counts
  # ----------------------------------------------------------
  if (is.null(covariates) || length(covariates) == 0) {
    # Simpler arm-based event simulation:
    # expected count = control_rate * pt in control arm
    # expected count = control_rate * irr_hat * pt in treated arm
    trt <- future_data[[trt_var]]
    
    mu <- ifelse(
      trt == 0,
      sim_inputs$control_rate * future_data[[pt_var]],
      sim_inputs$control_rate * sim_inputs$irr_hat * future_data[[pt_var]]
    )
    
  } else {
    # Covariate-conditional event simulation:
    # use the full fitted interim NB model to construct the mean
    # for each future participant.
    #
    # The NB model already includes offset(log(pt)), so if we call
    # predict(..., type = "link", newdata = future_data), it will use
    # the simulated future pt values inside the offset.
    eta <- stats::predict(sim_inputs$nb_fit, newdata = future_data, type = "link")
    
    mu <- exp(eta)
  }
  
  # Simulate event counts from the negative binomial distribution
  future_data[[event_var]] <- MASS::rnegbin(
    n = nrow(future_data),
    mu = mu,
    theta = sim_inputs$theta_hat
  )
  
  future_data
}


# ============================================================
# Check non-inferiority from a fitted final model
# ============================================================
check_noninferiority <- function(fitted_model,
                                 trt_var = "trt",
                                 ni_margin = 1.26,
                                 alpha_one_sided = 0.025) {
  
  beta_hat <- stats::coef(fitted_model)[trt_var]
  se_beta  <- sqrt(stats::vcov(fitted_model)[trt_var, trt_var])
  
  z_crit <- stats::qnorm(1 - alpha_one_sided)
  
  upper_log_irr <- beta_hat + z_crit * se_beta
  upper_irr <- exp(upper_log_irr)
  
  list(
    irr_hat     = unname(exp(beta_hat)),
    upper_irr   = unname(upper_irr),
    noninferior = is.finite(upper_irr) && (upper_irr < ni_margin)
  )
}


# ============================================================
# Estimate conditional power for a candidate total sample size
# ============================================================
estimate_conditional_power <- function(interim_data,
                                       total_n,
                                       event_var = "events",
                                       trt_var = "trt",
                                       pt_var = "pt",
                                       covariates = NULL,
                                       ni_margin = 1.26,
                                       alpha_one_sided = 0.025,
                                       nsim = 1000,
                                       seed = 12345,
                                       verbose = FALSE) {
  
  n_interim <- nrow(interim_data)
  
  if (total_n < n_interim) {
    stop("total_n must be at least as large as the interim sample size.")
  }
  
  # Fit interim model(s) and extract simulation quantities
  sim_inputs <- get_interim_simulation_inputs(
    data       = interim_data,
    event_var  = event_var,
    trt_var    = trt_var,
    pt_var     = pt_var,
    covariates = covariates
  )
  
  n_new <- total_n - n_interim
  
  if (verbose) {
    message("Interim N: ", n_interim)
    message("Candidate total N: ", total_n)
    message("Additional participants to simulate: ", n_new)
    message("Observed interim IRR: ", round(sim_inputs$irr_hat, 4))
    message("Observed interim theta: ", round(sim_inputs$theta_hat, 4))
    message("Covariates: ",
            if (is.null(covariates)) "none" else paste(covariates, collapse = ", "))
  }
  
  # If no future participants are needed, use the interim fit directly
  if (n_new == 0) {
    ni_result <- check_noninferiority(
      fitted_model    = sim_inputs$nb_fit,
      trt_var         = trt_var,
      ni_margin       = ni_margin,
      alpha_one_sided = alpha_one_sided
    )
    
    return(list(
      cp                = as.numeric(ni_result$noninferior),
      interim_irr_hat   = sim_inputs$irr_hat,
      interim_theta_hat = sim_inputs$theta_hat,
      control_rate      = sim_inputs$control_rate
    ))
  }
  
  set.seed(seed)
  success_vector <- logical(nsim)
  fit_failed     <- logical(nsim)
  
  analysis_cols <- c(trt_var, pt_var, event_var, covariates)
  
  for (sim_index in seq_len(nsim)) {
    
    # Simulate the future participants
    future_data <- simulate_future_data(
      interim_data = interim_data,
      sim_inputs   = sim_inputs,
      n_new        = n_new,
      trt_var      = trt_var,
      pt_var       = pt_var,
      event_var    = event_var,
      covariates   = covariates
    )
    
    # Combine observed interim data with simulated future data
    completed_data <- rbind(
      interim_data[, analysis_cols, drop = FALSE],
      future_data[, analysis_cols, drop = FALSE]
    )
    
    # Refit the planned final analysis model
    final_fit <- try(
      MASS::glm.nb(
        formula = sim_inputs$nb_formula,
        data    = completed_data,
        control = stats::glm.control(maxit = 100)
      ),
      silent = TRUE
    )
    
    # If fitting fails, count as unsuccessful
    if (inherits(final_fit, "try-error")) {
      fit_failed[sim_index] <- TRUE
      success_vector[sim_index] <- FALSE
    } else {
      ni_result <- check_noninferiority(
        fitted_model    = final_fit,
        trt_var         = trt_var,
        ni_margin       = ni_margin,
        alpha_one_sided = alpha_one_sided
      )
      success_vector[sim_index] <- ni_result$noninferior
    }
    
    if (verbose && sim_index %% 100 == 0) {
      message("Completed simulation ", sim_index, " / ", nsim)
    }
  }
  
  list(
    cp_all_simulations = mean(success_vector),
    cp_fitted_only     = mean(success_vector[!fit_failed]),
    interim_irr_hat   = sim_inputs$irr_hat,
    interim_theta_hat = sim_inputs$theta_hat,
    control_rate      = sim_inputs$control_rate,
    total_n           = total_n,
    n_interim         = n_interim,
    nsim              = nsim, 
    n_failed          = sum(fit_failed),
    pct_failed        = 100 * mean(fit_failed)
  )
}


# ============================================================
# Find the minimum total sample size reaching target CP
# ============================================================
find_required_sample_size <- function(interim_data,
                                      planned_n = 398,
                                      max_n = 597,
                                      cp_target = 0.80,
                                      step = 5,
                                      ...) {
  
  candidate_n_values <- seq(planned_n, max_n, by = step)
  cp_values <- numeric(length(candidate_n_values))
  
  for (i in seq_along(candidate_n_values)) {
    cp_result <- estimate_conditional_power(
      interim_data = interim_data,
      total_n      = candidate_n_values[i],
      ...
    )
    cp_values[i] <- cp_result$cp
  }
  
  first_success_index <- which(cp_values >= cp_target)[1]
  
  if (is.na(first_success_index)) {
    required_n <- max_n
    achieved_cp <- tail(cp_values, 1)
  } else {
    required_n <- candidate_n_values[first_success_index]
    achieved_cp <- cp_values[first_success_index]
  }
  
  list(
    search_grid = data.frame(
      total_n = candidate_n_values,
      cp      = cp_values
    ),
    required_n = required_n,
    achieved_cp = achieved_cp
  )
}


# ============================================================
# Apply the promising-zone SSR rule
# ============================================================
run_promising_zone_ssr <- function(interim_data,
                                   planned_n = 398,
                                   max_n = 597,
                                   cp_lower = 0.30,
                                   cp_target = 0.80,
                                   step = 5,
                                   # Allow user to calculate power based only on successful simulations
                                   exclude_failed_simulations = FALSE, 
                                   ...) {
  
  # Estimate CP at the originally planned sample size
  cp_at_planned_n <- estimate_conditional_power(
    interim_data = interim_data,
    total_n      = planned_n,
    ...
  )
  
  if (exclude_failed_simulations) {
    observed_cp <- cp_at_planned_n$cp_fitted_only
  } else {
    observed_cp <- cp_at_planned_n$cp_all_simulations
  }
  
  # Apply promising-zone classification
  if (observed_cp < cp_lower) {
    zone <- "Unfavourable"
    recommended_n <- planned_n
    achieved_cp <- observed_cp
    
  } else if (observed_cp < cp_target) {
    zone <- "Promising"
    
    size_search <- find_required_sample_size(
      interim_data = interim_data,
      planned_n    = planned_n,
      max_n        = max_n,
      cp_target    = cp_target,
      step         = step,
      ...
    )
    
    recommended_n <- size_search$required_n
    achieved_cp   <- size_search$achieved_cp
    
  } else {
    zone <- "Favourable"
    recommended_n <- planned_n
    achieved_cp   <- observed_cp
  }
  
  list(
    zone                          = zone,
    cp_at_planned_n               = observed_cp,
    recommended_total_n           = recommended_n,
    recommended_increase          = recommended_n - planned_n,
    projected_cp_at_recommended_n = achieved_cp,
    interim_irr_hat               = cp_at_planned_n$interim_irr_hat,
    interim_theta_hat             = cp_at_planned_n$interim_theta_hat, 
    failed_simulation             = paste0(cp_at_planned_n$n_failed, " (", 
                                           round(cp_at_planned_n$pct_failed, 1), 
                                           "%)")
  )
}

