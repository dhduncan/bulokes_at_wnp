
prep_design <- function(glm_formula, data_obj, 
                        add_ispl = FALSE,
                        k = NULL, 
                        ispl_interaction = FALSE,
                        ispl_intercept = FALSE,
                        pred_purpose = FALSE
                        ) {
  
  env_matrix <- model.matrix(glm_formula, data = data_obj)
  
  # need number environmental coeficients for priors
  env_preds <- ncol(env_matrix)
  
  # then an element that will deal with different shape of response according to treatment group. 
  
  
  if(!add_ispl) {
    
    x <- log(pmax(data_obj$days, 0.5))
    #x <- pmax(data_obj$days, 0.5)
    #x <- (x - mean(x)) / (2 * sd(x)) # this seems to make no difference to coefficients
    #ply_days <- as.matrix(cbind(days = I(x), days2 = I(x^2)))
    
    ply_matrix <- model.matrix(~ poly(x, 2) * treat_class - treat_class - 1, 
                                          data_obj)
    
    ply_preds <- ncol(ply_matrix)
    
    design_matrix <- cbind(env_matrix, ply_matrix)
    
    # this flag informs subsequent functions
    ispl_bases <- NULL 
    knot_pos <- NULL
    
  } else {
    # construct monotonic spline element
    x <- log(pmax(data_obj$days, 0.5)) # variable on which spline is based
    
    if (pred_purpose & !is.null(mort_design$knot_pos)) {
      
      knot_pos <- mort_design$knot_pos
      df <- NULL
      
    } else if (pred_purpose & is.null(mort_design$knot_pos)) {
      
      knot_pos <- mort_design$knot_pos
      df <- 5
    }
      
      if(is.null(ispl_k)){
      
      knot_pos <- NULL
      df <- 5
      
    } else {
      
      # indicate knot placement (not at extremes)
      knot_pos <- sort(kmeans(x, k)$centers)
      df <- NULL
      
    }
    
    ispl_days <- iSpline(x, knots = knot_pos, df = df, degree = 3, 
                         intercept = ispl_intercept, 
                         Boundary.knots = c(-0.7, 6.01)) # just beyond extent of x, specified here so that it does contract when applied to prediction dataset of 365 days.
    
    knot_pos <- attr(ispl_days, "knots")
    
    
    if(ispl_interaction == TRUE) {
      
      ispl_matrix <- model.matrix(~ ispl_days * treat_class - treat_class - 1, 
                                  data_obj)
      
      message("you have specified an interaction of the ispline (on days) with simplified treatment type (open vs guarded)")
    }  else {
      
      ispl_matrix <- model.matrix(~ ispl_days - 1, data_obj)
    }
    
    
    ispl_bases <- ncol(ispl_matrix)
    
    # combine matrices to simplify later operations
    design_matrix <- cbind(env_matrix, ispl_matrix)
    
    ply_preds <- NULL
  }
    
  #site id
  site <- data_obj$site
  
  # offset for exposure (n days)
  log_offset <- log(pmax(data_obj$days, 0.5))
  
  # extract names for later matching
  coef_names <- colnames(design_matrix)
  
  #return design list
  list(matrix = design_matrix, site = site , log_offset = log_offset, 
       env_preds = env_preds, coef_names = coef_names, ply_preds = ply_preds,
       ispl_bases = ispl_bases, ispl_k = ispl_k, knot_pos = knot_pos,
       ispl_intercept = ispl_intercept, ispl_interaction = ispl_interaction
       )
  
}





prep_coefs <- function(design_list) {
  
  # initiate model coefficients
  env_coef <- normal(0, 10, dim = design_list$env_preds)

  # priors for the junk interactions would be specified here to replace instances of hte above uniform set, e.g.:

#  coef[] <- normal(0, 2) # o lo que sea
  
  if(is.null(design_list$ispl_bases)) {
    
    ispl_coef <-  NULL
    
    #ply_coef <- normal(0, 1, truncation = c(0, Inf), 
    #                   dim = design_list$ply_preds)
    
    ply_coef <- normal(0, 10, 
                       dim = design_list$ply_preds)
    
  } else {
    
    ispl_coef <- normal(0, 10,#, truncation = c(0, Inf), 
                        dim = design_list$ispl_bases)
    
    
    ply_coef <- NULL
    # nbasis <- 5
    # times <- design_list$ispl_bases / nbasis
    # ispl_coef <- ispl_prior(nbasis, times)
  }
 
  
  # extract names for later matching with env covariates
  coef_names <- colnames(design_list$matrix)
  
  # site random effect
  n_site <- length(unique(design_list$site))
  site_sd <- normal(0, 3, truncation = c(0, Inf))
  site_coef <- normal(0, 1, dim = n_site) * site_sd
  
  list(env_coef = env_coef, ispl_coef = ispl_coef, ply_coef = ply_coef,
       site_sd = site_sd, site_coef = site_coef, 
       coef_names = coef_names)
}  

  
pred_prob <- function(design_list, coef_list, 
                      random_effect = TRUE, 
                      type = c('link', 'response')) {
  
  type = match.arg(type)
  
#  if (design_list$coef_names != coef_list$coef_names) stop("the coeficient list from design and coeficient lists do not match")
  
  # linear prediction matrix
  
  if(is.null(design_list$ispl_bases)) {
    
    #eta <- design_list$matrix %*% coef_list$env_coef + design_list$log_offset
    
    eta <- design_list$matrix[,1:design_list$env_preds] %*% coef_list$env_coef + design_list$matrix[,-c(1:design_list$env_preds)] %*% coef_list$ply_coef + design_list$log_offset
    
  } else {
    
     eta <- design_list$matrix[,1:design_list$env_preds] %*% coef_list$env_coef + design_list$matrix[,-c(1:design_list$env_preds)] %*% coef_list$ispl_coef + design_list$log_offset 

    # use the smooth for the positive rate thingo
    # smooth <- design_list$matrix[,-c(1:design_list$env_preds)] %*% coef_list$ispl_coef
    # eta <- design_list$matrix[,1:design_list$env_preds] %*% coef_list$env_coef + design_list$log_offset + log(smooth)
  
  }
  
  
  if (random_effect) {
    
    site_effect <- coef_list$site_coef[design_list$site]
    
    # linear prediction matrix with site random effect
    eta <- eta + site_effect
    
  }
  
  switch(type, 
         link = eta, 
         response = icloglog(eta))
}

# Generate df to plot predictions for P browsing ----

browse_pred_df <- function(df) {
  
  treatment  <-  c("Open", "Partial")
  kangaroo <- quantile(df$kangaroo, probs = c(.1,.25,.5,.75,.9))
  lagomorph <- quantile(df$lagomorph, probs = c(.1,.25,.5,.75,.9))
  context <- c('Buloke woodland', 'Mallee', 'Open grassland', 
    'Wattle dune')
  days <- seq(1, 365, length.out = 30)
  
  tmp <- data.frame(
    expand.grid('treatment' = treatment, 'kangaroo' = kangaroo, 
                'lagomorph' = lagomorph, 'context' = context, 'days' = days)
  )
  
  tmp <- tmp %>% mutate(
    treat_class = if_else(treatment == "Open", "O", "G")
  )
  
  return(tmp)
  
}

# Generate df to plot predictions for P mortality ----

mort_pred_df <- function(df) {
  
  treatment  <-  c("Open", "Partial", "Total")
  kangaroo <- quantile(df$kangaroo, probs = c(.1,.25,.5,.75,.9))
  lagomorph <- quantile(df$lagomorph, probs = c(.1,.25,.5,.75,.9))
  context <- c('Buloke woodland', 'Mallee', 'Open grassland', 
               'Wattle dune')
  days <- seq(1, 365, length.out = 30)
  
  tmp <- data.frame(
    expand.grid('treatment' = treatment, 
                'kangaroo' = kangaroo, 
                'lagomorph' = lagomorph, 'context' = context, 'days' = days)
    )
  
  tmp <- tmp %>% mutate(
    treat_class = if_else(treatment == "Open", "O", "G")
  )
  
  return(tmp)
}