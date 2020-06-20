
prep_design <- function(glm_formula, data_obj) {
  
  design_matrix <- model.matrix(glm_formula, data = data_obj)
  
  #site id
  site <- data_obj$site
  
  # offset for exposure (n days)
  log_offset <- log(pmax(data_obj$days, 0.001))
  
  # extract names for later matching
  coef_names <- colnames(design_matrix)
  
  #return design list
  list(matrix = design_matrix, site = site , log_offset = log_offset, 
       coef_names = coef_names)
  
}

prep_coefs <- function(design_list) {
  
  # initiate model coefficients
  coef <- normal(0, 10, dim = ncol(design_list$matrix))

  # priors for the junk interactions would be specified here to replace instances of hte above uniform set, e.g.:

#  coef[] <- normal(0, 2) # o lo que sea
  
  # extract names for later matching
  coef_names <- colnames(design_list$matrix)
  
  # site random effect
  n_site <- length(unique(design_list$site))
  site_sd <- normal(0, 3, truncation = c(0, Inf))
  site_coef <- normal(0, site_sd, dim = n_site)
  
  list(coef = coef, site_sd = site_sd, site_coef = site_coef, 
       coef_names = coef_names)
}  

  
pred_prob <- function(design_list, coef_list, random_effect = TRUE, 
                      type = c('link', 'response')) {
  
  type = match.arg(type)
  
#  if (design_list$coef_names != coef_list$coef_names) stop("the coeficient list from design and coeficient lists do not match")
  
  # linear prediction matrix
  eta <- design_list$matrix %*% coef_list$coef + design_list$log_offset 
  
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
  days <- c(1,15, 30, 45, 60, 90, 120, 180, 240, 300, 365)
  
  data.frame(
    expand.grid('treatment' = treatment, 'kangaroo' = kangaroo, 
                'lagomorph' = lagomorph, 'context' = context, 'days' = days)
    )
}

# Generate df to plot predictions for P mortality ----

mort_pred_df <- function(df) {
  
  treatment  <-  c("Open", "Partial", "Total")
  kangaroo <- quantile(df$kangaroo, probs = c(.1,.25,.5,.75,.9))
  lagomorph <- quantile(df$lagomorph, probs = c(.1,.25,.5,.75,.9))
  context <- c('Buloke woodland', 'Mallee', 'Open grassland', 
               'Wattle dune')
  days <- c(1,15, 30, 45, 60, 90, 120, 180, 240, 300, 365)
  
  data.frame(
    expand.grid('treatment' = treatment, 'kangaroo' = kangaroo, 
                'lagomorph' = lagomorph, 'context' = context, 'days' = days)
    )
}




# mort_pred_df <- function(df, q_roo = 0.5, q_rab = 0.5) {
#   
#   base_df <- data.frame("days"=rep(seq(min(df$days),
#                                        365, 
#                                        length = 30), times = 3), 
#                         "treatment" = rep(c("Open", "Partial", "Total"), 
#                                           each = 30),
#                         "kangaroo" = quantile(df$kangaroo,
#                                               probs = q_roo),
#                         'lagomorph' = quantile(df$lagomorph,
#                                            probs = q_rab),
#                         "sand" = mean(df$sand)
#   )
#   
#   context <- c('Buloke woodland', 'Open grassland', 'Wattle dune', 'Mallee')
#   
#   merge(base_df, as.data.frame(context))
# }


