# EVERYTHING YOU NEED TO CALCULATE#
# Two metrics: MPD, MNTD
# For each metric, five categories: observed, null1, null2, SES1, SES2
# Within each category: two sub-categories: abundance weighted and abundance non-weighted
# 1. Fmntd:  Fmntd(dist.mat, my.sample) Quantifies the functional mean nearest neighbor distance with no abundance weighting
# 2. Fmntd.a:  Fmntd.a(dist.mat, my.sample) Quantifies the functional mean nearest neighbor distance with abundance weighting
# 3. Fmpd
# 4. Fmpd.a
# 5. weighted.faith: Abundance weighted calculation of Faiths PD index

# MNTD is also known as MNND (mean nearest neighbour distance)
obs_mntd <- function(dist.mat, my.sample){
  
  
  Fmntd.sub = function(x){ 
    
    ## Get the names of the species present in a 
    ## community.
    com.names = names(x[x > 0])
    
    ## Make the community phylogenetic distance 
    ## matrix by extracting those rows and columns 
    ## that have species present in our community.
    my.com.dist = dist.mat[com.names, com.names]
    
    ## Set all diagonal values to NA so that the 
    ## zeros for conspecific comparisons do not 
    ## interfere with our calculation of nearest 
    ## neighbors.
    diag(my.com.dist) = NA
    
    ## Use apply() to calculate the minimum value in 
    ## each row of the community phylogenetic 
    ## distance matrix and take a mean of those 
    ## values.
    mean(apply(my.com.dist, MARGIN = 1, min, na.rm=T), na.rm=T)
    
  }
  
  apply(my.sample, MARGIN = 1, Fmntd.sub)
  
}
obs_mntd.a <- function(dist.mat, my.sample){
  
  Fmntd.a.sub = function(x){ 
    
    ## Get the names of species present in the 
    ## community.
    com.names = names(x[x > 0])
    
    ## Make the community phylogenetic distance 
    ## matrix using the names of the species present ## in the community.
    my.com.dist = dist.mat[com.names, com.names]
    
    ## Place NA values in the diagonals
    diag(my.com.dist) = NA
    
    ## Calculate a mean of the minimum values in each 
    ## row of the community phylogenetic distance 
    ## matrix weighed by the abundances of the 
    ## species present in the community.
    weighted.mean(apply(my.com.dist, 1, min, na.rm = T), x[x > 0])
    
  }
  
  
  apply(my.sample, MARGIN = 1, Fmntd.a.sub)
  
}


obs_mpd <- function(dist.mat, my.sample){
  
  Fmpd.sub = function(my.sub.sample){ 
    
    ## Get the names of species present in the 
    ## community
    com.names = names(my.sub.sample[my.sub.sample > 0])
    
    ## Calculate mpd by extracting the lower triangle 
    ## of a phylogenetic distance matrix comprised of 
    ## only the species in our community.
    mean(as.dist(dist.mat[com.names, com.names]))
    
  }
  
  
  apply(my.sample, MARGIN = 1, Fmpd.sub)
  
}
obs_mpd.a <-  function(dist.mat, my.sample){
  
  Fmpd.a.sub = function(x){ 
    
    ## Get the names of the species in the community.
    
    com.names = names(x[x > 0])
    
    ## Make a matrix with one row containing 
    ## abundances and names of all present species
    com = t(as.matrix(x[x > 0]))
    
    ## Make phylogenetic distance matrix for taxa in ## community.
    com.dist = dist.mat[com.names, com.names]
    
    ## Calculate the product of the abundances of all 
    ## species in the community.
    abundance.products = t(as.matrix(com[1, com[1, ] > 0, drop = F]))%*% as.matrix(com[1, com[1, ] > 0, drop = F])
    
    ## Calculate a mean of the community phylogenetic 
    ## distance matrix weighted by the products of 
    ## all pairwise abundances.
    weighted.mean(com.dist, abundance.products)
    
  }
  
  
  apply(my.sample, MARGIN = 1, Fmpd.a.sub)
  
}

######################################## SCRIPT TO COMPARE WHETHER THE SLOPES OF TWO LINEAR MODELS ARE STATITSICALLY SIGNIFICANTLY DIFFERENT######################
library(lmtest)
compare_slopes <- function(model1, model2, varname) {
  # extract the coefficients and variance-covariance matrices from the models
  beta1 <- coef(model1)[varname]
  beta2 <- coef(model2)[varname]
  vcov1 <- vcov(model1)
  vcov2 <- vcov(model2)
  
  # calculate the test statistic and p-value for the difference in slopes
  test_stat <- (beta1 - beta2) / sqrt(vcov1[varname,varname] + vcov2[varname,varname])
  df <- df.residual(model1) + df.residual(model2) - 2
  p_value <- 2 * pt(abs(test_stat), df = df, lower.tail = FALSE)
  
  # print the results
  cat("Test statistic:", round(test_stat, 3), "\n")
  cat("p-value:", format(p_value, scientific = TRUE, digits = 3), "\n")
  
  # return the p-value
  return(p_value)
}