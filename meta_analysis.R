library(dplyr)
library(metafor)

FisherZ <- function(rho)  {0.5*log((1+rho)/(1-rho)) }   #converts r to z

FisherZInv <- function(z) {(exp(2*z)-1)/(1+exp(2*z)) }   #converts back again

fisher_cor_mean <- function(corr_values){
  corr_values[corr_values == 1] = 0.99
  corr_values[corr_values == -1] = -0.99
  corr_values[corr_values > 1 | corr_values < -1] = NA
  z_values = FisherZ(corr_values)
  mean_value = mean(z_values, na.rm = TRUE)
  mean_corr = FisherZInv(mean_value)
  return(mean_corr)
}

data <- read.csv("data/clean_data.csv")

# Calculate Fisher's Z scores and their variances
pub_count <- data %>% 
  count(pub) %>% 
  select(pub, n_cors = n)

data <- data %>%
  left_join(., pub_count) %>% 
  mutate(n_corrected = as.numeric(n) / as.numeric(n_cors)) %>% 
  mutate(
    z = FisherZ(r),  # Fisher's Z transformation
    var_z = 1 / (n_corrected - 3),  # Variance of the Z scores
    class = scale(class, center = TRUE, scale = FALSE),
    var_r = (1 - r^2)^2 / (n_corrected - 1),
    achievement_type = 2*achievement_type - 3
  )

# Conduct the multi-level meta-analysis
# Random effects model with publication as a random effect
res <- rma.mv(yi = z, V = var_z, 
              random = ~ 1 | pub, 
              slab = authors,
              test = "t",
              method = "REML",
              mods = ~ class + achievement_type, 
              data = data)

# Summary of the results
summary(res)

results <- summary(res)

forest_res <- rma.mv(yi = r, V = var_r, 
                     slab = authors,
                     test = "t",
                     method = "REML",
                     data = data)

# Create a forest plot
metafor::forest(
  forest_res, 
  header = TRUE,
  # efac = c(0,1),
  xlim = c(-0.6, 1.2),
  cex=0.7,
  # xlim = c(-1, 1),
  slab = ifelse(duplicated(paste(data$authors, data$year)), "-", paste(data$authors, data$year)),
  xlab="Effect Size (r)",
  mlab="Overall",
)


# Create a funnel plot
funnel(res)

fsn_res <- fsn(x = z, vi = var_z, data = data)

# Metagen ----
simple_data <- data %>% 
  group_by(pub, authors, year) %>% 
  summarize(
    z = mean(z),
    vi = mean(var_z)
  )

m.gen <- meta::metagen(TE = z,
                 seTE = vi,
                 studlab = authors,
                 data = simple_data,
                 sm = "ZCOR",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Correlation between Interest and Achievement")

# Summary of the results
summary(m.gen)

# Create a forest plot
meta::forest(m.gen, 
       leftlabs = c("z", "se"),
       rightlabs = c("Effect Size (Fisher's Z)", "95% CI"),
       studlab = FALSE, 
       sortvar = z,
       prediction = TRUE,
       print.tau2 = FALSE,
       text.random = "Overall Effect Size", 
       xlab = "Effect Size (Fisher's Z)")

