# Nome: Entendendo correlações
# Autor: Gabriel dos Reis Rodrigues
# June, 2021
# Last update: 2021-06-25
# ----------------------------------------

# Initial loading ====
if(!require("faux"))
  install.packages("faux"); library(faux)
if(!require("ggplot2"))
  install.packages("ggplot2"); library(ggplot2)
if(!require("plotly"))
  install.packages("plotly"); library(plotly)

# Minimalist theme ====
project_theme <- 
  theme(legend.position = "none",
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.text = element_blank(),
      axis.title = element_text(size = 14))

# Correlation plot function ====
corr_plot <- function(corr = 0,
                      sample = 1000,
                      line = T){
  set.seed(42)
  
  df <- rnorm_multi(n = sample, 
                    mu = c(20, 20),
                    sd = c(5, 5),
                    r = corr, 
                    varnames = c("X", "Y"),
                    empirical = T)
  
  if(line == T){
    ggplot(df, aes(x = X, y = Y)) +
      
      # Points
      geom_point(alpha = 0.5, position = 'jitter', color = "#011e5a") +
      
      # Line
      stat_smooth(method = "lm", se = F, color = "#011F5a", size = 1.2) +
      
      # Themes
      theme_classic() + project_theme} else{
        
        ggplot(df, aes(x = X, y = Y)) +
        
          # Points
          geom_point(alpha = 0.5, position = 'jitter', color = "#011e5a") +
        
          # Themes
          theme_classic() + project_theme}
  
}