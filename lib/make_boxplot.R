# Import data
df <- read.table("data/chi_2019_stats.tsv", header=T)

# Create factor column for grouping papers by number of figures
df$figure_group <- NA

# Use a mega weird for-loop to group papers.
for(row in 1:nrow(df)){
  x <- df[row,"figures"]
  df[row,"figure_group"] <- "25-29"
  if(x < 25){
    df[row,"figure_group"] <- "20-24"
    if(x < 20){
      df[row,"figure_group"] <- "15-19"
      if(x < 15){
        df[row,"figure_group"] <- "10-14"
        if(x < 10){
          df[row,"figure_group"] <- "5-9"
          if(x < 5){
            df[row,"figure_group"] <- "0-4"
          }
        }
      }
    }
  }
}

# Set order of groups for figure.
df$figure_group <- factor(df$figure_group, levels=c("0-4","5-9","10-14","15-19","20-24","25-29"))

library(ggplot2)
library(ggthemes)

# Make box plot.
p <- ggplot(df, aes(x=figure_group, y=downloads)) +
  geom_boxplot(fill='#0FA3B1', color='#0FA3B1', outlier.shape=NA)+
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  xlab("Number of Figures") +
  ylab("Total Downloads") +
  theme_minimal(base_size=28) +
  ylim(c(0,1000)) +
  ggtitle("CHI 2019 Paper Downloads") +
  theme(plot.title = element_text(hjust = 0.5))

# Display box plot.
p
