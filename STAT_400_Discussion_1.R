#1: Using Cancer Re-Occurrence File, create and submit your own dot plot
library(ggformula)
cancer_recurance_url <- 'https://raw.githubusercontent.com/kathrynKozak/MAT160/master/docs/cancer_recurance.csv'
cancer_recurance <- read.csv(cancer_recurance_url)

gf_dotplot(~ tummor, data = cancer_recurance, title='Tumor Recurrence After Chemotherapy') +
  theme_minimal()


#2: Using the cancer file, give the mean, standard deviation, variance, and five-number summary.  
cancer_url <- 'https://raw.githubusercontent.com/kathrynKozak/MAT160/master/docs/cancer.csv'
cancer <- read.csv(cancer_url)

mean_survival <- df_stats(~survival, data=cancer, mean)
median_survival <- df_stats(~survival, data=cancer, median)
sd_survival <- df_stats(~survival, data=cancer, sd)
summary_survival <- df_stats(~survival, data=cancer, summary)

gf_boxplot(~survival, data=cancer, title="Cancer Survival Boxplot", color="blue", outlier.color = "red") +
  gf_refine(coord_flip())+
  gf_theme(theme_minimal())

#3: Using the Cancer Re-Occurrence File, do a histogram chart
gf_histogram(~tummor, data=cancer_recurance, title='Cancer Re-Occurence Histogram',
             color='lightblue',fill='lightblue', alpha=0.9)+
  gf_theme(theme_minimal())

