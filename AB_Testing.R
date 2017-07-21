# The dplyr package has a host of functions that are highly useful for 
# manipulating data when using 'data frames'.
library(dplyr)

# We begin by simulating 200 observations for each group by sampling 
# from a binomial distribution using rbinom. The raw data is converted 
# into a data frame with data_frame, and the data are stored into a 
# single column called signup.
bi_dist_orig <- data_frame(signup=rbinom(200, 1, .22))
bi_dist_pollu <- data_frame(signup=rbinom(200, 1, .28))
bi_dist_grass <- data_frame(signup=rbinom(200, 1, .42))

# Below are is hard coded output from the original call made to rbinom. 
# Either remove these lines of code or keep them to replicate the results 
# reported in the 'AB_Testing.Rmd'. 
bi_dist_orig <- data_frame(signup=c(0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	
                                    1,	0,	1,	0,	0,	1,	1,	0,	0,	0,	0,	1,	0,	0,	
                                    1,	1,	0,	0,	1,	0,	0,	0,	0,	1,	0,	0,	0,	1,	
                                    0,	0,	0,	0,	0,	1,	0,	0,	1,	1,	0,	0,	1,	0,	
                                    0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	1,	1,	1,	
                                    1,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	
                                    0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	1,	0,	0,	0,	
                                    1,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	
                                    0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	
                                    0,	0,	0,	0,	0,	1,	0,	0,	0,	1,	0,	0,	0,	0,	
                                    1,	1,	0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	1,	0,	
                                    0,	0,	0,	0,	0,	1,	0,	0,	1,	0,	0,	0,	0,	0,	
                                    0,	0,	1,	0,	1,	0,	1,	0,	0,	0,	0,	0,	0,	0,	
                                    1,	1,	1,	0,	0,	0,	0,	1,	1,	0,	1,	0,	0,	0,	
                                    0,	1,	1,	0))

bi_dist_pollu <- data_frame(signup=c(1,	0,	1,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	
                                     1,	0,	1,	0,	0,	0,	0,	0,	0,	1,	0,	0,	1,	1,	
                                     1,	1,	0,	0,	1,	0,	1,	0,	0,	1,	0,	1,	0,	0,	
                                     0,	1,	1,	0,	1,	0,	0,	1,	0,	0,	1,	1,	0,	0,	
                                     0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	1,	
                                     0,	0,	0,	1,	0,	1,	0,	0,	0,	0,	0,	0,	1,	0,	
                                     0,	1,	1,	1,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	
                                     0,	0,	0,	0,	0,	1,	1,	1,	1,	1,	1,	1,	0,	0,	
                                     0,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	
                                     0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	
                                     0,	0,	1,	1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	1,	
                                     0,	1,	1,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	
                                     0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	1,	0,	1,	0,	
                                     1,	0,	0,	1,	0,	1,	1,	0,	0,	1,	0,	0,	0,	0,	
                                     0,	0,	0,	0))

bi_dist_grass <- data_frame(signup=c(0,	1,	0,	0,	1,	1,	0,	1,	1,	0,	0,	0,	0,	0,	
                                     0,	0,	0,	1,	0,	1,	1,	1,	0,	0,	0,	1,	1,	0,	
                                     0,	1,	1,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	
                                     0,	0,	1,	0,	0,	0,	1,	0,	0,	1,	0,	1,	0,	1,	
                                     1,	0,	1,	1,	0,	0,	1,	0,	1,	0,	1,	0,	1,	1,	
                                     1,	1,	1,	0,	0,	1,	0,	0,	1,	0,	0,	1,	1,	0,	
                                     0,	0,	1,	1,	1,	1,	0,	1,	1,	1,	0,	1,	0,	1,	
                                     0,	0,	1,	0,	0,	0,	1,	1,	0,	0,	1,	1,	1,	0,	
                                     1,	1,	1,	1,	0,	0,	0,	1,	0,	1,	1,	0,	0,	1,	
                                     0,	0,	0,	1,	1,	0,	0,	1,	1,	0,	0,	0,	1,	0,	
                                     1,	0,	0,	0,	1,	0,	0,	1,	1,	0,	1,	0,	0,	1,	
                                     0,	0,	1,	1,	0,	0,	0,	1,	1,	0,	0,	0,	0,	1,	
                                     0,	1,	1,	0,	0,	0,	1,	1,	1,	0,	1,	0,	1,	0,	
                                     1,	0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0,	0,	0,	
                                     0,	1,	0,	0))

# A new column 'pagetype' is created to signify the version of the sign-up page 
# that a user sees, along with two other variables 'successes' and 'total', which 
# reflect the raw number of conversions, and the total number of users that viewed 
# that version of the page.
bi_dist_orig$pagetype <- "original"
bi_dist_orig$success <- sum(bi_dist_orig$signup)
bi_dist_orig$total <- nrow(bi_dist_orig)

bi_dist_pollu$pagetype <- "pollution"
bi_dist_pollu$success <- sum(bi_dist_pollu$signup)
bi_dist_pollu$total <- nrow(bi_dist_pollu)

bi_dist_grass$pagetype <- "grass"
bi_dist_grass$success <- sum(bi_dist_grass$signup)
bi_dist_grass$total <- nrow(bi_dist_grass)

# The data frame 'full_data_success_rates' holds the raw number of conversions and the total 
# number of users that viewed each of the 3 page types. The 'signup' column is removed because 
# it is no longer needed. 
full_data_success_rates <- rbind(bi_dist_orig[1,], bi_dist_grass[1,], bi_dist_pollu[1,]) %>%
  select(-signup)

# The *Chi Square* is run with `prop.test`, and we are interested in whether the p-value of 
# X-Squared is below 0.05, which it is (p < 0.0005), meaning that at least 
# one of the three proportions significantly differs from one of the others. The grassy version 
# had the highest conversion rate at 42%, followed by the pollution version with 29.5%, finishing 
# off with the original version that had a 23% conversion rate.
prop.test(full_data_success_rates[[2]], full_data_success_rates[[3]])

# For plotting purposes all 3 data sets are bound together with 'rbind' into a single data frame.
full_data <- rbind(bi_dist_orig, bi_dist_pollu, bi_dist_grass)

# The library 'ggplot2' is loaded and 'full_data' is piped into 'ggplot' with 'pagetype' and 'signup' 
# entered as the x and fill properties of the graph. The 'geom_bar' call produces the bar graph and 
# 'labs' labels the x and y axes.
library(ggplot2)
full_data %>%
  ggplot(aes(x = as.factor(pagetype), fill = as.factor(signup))) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(name="Conversion Type", palette="Paired", labels=c("No Conversion","Conversion")) +
  ggtitle("Conversion Rate for Sign-up Pages") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Sign-up Page Type", y = "Proportion")

# We conduct post-hoc (pairwise) comparisons using 'pairwise.prop.test', which uses the Holm correction 
# method to account for multiple comparisons. Groups 1, 2 and 3 correspond to the original, grass 
# and pollution sign-up pages, respectively.
multi_comp <- pairwise.prop.test(full_data_success_rates[[2]], full_data_success_rates[[3]])
multi_comp$p.value







