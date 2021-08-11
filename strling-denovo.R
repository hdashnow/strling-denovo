library('ggplot2')
library('tidyverse')

strling_dnms = read.csv('data/STRs150bpthresholdTRUE_depth.tsv', sep = '\t')

# Filter to those with reads in both parents
#strling_dnms = subset(strling_dnms, depth_mom > 10 & depth_dad > 10 & depth_kid > 10)

strling_dnms %>% group_by(sample, mutation) %>%
  summarise(n_denovos = sum(novel_amp =='True'), 
            n_denovos_vs_mom = sum(kiddelmom > 150),
            n_denovos_vs_dad = sum(kiddeldad > 150), 
            #mutation = mutation[1],
            median_depth_kid = median(depth_kid),
            median_depth_mom = median(depth_mom),
            median_depth_dad = median(depth_dad),
            ) %>% 
  gather(denovo_origins, counts, n_denovos:n_denovos_vs_dad) -> sample_strling_dnms

ggplot(sample_strling_dnms,
       aes(x = denovo_origins, y = counts, color = mutation)) + geom_jitter() #+
  labs(x = 'Median depth at de novo STR loci in mom', 
       y = 'Number of STRs bigger in kid than mom')

ggplot(subset(sample_strling_dnms, denovo_origins == "n_denovos_vs_mom"),
              aes(x = median_depth_mom, y = counts)) + geom_point() +
  labs(x = 'Median depth at de novo STR loci in mom', 
       y = 'Number of STRs bigger in kid than mom')

ggplot(subset(sample_strling_dnms, denovo_origins == "n_denovos_vs_dad"),
       aes(x = median_depth_dad, y = counts)) + geom_point() +
  labs(x = 'Median depth at de novo STR loci in dad', 
       y = 'Number of STRs bigger in kid than dad')

ggplot(strling_dnms,
       aes(x = depth_kid)) + geom_histogram() 
ggplot(strling_dnms,
       aes(x = depth_mom)) + geom_histogram() 
ggplot(strling_dnms,
       aes(x = depth_dad)) + geom_histogram() 
