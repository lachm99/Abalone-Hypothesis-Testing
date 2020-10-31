# Initial Data Exploration

library(tidyverse)
library(reshape2)
library(gridExtra)


# Names found in `abalone.names` information file.
cols = c("sex",	
             "length",
             "diam",
             "height",
             "weight_whole",
             "weight_shucked",
             "weight_viscera",
             "weight_shell",
             "rings")

# Read the data from `abalone.data` file.
# data_url = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
# raw_data = readr::read_csv(data_url, cols)
# raw_data = readr::read_csv("Data/abalone.data", cols)


# Correct the automatic variable typing
data = raw_data %>% mutate(sex = factor(sex, c("M", "F", "I")),
                       rings = as.integer(rings))

# Coerce into a long format where each entry contains only one weight measurement
# This allows a density plot comparing each weight measurement.
long = gather(data, "weight_type", "grams", 5:8)


# Density plots of different weight measurements, split by sex/maturity
ggplot(long, aes(x=grams, fill=weight_type)) + geom_density(alpha=.5) +
    facet_wrap(nrow=3, ~sex)

# Create dataframes pertaining only to adult/infantile abalone
adult = filter(data, sex != "I")
infant = filter(data, sex == "I")


# Plot diameter vs height for adults and infants
p1 = ggplot(infant, aes(x=diam, y=height)) + geom_point() + labs(title="INFANTS") + ylim(0,0.4)
p2 = ggplot(adult, aes(x=diam, y=height)) + geom_point() + labs(title="MATURE") + ylim(0,0.4)
grid.arrange(p1, p2, nrow = 1)



# Test if the mean weight is different across male and female abalone.
mWeight = filter(data, sex=="M")$weight_whole
fWeight = filter(data, sex=="F")$weight_whole

ggplot(adult, aes(y=weight_whole)) + geom_boxplot() + facet_wrap(~sex)

(t = t.test(mWeight, fWeight, var.equal=TRUE))
t$p.val


