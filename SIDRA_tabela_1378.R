# # This code intends to generate the same values presented at SIDRA table 1378
# 
# Variable: Frequency of habitants (absolute and relative)
# Class 1: Urban-rural condition
# Class 2: Gender
# Class 3: Age
# Class 4: Condition in the household
# 
# Author: Rik Ferreira Alves
# E-mail: rikferreiraalves@gmail.com
# Lattes CV: http://lattes.cnpq.br/9527400491453685
# Orcid ID: https://orcid.org/0000-0002-8084-7951
# Date: 2020-06-11

library(tidyverse)
library(survey)

sample <- read.table( # Reads the dataset
  file = "./data/1378.csv",
  header = TRUE,
  sep = ";",
  encoding = "UTF-8"
)

# Verifying the dataset
# V0001: UF
# V0002: Municipality
# V1006: Urban-rural condition
# V0010: Weights
# V0011: Weighing area
# V0502: Condition in the household
# V0601: Gender
# V6036: Age (in years)
# V6037: Age (in months for less than 1 year)
str(sample)

# Convert variables to the proper type
sample$V1006 <- as.factor(sample$V1006)
sample$V0502 <- as.factor(sample$V0502)
sample$V0601 <- as.factor(sample$V0601)
sample$V0502 <- as.factor(sample$V0502)
sample$V0011 <- as.character(sample$V0011)
sample$V6036 <- as.factor(sample$V6036)
sample$V6037 <- as.factor(sample$V6037)

# Verify the changes
str(sample)

# Count the number of households per stratum
sample <-
  sample %>%
  group_by(V0011) %>%
  mutate(strHous = n()) %>%
  ungroup()

# Verify the new variable
str(sample)

# Here we need the columns: strata; finite population correction in absolute terms; weights
# We also need to pass the data.frame to svydesign function
design <- svydesign(
  id = ~1,
  strata = ~V0011,
  fpc = ~strHous,
  weights = ~V0010,
  data = sample
)

# Frequency of habitants by urban-rural condition
svytotal(~V1006, design)
svymean(~V1006, design)

# Frequency of habitants by gender
svytotal(~V0601, design)
svymean(~V0601, design)

# Frequency of habitants by age (years)
svytotal(~V6036, design)
svymean(~V6036, design)

# Frequency of habitants by age (months)
svytotal(~V6037, design, na.rm = TRUE)
svymean(~V6037, design, na.rm = TRUE)

# Frequency of habitants by condition in the household
svytotal(~V0502, design)
svymean(~V0502, design)

# Frequency of habitants by gender, by urban-rural condition
svytotal(~interaction(V1006, V0601), design)
svymean(~interaction(V1006, V0601), design)

# Since the number of combinations between the variables of the SIDRA tabular plan is huge,
# and the methods were explored, this script will end here.
# Any question should be pointed in an issue or addressed to me by e-mail.
# 
# To assess this code, we can compare these results with the table from the IBGE.
# Here is the link where you can access this specific table: https://sidra.ibge.gov.br/tabela/1378
# Montes Claros geocode: 3143302