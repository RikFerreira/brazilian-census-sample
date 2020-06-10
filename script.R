# This code is just an empirical demonstration of some concepts
# presented by Thomas Lumley regarding complex survey samples.
# 
# At the end of this code, any student may calculate by themselves
# the measures from the Brazilian Census, made public by the IBGE (Brazilian Nation Institute of Statistics and Geography)
# 
# Author: Rik Ferreira Alves
# E-mail: rikferreiraalves@gmail.com
# Lattes CV: http://lattes.cnpq.br/9527400491453685
# Orcid ID: https://orcid.org/0000-0002-8084-7951
# Date: 2020-06-10

library(tidyverse) # To manipulate the data
library(survey) # To reconstruct the sample design

sample <- read.table( # Reads the dataset
  file = "./data/moc_pop.csv",
  header = TRUE,
  sep = ";",
  encoding = "UTF-8"
)

# V1006: Urban-rural classification; 1 = Urban, 2 = Rural
# V0010: Weight
# V0011: Weighing area (or strata)
str(sample)

# Convert variables to the proper type
sample$V1006 <- as.factor(sample$V1006)
sample$V0011 <- as.character(sample$V0011)

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

# By now, we can use the survey package to compute the measures we want
# For the purpose of this demonstration, we will calculate the absolute and relative
# populations of Montes Claros municipality by urban-rural condition
svytotal(~V1006, design) # Here we have the absolute population
svymean(~V1006, design) # Here we have the relative population
svymean(~V1006, design) * 100 # And here we have the percentage

# To assess this code, we can compare these results with the table from the IBGE.
# Here is the link where you can access this specific table: https://sidra.ibge.gov.br/tabela/1552
# Montes Claros geocode: 3143302