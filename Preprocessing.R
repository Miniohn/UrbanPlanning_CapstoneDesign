### "And the patriarchs, moved with envy, sold Joseph into Egypt: but God was with him"

### Preprocessing

## import packages
library(readxl)
library(dplyr)
library(tidygraph)
library(stringr)
library(tidyr)
library(ggplot2)
library(tibble)


## load data

# analyzing data
data_2010 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2010_industry_data.xlsx",sheet="industry_2010") %>%
  data.frame()
data_2015 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2015_industry_data.xlsx",sheet="industry_2015") %>%
  data.frame()
data_2020 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2020_industry_data.xlsx",sheet="industry_2020") %>%
  data.frame()
data_2022 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2022_industry_data.xlsx",sheet="industry_2022") %>%
  data.frame()

# code data
code_2010 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2010_industry_data.xlsx",sheet="code_2010") %>% 
  data.frame()
code_2015 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2015_industry_data.xlsx",sheet="code_2015") %>% 
  data.frame()
code_2020 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2020_industry_data.xlsx",sheet="code_2020") %>% 
  data.frame()
code_2022 <- read_xlsx("/Users/haley/Desktop/2025-1/Research/data/Korea/data_analyze/2022_industry_data.xlsx",sheet="code_2022") %>% 
  data.frame()

head(data_2010)
head(data_2015)
head(data_2020)
head(data_2022)

head(code_2010)
head(code_2015)
head(code_2020)
head(code_2022)


## Preprocessing

# Arranging
# (OD matrix --> 3 columns table)

# new variables 1
data_2010_n <- data_2010
data_2015_n <- data_2015
data_2020_n <- data_2020
data_2022_n <- data_2022

# colnames <- rownames
data_2010_n <- data_2010_n %>% select(-...1)
row.names(data_2010_n)<- colnames(data_2010_n)
head(data_2010_n)
row.names(data_2010_n)

data_2015_n <- data_2015_n %>% select(-...1)
row.names(data_2015_n)<- colnames(data_2015_n)
head(data_2015_n)
row.names(data_2015_n)

data_2020_n <- data_2020_n %>% select(-...1)
row.names(data_2020_n)<-colnames(data_2020_n)
head(data_2020_n)

data_2022_n <- data_2022_n %>% select(-...1)
row.names(data_2022_n)<-colnames(data_2022_n)
head(data_2022_n)


# OD matrix --> long format
data_2010_table <- data_2010_n %>%
  as.data.frame() %>%
  mutate(from = rownames(.)) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "weight"
  )
data_2010_table <- data_2010_table %>% data.frame()
head(data_2010_table,15)
unique(data_2010_table$from)
data_2010_table %>% str()

# 1. wait! : names preprocessing
# 2. groupby -> from, to
data_2010_table<-
  data_2010_table %>%
  mutate(from_new = substr(from,3,6),
         to_new = substr(to,3,6)) %>% select(from_new,to_new,weight) %>% rename(from=from_new, to=to_new) %>%
  group_by(from, to) %>% summarise(weight=sum(weight)) %>% as.data.frame()

## 
data_2015_table <- data_2015_n %>%
  as.data.frame() %>%
  mutate(from = rownames(.)) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "weight"
  )
data_2015_table <- data_2015_table %>% data.frame()
head(data_2015_table,15)
unique(data_2015_table$from)
data_2015_table %>% str()

# 1. wait! : names preprocessing
# 2. groupby -> from, to
data_2015_table<-
  data_2015_table %>%
  mutate(from_new = substr(from,3,5),
       to_new = substr(to,3,5)) %>% select(from_new,to_new,weight) %>% rename(from=from_new, to=to_new) %>%
  group_by(from, to) %>% summarise(weight=sum(weight)) %>% as.data.frame()


##
data_2020_table <- data_2020_n %>%
  as.data.frame() %>%
  mutate(from = rownames(.)) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "weight"
  )
data_2020_table <- data_2020_table %>% data.frame()
unique(data_2020_table$from)
head(data_2020_table,15)

# 1. wait! : names preprocessing
# 2. groupby -> from, to
data_2020_table<-
  data_2020_table %>%
  mutate(from_new = substr(from,3,5),
         to_new = substr(to,3,5)) %>% select(from_new,to_new,weight) %>% rename(from=from_new, to=to_new) %>%
  group_by(from, to) %>% summarise(weight=sum(weight)) %>% as.data.frame()

##
data_2022_table <- data_2022_n %>%
  as.data.frame() %>%
  mutate(from = rownames(.)) %>%
  pivot_longer(
    cols = -from,
    names_to = "to",
    values_to = "weight"
  )
data_2022_table <- data_2022_table %>% data.frame()
head(data_2022_table,15)


# 1. wait! : names preprocessing
# 2. groupby -> from, to
data_2022_table<-
  data_2022_table %>%
  mutate(from_new = substr(from,2,4),
         to_new = substr(to,2,4)) %>% select(from_new,to_new,weight) %>% rename(from=from_new, to=to_new) %>%
  group_by(from, to) %>% summarise(weight=sum(weight)) %>% as.data.frame()

##

unique(data_2010_table$from)
unique(data_2015_table$from)
unique(data_2020_table$from)
unique(data_2022_table$from)


# from!=to | weight != 0 
data_2010_fin <-data_2010_table %>% filter(from!=to) %>% filter(weight!=0)
data_2015_fin <-data_2015_table %>% filter(from!=to) %>% filter(weight!=0)
data_2020_fin <-data_2020_table %>% filter(from!=to) %>% filter(weight!=0)
data_2022_fin <-data_2022_table %>% filter(from!=to) %>% filter(weight!=0)

head(data_2010_fin,15)
head(data_2015_fin,15)
head(data_2020_fin,15)
head(data_2022_fin,15)

data_2010_fin %>% group_by(from) %>% summarise(count=n()) %>% as.data.frame() %>%
  View()
data_2020_fin %>% group_by(from) %>% summarise(count=n()) %>% as.data.frame() %>%
  View()
