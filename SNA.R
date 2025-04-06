### "And the patriarchs, moved with envy, sold Joseph into Egypt: but God was with him"

### SNA(Social Network Analysis) with igraph

## Import packages
library(igraph)
library(stringr)
library(openxlsx)

## Data
head(data_2010_fin)
head(data_2015_fin)
head(data_2020_fin)
head(data_2022_fin)

# use "igraph_from_data_frame" function
g_2010 <- graph_from_data_frame(data_2010_fin, directed = TRUE)
g_2015 <- graph_from_data_frame(data_2015_fin, directed = TRUE)
g_2020 <- graph_from_data_frame(data_2020_fin, directed = TRUE)
g_2022 <- graph_from_data_frame(data_2022_fin, directed = TRUE)

# calculate "eigenvector"
ev_2010 <- eigen_centrality(g_2010, directed = TRUE, weights = E(g_2010)$value)
ev_2015 <- eigen_centrality(g_2015, directed = TRUE, weights = E(g_2015)$value)
ev_2020 <- eigen_centrality(g_2020, directed = TRUE, weights = E(g_2020)$value)
ev_2022 <- eigen_centrality(g_2022, directed = TRUE, weights = E(g_2022)$value)

# Post processing

# good to look
# 2010
df_ec_2010 <-
  ev_2010$vector %>% as.data.frame() %>%
  tibble::rownames_to_column("id") %>%
  rename(eigen_centrality = ".")

df_ec_2010$eigen_centrality <- round(df_ec_2010$eigen_centrality, 6)
df_ec_2010 <- df_ec_2010 %>% arrange(desc(eigen_centrality))

# 2015
df_ec_2015 <-
  ev_2015$vector %>% as.data.frame() %>%
  tibble::rownames_to_column("id") %>%
  rename(eigen_centrality = ".")

df_ec_2015$eigen_centrality <- round(df_ec_2015$eigen_centrality, 6)
df_ec_2015 <- df_ec_2015 %>% arrange(desc(eigen_centrality))

# 2020
df_ec_2020 <-
  ev_2020$vector %>% as.data.frame() %>%
  tibble::rownames_to_column("id") %>%
  rename(eigen_centrality = ".")

df_ec_2020$eigen_centrality <- round(df_ec_2020$eigen_centrality, 6)
df_ec_2020 <- df_ec_2020 %>% arrange(desc(eigen_centrality))

# 2022
df_ec_2022 <-
  ev_2022$vector %>% as.data.frame() %>%
  tibble::rownames_to_column("id") %>%
  rename(eigen_centrality = ".")

df_ec_2022$eigen_centrality <- round(df_ec_2022$eigen_centrality, 6)
df_ec_2022 <- df_ec_2022 %>% arrange(desc(eigen_centrality))

head(df_ec_2010)
head(df_ec_2015)
head(df_ec_2020)
head(df_ec_2022)


## Append with code

# preprocessing code name
code_2010$code<-paste0("X",code_2010$code)
code_2015$code<-paste0("X",code_2015$code)
code_2020$code<-paste0("X",code_2020$code)
code_2022$code<-paste0("X",code_2022$code)

head(code_2010)
head(code_2015)
head(code_2020)
head(code_2022)


# merge
final_2010 <- left_join(df_ec_2010,code_2010,by = c("id" = "code")) %>% select(id,title,eigen_centrality)
final_2015 <- left_join(df_ec_2015,code_2015,by = c("id" = "code")) %>% select(id,title,eigen_centrality)
final_2020 <- left_join(df_ec_2020,code_2020,by = c("id" = "code")) %>% select(id,title,eigen_centrality)
final_2022 <- left_join(df_ec_2022,code_2022,by = c("id" = "code")) %>% select(id,title,eigen_centrality)

head(final_2010)
head(final_2015)
head(final_2020)
head(final_2022)


## Result Save to xlsx
example <- createWorkbook("example")
example

addWorksheet(example, "eigen_2010")
addWorksheet(example, "eigen_2015")
addWorksheet(example, "eigen_2020")
addWorksheet(example, "eigen_2022")
example

writeDataTable(example,"eigen_2010",final_2010)
writeDataTable(example,"eigen_2015",final_2015)
writeDataTable(example,"eigen_2020",final_2020)
writeDataTable(example,"eigen_2022",final_2022)

saveWorkbook(example, file="/Users/haley/Desktop/2025-1/Research/data/Korea/data_result/industry_result_final.xlsx")


## For Gephi Visualization

# Make EdGE csv file
# name merge

data_2010_edge <- 
  left_join(data_2010_fin,code_2010,by = c("from" = "code")) %>% 
  rename(from2=title) %>% 
  left_join(code_2010,by = c("to" = "code")) %>%
  rename(to2=title) %>% select("from","to","from2","to2","weight") %>% rename(rom_label=from2, to_label=to2)

data_2015_edge <- 
  left_join(data_2015_fin,code_2015,by = c("from" = "code")) %>% 
  rename(from2=title) %>% 
  left_join(code_2015,by = c("to" = "code")) %>%
  rename(to2=title) %>% select("from","to","from2","to2","weight") %>% rename(rom_label=from2, to_label=to2)

data_2020_edge <- 
  left_join(data_2020_fin,code_2020,by = c("from" = "code")) %>% 
  rename(from2=title) %>% 
  left_join(code_2020,by = c("to" = "code")) %>%
  rename(to2=title) %>% select("from","to","from2","to2","weight") %>% rename(from_label=from2, to_label=to2)

data_2022_edge <- 
  left_join(data_2022_fin,code_2022,by = c("from" = "code")) %>% 
  rename(from2=title) %>% 
  left_join(code_2022,by = c("to" = "code")) %>%
  rename(to2=title) %>% select("from","to","from2","to2","weight") %>% rename(from_label=from2, to_label=to2)

head(data_2010_edge)
head(data_2015_edge)
head(data_2020_edge)
head(data_2022_edge)

# save EDGE table
write.csv(data_2010_edge, "/Users/haley/Desktop/2025-1/Research/data/Korea/edge_industry_2010", row.names = FALSE)
write.csv(data_2015_edge, "/Users/haley/Desktop/2025-1/Research/data/Korea/edge_industry_2015", row.names = FALSE)
write.csv(data_2020_edge, "/Users/haley/Desktop/2025-1/Research/data/Korea/edge_industry_2020", row.names = FALSE)
write.csv(data_2022_edge, "/Users/haley/Desktop/2025-1/Research/data/Korea/edge_industry_2022", row.names = FALSE)


# make NODe csv file
data_2010_node <-final_2010 %>% rename(label=title) %>% select(id, label, eigen_centrality)
data_2015_node <-final_2015 %>% rename(label=title) %>% select(id, label, eigen_centrality)
data_2020_node <-final_2020 %>% rename(label=title) %>% select(id, label, eigen_centrality)
data_2022_node <-final_2022 %>% rename(label=title) %>% select(id, label, eigen_centrality)

# save NODE table
write.csv(data_2010_node, "/Users/haley/Desktop/2025-1/Research/data/Korea/node_industry_2010", row.names = FALSE)
write.csv(data_2015_node, "/Users/haley/Desktop/2025-1/Research/data/Korea/node_industry_2015", row.names = FALSE)
write.csv(data_2020_node, "/Users/haley/Desktop/2025-1/Research/data/Korea/node_industry_2020", row.names = FALSE)
write.csv(data_2022_node, "/Users/haley/Desktop/2025-1/Research/data/Korea/node_industry_2022", row.names = FALSE)

