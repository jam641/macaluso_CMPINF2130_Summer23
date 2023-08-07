# Global Script that supports the final_project app
### the global script is the FIRST thing EXECUTED when the APP is launched!!!!

## `education` Final Data Frame ##################################################

## Load Packages
library(tidyverse)
library(shiny)
library(corrplot)
library(corrr)
library(visdat)
library(GGally)
library(factoextra)
library(cowplot)

# Load Cleaned Data
education <- readr::read_csv('education.csv', show_col_types = FALSE)

# Change/Update Data Types
education %>% mutate(student_ID = as.factor(student_ID),
                     Gender = as.factor(Gender),
                     EthnicGroup = as.factor(EthnicGroup),
                     ParentEduc = as.factor(ParentEduc),
                     LunchType = as.factor(LunchType),
                     TestPrep = as.factor(TestPrep),
                     ParentMaritalStatus = as.factor(ParentMaritalStatus),
                     PracticeSport = as.factor(PracticeSport),
                     IsFirstChild = as.factor(IsFirstChild),
                     NrSiblings = as.factor(NrSiblings),
                     TransportMeans = as.factor(TransportMeans),
                     WklyStudyHours = as.factor(WklyStudyHours),
                     MathScore = as.numeric(MathScore),
                     ReadingScore = as.numeric(ReadingScore),
                     WritingScore = as.numeric(WritingScore),
                     School = as.factor(School)) -> education

# rename education dataframe as df
df <- education

## Functions to Create Data Dictionaries #######################################

overall_info2 <- function(my_df)
{
  tibble::tibble(
    `Number of rows` = nrow(my_df),
    `Number of columns` = ncol(my_df),
    `Number of unique datatypes` = my_df %>%
      map_chr(~paste(class(.), collapse = ' ')) %>% n_distinct(),
    `Total number of missing values` = my_df %>%
      map_dbl(~sum( is.na(.) ) ) %>% sum()
  ) %>% pivot_longer(cols = everything())
}

variable_info <- function(my_df) 
{
  tibble::tibble(
    Variable = my_df %>% names(),
    `Data type` = my_df %>% map_chr(~paste(class(.), collapse = ' ')),
    `Unique values` = my_df %>% map_dbl(n_distinct),
    `Missing values` = my_df %>% map_dbl(~sum(is.na(.)))
  )
}

## Visualize Data Types & Missingness ########################################################
  # 12 categorical
  # 3 continuous
# make df from mean values provided above
datatypes_df <- data.frame(categorical = 13, continuous = 3)
head(datatypes_df) # sanity check

# Pivot to long format
datatypes_df %>% pivot_longer(cols=c(categorical:continuous), names_to="datatype_class",
                              values_to="num_count") -> datatypes_df_long
head(datatypes_df_long) # sanity check

# plotting
data_type_fig_static <- datatypes_df_long %>%
  ggplot(mapping = aes(x = datatype_class, y = num_count)) +
  geom_bar(aes(x = datatype_class, y = num_count, fill = datatype_class),        
            stat="identity", show.legend = FALSE) + 
  labs(title = 'Number of Variables Per Data Type', x = 'Variable Type', y = 'Count') +
  theme_classic()

## Missingness Visual
missingness_fig_static <- vis_dat(education)

## SCHOOL COMPARE TAB ##########################################################

# make list of unique school names
df %>%
  distinct(School) %>%
  arrange(School) %>% 
  pull(School) %>%
  as.character() -> unique_school_names

head(df)

## PCA  ########################################################################
cont_var_only <- education %>% select('MathScore','ReadingScore','WritingScore')
cont_var_only

  # PREPROCESSING CLUSER ANALYSIS
education_df <- education %>%
  select(where(is.numeric)) %>% as.data.frame %>%
  scale(center = TRUE, scale = TRUE) %>%
  as.data.frame %>% tibble::as_tibble()

  ## VIOLIN AND BOX PLOTS
# education_df %>% 
#   tibble::rowid_to_column() %>% 
#   pivot_longer(!c("rowid")) %>% 
#   ggplot(mapping = aes(y = name, x = value)) +
#   geom_violin(fill = 'grey') +
#   geom_boxplot(fill = NA, color = 'black', size = 1.05) +
#   theme_bw()

  ## KMEANS CLUSTERING
set.seed(12345)

education_k2 <- kmeans(x = education_df, centers = 2, iter.max = 50, nstart = 25)
# education_k2 %>% glimpse()
# education_k2$cluster %>% head()

  ## PAIRSPLOT
pairs_plot_static <- cont_var_only %>% 
  mutate(k2 = as.character(education_k2$cluster)) %>% 
  GGally::ggpairs(columns = 1:ncol(cont_var_only), progress = FALSE,
                  mapping = aes(color = k2)) +
  theme_classic()

  ## PCA
education_pca <- prcomp(education %>% select(where(is.numeric)) %>% as.data.frame(),
                   scale. = TRUE)

# education_pca %>% glimpse()

  ## PC SCORE, NUMBER INCREASE
# education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>%
#   tibble::rowid_to_column() %>%
#   pivot_longer(!c('rowid')) %>%
#   mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>%
#   ggplot(mapping = aes(x = pc_id, y = value)) +
#   geom_violin(fill = 'grey',
#               mapping = aes(group = pc_id)) +
#   geom_boxplot(mapping = aes(group = pc_id),
#                fill = NA, color = 'black') +
#   labs(x = "PC", y = "PC score") +
#   theme_bw()

  # VARIANCE OF EACH PC SCORE
# education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>%
#   tibble::rowid_to_column() %>%
#   pivot_longer(!c('rowid')) %>%
#   mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>%
#   ggplot(mapping = aes(x = pc_id, y = value)) +
#   stat_summary(fun = var,
#                geom = 'line', size = 1.2) +
#   stat_summary(fun = var,
#                geom = 'point', size = 3.5) +
#   labs(x = "PC", y = "PC score variance") +
#   theme_bw()

  ## HISTOGRAM PC SCORES 
# education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% 
#   tibble::rowid_to_column() %>% 
#   pivot_longer(!c('rowid')) %>% 
#   mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
#   filter(pc_id < 4) %>% 
#   ggplot(mapping = aes(x = value)) +
#   geom_histogram(bins = 21) +
#   facet_wrap(~pc_id, labeller = "label_both", scales = "free_y") +
#   theme_bw()

  ## SCATTER PLOT FIRST TWO PCs
# education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% glimpse()
# 
# education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>%
#   ggplot(mapping = aes(x = PC1, y = PC2)) +
#   geom_point(size = 3.5) +
#   theme_classic()

  ## VISUALIZE IN PC SPACE
pca_scatter_static <- education_pca$x %>% as.data.frame() %>% tibble::as_tibble() %>% 
  mutate(k2 = as.character(education_k2$cluster)) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(size = 3.5,
             mapping = aes(color = k2)) +
  ggthemes::scale_color_colorblind() +
  theme_classic()

  ## STREAMLINED FIGURES
# fviz_cluster(education_k2, data = education_df) +
  # theme_classic()
fviz_cluster(education_k2, geom='point', data = education_df) +
  theme_classic()

  ## ADD MORE CLUSTERS
education_k3 <- kmeans(x = education_df, centers = 3, iter.max = 50, nstart = 25)

  # GGPLOT FIGURES TO OBJECTS
p2 <- fviz_cluster(education_k2, geom = "point", data = education_df) + ggtitle("K = 2") +
  ggthemes::scale_color_colorblind() + ggthemes::scale_fill_colorblind() +
  theme_bw() + theme(legend.position = "none")
p3 <- fviz_cluster(education_k3, geom = "point", data = education_df) + ggtitle("K = 3") +
  ggthemes::scale_color_colorblind() + ggthemes::scale_fill_colorblind() +
  theme_bw() + theme(legend.position = "none")

prow <- plot_grid(p2, p3, align='vh', nrow=2)

legend_common <- get_legend(p3 + theme(legend.position = "right"))

plot_grid(prow, legend_common, rel_widths = c(1,.1))

  # STREAMLINED PCA
education_pca %>% summary()
factoextra::get_eigenvalue( education_pca )

  ## SCREE PLOT
scree_plot <- factoextra::fviz_screeplot(education_pca, addlabels = TRUE)

  ## VARIANCE WITH RESPECT TO THE PC
factoextra::get_eigenvalue( education_pca ) %>% 
  select(ends_with("percent")) %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(rowname, "\\d+"))) %>% 
  pivot_longer(!c("rowname", "pc_id")) %>% 
  ggplot(mapping = aes(x = pc_id, y = value)) +
  geom_line(mapping = aes(group = name),
            linewidth = 1.1) +
  geom_point(size = 4.5) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  theme_bw()

pc_var_exp_static <- factoextra::get_eigenvalue( education_pca ) %>% 
  select(ends_with("percent")) %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(rowname, "\\d+"))) %>% 
  pivot_longer(!c("rowname", "pc_id")) %>% 
  ggplot(mapping = aes(x = pc_id, y = value)) +
  geom_line(mapping = aes(group = name),
            linewidth = 1.1) +
  geom_point(size = 4.5) +
  geom_hline(data = tibble::tibble(threshold_value = c(50, 80, 95)) %>% 
               mutate(name = "cumulative.variance.percent"),
             mapping = aes(yintercept = threshold_value),
             color = 'red', linetype = 'dashed', linewidth = 1.) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  theme_bw()

  ## SPECIALIZED SCATTER PLOT
factoextra::fviz_pca_var(education_pca, col.var = 'black')
factoextra::fviz_pca_var(education_pca, col.var = 'contrib')
factoextra::fviz_pca_var(education_pca, col.var = 'contrib',
                         gradient.cols = c("darkorange", "grey", "navyblue"))

specialized_pca_scatter_static <- factoextra::fviz_pca_var(education_pca, col.var = 'contrib',
                         gradient.cols = c("darkorange", "grey", "navyblue"),
                         repel = TRUE)

factoextra::fviz_contrib( education_pca, choice='var', axes=1 )
factoextra::fviz_contrib( education_pca, choice='var', axes=2 )

  ## HEAT MAP
# (factoextra::get_pca(education_pca))$contrib %>% as.data.frame() %>% 
#   tibble::rownames_to_column() %>% 
#   tibble::as_tibble() %>% 
#   pivot_longer(!c("rowname")) %>% 
#   mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
#   ggplot(mapping = aes(x = pc_id, y = rowname)) +
#   geom_tile(mapping = aes(fill = value,
#                           group = interaction(pc_id, rowname)),
#             color = 'black') +
#   scale_fill_gradient2("Variable contribution percent",
#                        low = 'black', mid = 'white', high = 'navyblue',
#                        midpoint = 100 * (1 / length(education_pca$center))) +
#   theme_bw() +
#   theme(legend.position = "top")

heat_map_pca_static <- (factoextra::get_pca(education_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  pivot_longer(!c("rowname")) %>% 
  mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  ggplot(mapping = aes(x = pc_id, y = rowname)) +
  geom_tile(mapping = aes(fill = value > 100 * (1 / length(education_pca$center)),
                          group = interaction(pc_id, rowname)),
            color = 'black') +
  scale_fill_manual("Variable actively contributes to PC?",
                    values = c("TRUE" = "darkred",
                               "FALSE" = "grey70")) +
  theme_bw() +
  theme(legend.position = "top")

  ## HIERARCHICAL CLUSTERING
# factoextra::fviz_dist( dist(education_df, method = "euclidean") )

# education_ward <- hclust(d = dist(education_df, method = 'euclidean'), method = 'ward.D2')
# plot(education_ward, labels = FALSE)
