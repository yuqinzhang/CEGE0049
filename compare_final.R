library("ggplot2")
library("dplyr")
library("ggfortify")
library("dendextend")
#load.libraries <- c('plyr', 'dplyr','data.table', 'readxl', 'reshape2', 'stringr', 'stringi', 'ggplot2', 'tidyverse', 'gridExtra','matrixStats','lubridate','corrplot','e1071','xgboost','caret','zoo','factoextra','plotly','DT','rpart')
library(ggplot2)
library(colorspace)
library(bestNormalize)
library(reshape2)
library(Rcpp)
library(cluster)
library(factoextra)
library(NbClust)
library(ggraph)
library(ggalluvial)
library(networkD3)
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

#mobility
data <- read.csv("cleaned_dataset.csv")
cleaned2 <- data[rowSums(is.na(data))<8,]
cleaned3 <- cleaned2 %>% 
  group_by(individual_id.x)%>%
  summarise(n=n())%>%
  filter(n==3)

data2 <- data %>% filter(individual_id.x %in% cleaned3$individual_id.x)
data2[is.na(data2)] <- 0
cleaned_final <- reshape(data2, idvar=c("individual_id.x"), timevar = "label", direction="wide")

cleaned_final_nor <- cleaned_final
cleaned_final_nor[2:ncol(cleaned_final_nor)] <- lapply(cleaned_final_nor[2:ncol(cleaned_final_nor)], function(x) orderNorm(x)$x.t )

data_cluster2 <- cleaned_final_nor %>%
  select_if(is.numeric)
scaled_data2 <- scale(data_cluster2)

res.hk <-hkmeans(scaled_data2, 6)
#clustergram(scaled_data2, k.range = 2:8, line.width = 0.004)
res.hk$size

hkm6 = as.data.frame(cbind(as.character(cleaned_final$individual_id.x), res.hk$cluster))
colnames(hkm6) = c("id", "Mobility_Cluster")
#replacemob <- replace(hkm6$Mobility_Cluster, hkm6$Mobility_Cluster == 1, "Cluster 1: active mobility")
#hkm6$Mobility_Cluster <- replacemob

#Merge Clusters onto original data
input_data_hkm6 = left_join(cleaned_final,hkm6, by = c('individual_id.x' = 'id'))

#Create a national mean score
reg_means = colMeans(input_data_hkm6[,2:(ncol(input_data_hkm6)-1)]) # the first colunm is GeographyCode; the last colunm is the clustering results
#Create a mean score for each variable, by the clusters
hkm6_mean = input_data_hkm6[,2:(ncol(input_data_hkm6))] %>% dplyr::group_by(Mobility_Cluster) %>% dplyr::summarise_all(., mean)

#Calculate cluster index score
hkm6_mean$Mobility_Cluster = NULL
for (i in (1:length(reg_means))){hkm6_mean[,i] = hkm6_mean[,i] / reg_means[i] * 100 }

cluster_differences = hkm6_mean
rownames(cluster_differences) = c(paste0("Mobility_Cluster",' ',as.character(seq(1:nrow(cluster_differences)))))
datalookup = cluster_differences

cluster_d = datalookup
cluster_d$Mobility_Cluster = rownames(cluster_d)
cluster_d_m = melt(cluster_d)

cluster_d_m$variable <- factor(cluster_d_m$variable, levels = unique(cluster_d_m$variable))

p <- ggplot(cluster_d_m, aes(x = variable, y = as.character(Mobility_Cluster),fill = value))+
  scale_fill_continuous_divergingx(name = 'Index',
                                   palette = 'Spectral',
                                   mid = 100,
                                   l1=0,l3 = 0,p3 = .4,
                                   rev=F,n_interp = 15,alpha = 1)+
  geom_tile(colour = "white",alpha = 0.7) +scale_x_discrete(position = "bottom") +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.background = element_blank()) + labs(x = "", y = "") + coord_equal()
p

hk.clust <- res.hk$cluster
dend1 <- fviz_dend(res.hk, cex = 0.1, k=6, 
                   rect = TRUE,  
                   k_colors = "jco",
                   rect_border = "jco", 
                   rect_fill = TRUE,
                   main = "Mobility Cluster",
                   xlab = "     Cluster 1                               Cluster 2                        Cluster 3                    Cluster 4                 Cluster 5                           Cluster 6"
                   ) + theme_minimal() 


dend1

fviz_dend(res.hk, cex = 0.1, k=6, 
          type = "circular" ,  
          k_colors = "jco",
          main = "Mobility Cluster Circular",
          ) + theme_dendro() 



#geodemo
data_merged <- read.csv("individual_data2.csv")
cleaned_final_nor_geo <- data_merged

cleaned_final_nor_geo[3:ncol(cleaned_final_nor_geo)] <- lapply(cleaned_final_nor_geo[3:ncol(cleaned_final_nor_geo)], function(x) bestNormalize(x)$x.t )

data_cluster_geo <- cleaned_final_nor_geo %>%
  select_if(is.numeric)
scaled_data_geo <- scale(data_cluster_geo)

res.hk_geo <-hkmeans(scaled_data_geo, 6)
res.hk_geo$size

hkm6_geo = as.data.frame(cbind(as.character(data_merged$individual_id.x), res.hk_geo$cluster))
colnames(hkm6_geo) = c("id", "Demographics_Cluster")
#Merge Clusters onto original data
input_data_hkm6_geo = left_join(data_merged,hkm6_geo, by = c('individual_id.x' = 'id'))

#Create a national mean score
reg_means_geo = colMeans(input_data_hkm6_geo[,3:(ncol(input_data_hkm6_geo)-1)]) # the first colunm is GeographyCode; the last colunm is the clustering results
#Create a mean score for each variable, by the clusters
hkm6_mean_geo = input_data_hkm6_geo[,3:(ncol(input_data_hkm6_geo))] %>% dplyr::group_by(Demographics_Cluster) %>% dplyr::summarise_all(., mean)

#Calculate cluster index score
hkm6_mean_geo$Demographics_Cluster = NULL
radar_geo <- hkm6_mean_geo
radar_geo$Demographics_Cluster <- NULL
meangeo_mean = hkm6_mean_geo[,2:(ncol(hkm6_mean_geo))] %>%  dplyr::summarise_all(., mean)


meangeo_mean[1,1] = mean(radar_geo$Income.Score..rate.)
meangeo_mean[1,2] = mean(radar_geo$Employment.Score..rate.)
meangeo_mean[1,3] = mean(radar_geo$Education..Skills.and.Training.Score)
meangeo_mean[1,4] = mean(radar_geo$Health.Deprivation.and.Disability.Score)
meangeo_mean[1,5] = mean(radar_geo$Geographical.Barriers.Sub.domain.Score)
meangeo_mean[1,6] = mean(radar_geo$Indoors.Sub.domain.Score)
meangeo_mean[1,7] = mean(radar_geo$Outdoors.Sub.domain.Score)

write.csv(meangeo_mean,"~/MSc/Dissertation/meangeo_mean.csv", row.names = FALSE)
meangeo_mean <- read.csv("meangeo_mean.csv")
for (i in (1:length(meangeo_mean))){radar_geo[,i] = radar_geo[,i] / meangeo_mean[i]}
set.seed(99)
coul <- brewer.pal(6, "Paired")
coul2 <- brewer.pal(1, "Paired")
colors_border <- coul
colors_in <- alpha("grey",0.2)
radar_geo <- as.data.frame(scale(radar_geo))
radar_geo <- rbind(rep(2,6) , rep(-2,6) , radar_geo)
colnames(radar_geo) = c("Income", "Employment","Education","Health","Geographical Barriers","Indoors","Outdoors")
# plot with default options:
cluster1 = radar_geo[-c(2:8),]

radarchart( radar_geo, axistype=1 , 
            #custom polygon
            pcol=colors_border , plwd=2 , pfcol=colors_in,plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(-2,2,0.5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)


, pfcol=colors_in
# Add a legend
legend(x=1.2, y=1, legend = c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5","Cluster6"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=1,y.intersp=0.5)

for (i in (1:length(reg_means_geo))){
  hkm6_mean_geo[,i] = hkm6_mean_geo[,i] / reg_means_geo[i] * 100 }

cluster_differences_geo = hkm6_mean_geo
rownames(cluster_differences_geo) = c(paste0("Demographics_Cluster",' ',as.character(seq(1:nrow(cluster_differences_geo)))))
datalookup_geo = cluster_differences_geo

cluster_d_geo = datalookup_geo
colnames(cluster_d_geo) = c("Income", "Employment","Education","Health","Geographical Barriers","Indoors","Outdoors")
cluster_d_geo$Demographics_Cluster = rownames(cluster_d_geo)
cluster_d_m_geo = melt(cluster_d_geo)

cluster_d_m_geo$variable <- factor(cluster_d_m_geo$variable, levels = unique(cluster_d_m_geo$variable))

p2 <- ggplot(cluster_d_m_geo, aes(x = variable, y = as.character(Demographics_Cluster),fill = value))+
  scale_fill_continuous_divergingx(name = 'Index',
                                   palette = 'Spectral',
                                   mid = -1000,
                                   l1=0,l3 = 0,p3 = .4,
                                   rev=F,n_interp = 15,alpha = 1)+
  geom_tile(colour = "white",alpha = 0.7) +scale_x_discrete(position = "bottom") +
  theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.background = element_blank(),axis.text.x = element_text(angle=45, hjust=0.5)) + labs(x = "", y = "") + coord_equal()
p2

dend2 <- fviz_dend(res.hk_geo, cex = 0.1, k=6, 
                   rect = TRUE,  
                   k_colors = "jco",
                   rect_border = "jco", 
                   rect_fill = TRUE,
                   main = "Geodemographics Cluster",
                   xlab = "cluster1                                cluster1                                cluster1                                cluster1                                cluster                                cluster"
) + theme_minimal() 


dend2

fviz_dend(res.hk_geo, cex = 0.1, k=6, 
          type = "circular" ,  
          k_colors = "jco",
          main = "Mobility Cluster Circular",
) + theme_dendro() 


#compare
input_data_hkm6_comb = left_join(input_data_hkm6,hkm6_geo, by = c('individual_id.x' = 'id'))

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 1, "Active Mobility")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 2, "TD > TR")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 3, "High stationary TD")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 4, "Inactive Mobility")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 5, "No difference pre & post")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

replacemob <- replace(input_data_hkm6_comb$Mobility_Cluster, input_data_hkm6_comb$Mobility_Cluster == 6, "More active after 1st dose")
input_data_hkm6_comb$Mobility_Cluster <- replacemob

#geo change cluster name

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 1, "Good outdoor,Low barrier")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 2, "Good indoor,Bad outdoor")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 3, "Average,better indoor")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 4, "High income")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 5, "Slightly above average")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo

replacegeo <- replace(input_data_hkm6_comb$Demographics_Cluster, input_data_hkm6_comb$Demographics_Cluster == 6, "High barrier,Better indoor")
input_data_hkm6_comb$Demographics_Cluster <- replacegeo



compare_cleaned <- input_data_hkm6_comb[41:ncol(input_data_hkm6_comb)]
my_table <- as.data.frame(table(compare_cleaned$Mobility_Cluster, compare_cleaned$Demographics_Cluster))
colnames(my_table) <- c("Mobility_Cluster","Demographics_Cluster","Freq")

ggplot(my_table, aes(Demographics_Cluster, Mobility_Cluster))+
  geom_tile(aes(fill = Freq), colour = "black")+
  geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high = "steelblue")+ 
  theme(axis.text.x = element_text(angle=45, hjust=1))

links <- data.frame(
  source=my_table$Mobility_Cluster, 
  target=my_table$Demographics_Cluster, 
  value=my_table$Freq
)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


# prepare colour scale
ColourScal ='d3.scaleOrdinal() .range(["#6DCD59FF","#35B779FF","#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF"])'
links$group <- as.factor(c("type_a","type_b","type_c","type_d","type_e","type_f","type_a","type_b","type_c","type_d","type_e","type_f","type_a","type_b","type_c","type_d","type_e","type_f","type_a","type_b","type_c","type_d","type_e","type_f","type_a","type_b","type_c","type_d","type_e","type_f","type_a","type_b","type_c","type_d","type_e","type_f"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey


# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   colorScale = my_color, LinkGroup = "group", NodeGroup = "group",
                   sinksRight=FALSE)


library(networkD3)
install.packages("flipPlots")
library(flipPlots)
SankeyDiagram(my_table[,1:2], link.color = "Source", label.show.varname = FALSE, weights = my_table$Freq)
# Make the Network
alluvial(my_table[,1:2], freq=my_table$Freq)
colors <- hcl.colors(6, "Dynamic")
#install.packages("alluvial")
ggplot(data = my_table,
       aes(axis1 = Mobility_Cluster,
           axis2 = Demographics_Cluster,
           y = Freq))+
  geom_alluvium(aes(fill = Mobility_Cluster)) +
  geom_stratum(aes(fill = Mobility_Cluster)) +
  geom_text(stat = "stratum",
              aes(label = after_stat(stratum)) +
  scale_x_discrete(limits = c("Mobility_Cluster", "Demographics_Cluster"),
                   expand = c(0.15, 0.05))+
  theme_minimal()+
  scale_fill_manual(values = colors)
  

table_long <- to_lodes_form(data.frame(my_table),
          key = "Clusterings", value = "Clusters", id = "Cohort",axes = 1:2)

colors <- hcl.colors(12, "Geyser")

ggplot(data = table_long,
       aes(x = Clusterings, stratum = Clusters, alluvium = Cohort, y=Freq)) +
  geom_alluvium(aes(fill=Clusters)) +
  geom_stratum(aes(fill=Clusters),width = 0.5) +
  geom_text(stat = "stratum", aes(label = Clusters)) +
  theme_minimal() +
  ylab("Number of clusters")+
  ggtitle("Alluviam diagram of two clusterings", "stratified by mobility and demographics")+
  scale_fill_manual(values = colors)

links <- data.frame(
  source=my_table$Mobility_Cluster, 
  target=my_table$Demographics_Cluster, 
  value=my_table$Freq
)
colnames(links) <- c("source", "target", "value")
links$target <- paste(links$target, " ", sep="")
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(links$source), as.character(links$target)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

# prepare colour scale
ColourScal ='d3.scaleOrdinal(d3.schemeCategory10)'

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
