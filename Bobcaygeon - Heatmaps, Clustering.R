library(gplots)
library(readxl)
library(RColorBrewer)
library(dendextend)
library(heatmap.plus)
library('psych')
library('sqldf')
library('readxl')


commonloon <- read_excel('C:\\Users\\coconnor\\MMA\\831 - Marketing\\Pivoted Sales by Product.xlsx')
northernlights <- read_excel('C:\\Users\\coconnor\\MMA\\831 - Marketing\\Pivoted Sales by Product.xlsx',2)
dockside <- read_excel('C:\\Users\\coconnor\\MMA\\831 - Marketing\\Pivoted Sales by Product.xlsx',3)

dockside2 <- read_excel('C:\\Users\\coconnor\\MMA\\831 - Marketing\\Pivoted Sales by Product.xlsx',4)
commonloon2 <- read_excel('C:\\Users\\coconnor\\MMA\\831 - Marketing\\Pivoted Sales by Product.xlsx',5)


## data cleaning
commonloon[is.na(commonloon)] <- 0
northernlights[is.na(northernlights)] <- 0
dockside[is.na(dockside)] <- 0
dockside2[is.na(dockside2)] <- 0
commonloon2[is.na(commonloon2)] <- 0

cl_rownames <- commonloon$Location
commonloon$Location <- NULL
cl_matrix <- as.matrix(commonloon)
rownames(cl_matrix)<-cl_rownames

nl_rownames <-northernlights$Location
northernlights$Location <- NULL
nl_matrix <- as.matrix(northernlights)
rownames(nl_matrix) <- nl_rownames

d_rownames <- dockside$Location
dockside$Location <- NULL
d_matrix <- as.matrix(dockside)
rownames(d_matrix) <- d_rownames

d2_rownames <- dockside2$Location
dockside2$Location <- NULL
d2_matrix <- as.matrix(dockside2)
rownames(d2_matrix) <- d2_rownames

cl2_rownames <- commonloon2$Location
commonloon2$Location <- NULL
cl2_matrix <- as.matrix(commonloon2)
rownames(cl2_matrix) <- cl2_rownames

##colour pallettes
regioncolours_cl <- unlist(lapply(rownames(cl_matrix),function(x){
  if(grepl("Central",x)) '#0000CC' #blue
  else if(grepl('Eastern',x)) '#00CC00' #green
  else if(grepl('Western',x)) '#FFFF00' #yellow
  else if(grepl('Northern',x)) '#FF0000' ##red
  else '330000' ##black  
}))

regioncolours_nl <- unlist(lapply(rownames(nl_matrix),function(x){
  if(grepl("Central",x)) '#0000CC' #blue
  else if(grepl('Eastern',x)) '#00CC00' #green
  else if(grepl('Western',x)) '#FFFF00' #yellow
  else if(grepl('Northern',x)) '#FF0000' ##red
  else '330000' ##black  
}))

regioncolours_d <- unlist(lapply(rownames(d_matrix),function(x){
  if(grepl("Central",x)) '#0000CC' #blue
  else if(grepl('Eastern',x)) '#00CC00' #green
  else if(grepl('Western',x)) '#FFFF00' #yellow
  else if(grepl('Northern',x)) '#FF0000' ##red
  else '330000' ##black  
}))


rankcolours_cl <- unlist(lapply(rownames(cl_matrix),function(x){
  if(grepl("R_AAA",x)) '#004529' 
  else if(grepl('R_AA',x)) '#006837' 
  else if(grepl('R_A',x)) '#41ab5d' 
  else if(grepl('R_B',x)) '#addd8e' 
  else if(grepl('R_C',x)) '#f7fcb9'
  else '#ffffe5'   
}))

rankcolours_nl <- unlist(lapply(rownames(nl_matrix),function(x){
  if(grepl("R_AAA",x)) '#004529' 
  else if(grepl('R_AA',x)) '#006837' 
  else if(grepl('R_A',x)) '#41ab5d' 
  else if(grepl('R_B',x)) '#addd8e' 
  else if(grepl('R_C',x)) '#f7fcb9'
  else '#ffffe5'      
}))

rankcolours_d <- unlist(lapply(rownames(d_matrix),function(x){
  if(grepl("R_AAA",x)) '#004529' 
  else if(grepl('R_AA',x)) '#006837' 
  else if(grepl('R_A',x)) '#41ab5d' 
  else if(grepl('R_B',x)) '#addd8e' 
  else if(grepl('R_C',x)) '#f7fcb9'
  else '#ffffe5'  
}))

cl_col <- cbind(regioncolours_cl, rankcolours_cl)
nl_col <- cbind(regioncolours_nl, rankcolours_nl)
d_col <- cbind(regioncolours_d, rankcolours_d)

colnames(cl_col)[1] <- 'Region'
colnames(cl_col)[2] <- 'Rank'

colnames(nl_col)[1] <- 'Region'
colnames(nl_col)[2] <- 'Rank'

colnames(nl_col)[1] <- 'Region'
colnames(nl_col)[2] <- 'Rank'


#weekly sales heatmaps
cl_heatmap <- heatmap.2(cl_matrix, trace = 'none', col = brewer.pal(9, 'Blues'), dendrogram = 'row', Colv = FALSE, scale = 'row', key = TRUE, main = "Common Loon - American Pale Ale", RowSideColors = regioncolours_cl)
nl_heatmap <- heatmap.2(nl_matrix, trace = 'none', col = brewer.pal(9, 'Greens'), dendrogram = 'row', Colv = FALSE, scale = 'row', key = TRUE, main = 'Northern Lights - IPA', RowSideColors = regioncolours_nl)
d_heatmap <- heatmap.2(d_matrix, trace = 'none', col = brewer.pal(9, 'Reds'), dendrogram = 'row', scale = 'row', Colv = FALSE, key = TRUE, main = 'Dockside - Red Ale', RowSideColors = regioncolours_d)

##legend("topleft", legend = regions, col = c('blue', 'green', 'red', 'yellow'))


##correl heatmaps
cl_heatmap_correl <-heatmap.2(cor(t(cl_matrix)), trace ='none', col = bluered(20), hclust=function(x) hclust(x,method="complete"),distfun=function(x) as.dist((1-cor(t(x)))), main = 'Common Loon APA, Pearson Distance', ColSideColors = regioncolours_cl, RowSideColors = rankcolours_cl)
nl_heatmap_correl <-heatmap.2(cor(t(nl_matrix)), trace ='none', col = bluered(20), hclust=function(x) hclust(x,method="complete"),distfun=function(x) as.dist((1-cor(t(x)))), main = 'Northern Lights IPA, Pearson Distance', ColSideColors = regioncolours_nl, RowSideColors = rankcolours_nl)
d_heatmap_correl <-heatmap.2(cor(t(d_matrix)), trace ='none', col = bluered(20), hclust=function(x) hclust(x,method="complete"),distfun=function(x) as.dist((1-cor(t(x)))), main = 'Dockside Red Ale, Pearson Distance', ColSideColors = regioncolours_d, RowSideColors = rankcolours_d)

##legend(0,0.9, legend = c('Central', 'Eastern', 'Western', 'Northern'), fill = c('#0000CC', '#00CC00', '#FFFF00', '#FF0000'), cex = 0.6)
##legend(0.5,0.5, legend = c('AAA', 'AA', 'A', 'B', 'C', 'D'), fill = c('#004529', '#006837', '#41ab5d', '#addd8e', '#f7fcb9', '#ffffe5'), cex = 0.6)


##hierarchical clustering by product
cl <- cor(t(cl_matrix), method = 'pearson')
cld <- as.dist(1-cl)
hclustcl <- hclust(cld, method = 'complete')
cutcl <- cutree(hclustcl, k = 5)
table(cutcl)

  
nl <- cor(t(nl_matrix), method = 'pearson')
nld <- as.dist(1-nl)
hclustnl <- hclust(nld, method ='complete')
cutnl <- cutree(hclustnl, k = 5)
table(cutnl)


ds <- cor(t(d_matrix), method = 'pearson')
dsd <- as.dist(1-ds)
hclustds <- hclust(dsd, method = 'complete')
cutd <- cutree(hclustds, k = 5)
table(cutd)

ds2 <- cor(t(d2_matrix), method = 'pearson')
ds2d <- as.dist(1-ds2)
hclustds2 <-hclust(ds2d, method = 'complete')
cutd2 <- cutree(hclustds2, k = 5)
table(cutd2 )

cl2 <- cor(t(cl2_matrix), method = 'pearson')
cl2d <- as.dist(1-cl2)
hclustcl2 <- hclust(cl2d, method = 'complete')
cutcl2 <- cutree(hclustcl2, k =5)
table(cutcl2)



dendcl <- as.dendrogram(hclustcl)
dendnl <- as.dendrogram(hclustnl)
dendd <- as.dendrogram(hclustds)
dendd2 <- as.dendrogram(hclustds2)
dendcl2 <- as.dendrogram(hclustcl2)

tanglegram(dendcl2, dendd2, highlight_distinct_edges = FALSE, common_subtrees_color_lines = FALSE, common_subtrees_color_branches = FALSE, main = 'Tanglegram - Complete Linkage', sub =  paste('Entanglement = ', round(entanglement(dendcl2,dendd2,1),2)), main_left = 'Common Loon', main_right = 'Dockside', lwd = 2.5)
entanglement(dendcl2, dendd2, 1)


##circlize_dendrogram(dendcl)
## see https://mran.microsoft.com/snapshot/2015-08-08/web/packages/dendextend/vignettes/introduction.html

##all.equal(dendcl2, dendd2)


##Scree Plots

wss_nl <- (nrow(nl)-1)*sum(apply(nl, 2, var))
for (i in 2:20) wss_nl[i] <- sum(kmeans(nl, centers = i)$withinss)
plot(1:20, wss_nl, type = 'b', xlab = 'Number of Clusters', ylab =  'Within Group SS', main = 'Northern Lights Pearson Distance')

wss_cl <- (nrow(cl)-1)*sum(apply(cl, 2, var))
for (i in 2:20) wss_cl[i] <- sum(kmeans(cl, centers = i)$withinss)
plot(1:20, wss_cl, type = 'b', xlab = 'Number of Clusters', ylab =  'Within Group SS', main = 'Common Loon Pearson Distance')

wss_ds <- (nrow(ds)-1)*sum(apply(ds, 2, var))
for (i in 2:20) wss_ds[i] <- sum(kmeans(ds, centers = i)$withinss)
plot(1:20, wss_ds, type = 'b', xlab = 'Number of Clusters', ylab =  'Within Group SS', main = 'Dockside Pearson Distance')



out_cl <- cbind(cl_matrix, cutcl)
out_nl <- cbind(nl_matrix, cutnl)
out_d <- cbind(d_matrix, cutd)

out_cl <- as.data.frame(out_cl)
out_nl <- as.data.frame(out_nl)
out_d <- as.data.frame(out_d)

out_cl <- subset(out_cl, select = cutcl)
out_nl <- subset(out_nl, select = cutnl)
out_d <- subset(out_d, select = cutd)

write.csv(out_cl, file = 'CL Clust.csv')
write.csv(out_nl, file = 'NL Clust.csv')
write.csv(out_d, file = 'D Clust.csv')

# creating the bi-plot

df_3 <- read.csv("PCA3.csv") #This is the same table as Figure 3c within Appendix 3, it is the averaged data for each feature, within each cluster
df_3$Cluster <- NULL
pc.cr <- princomp(df_3, cor = TRUE)
summary(pc.cr)
biplot(pc.cr, main = "Cluster Positioning Map",xlim=c(-1, 1))
