#packages
library("qgraph")
library("bootnet")
library("dplyr")
library("NetworkComparisonTest")
library("mgm")
library("RColorBrewer")
library("viridis")
library("igraph")
library(reshape2) #to melt correlation matrix
library(ggplot2)
library(reshape2) #to reshape objects in R
library(gplots)
library(igraph)
library(devtools)
library(FactoMineR)#install from github @kassambara/factoextra
library(mgm)
library(networktools)
library("ggplot2")
library("cowplot")
library("psych")
library(latex2exp) # latex in ggplot2
library("brainGraph")
library("parallel")

longnames <- c(
  "FFMQ_O","FFMQ_D","FFMQ_A","FFMQ_J","FFMQ_R","NAS","EQ","MAIA", #mindfulness
  "SCS_A","SCS_H","SCS_M","CSP","IRI_E", #compassion
  "SWLS","LOT","PHI", # psychologycal wellbeing
  "DASS_S","DASS_D","DASS_A", # psychologycal distress
  "WBSI", "RRS_B", "RRS_R", "ERQ_R", "ERQ_S", "ACS") # Emotional and cognitive control

#Networks:
network_pre_GGM <- estimateNetwork(network_pre, default="EBICglasso")
network_post_GGM <- estimateNetwork(network_post, default="EBICglasso")
mynetwork <- EBICglasso(pre_cor_matrix_spearman,nrow(pre_cor_matrix_spearman))

#Groups:
group_scales<-list('Mindfulness' = c(1,2,3,4,5,6,7,8),
                  'Compassion'= c(9,10,11,12,13),
                  'Psychologycal well-being' = c(14,15,16),
                  'Psychologycal distress' =  c(17,18,19),
                  'Emotional and cognitive regulation' = c(20,21,22,23,24,25))
grupos_titulos = list('Mindfulness','Mindfulness','Mindfulness','Mindfulness',
                      'Mindfulness','Mindfulness','Mindfulness','Mindfulness', 'Compassion','Compassion','Compassion','Compassion','Compassion' , 'Psychologycalwell-being','Psychologycalwell-being','Psychologycalwell-being',
                      'Psychologycaldistress', 'Psychologycaldistress', 'Psychologycaldistress',
                      'Emotionalandcognitiveregulation', 'Emotionalandcognitiveregulation',
                      'Emotionalandcognitiveregulation', 'Emotionalandcognitiveregulation',
                      'Emotionalandcognitiveregulation', 'Emotionalandcognitiveregulation')

df_groups <- data.frame(xmin=c(1,8.5,13.5,16.5,19.5),
       xmax=c(8.5,13.5,16.5,19.5,25),
       ymin=c(-Inf,-Inf, -Inf,-Inf, -Inf),
       ymax=c(Inf,Inf, Inf,Inf,Inf),
       names_groups=c("Mindfulness","Compassion",'Psychologycal well-being','Psychologycal distress', 'Emotional and cognitive regulation'),
       index =c(1:5))

#Visualizing

par(mfrow=c(3,2))
network_pre_GGM_plot<-plot(network_pre_GGM, layout="spring",
                           vsize=8, cut=0, border.width=1.5, border.color="black",
                           nodeNames = longnames,legend=FALSE, tuning = 0.25, aspect=T,
                           title = "PRE GGM")
network_post_GGM_plot<-plot(network_post_GGM, layout="spring",
                            vsize=8, cut=0, border.width=1.5, border.color="black",
                            nodeNames = longnames,legend=FALSE, tuning = 0.25,
                            aspect=T, title = "POST GGM")
network_pre_GGM_plot<-plot(network_pre_GGM, layout="circle", vsize=8, cut=0,
                           border.width=1.5, border.color="black", nodeNames = longnames,
                           legend=FALSE,tuning = 0.25, aspect=T, title = "PRE GGM")
network_post_GGM_plot<-plot(network_post_GGM, layout="circle", vsize=8, cut=0,
                            border.width=1.5, border.color="black",
                            nodeNames = longnames,legend=FALSE,tuning = 0.25, aspect=T,
                            title = "POST GGM")

network_pre_GGM_plot<-plot(network_pre_GGM, layout="circular", vsize=8, cut=0,
                           border.width=1.5, border.color="black",
                           nodeNames = longnames,legend=T, groups=group_scales,
                           tuning = 0.25,aspect=T, title = "PRE GGM", colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"))
network_post_GGM_plot<-plot(network_post_GGM, layout="circular", vsize=8, cut=0,
                            border.width=1.5, border.color="black",
                            nodeNames = longnames,legend=T,groups=group_scales,
                            tuning = 0.25, aspect=T, title = "POST GGM", colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"))
dev.copy(png,
         filename="network_GGM_plot.png", height=6000, width=4000, res = 300);
dev.off();

#Visualizing networks with theoretical groups:
par(mfrow=c(1,2))
network_pre_glasso <- qgraph(pre_cor_matrix_spearman, sampleSize = nrow(network_pre),graph="glasso",nodeNames = longnames,legend=T,groups=group_scales,layout = "spring", vsize = 8, colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"),legend.mode = "groups", GLratio = 1.7, title = "PRE MBSR", title.cex = 2)
network_post_glasso <- qgraph(post_cor_matrix_spearman, sampleSize = nrow(network_post),graph="glasso",nodeNames = longnames,legend=T,groups=group_scales,layout = "spring", vsize = 8, colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"), legend.mode = "groups", GLratio = 1.7, title = "POST MBSR", title.cex = 2)
dev.copy(png,
         filename="img/network_GGM_plot.png", height=2000, width=6000, res = 300);
dev.off();

## PCA

fa.parallel(network_pre)
vss(network_pre)

library("psych")
PCA_pre <- principal(cor(network_pre), nfactors = 2)
qgraph(cor(network_pre),
       layout=PCA_pre$loadings,
       groups = group_scales,
       colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"),
       title="PCA MBSR PRE",
       borders=FALSE,
       vsize=4.5, palette = 'pastel',fade=T,
       rotation="promax",vTrans=210,
       posCol = ("#457B9D"),negCol=("#E63946"),
       minimum=0.1, curveAll=T)
dev.copy(png,
         filename="img/PCA_pre.png", height=3000, width=3000, res = 200);
dev.off();

library("psych")
PCA_pre <- principal(cor(network_post), nfactors = 2)
qgraph(cor(network_post),
       layout=PCA_pre$loadings,
       groups = group_scales,
       title="PCA MBSR PRE",
       borders=FALSE,
       colors = c("#eee361", "#6fb3e4", "#469b77", "#d16d6f", "#bbb2d4"),
       vsize=4.5, palette = 'pastel',fade=T,
       rotation="promax",vTrans=210,
       posCol = ("#457B9D"),negCol=("#E63946"),
       minimum=0.1, curveAll=T)
dev.copy(png,
         filename="img/PCA_post.png", height=3000, width=3000, res = 200);
dev.off();

# Comparison of Networks
#We use networkcomparisontest using @VanBorkulo2018
mbsr_comparison <- NCT(network_post, network_pre, it = 1000,
  binary.data = FALSE, paired = TRUE)
# Ploting the results
par(mfrow=c(1,2))
plot(mbsr_comparison, what="network")
plot(mbsr_comparison, what="strength")

#We can create a object of adjacency Matrix:

network_pre_adjacencyMatrix <- network_pre_GGM$graph
network_post_adjacencyMatrix <- network_post_GGM$graph

## Heat plot for pre:


heatmap.2(network_pre_adjacencyMatrix,
        main="Network PRE Adjacency Matrix",
        trace="none",
        col = viridis(24),
        scale="none")


## Heat plot for post:

heatmap.2(network_post_adjacencyMatrix,
        main="Network POST Adjacency Matrix",
        trace="none",
        col = viridis(24),
        scale="none")

# Centrality analysis

#calculating binary matrix to compute degree
network_pre_bin <- as.matrix((network_pre_adjacencyMatrix > 0) + 0)
centRes_pre_bin <- centrality(network_pre_bin)
centRes_pre <- centrality(network_pre_GGM) # to compute strength
#To compute local efficiency
network_pre_qgraph <- qgraph(pre_cor_matrix_spearman,
  sampleSize = nrow(network_pre), graph = "glasso")
network_post_qgraph <- qgraph(post_cor_matrix_spearman,
  sampleSize = nrow(network_pre), graph = "glasso")
network_pre_igraph <- as.igraph(network_pre_qgraph)
network_post_igraph <- as.igraph(network_post_qgraph)

local.eff <- function(g) {
  if ('degree' %in% vertex_attr_names(g)) {
    degs <- V(g)$degree
  } else {
    degs <- degree(g)
  }

  eff <- numeric(length(degs))
  nodes <- which(degs > 1)

  eff[nodes] <- simplify2array(mclapply(nodes, function(x) {
    neighbs <- neighbors(g, v=x)
    g.sub <- induced.subgraph(g, neighbs)
    Nv <- vcount(g.sub)

    paths <- shortest.paths(g.sub, weights=NA)
    paths <- paths[upper.tri(paths)]
    2 / Nv / (Nv - 1) * sum(1 / paths[paths != 0])
    }, mc.cores=detectCores())
  )
eff
}

efficiency_pre <- local.eff(network_pre_igraph)
efficiency_post <- local.eff(network_post_igraph)
#Computing clustering
clustering_pre <- transitivity(network_pre_igraph, type = "local")
clustering_post <- transitivity(network_post_igraph, type = "local")
##Ploting with dimensions:
df_centrality <- data.frame(centRes_pre$InExpectedInfluence,
                            centRes_pre$InDegree,
                            centRes_pre_bin$InDegree,
                            clustering_pre,
                            efficiency_pre,
                            centRes_post$InExpectedInfluence,
                            centRes_post$InDegree,
                            centRes_post_bin$InDegree,
                            clustering_post,
                            efficiency_post)
df_centrality$Group <- grupos_titulos
df_centrality$Item <- longnames
df_centrality$index <- (1:25)

df_centrality_long<- reshape(data=df_centrality, idvar="Item", varying = c("centRes_pre.InExpectedInfluence", "centRes_pre.InDegree",
                                     "centRes_pre_bin.InDegree",
                                     "clustering_pre",
                                     "efficiency_pre",
                                     "centRes_post.InExpectedInfluence",
                                     "centRes_post.InDegree",
                                     "centRes_post_bin.InDegree",
                                     "clustering_post",
                                     "efficiency_post"), times = c("centRes_pre.InExpectedInfluence", "centRes_pre.InDegree",
                                     "centRes_pre_bin.InDegree",
                                     "clustering_pre",
                                     "efficiency_pre",
                                     "centRes_post.InExpectedInfluence",
                                     "centRes_post.InDegree",
                                     "centRes_post_bin.InDegree",
                                     "clustering_post",
                                     "efficiency_post"),
                         v.name=c("value"),
                         new.row.names = 1:10000,
                         direction="long")
df_centrality_long$state <- rep(c("PRE", "POST"), 1, each=125)
df_centrality_long$central_parameter <- rep(c("ExpectedInfluence", "Strength", "Degree", "Clustering", "Efficiency"), 2, each=25)
df_centrality_long$central_parameter_f <- factor (df_centrality_long$central_parameter, levels = c("ExpectedInfluence", "Strength", "Degree", "Clustering", "Efficiency"))
df_centrality_long$index_state <- rep(c(1:125), 2)
df_centrality_long$state <- as.factor(df_centrality_long$state)
df_centrality_long$central_parameter <- as.factor(df_centrality_long$central_parameter)

p1 <- ggplot(df_centrality_long, aes(x=index))
p1 <- p1 + geom_line(aes(y = value, color = state)) +geom_point(aes(y = value, color = state))
p1 <- p1 + ylab("") +xlab("") +  labs(color = "", fill= "Groups")
p1 <- p1 + facet_grid(central_parameter_f ~ ., scales="free")
p1 <- p1 + geom_rect(data=df_groups,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=names_groups),
        alpha=0.3,inherit.aes=FALSE)+
    scale_fill_manual(breaks = c("Mindfulness","Compassion",
"Psychologycal well-being", "Psychologycal distress",
"Emotional and cognitive regulation"),
values=c("#6fb3e4", "#bbb2d4", "#eee361", "#d16d6f","#469b77"),
labels = c("Mindfulness","Compassion",
"Psychologycal well-being", "Psychologycal distress",
"Emotional and cognitive regulation"))
p1 <- p1 + scale_x_continuous(breaks = 1:25,label = longnames, limits = c(1,25))
p1 <- p1 + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
p1
dev.copy(png,
         filename="img/comparison_centrality_coefficients.png", height=3000, width=5000, res = 300);
dev.off();

# Edge-weight accuracy
# PRE
pre_boot <- bootnet(network_pre_GGM, nBoots = 500,  nCores = 8)
plot(pre_boot, labels = T, order = "sample")
dev.copy(png,
         filename="img/PRE_bootstraping_accuracy.png", height=8000, width=3000, res = 200);
dev.off();
plot(pre_boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

dev.copy(png,
         filename="img/PRE_bootstraping_difference.png", height=5000, width=6000, res = 200);
dev.off();
plot(pre_boot, "strength")
dev.copy(png,
         filename="img/PRE_bootstraping_strength.png", height=5000, width=6000, res = 200);
dev.off();
# POST
post_boot <- bootnet(network_post_GGM, nBoots = 500,  nCores = 8)
plot(post_boot, labels = TRUE, order = "sample")
dev.copy(png,
         filename="img/POST_bootstraping_accuracy.png", height=8000, width=3000, res = 200);
dev.off();
plot(post_boot, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.copy(png,
         filename="img/POST_bootstraping_difference.png", height=5000, width=6000, res = 200);
dev.off();
plot(post_boot, "strength")
dev.copy(png,
         filename="img/POST_bootstraping_strength.png", height=5000, width=6000, res = 200);
dev.off();

#Centrality Stability
pre_boot2<-bootnet(network_pre_GGM, default="EBICglasso",
                  statistics = c("strength","closeness","betweenness"),
                  nBoots = 1000,type="case",nCores = 8)
post_boot2<-bootnet(network_post_GGM, default="EBICglasso",
                    statistics = c("strength","closeness","betweenness"),
                    nBoots = 1000,type="case",nCores = 8)


#PRE
plot(pre_boot2, statistics = c("strength","closeness","betweenness" ))
dev.copy(png,filename="img/PRE_centrality_stability.png",
        height=1500, width=3000, res = 300);
dev.off();

corStability(pre_boot2)
#POST
plot(post_boot2, statistics = c("strength","closeness","betweenness" ))
dev.copy(png,filename="img/POST_centrality_stability.png",
        height=1500, width=3000, res = 300);
dev.off();

corStability(post_boot2)


# Community detection

## Igraph | Spinglass community

g_pre = as.igraph(network_pre_GGM_plot, attributes=TRUE)
set.seed(2018)
sgc_pre <- spinglass.community(g_pre)
sgc_pre$membership
group_spinglass_pre<-list(c(2,3,12,13,25), c(4,9,10,11), c(14,15,16,17,18), c(19,20,22,23,24) , c(1,5,6,7,8,21))

network_pre_GGM_plot<-plot(network_pre_GGM, layout="spring", cut=0,
                           border.width=1.5, border.color="black",
                           sampleSize = nrow(network_pre), groups=group_spinglass_pre,
                           nodeNames = longnames,
                           title="Comunities in MBS_PRE (Spinglass Algorithm)",
                           vsize=7,  aspect=F, maximum=.45, legend=T)

g_post = as.igraph(network_post_GGM_plot, attributes=TRUE)
set.seed(2018)
sgc_post <- spinglass.community(g_post)
sgc_post$membership

group_spinglass_post<-list(c(14,15,16,17,18), c(12,13), c(1,5,7,8,19), c(2,3,22,24,25) , c(4,6,9,10,11,20,21,23))

network_post_GGM_plot<-plot(network_post_GGM, layout="spring",
                            cut=0, border.width=1.5, border.color="black",
                            sampleSize = nrow(network_post), groups=group_spinglass_post,
                            nodeNames = longnames,
                            title="Comunities in MBS_POST (Spinglass Algorithm)",
                            vsize=7,  aspect=F, maximum=.45, legend=T)


# Computing Predictability
#PRE
pre_obj <- mgm(data = network_pre,
               type = rep('g', 25),
               level = rep(1,25),
               lambdaSel = 'CV',
               ruleReg = 'OR')

pred_pre_obj <- predict(object = pre_obj,
                    data = network_pre,
                    errorCon = 'R2')

tabla_prediccion_pre <- data.frame(pred_pre_obj$error)
write.csv(tabla_prediccion_pre, "tabla_prediccion_pre.csv")

mean(pred_pre_obj$errors$R2)


qgraph(pre_obj$pairwise$wadj, # weighted adjacency matrix as input
       layout = 'spring',
       pie = pred_pre_obj$error[,2], # provide errors as input
       pieColor = rep('#377EB8',25),
       edge.color = pre_obj$pairwise$edgecolor,
       labels = longnames,
       title = "PRE Predictability")
dev.copy(png,
         filename="img/PRE_network_node_predictability.png", height=2000, width=3000, res = 300);
dev.off();


set.seed(1)
ind <- sample(c(TRUE,FALSE), prob=c(.6, .4), size=nrow(network_pre), replace=T)


set.seed(1)
fit_obj_ts <- mgm(data = network_pre[ind,],
               type = rep('g', 25),
               level = rep(1, 25),
               lambdaSel = 'CV',
               ruleReg = 'OR')

# Compute Preditions on training data 60%
pred_obj_train <- predict(object = fit_obj_ts,
                          data = network_pre[ind,],
                          errorCon = 'R2')

# Compute Predictions on test data 40%
pred_obj_test <- predict(object = fit_obj_ts,
                          data = network_pre[!ind,],
                          errorCon = 'R2')


mean(pred_obj_train$error[,2])
mean(pred_obj_test$error[,2])


cor(pred_obj_train$error[,2], pred_obj_test$error[,2])

#POST
set.seed(666)
post_obj <- mgm(data = network_post,
               type = rep('g', 25),
               level = rep(1,25),
               lambdaSel = 'CV',
               ruleReg = 'OR')

pred_post_obj <- predict(object = post_obj,
                    data = network_post,
                    errorCon = 'R2')

pred_post_obj$error

tabla_prediccion_post <- data.frame(pred_post_obj$error)
write.csv(tabla_prediccion_post, "tabla_prediccion_post.csv")


mean(pred_post_obj$errors$R2)


qgraph(post_obj$pairwise$wadj, # weighted adjacency matrix as input
       layout = 'spring',
       pie = pred_post_obj$error[,2], # provide errors as input
       pieColor = rep('#377EB8',25),
       edge.color = post_obj$pairwise$edgecolor,
       labels = longnames,
       title = "POST Predictability")
dev.copy(png,
         filename="img/POST_network_node_predictability.png", height=2000, width=3000, res = 300);
dev.off();


set.seed(1)
ind <- sample(c(TRUE,FALSE), prob=c(.6, .4), size=nrow(network_post), replace=T)


set.seed(1)
fit_obj_ts <- mgm(data = network_post[ind,],
               type = rep('g', 25),
               level = rep(1, 25),
               lambdaSel = 'CV',
               ruleReg = 'OR')

# Compute Preditions on training data 60%
pred_obj_train <- predict(object = fit_obj_ts,
                          data = network_post[ind,],
                          errorCon = 'R2')

# Compute Predictions on test data 40%
pred_obj_test <- predict(object = fit_obj_ts,
                          data = network_post[!ind,],
                          errorCon = 'R2')



mean(pred_obj_train$error[,2])
mean(pred_obj_test$error[,2])




cor(pred_obj_train$error[,2], pred_obj_test$error[,2])

## Computing graph with node predictability


tabla_prediccion <- tabla_prediccion_pre
tabla_prediccion$R2_POST<- tabla_prediccion_post$R2
tabla_prediccion$index<-(1:25)

#Ploting all the information of predictability of the network together

ggplot(tabla_prediccion, aes(x=index))+
  geom_line(aes(y=R2, color="PRE"))+
  geom_line(aes(y=R2_POST, color="POST"))+
  geom_point(aes(y=R2, color="PRE"))+
  geom_point(aes(y=R2_POST, color="POST"))+
  xlab("") + ylab(TeX("$R^2$")) + ggtitle("Node Predictability")+ labs(colour="")+
  scale_x_continuous(breaks = 1:25,label = longnames)+
  theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.copy(png,
         filename="img/node_predictability.png", height=2000, width=4000, res = 300);
dev.off();
