'-Name: Ke Er Ang - 
-Student ID: 32581343 -'

rm(list = ls())

library(NLP)
library(tm)
library(SnowballC)
library(igraph)

'---------------Question 1 & 2---------------'
# Set working directory 
setwd("/Users/chloeang/Desktop/FIT3152")
# Point to the correct folder in the working directory
cname = file.path(".", "Asgn3")
# Create a Corpus object
docs = Corpus(DirSource(cname)) 
# Inspect the Corpus object 
summary(docs)

writeLines(as.character(docs[[1]]))
writeLines(as.character(docs[[2]]))
writeLines(as.character(docs[[3]]))

'---------------Question 3---------------'
# Preprocess the corpus
# Tokenization
# Remove numbers
docs = tm_map(docs,removeNumbers)
# Remove punctuation
docs = tm_map(docs, removePunctuation)
# Convert text to lowercase
docs = tm_map(docs, content_transformer(tolower))
# Remove common stopwords
docs = tm_map(docs, removeWords, stopwords("english"))

# Create a transformer function to replace specific patterns
# Remove dash lines
toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs = tm_map(docs,toSpace, "–")
docs = tm_map(docs,toSpace, "—")
# Remove non-printable characters
removeNonPrintable = content_transformer(function(x) gsub("\\p{C}", "", x, perl = TRUE))
docs = tm_map(docs, removeNonPrintable)

# Remove those frequently used but low-value words
docs = tm_map(docs,toSpace, "also")
docs = tm_map(docs,toSpace, "may")
docs = tm_map(docs,toSpace, "since")
docs = tm_map(docs,toSpace, "will")
docs = tm_map(docs,toSpace, "just")
docs = tm_map(docs,toSpace, "can")
docs = tm_map(docs,toSpace, "like")

# Remove words that are common conversation but not directly related to the any of the selected topics
docs = tm_map(docs,toSpace, "say")
docs = tm_map(docs,toSpace, "said")
docs = tm_map(docs,toSpace, "around")
docs = tm_map(docs,toSpace, "back")
docs = tm_map(docs,toSpace, "make")
docs = tm_map(docs,toSpace, "made")
docs = tm_map(docs,toSpace, "told")
docs = tm_map(docs,toSpace, "year")
docs = tm_map(docs,toSpace, "take")
docs = tm_map(docs,toSpace, "become")
docs = tm_map(docs,toSpace, "increase")
docs = tm_map(docs,toSpace, "show")
docs = tm_map(docs,toSpace, "see")
docs = tm_map(docs,toSpace, "know")
docs = tm_map(docs,toSpace, "remain")
docs = tm_map(docs,toSpace, "get")

# Remove too generic words that could fit in any topics
docs = tm_map(docs,toSpace, "one")
docs = tm_map(docs,toSpace, "two")
docs = tm_map(docs,toSpace, "three")
docs = tm_map(docs,toSpace, "per")
docs = tm_map(docs,toSpace, "cent")

# Stemming documents
docs = tm_map(docs,stemDocument, language = "english")
# Remove white space
docs = tm_map(docs, stripWhitespace)

# Create a Document-Term Matrix
dtm = DocumentTermMatrix(docs)
inspect(dtm)

# Remove sparse terms
dtm_reduced = removeSparseTerms(dtm, sparse = 0.75)
inspect(dtm_reduced)

# Check and insepct word frequencies
freq = colSums(as.matrix(dtm_reduced))
length(freq)
sort(freq, decreasing = T)

ord = order(freq)
sort(ord)

# Inspect lowest and highest frequency 
freq[head(ord)]
freq[tail(ord)]

# Check frequency of frequencies 
head(table(freq), 10)
tail(table(freq), 10)

# Check dimension of dtm
dim(dtm_reduced)

# Identify key terms
findFreqTerms(dtm_reduced)

# Save dtm as csv 
dtms = as.matrix(dtm_reduced)
write.csv(dtms, "dtm_A3.csv")

'---------------Question 4---------------'

# Calculate cosine distance
distMatrix = proxy:: dist(as.matrix(dtms), method="cosine")

# Perform hierarchical clustering
fit = hclust(distMatrix, method = "ward.D")

# Plot the hierarchical clustering
plot(fit, hang=-1, main = "Abstracts Consine Distance")

# Create topic vector
topics = c("business","business","business","business","business","business","business",
           "crime","crime","crime","crime","crime","crime","crime",
           "politics","politics","politics","politics","politics","politics","politics")

# Divide the dendrogram into 3 clusters
cosin_groups = cutree(fit, k = 3)

# make a table of topic labels vs cluster numbers
cosin_cluster_table = table(GroupNames = topics, Clusters = cosin_groups) 
print(cosin_cluster_table)

# Calculate accuracy
accuracy = (6+3+6)/(6+1+2+2+3+1+6)
accuracy


'---------------Question 5---------------'
# Create networks by asbtract
dtmsx = as.matrix(dtms)
# transform into a binary matrix
dtmsx = as.matrix((dtmsx > 0)+0)
# multiply binary matrix by its transpose to find shared term
ByAbsMatrix = dtmsx%*%t(dtmsx)
# make leading diagonal zero to remove self-loops
diag(ByAbsMatrix)= 0
# create graph object
ByAbs = graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)
# plot the graph
plot(ByAbs,vertex.label.cex=0.6)

# calc average path length
a = average.path.length(ByAbs)
# calc graph density
g = graph.density(ByAbs)
# calc transitivity
t = transitivity(ByAbs)
# calc betweenness
b = format(betweenness(ByAbs),digits=2)
# calc closeness centrality
closeness.centrality.abs = closeness(ByAbs,normalized = TRUE)
c = format(closeness.centrality.abs,digits = 2)
# calc degreeness
d = degree(ByAbs)
# calc Eigencentrality
e = format(evcent(ByAbs)$vector, digits =2)

# combine centrality measures into a dataframe
Asum = as.data.frame(cbind(c,b,d,e))
# rename the data frame
colnames(Asum) = c("closeness","betweenness", "degree","Eigencentrality")


# Graph Improvement
# Edge Weight 
E(ByAbs)$width = E(ByAbs)$weight / max(E(ByAbs)$weight) * 5

# Calculate communities using the Louvain method
communities = cluster_louvain(ByAbs)

# Define the custom color palette
custom_colors = c("#8ecae6", "#ffb703", "#f07167")
# Assign colour to node based on communities membership
node_colors = custom_colors[communities$membership]

# Plot the graph
plot(communities, ByAbs ,
     vertex.color = communities$membership,
     edge.width = E(ByAbs)$width,
     vertex.size = closeness.centrality.abs * 50,
     vertex.label.cex = 0.6,
     col = node_colors,
     mark.border = NA,
     mark.col = adjustcolor(brewer.pal(n = 3, name = "Pastel2"),0.7),
     edge.color = adjustcolor("gray", alpha.f = 0.8),
     main = "Single-Mode Graph by Abstracts")



'---------------Question 6---------------'
# Create networks by token
# Token matrix
dtmsx.token = as.matrix(dtms)
# transform into a binary matrix
dtmsx.token = as.matrix((dtmsx.token > 0)+0)
# multiply binary matrix by its transpose to find co-occurrence
ByTokenMatrix = t(dtmsx.token)%*% dtmsx.token
# make leading diagonal zero to remove self-loops
diag(ByTokenMatrix)= 0
# create graph object
ByToken = graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)
# plot the graph
plot(ByToken, vertex.label.cex = 0.6)

# calculate average path length
a = average.path.length(ByToken)
# calculate graph density
g = graph.density(ByToken)
# calculate transitivity
t = transitivity(ByToken)

# calculate betweenness
b = format(betweenness(ByToken),digits=2)

# calculate closeness centrality
closeness.centrality.token = closeness(ByToken,normalized = TRUE)
c = format(closeness.centrality.token, digits = 2)

# calculate  degreeness
d = degree(ByToken)

# calculate Eigencentrality
e = format(evcent(ByToken)$vector, digits =2)

# combine all centrality measures as a data frame
Tsum = as.data.frame(cbind(c,b,d,e))
# rename the data frame
colnames(Tsum) = c("closeness","betweenness","degree","Eigencentrality")
# View(Tsum)

# Graph Improvement
# adjust the width of edges based on their weights
E(ByToken)$width = E(ByToken)$weight / max(E(ByToken)$weight) * 5

# Find communities using the Louvain method
communities.token = cluster_louvain(ByToken)

# Define the custom color palette
custom_colors = c("#8ecae6","#f07167" ,"#ffb703" )
# assign colour based on their community membership
node_colors = custom_colors[communities.token$membership]

# plot the graph
plot(communities.token, ByToken ,
     edge.width = E(ByToken)$width,
     vertex.size = closeness.centrality.token  * 50,
     vertex.label.cex = 0.6,
     col = node_colors,
     mark.border = NA,
     mark.col = adjustcolor(brewer.pal(n = 3, name = "Pastel2"),alpha =0.7),
     edge.color = adjustcolor("gray", alpha.f = 0.8),
     main = "Single-Mode Graph by Token")

'---------------Question 7---------------'
# Create bipartite network
# convert dtm into a data frame
dtmsa = as.data.frame(dtms)

# assign row names of dtm into a new column 'ABS' in dtmsa
dtmsa$ABS = rownames(dtm)
# create an empty data frame
dtmsb = data.frame()

# A loop that create a data frame, with each row representing a term-document combination.
# The colums include the document's identifier, the term's frequency in the document and the term itself.
for(i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
    touse = cbind(dtmsa[i,j],dtmsa[i,ncol(dtmsa)],colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb,touse)
  }
}
# rename columns, weight = frequency, abs = document, token = the term itself 
colnames(dtmsb) = c("weight","abs","token")
# remove  entries where the term frequency is zero
dtmsc = dtmsb[dtmsb$weight != 0,]
# rearrange columns
dtmsc = dtmsc[,c(2,3,1)]

# create graph data frame
g =  graph.data.frame(d = dtmsc[,c("abs","token", "weight")], directed=FALSE)
# assign node types for bipartite structure 
V(g)$type = bipartite_mapping(g)$type

# Colour the nodes
# Add a new column which categorize each row into a group based on the keywords found
dtmsc$group = ifelse(grepl("business", dtmsc $abs), "business",
                      ifelse(grepl("crime", dtmsc $abs), "crime",
                             ifelse(grepl("politics", dtmsc $abs), "politics", NA)))
# Create a unique pair of abstract and their corresponding group to ensure each abstract is listed only once
abs_to_group = unique(dtmsc[, c("abs", "group")])
# Assign groups to the nodes
V(g)$group = abs_to_group$group[match(V(g)$name, abs_to_group$abs)]
# Set colors for different groups
group_colors = c("business" = "skyblue", "crime" = "#f07167", "politics" = "#ffb703")
# Assign colour to the nodes based on their group
V(g)$color = group_colors[V(g)$group]

# calculate closeness centrality
closeness.centrality.bipartite = closeness(g,normalized = TRUE)

# set node shape based on type
V(g)$shape = ifelse(V(g)$type, "circle", "square")
# set node size based on closeness centrality
V(g)$size = closeness.centrality.bipartite * 0.2
# set edges colours
E(g)$color = "lightgray"
# set edges width based on weight
E(g)$width = 0.3* as.numeric(E(g)$weight)

# plot the graph
plot(g, 
     vertex.size=15, 
     vertex.label.cex=0.5,
     edge.curved=0.1, 
     main="Bipartite Graph Visualization")












