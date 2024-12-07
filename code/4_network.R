## Library to produce network graphs
library(igraph) 
library(haven)
library(foreign)
library(lubridate)
library(dplyr)
library(ggplot2)
#install.packages("leidenAlg")
library(leidenAlg)

#join_data <- readRDS("C:/Users/ecalvo/Downloads/join_data.rds")
join_data <- readRDS("./data/join_data.rds")

# Create Network
#Select the directed dyads to create the network object
data<-cbind(join_data$title_articulo_secundario,join_data$title_articulo_principal)
nrow(data)

#Open a graph
net <- graph.empty()

#Add the name of the members of Congress to the nodes.
net <- add_vertices(net, 
                    length(unique(c(data))),
                    name=as.character(unique(c(data))),
                    ID = seq_len(length(unique(c(data))))    
)

#Add the dyads.
net <- add_edges(net, t(data))

#V(net)$ID <- join_data$ID
#Add variables to the nodes and edges.
E(net)$tituloAuth <- join_data$title_articulo_principal
E(net)$tituloHub <- join_data$title_articulo_secundario
E(net)$citationAuth <- join_data$number_citation_articulo_principal
E(net)$citationHub <- join_data$number_citation_articulo_secundario
E(net)
#Add the eigenvector centrality as a variable
V(net)$eig<-eigen_centrality(net)$vector
#hist(V(net)$eig)

#Add the in-degree as a variable
V(net)$ind<- degree(net,mode="in")


#Add the out-degree as a variable
V(net)$outd<- degree(net,mode="out")

#Add color to the edges if needed
E(net)$color <- "gray"

igraph::is_igraph(net)

### Community detection, retain the top communities
#https://github.com/TomKellyGenetics/leiden
my.com.leiden <- leiden.community(net)
str(my.com.leiden)

V(net)$membership<-membership(my.com.leiden)

l <-layout_with_fr(net, grid = c("nogrid")) 
V(net)$l1<-l[,1]
V(net)$l2<-l[,2]


# Make a table of the number of tweets Most active Authorities com 1

for(i in c("2","8","9","11","12","13", "15", "16", "23", "24")){
  d <- degree(induced_subgraph(graph=net, vids=which(V(net)$membership==i)), mode="in")
  d <- as.data.frame(sort(d,decreasing = FALSE))
  d$number_community <- i
  names(d)[1]
  d_empty <- rbind(d, d_empty)
  colnames(d) <- c("Tweets")
  #print(tail(d))
  #windows()
  # png(paste0(plots_folder, "Community_", i, ".png"), width=700, height=1000)
  # 
  # # Increase the left margin (second value in `mar`)
  # par(mar = c(5, 40, 2, 2)) 
  # 
  # with(
  #   tail(d, n = 30), 
  #   barplot(Tweets, 
  #           names.arg = tail(rownames(d), n = 30), 
  #           horiz = TRUE, 
  #           las = 1, 
  #           main = paste("Community - Net ", i, sep = ""), 
  #           col = i)
  # )
  # dev.off()
}


pdf(file =paste0(plots_folder, "Membership-community-all-communities.pdf"), 20, 20, pointsize=12, compress=FALSE) 
plot.igraph(net,layout=cbind(V(net)$l1,V(net)$l2), 
            vertex.color=adjustcolor(V(net)$membership,alpha.f = 0.6 ),
            vertex.frame.color=adjustcolor(V(net)$membership,alpha.f = 0.8 ), 
            vertex.label=NA,
            vertex.size=log(V(net)$ind+1), edge.width=log(E(net)$weight+1)/3, 
            vertex.label.cex=.001, edge.curved=TRUE,
            edge.lty=3, edge.width=.3,
            edge.arrow.size = 0.03,  # Reduce this value for smaller arrows
            asp = 0)
dev.off()


for(i in c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
           "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25")){
  d <- degree(induced_subgraph(graph=net, vids=which(V(net)$membership==i)), mode="in")
  d <- as.data.frame(sort(d,decreasing = FALSE))
  colnames(d) <- c("Tweets")
  print(tail(d))
  #windows()
  png(paste0(plots_folder, "Community_", i, ".png"), width=700, height=1000)
  
  # Increase the left margin (second value in `mar`)
  par(mar = c(5, 40, 2, 2)) 
  
  with(
    tail(d, n = 30), 
    barplot(Tweets, 
            names.arg = tail(rownames(d), n = 30), 
            horiz = TRUE, 
            las = 1, 
            main = paste("Community - Net ", i, sep = ""), 
            col = i)
  )
  dev.off()
}



V(net)$color <- "#000000"
V(net)$color <- ifelse(V(net)$membership == "2", "#0072B2", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "8",  "#D55E00",V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "9", "#CC79A7", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "11", "#E69F00",V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "12", "#009E73", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "13","#F0E442"  , V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "15", "#56B4E9", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "16",   "#E41A1C", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "23",   "#7F7F7F", V(net)$color)
V(net)$color <- ifelse(V(net)$membership == "24",  "#8B008B", V(net)$color)

new_net<- induced_subgraph(graph=net, vids=which(V(net)$membership %in% c("2","8","9","11","12","13", "15", "16", "23", "24")))

l <-layout_with_fr(new_net, grid = c("nogrid") ) 
V(new_net)$l1<-l[,1]
V(new_net)$l2<-l[,2]
layout <- cbind(V(new_net)$l1, V(new_net)$l2)

target_nodes <- ends(new_net, es = E(new_net), names = FALSE)[, 2]

# Assign edge colors based on the color of the target nodes
E(new_net)$color <- V(new_net)$color[target_nodes]


legend_labels <- c(
  "Parliamentary Democracies",
  "Presidential Systems",
  "Autocracies / Gender Dynamics",
  "Survival Analysis / Foreign Ministers",
  "European Countries Analysis",
  "Coalitions and Cabinet Stability",
  "Ministerial Expertise and Popularity",
  "Post-War Politics and Factionalism",
  "Multiple Principal-Agent Models",
  "Semi-Presidential Systems"
)


legend_colors <- c(
  "#0072B2", # "Parliamentary Democracies" - Bold blue for stability and consensus.
  "#D55E00", # "Presidential Systems" - Strong orange for executive authority.
  "#CC79A7", # "Autocracies and Gender Dynamics" - Vibrant magenta for the intersection of gender and power.
  "#E69F00", # "Survival Analysis / Foreign Ministers" - Rich gold for analytical frameworks.
  "#009E73", # "European Countries" - Deep green for geographical and historical context.
  "#F0E442", # "Coalitions and Cabinet Stability" - Strong yellow for the dynamism of coalitions.
  "#56B4E9", # "Ministerial Expertise and Popularity" - Clear blue for professional competence.
  "#E41A1C", # "Post-War Politics and Factionalism" - Intense red for post-war challenges.
  "#7F7F7F", # "Multiple Principal-Agent Models" - Neutral gray for theoretical abstraction.
  "#8B008B"  # "Semi-Presidential Systems" - Dark magenta for the mix of powers.
)

rotate_layout <- function(layout, angle) {
  angle_rad <- angle * pi / 180  # Convert angle to radians
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad),
                              sin(angle_rad), cos(angle_rad)), nrow = 2)
  t(rotation_matrix %*% t(layout))
}

# Apply the rotation to the layout
rotated_layout <- rotate_layout(layout, angle = 225) 


pdf(file =paste0(plots_folder, "Membership-community-communities-subgraph-11-communities_try-LABEL.pdf"), 20, 20, pointsize=12, compress=FALSE) 
plot.igraph(new_net,layout=rotated_layout,
            vertex.color = adjustcolor(V(new_net)$color, alpha.f = 0.7),
            vertex.frame.color = "white", 
            
            #vertex.color=adjustcolor(V(new_net)$color,alpha.f = 0.6 ),
            #vertex.frame.color=adjustcolor(V(new_net)$color,alpha.f = 0.8 ), 
            vertex.label=V(new_net)$ID,
            vertex.size=log(V(new_net)$ind+1)+2, 
            edge.width=log(E(new_net)$weight+1)/3, 
            vertex.label.cex=.6, edge.curved=TRUE,
            edge.color = adjustcolor(E(new_net)$color, alpha.f = 0.5) ,  # Use the updated edge colors
            
           # edge.color = adjustcolor("gray", alpha.f = 0.5),
            edge.width = log(E(new_net)$weight + 1) / 2, 
            edge.curved = 0.2, 
            edge.arrow.size = 0.5, 
            asp = 0)

legend("topright", legend = legend_labels, 
       col = legend_colors, 
       pch = 19,  # Use solid circles
       bty = "y",  # No box around the legend
       cex = 1.8)  # Adjust the size of the legend text
dev.off()

summary(new_net)
length(V(new_net))

# Find the index of the vertex with ID 985
index <- which(V(net)$ID == 985)

# Retrieve the name of the vertex corresponding to that ID
vertex_name <- V(net)$name[index]

# Print the result
print(vertex_name)


layout <- cbind(V(new_net)$l1, V(new_net)$l2)
dim(layout)
any(is.na(V(new_net)$l1))
any(is.na(V(new_net)$l2))                           

loops.v <- which(V(new_net)$membership <= 6)  # Example condition
all(loops.v %in% seq_along(V(new_net)))  # Ensure valid indices


# Count per community:
count <- table(V(new_net)$membership)

# Percentage per community:
percentage <- round(table(V(new_net)$membership)/sum(table(V(new_net)$membership))*100,1)
#2          8          9         11         12         13         15         16         23         24 
#0.17764165 0.12098009 0.11944870 0.09800919 0.09341501 0.09188361 0.08728943 0.08422665 0.06584992 0.06125574 

table_frequency<- data.frame(count, percentage, legend_labels) %>%
  select(legend_labels, Frequency=Freq, Percentage=Freq.1)


library(xtable)

latex_table <- xtable(table_frequency)

# Initialize an empty dataframe to store results
# Ensure `d_empty` is initialized as an empty dataframe
d_empty <- data.frame(
  title_of_paper = character(),
  indegree_paper = numeric(),
  number_of_community = character(),
  stringsAsFactors = FALSE
)

# Loop over the specified community numbers
for (i in c("2", "8", "9", "11", "12", "13", "15", "16", "23", "24")) {
  # Subgraph for the current community
  subgraph <- induced_subgraph(graph = net, vids = which(V(net)$membership == i))
  
  # Calculate in-degree for each node
  d <- degree(subgraph, mode = "in")
  
  # Create a dataframe from the degree values
  d <- data.frame(indegree_paper = d, title_of_paper = names(d))
  
  # Add a column with the community number
  d$number_of_community <- i
  
  # Combine the results into the main dataframe
  d_empty <- rbind(d_empty, head(d,10))
}

# Add a column for row names
d_empty <- cbind(newColName = rownames(d_empty), d_empty)

# Reset rownames to be sequential
rownames(d_empty) <- 1:nrow(d_empty)

d_empty$name_of_community  <- NA
unique(d_empty$number_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "2", "Parliamentary Democracies", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "8", "Presidential Systems", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "9", "Autocracies / Gender Dynamics", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "11", "Survival Analysis / Foreign Ministers", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "12", "European Countries Analysis", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "13", "Coalitions and Cabinet Stability", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "15", "Ministerial Expertise and Popularity", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "16", "Post-War Politics and Factionalism", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "23", "Multiple Principal-Agent Models", d_empty$name_of_community)
d_empty$name_of_community <- ifelse(d_empty$number_of_community == "24", "Semi-Presidential Systems", d_empty$name_of_community)

d_empty$name_of_community <- factor(d_empty$name_of_community, levels = legend_labels)
d_empty <- d_empty[order(d_empty$name_of_community), ]

d_empty_<-d_empty %>%
  filter(indegree_paper != 0) %>%
  select(name_of_community, title_of_paper,indegree_paper) 
d_empty_
#write.csv(x = d_empty_, file = "~/GitHub/scraping-google-scholar/scraping-google-scholar/network/tables_community/table_n1.csv", row.names = FALSE)
