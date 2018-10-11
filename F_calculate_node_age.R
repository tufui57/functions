#################################################################################
### Calculate node ages
#################################################################################

calculateNodeAge <- function(tree){
  ### Distance between all combinations of tips
  distances <- dist.nodes(tree)
  
  # Calculate species ages
  ages <- sapply(nodes[[1]], function(i){
    # Phylogenetic distance list has 0 (distance to themselves)
    (distances[,i] > 0) %>% distances[., i] %>% min
  }
  )
  
  ages <- as_tibble(ages) %>% mutate(., spname = row.names(nodes))
  
  # Calculate internal node ages
  nodeage <- cbind(rep(NA, length(branching.times(tree))), branching.times(tree))
  colnames(nodeage) <- c("spname", "value")
  
  # Combine tip and internal node ages
  agesTipAndNode <- rbind(ages, nodeage)
  
  return(agesTipAndNode)

}


#################################################################################
### Function to get branch length for the clade pair's closest ancestor
#################################################################################

get.ancestorNodeLength <- function(tree, # tree object 
                                   node # node number of one of the clades/species
){
  
  # Get phylogentic tree data
  tree2 <- extractTree(tree)
  # Get node number of the ancestor
  ancNode <- ancestor(tree2, node)
  # Get branch length
  dis <- dist.nodes(tree)
  ancNodeAge <- dis[min(getDescendants(tree, ancNode)), ancNode]
  
  return(ancNodeAge)
}
