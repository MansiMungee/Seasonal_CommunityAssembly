################ Function 01 - generating bird community data frames ###############
bird_taxo_div <- function(inputfile){
  #MAX OF TRANSECT A & B
  birddata <- read.csv(file = inputfile,header=TRUE)
  #summing multiple observations for the same species recorded multiple times on the same date and within the same transect
  birddata$Counts <- as.numeric(birddata$Counts)
  birddata <- aggregate(birddata$Counts,by=list(birddata$Elevation,birddata$Set,birddata$Transect,birddata$Date,birddata$ScientificName),FUN = sum)
  colnames(birddata) <- c("Elevation","Set","Transect","Date","Species","Counts")
  
  #Generating the community data frame for 50m resolution - Sum of transects A & B
  birddata_Sum <- birddata
  birddata_Sum <- aggregate(birddata_Sum$Counts,by=list(birddata_Sum$Elevation,birddata_Sum$Set,birddata_Sum$Date,birddata_Sum$Species),FUN = sum)
  colnames(birddata_Sum) <- c("Elevation","Set","Date","Species","Counts")
  jnk1 <- birddata_Sum$Elevation
  jnk2 <- birddata_Sum$Species
  jnk3 <- birddata_Sum$Counts
  jnk3 <- as.numeric(jnk3)
  jnk4 <- rep(jnk2,jnk3)
  jnk5 <- rep(jnk1,jnk3)
  jnk6 <- data.frame(jnk4,jnk5)
  jnk7 <- table(jnk6$jnk4,jnk6$jnk5)
  jnk7 <- t(jnk7)
  birddata_Sum <- as.data.frame.matrix(jnk7)
  birddata_50 <- birddata_Sum
  jnk1 <- colnames(birddata_50)
  jnk1_split <- data.frame(do.call('rbind',strsplit(as.character(jnk1),' ',fixed=TRUE)))
  colnames(birddata_50) <- paste(jnk1_split$X2, jnk1_split$X3, sep = "_")
  
  #Generating the community data frame for 200 m resolution - Sum of 4 transects
  n = 4
  jnk1 <- birddata_50[1,]
  birddata_200 <- birddata_50[-1,]
  birddata_200 <- aggregate(birddata_200,list(rep(1:(nrow(birddata_200)%/%n+1),each=n,len=nrow(birddata_200))),sum)[-1]
  rownames(birddata_200) <- c("E0575","E0775","E0975","E1175","E1375","E1575","E1775","E1975","E2175","E2375","E2575","E2775")
  birddata_200 <- rbind(jnk1,birddata_200)
  rownames(birddata_200) <- c("E0200","E0575","E0775","E0975","E1175","E1375","E1575","E1775","E1975","E2175","E2375","E2575","E2775")
  birddata <- list(as.data.frame.matrix(birddata_50),as.data.frame.matrix(birddata_200))
  return(birddata)
}

################ Function 02 - generating bird trait data frames ###############
bird_trait <- function(x){
  trait_df_birds_245 <- read.csv(x, header = TRUE) 
  jnk1 <- trait_df_birds_245[,1]
  trait_df_birds_245 <- trait_df_birds_245[,-1]
  rownames(trait_df_birds_245) <- gsub(" ","_",jnk1)
  return(trait_df_birds_245)
}
proc_bird_traits_for_allometry <- function(x){
  allometric_Adjustment <- function(x){
    # RESIDUAL beak
    beak <- x$BLength * x$BWidth * x$BDepth
    y1 <- log10(beak)
    x1 <- log10(x$Mass)
    linear_fit1 <- lm(y1 ~ x1)
    slope_fit <- summary(linear_fit1)$coefficients[2,1]
    inter_fit <- summary(linear_fit1)$coefficients[1,1]
    residual_beak <- y1 - inter_fit - slope_fit * x1
    
    # Resdiual Tarsus
    y1 <- log10(x$Tarsus)
    x1 <- log10(x$Mass)
    linear_fit1 <- lm(y1 ~ x1)
    slope_fit <- summary(linear_fit1)$coefficients[2,1]
    inter_fit <- summary(linear_fit1)$coefficients[1,1]
    residual_tarsus <- y1 - inter_fit - slope_fit * x1
    
    # Resdiual wing
    y1 <- log10(x$Wing)
    x1 <- log10(x$Mass)
    linear_fit1 <- lm(y1 ~ x1)
    slope_fit <- summary(linear_fit1)$coefficients[2,1]
    inter_fit <- summary(linear_fit1)$coefficients[1,1]
    residual_wing <- y1 - inter_fit - slope_fit * x1
    
    df <- data.frame(Residual_Wing = residual_wing, Residual_Beak = residual_beak, Residual_Tarsus = residual_tarsus, Mass = log10(x$Mass))
    rownames(df) <- rownames(x)
    return(df)
  }
  jnk1 <- allometric_Adjustment(x)
  jnk2 <- x[,c(7:10)]
  jnk3 <- merge(jnk1,jnk2, by = "row.names")
  rownames(jnk3) <- jnk3$Row.names
  jnk3 <- jnk3[,-1]
  return(jnk3)
  
}

################ Function 03 - generating bird phylogenetic tree ###############
bird_tree_summer <- function(x){
  # BIRD TREE GENERATION FROM SCHUMM ET AL DATA
  # read the schumm tree; Matching done in file named "SchummTree_NameRevision.csv
  # specis that directly match = 187 (Schumm_MM_DirectMatch)
  # species that match upon name revision = 187 + 42 = 229; names were revised for thse 42 species (Schumm_OrigNames + ChangeNameInSchumm)
  # species finally missing = 16 (FInal_Extra_in_MM)
  # I downloaded 100 trees from birdtree.org : Hackett All Species: a set of 10000 trees with 9993 OTUs - containing 16 missing species + 5 random "common" species between schumm & MM : 
  # Buceros bicornis, Yuhina occipitalis, Sitta formosa, Garrulax albogularis, Dendrocopos cathpharius
  # tHIS NEW TREE IS SAVED AS Jetz_tree.nex
  # Read the Schumm et al tree
  bird_tree_schumm <- read.tree("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummetalTree.tree")
  
  # Read the CSV File with Matching names, etc
  tree_tip_file <- read.csv("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummTree_NameRevision.csv", header = TRUE)
  keep_tip <- c(as.character(tree_tip_file$Schumm_MM_DirectMatch), as.character(tree_tip_file$Schumm_OrigNames))#Tips that match between schumm and our data
  keep_tip <- data.frame(keep_tip)
  keep_tip <- keep_tip[!apply(keep_tip == "", 1, all),]#deleting empty rows
  #keep_tip <- droplevels(keep_tip)
  keep_tip <- as.character(keep_tip)
  tree_tips_schumm <- bird_tree_schumm$tip.label
  length(tree_tips_schumm)
  length(keep_tip)
  remove_tips <- setdiff(tree_tips_schumm, keep_tip) #removing all extra tips from the tree
  
  #SCHUMM TREE - TRUNCATED
  schumm_tree_trunc <- ape::drop.tip(phy = bird_tree_schumm, tip = remove_tips) #this is the truncated tree with 229 common species btw schumm and our data
  branch_length_schumm <- compute.brlen(schumm_tree_trunc)$edge.length #computing branch lengths
  rel_br_len_schumm <- branch_length_schumm/sum(branch_length_schumm) #converting it to relative brnach lengths
  #In an object of class phylo: 
  # tree$edge = a two column matrix where each row is an edge whose length is in the same position in the tree$edge.length vector
  # the first column gives the parent node and the second gives the child node
  # for example in schumm_tree_trunc: the branch length of 230 to 231 = 2.587221
  #ASSIGNING RELATIVE BRANCH LENGTHS TO SCHUMM FOR COMPARISON WITH JETZ
  schumm_tree_trunc_rel <- schumm_tree_trunc
  schumm_tree_trunc_rel$edge.length <- rel_br_len_schumm
  sp_distances_schumm <- cophenetic(schumm_tree_trunc_rel)
  node_distances_schumm <- dist.nodes(schumm_tree_trunc_rel)
  
  #JETZ TREE 
  jetz_tree <- read.nexus("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/Jetz_tree.nex")#Reading the downloaded jetz tree: which is a multiphylo object - so you sample one tree from it
  jetz_tree <- sample(jetz_tree, size = 1)[[1]]
  branch_length_jetz <- compute.brlen(jetz_tree)$edge.length #computing branch lengths
  rel_br_len_jetz <- branch_length_jetz/sum(branch_length_jetz) #converting it to relative brnach lengths
  #ASSIGNING RELATIVE BRANCH LENGTHS TO SCHUMM FOR COMPARISON WITH JETZ
  jetz_tree_rel <- jetz_tree
  jetz_tree_rel$edge.length <- rel_br_len_jetz
  sp_distances_jetz <- cophenetic(jetz_tree_rel)
  node_distances_jetz <- dist.nodes(jetz_tree_rel)
  
  #Now plot both tree and examine the nodes where the missing species are placed
  # jpeg("Jetz_tree.jpg",height = 16, width = 16, unit = "cm", res = 300)
  # plot(jetz_tree_rel, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  # nodelabels(text = seq(1,244), col = "blue", cex = 0.3, bg = "white")
  # dev.off()
  # 
  # jpeg("Schumm_tree_rel_branchlength.jpg",height = 16, width = 16, unit = "cm", res = 300)
  # plot(schumm_tree_trunc_rel, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  # nodelabels(text = seq(1,226), col = "blue", cex = 0.3, bg = "white")
  # dev.off()
  # 
  # #ALL SPECIES TO ADD TO SCHUMM BASED ON THEIR PLACEMENT IN JETZ
  # cophenetic(jetz_tree_rel)
  # dist.nodes(jetz_tree_rel)
  # jetz_tree_rel$edge
  # schumm_tree_trunc_rel$edge
  # 
  sp_to_add <- as.character(tree_tip_file$Final_Extra_in_MM[1:16])
  #these are all the species to be added:
  #"Accipiter_trivirgatus"     "Dendrocopos_hyperythrus"   "Gallus_gallus"             "Lophura_leucomelanos"      "Microhierax_melanoleucos"  "Nisaetus_nipalensis"       "Polyplectron_bicalcaratum"
  #"Spilornis_cheela"          "Tragopan_blythii"          "Tragopan_temminckii"       "Arborophila_mandellii"     "Arborophila_rufogularis"   "Arborophila_torqueola"     "Ictinaetus_malaiensis"    
  #"Pernis_ptilorhynchus"      "Sibia_waldeni"
  
  ##8 of the 16 species will go to node # 2
  #"Tragopan_blythii", "Tragopan_temminckii","Arborophila_mandellii","Arborophila_rufogularis","Arborophila_torqueola""Lophura_leucomelanos" "Polyplectron_bicalcaratum""Gallus_gallus"
  # 4 of these 16 will go to node # 227
  #Accipiter_trivirgatus", "Nisaetus_nipalensis", "Ictinaetus_malaiensis", "Spilornis_cheela", #"Pernis_ptilorhynchus"    
  #Dendrocopos hyperythrus will go to its genus at node # 42
  #"Microhierax_melanoleucos" will go near aethopyga nipalensis at node #  181      
  #"Sibia_waldeni"  will go to its genus actinudura egertoni at node # 130
  
  
  
  require(phangorn)
  #adding at node 2
  tree1 <- add.tips(schumm_tree_trunc_rel, sp_to_add[3], c(2))
  tree1 <- add.tips(tree1, sp_to_add[4], c(2))
  tree1 <- add.tips(tree1, sp_to_add[7], c(2))
  tree1 <- add.tips(tree1, sp_to_add[9], c(2))
  tree1 <- add.tips(tree1, sp_to_add[10], c(2))
  tree1 <- add.tips(tree1, sp_to_add[11], c(2))
  tree1 <- add.tips(tree1, sp_to_add[12], c(2))
  tree1 <- add.tips(tree1, sp_to_add[13], c(2))
  #adding at node 227
  tree1 <- add.tips(tree1, sp_to_add[1], c(227))
  tree1 <- add.tips(tree1, sp_to_add[6], c(227))
  tree1 <- add.tips(tree1, sp_to_add[8], c(227))
  tree1 <- add.tips(tree1, sp_to_add[14], c(227))
  tree1 <- add.tips(tree1, sp_to_add[15], c(227))
  #adding at node # 41
  tree1 <- add.tips(tree1, sp_to_add[2], c(41))
  #adding at node #197
  tree1 <- add.tips(tree1, sp_to_add[5], c(197))
  #adding at node #130
  tree1 <- add.tips(tree1, sp_to_add[16], c(119))
  
  
  
  jnk1 <- tree1$Nnode
  #where to bind the answer liew in tree$edge matrix
  # jpeg("tree1",height = 16, width = 16, unit = "cm", res = 300)
  # plot(tree1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  # nodelabels(text = seq(1,jnk1), col = "blue", cex = 0.3, bg = "white")
  # dev.off()
  # 
  
  #final tree is now called tree1
  bird_tree_final <- tree1
  
  #YOu have manually added all missing species by matching their relative placement in the jetz global tree; keeping the relative brnach length (rooted) constant) and your
  jnk1 <- bird_tree_final$tip.label
  jnk2 <- tdf_summer$Name
  jnk3 <- colnames(tdf_summer)
  jnk4 <- read.csv("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummTree_NameRevision.csv", header = TRUE)
  
  jnk5 <- jnk4$Schumm_OrigNames
  jnk5 <- as.character(jnk5)
  jnk5 <- jnk5[1:42]
  
  jnk6 <- jnk4$ChangeNameInSchumm
  jnk6 <- as.character(jnk6)
  jnk6 <- jnk6[1:42]
  
  for(ii in 1:length(jnk1)){
    for(jj in 1:length(jnk6)){
      if(jnk1[ii] == jnk5[jj]){
        jnk1[ii] <- jnk6[jj]
      } else{
        jnk1[ii] <- jnk1[ii]
      }
    }
  }
  for(ii in 1:length(jnk1)){
    if(jnk1[ii] == "Microhierax_melanoleucus"){
      jnk1[ii] <- "Microhierax_melanoleucos"
    } else{jnk1[ii] <- jnk1[ii]}
  }
  bird_tree_final$tip.label <- jnk1
  bird_tree_245 <- bird_tree_final
  
  
  return(bird_tree_245)
  
  
}
bird_tree_winter <- function(x){
  # BIRD TREE GENERATION FROM SCHUMM ET AL DATA
  # read the schumm tree; Matching done in file named "SchummTree_NameRevision.csv
  bird_tree_schumm <- read.tree("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummetalTree.tree")
  
  # Read the CSV File with Matching names, etc
  tree_tip_file <- read.csv("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummTree_NameRevision_WInter.csv", header = TRUE)
  keep_tip <- c(as.character(tree_tip_file$Schumm_MM_DirectMatch), as.character(tree_tip_file$Schumm_OrigNames))#Tips that match between schumm and our data
  keep_tip <- data.frame(keep_tip)
  keep_tip <- keep_tip[!apply(keep_tip == "", 1, all),]#deleting empty rows
  #keep_tip <- droplevels(keep_tip)
  keep_tip <- as.character(keep_tip)
  tree_tips_schumm <- bird_tree_schumm$tip.label
  length(tree_tips_schumm)
  length(keep_tip)
  remove_tips <- setdiff(tree_tips_schumm, keep_tip) #removing all extra tips from the tree
  
  #SCHUMM TREE - TRUNCATED
  schumm_tree_trunc <- ape::drop.tip(phy = bird_tree_schumm, tip = remove_tips) #this is the truncated tree with 229 common species btw schumm and our data
  branch_length_schumm <- compute.brlen(schumm_tree_trunc)$edge.length #computing branch lengths
  rel_br_len_schumm <- branch_length_schumm/sum(branch_length_schumm) #converting it to relative brnach lengths
  #In an object of class phylo: 
  # tree$edge = a two column matrix where each row is an edge whose length is in the same position in the tree$edge.length vector
  # the first column gives the parent node and the second gives the child node
  # for example in schumm_tree_trunc: the branch length of 230 to 231 = 2.587221
  #ASSIGNING RELATIVE BRANCH LENGTHS TO SCHUMM FOR COMPARISON WITH JETZ
  schumm_tree_trunc_rel <- schumm_tree_trunc
  schumm_tree_trunc_rel$edge.length <- rel_br_len_schumm
  sp_distances_schumm <- cophenetic(schumm_tree_trunc_rel)
  node_distances_schumm <- dist.nodes(schumm_tree_trunc_rel)
  
  jpeg("Schumm_tree_rel_branchlength.jpg",height = 24, width = 12, unit = "cm", res = 300)
  plot(schumm_tree_trunc_rel, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.3, no.margin = TRUE)
  nodelabels(text = seq(1,192), col = "blue", cex = 0.35, bg = "white")
  dev.off()
  
  
  
  #JETZ TREE 
  jetz_tree <- read.nexus("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/Jetz_tree_winter.nex")#Reading the downloaded jetz tree: which is a multiphylo object - so you sample one tree from it
  jetz_tree <- sample(jetz_tree, size = 1)[[1]]
  branch_length_jetz <- compute.brlen(jetz_tree)$edge.length #computing branch lengths
  rel_br_len_jetz <- branch_length_jetz/sum(branch_length_jetz) #converting it to relative brnach lengths
  #ASSIGNING RELATIVE BRANCH LENGTHS TO SCHUMM FOR COMPARISON WITH JETZ
  jetz_tree_rel <- jetz_tree
  jetz_tree_rel$edge.length <- rel_br_len_jetz
  sp_distances_jetz <- cophenetic(jetz_tree_rel)
  node_distances_jetz <- dist.nodes(jetz_tree_rel)
  
  #Now plot both tree and examine the nodes where the missing species are placed
  jpeg("Jetz_tree.jpg",height = 16, width = 16, unit = "cm", res = 300)
  plot(jetz_tree_rel, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  nodelabels(text = seq(1,182), col = "blue", cex = 0.3, bg = "white")
  dev.off()
  
  
  
  
  sp_to_add <- as.character(tree_tip_file$Final_Extra_in_MM[1:22])
  # these are all the species to be added:
  # Accipiter_nisus, Accipiter_trivirgatus, Accipiter_virgatus, 
  # Buteo_refectus, Dinopium_javanese, Ficedula_albicilla,  Hypothymis_azurea, Lophotriorchis_kienerii, 
  # Mixornis_gularis, Nisaetus_nipalensis, Pernis_ptilorhynchus, , Pteruthius_aeralatus, Sibia_waldeni, Spilornis_cheela, Spinus_thibetanus
  # now add at specific nodes by looking at the jetz tree
  # adding all galliformes to node 1
  # # all galliformes: 
  # Arborophila_mandellii
  # Arborophila_rufogularis
  # Arborophila_torqueola
  # Gallus_gallus
  # Lophura_leucomelanos
  # Polyplectron_bicalcaratum
  # Tragopan_blythii
  
  tree1 <- add.tips(schumm_tree_trunc_rel, sp_to_add[4], c(1))
  tree1 <- add.tips(tree1, sp_to_add[5], c(1))
  tree1 <- add.tips(tree1, sp_to_add[6], c(1))
  tree1 <- add.tips(tree1, sp_to_add[10], c(1))
  tree1 <- add.tips(tree1, sp_to_add[13], c(1))
  tree1 <- add.tips(tree1, sp_to_add[17], c(1))
  tree1 <- add.tips(tree1, sp_to_add[22], c(1))
  jpeg("Schumm_tree_rel_branchlength.jpg",height = 24, width = 12, unit = "cm", res = 300)
  plot(tree1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.3, no.margin = TRUE)
  nodelabels(text = seq(1,199), col = "blue", cex = 0.35, bg = "white")
  dev.off()
  
  # # adding all raptors to node 2
  # all raptors:
  # Accipiter_nisus
  # Accipiter_trivirgatus
  # Accipiter_virgatus
  # Buteo_refectus
  # Lophotriorchis_kienerii
  # Nisaetus_nipalensis
  # Pernis_ptilorhynchus
  # Spilornis_cheela
  tree1 <- add.tips(tree1, sp_to_add[1], c(2))
  tree1 <- add.tips(tree1, sp_to_add[2], c(2))
  tree1 <- add.tips(tree1, sp_to_add[3], c(2))
  tree1 <- add.tips(tree1, sp_to_add[7], c(2))
  tree1 <- add.tips(tree1, sp_to_add[12], c(2))
  tree1 <- add.tips(tree1, sp_to_add[15], c(2))
  tree1 <- add.tips(tree1, sp_to_add[16], c(2))
  tree1 <- add.tips(tree1, sp_to_add[20], c(2))
  
  jpeg("Schumm_tree_rel_branchlength.jpg",height = 24, width = 12, unit = "cm", res = 300)
  plot(tree1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  nodelabels(text = seq(1,212), col = "blue", cex = 0.2, bg = "white")
  dev.off()
  
  
  
  
  
  # adding at node #197
  tree1 <- add.tips(tree1, sp_to_add[9], c(45)) # ficedula with other ficedula
  tree1 <- add.tips(tree1, sp_to_add[19], c(99)) # Sibia_waldeni with leiothrix
  tree1 <- add.tips(tree1, sp_to_add[8], c(22)) # Dinopium_javanese with dendroscopus
  tree1 <- add.tips(tree1, sp_to_add[14], c(120)) # Mixornis_gularis with Pomatorhinus
  tree1 <- add.tips(tree1, sp_to_add[18], c(172)) # Pteruthius_aeralatus with Pteruthius
  tree1 <- add.tips(tree1, sp_to_add[21], c(158)) # Spinus_thibetanus with Pyrrhula
  
  
  
  
  
  jnk1 <- tree1$Nnode
  #where to bind the answer liew in tree$edge matrix
  # jpeg("tree1",height = 16, width = 16, unit = "cm", res = 300)
  # plot(tree1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  # nodelabels(text = seq(1,jnk1), col = "blue", cex = 0.3, bg = "white")
  # dev.off()
  # 
  
  #final tree is now called tree1
  bird_tree_final <- tree1
  
  #YOu have manually added all missing species by matching their relative placement in the jetz global tree; keeping the relative brnach length (rooted) constant) and your
  jnk1 <- bird_tree_final$tip.label
  jnk1 <- jnk1[complete.cases(jnk1)]
  jnk2 <- colnames(cdf_winter)
  jnk3 <- rownames(tdf_winter)
  jnk4 <- read.csv("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummTree_NameRevision_WInter.csv", header = TRUE)
  
  jnk5 <- jnk4$Schumm_OrigNames
  jnk5 <- as.character(jnk5)
  jnk5 <- jnk5[1:51]
  
  jnk6 <- jnk4$ChangeNameInSchumm
  jnk6 <- as.character(jnk6)
  jnk6 <- jnk6[1:51]
  
  for(ii in 1:length(jnk1)){
    for(jj in 1:length(jnk6)){
      if(jnk1[ii] == jnk5[jj]){
        jnk1[ii] <- jnk6[jj]
      } else{
        jnk1[ii] <- jnk1[ii]
      }
    }
  }
  for(ii in 1:length(jnk1)){
    if(jnk1[ii] == "Microhierax_melanoleucus"){
      jnk1[ii] <- "Microhierax_melanoleucos"
    } else{jnk1[ii] <- jnk1[ii]}
  }
  bird_tree_final$tip.label <- jnk1
  
  # jnk1 <- bird_tree_final$tip.label
  # jnk2 <- colnames(cdf_winter)
  # jnk3 <- setdiff(jnk1,jnk2)  
  
  #bird_tree <- drop.tip(bird_tree_final, jnk3)
  return(bird_tree_final)
  
  
}

############################################ Processing ########################
cdf_summer <- bird_taxo_div(inputfile = "~/Documents/04_MyPublications/00_MasterDataFiles/Birds/BirdData_Raw50m_Summer.csv")[[1]]
cdf_winter <- bird_taxo_div(inputfile = "~/Documents/04_MyPublications/00_MasterDataFiles/Birds/BirdData_Raw50m_Winter.csv")[[1]]

tdf_summer <- bird_trait(x = "~/Documents/04_MyPublications/00_MasterDataFiles/Birds/BirdTraits_Summer.csv")
tdf_summer <- proc_bird_traits_for_allometry(x = tdf_summer)
tdf_winter <- bird_trait(x = "~/Documents/04_MyPublications/00_MasterDataFiles/Birds/BirdTraits_Winter.csv")
tdf_winter <- proc_bird_traits_for_allometry(x = tdf_winter)

pdf_summer <- bird_tree_summer(x)
pdf_winter <- bird_tree_winter(x)



#### FOMATTING AND MATCHING ORDER OF NAMES ACROSS DFS FOR FUMMER ####################
# matching names
jnk1 <- read.csv("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/Schumm_TraitNameRevision.csv", header = TRUE)
for(ii in 1:nrow(tdf_summer)){
  for(jj in 1:nrow(jnk1)){
    if(rownames(tdf_summer)[ii] == jnk1$OrigNamesInSchumm[jj]){
      rownames(tdf_summer)[ii] <- jnk1$RevisedNames[jj]
    }
  }
}

# Re-dordering data
cdf_summer <- cdf_summer[,order(colnames(cdf_summer))]
tdf_summer <- tdf_summer[order(rownames(tdf_summer)),]
tdf_summer <- ReorderData(tree = pdf_summer, data = tdf_summer, taxa.names = "row names")
# Scale data
tdf_summer <- tdf_summer %>% mutate_at(c(1:4), funs(c(scale(.))))
# make sure things are in proper class
tdf_summer$Residual_Wing <- as.numeric(tdf_summer$Residual_Wing)
tdf_summer$Residual_Beak <- as.numeric(tdf_summer$Residual_Beak)
tdf_summer$Residual_Tarsus <- as.numeric(tdf_summer$Residual_Tarsus)
tdf_summer$Mass <- as.numeric(tdf_summer$Mass)
tdf_summer$PySub <- as.factor(tdf_summer$PySub)
tdf_summer$ForMode <- as.factor(tdf_summer$ForMode)
tdf_summer$Diet <- as.factor(tdf_summer$Diet)
tdf_summer$Habitat <- as.factor(tdf_summer$Habitat)
tdf_summer <- as.data.frame.matrix(tdf_summer)
# calculate dist mats for use downstream
dist_tdf_summer <- as.matrix(gowdis(tdf_summer, ord = "podani"))
dist_pdf_summer <- cophenetic.phylo(pdf_summer)
# Pagels lambda and phylogenetic signals
pagel_morpho_summer <- geiger::fitContinuous(phy = pdf_summer, dat = tdf_summer[,c(1:4)], model= "lambda")
pagel_morph_summer_bm <- geiger::fitContinuous(phy = pdf_summer, dat = tdf_summer[,c(1:4)], model = "BM") 
pagel_ecol_summer <- geiger::fitDiscrete(phy = pdf_summer, dat = tdf_summer[,c(5:8)], transform = "lambda")
pagel_ecol_summer_bm <- geiger::fitDiscrete(phy = pdf_summer, dat = tdf_summer[,c(5:8)], transform = "none")

### calculating p values associated with the log likelihood ratios - with geiger this needs to be done manually
for(ii in 1:length(pagel_morpho_summer)){
  trait.pagel <- pagel_morpho_summer[[ii]]$opt$lnL
  trait.bm <- pagel_morph_summer_bm[[ii]]$opt$lnL
  trait.stat <- 2 * trait.pagel - trait.bm
  print(pchisq(trait.stat, 1, lower.tail = F))
}
for(ii in 1:length(pagel_ecol_summer)){
  trait.pagel <- pagel_ecol_summer[[ii]]$opt$lnL
  trait.bm <- pagel_ecol_summer_bm[[ii]]$opt$lnL
  trait.stat <- 2 * trait.pagel - trait.bm
  print(pchisq(trait.stat, 1, lower.tail = F))
}

# checking
setdiff(colnames(cdf_summer), rownames(tdf_summer))
setdiff(colnames(cdf_summer), pdf_summer$tip.label)

#### FOMATTING AND MATCHING ORDER OF NAMES ACROSS DFS FOR WINTER ####################
# checking
setdiff(colnames(cdf_winter), rownames(tdf_winter))
jnk1 <- setdiff(pdf_winter$tip.label, colnames(cdf_winter))
jnk2 <- setdiff(colnames(cdf_winter), pdf_winter$tip.label)

cdf_winter <- cdf_winter[,!colnames(cdf_winter) %in% jnk1 ]
tdf_winter <- tdf_winter[!rownames(tdf_winter) %in% jnk1, ]
pdf_winter <- drop.tip(pdf_winter, jnk1)

cdf_winter <- cdf_winter[,!colnames(cdf_winter) %in% jnk2 ]
tdf_winter <- tdf_winter[!rownames(tdf_winter) %in% jnk2, ]

# Re-dordering data
cdf_winter <- cdf_winter[,order(colnames(cdf_winter))]
tdf_winter <- tdf_winter[order(rownames(tdf_winter)),]
tdf_winter <- ReorderData(tree = pdf_winter, data = tdf_winter, taxa.names = "row names")
# Scale data
tdf_winter <- tdf_winter %>% mutate_at(c(1:4), funs(c(scale(.))))
# make sure things are in proper class
tdf_winter$Residual_Wing <- as.numeric(tdf_winter$Residual_Wing)
tdf_winter$Residual_Beak <- as.numeric(tdf_winter$Residual_Beak)
tdf_winter$Residual_Tarsus <- as.numeric(tdf_winter$Residual_Tarsus)
tdf_winter$Mass <- as.numeric(tdf_winter$Mass)
tdf_winter$PySub <- as.factor(tdf_winter$PySub)
tdf_winter$ForMode <- as.factor(tdf_winter$ForMode)
tdf_winter$Diet <- as.factor(tdf_winter$Diet)
tdf_winter$Habitat <- as.factor(tdf_winter$Habitat)
tdf_winter <- as.data.frame.matrix(tdf_winter)
# calculate dist mats for use downstream
dist_tdf_winter <- as.matrix(gowdis(tdf_winter, ord = "podani"))
dist_pdf_winter <- cophenetic.phylo(pdf_winter)
# Pagels lambda and phylogenetic signals
pagel_morpho_winter <- geiger::fitContinuous(phy = pdf_winter, dat = tdf_winter[,c(1:4)], model= "lambda")
pagel_morph_winter_bm <- geiger::fitContinuous(phy = pdf_winter, dat = tdf_winter[,c(1:4)], model = "BM") 
pagel_ecol_winter <- geiger::fitDiscrete(phy = pdf_winter, dat = tdf_winter[,c(5:8)], transform = "lambda")
pagel_ecol_winter_bm <- geiger::fitDiscrete(phy = pdf_winter, dat = tdf_winter[,c(5:8)], transform = "none")

### calculating p values associated with the log likelihood ratios - with geiger this needs to be done manually
for(ii in 1:length(pagel_morpho_winter)){
  trait.pagel <- pagel_morpho_winter[[ii]]$opt$lnL
  trait.bm <- pagel_morph_winter_bm[[ii]]$opt$lnL
  trait.stat <- 2 * trait.pagel - trait.bm
  print(pchisq(trait.stat, 1, lower.tail = F))
}
for(ii in 1:length(pagel_ecol_winter)){
  trait.pagel <- pagel_ecol_winter[[ii]]$opt$lnL
  trait.bm <- pagel_ecol_winter_bm[[ii]]$opt$lnL
  trait.stat <- 2 * trait.pagel - trait.bm
  print(pchisq(trait.stat, 1, lower.tail = F))
}

############# creating a super tree - combined phylogenetic tree for summer and winter ##############
func_to_generate_cdf_annual <- function(x){
  jnk1 <- t(cdf_summer)
  jnk2 <- t(cdf_winter)
  jnk1 <- as.data.frame.matrix(jnk1)
  jnk2 <- as.data.frame.matrix(jnk2)
  jnk1$Species <- rownames(jnk1)
  jnk2$Species <- rownames(jnk2)
  jnk1 <- jnk1[,-1]
  
  library(dplyr)
  
  merged_df <- bind_rows(jnk1, jnk2) %>%
    group_by(Species) %>% 
    summarise_all(sum, na.rm = T)
  
  merged_df <- as.data.frame(merged_df)
  rownames(merged_df) <- merged_df$Species
  merged_df <- merged_df[,-1]
  return(merged_df)

  
  }
func_to_generate_tdf_annual <- function(x){
  
}   
  
annual_tree <- function(x){
  
  birds_annual <- c(colnames(cdf_summer), colnames(cdf_winter))
  birds_annual <- unique(birds_annual)
  bird_tree_schumm <- read.tree("~/Documents/04_MyPublications/00_MasterDataFiles/Birds/SchummetalTree.tree")
  n_orig <- length(bird_tree_schumm$tip.label)
  ## match again - manually
  n1 = length(bird_tree_schumm$tip.label)
  n2 <- length(birds_annual)
  n3 <- n1 - n2
  jnk1 <- data.frame(Oberved = c(birds_annual, rep("NA", n3)), Schumm_Tree = bird_tree_schumm$tip.label)
  write.csv(jnk1, "Matching_Annual_Species_List.csv") # done a whole lot of manual matching (Details here - "./../../../00_MasterDataFiles/Birds/Tree_Match_Summer_Winter_Combined_Master.ods")
  # read the manually edited file
  tips <- read.csv("./../../../00_MasterDataFiles/Birds/Tree_Match_Summer_Winter_Combined.csv", header = TRUE)
  # prune/change the schumm tree
  remove_tips <- tips$Step2_DeleteTips[complete.cases(tips$Step2_DeleteTips)]
  n_remove <- length(remove_tips)
  schumm_tree_trunc1 <- ape::drop.tip(phy = bird_tree_schumm, tip = remove_tips) #this is the truncated tree with 229 common species btw schumm and our data
  n_orig - n_remove
  length(schumm_tree_trunc1$tip.label) # checks out
  change_tipnames <- tips[,1:2]
  change_tipnames <- change_tipnames[complete.cases(change_tipnames$Step1_ChangeNames),]
  for(ii in 1:length(schumm_tree_trunc1$tip.label)){
    for(jj in 1:nrow(change_tipnames)){
      if(schumm_tree_trunc1$tip.label[ii] == change_tipnames$For_all_in_Schumm[jj]){
        schumm_tree_trunc1$tip.label[ii] <- change_tipnames$Step1_ChangeNames[jj]
      } else{
        schumm_tree_trunc1$tip.label[ii] <- schumm_tree_trunc1$tip.label[ii]
      }
    }
  }
  
  length(schumm_tree_trunc1$tip.label)
  
  
  ## final mismatches
  mismatches1 <- setdiff(birds_annual, schumm_tree_trunc1$tip.label)
  mismatches2 <- setdiff(schumm_tree_trunc1$tip.label, birds_annual)
  
  #### 21 missing species - 
  # repeat all analyses after dropping these species
  # store them
  species_drop_repeat_analysis <- mismatches1
  
  ### study the jetz tree for relative placement
  jetz_tree <- ape::read.nexus("./../../../00_MasterDataFiles/Birds/Summer_Winter_Combined_Jetz.nex")
  tiff("jetz.tiff", width = 12, height = 24, unit = "cm", res = 300)
  plot(jetz_tree$tree_2601, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  jnk1 <- jetz_tree$tree_2601$Nnode
  nodelabels(text = seq(1,jnk1), col = "blue", cex = 0.3, bg = "white")
  dev.off()
  
  tiff("schumm.tiff", width = 12, height = 24, unit = "cm", res = 300)
  plot(schumm_tree_trunc1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  jnk1 <- schumm_tree_trunc1$Nnode
  nodelabels(text = seq(1,jnk1), col = "blue", cex = 0.3, bg = "white")
  dev.off()
  
  
  sp_to_add <- mismatches1
  #these are all the species to be added:
  #[1] "Accipiter_trivirgatus"     "Arborophila_mandellii"     "Arborophila_rufogularis"   "Arborophila_torqueola"     "Gallus_gallus"             "Ictinaetus_malaiensis"     "Lophura_leucomelanos"     
  # [8] "Nisaetus_nipalensis"       "Pernis_ptilorhynchus"      "Phaenicophaeus_tristis"    "Polyplectron_bicalcaratum" "Sibia_waldeni"             "Spilornis_cheela"          "Tragopan_blythii"         
  # [15] "Tragopan_temminckii"       "Accipiter_nisus"           "Accipiter_virgatus"        "Buteo_refectus"            "Lophotriorchis_kienerii"   "Mixornis_gularis"          "Pteruthius_aeralatus"     
  # [22] "Spinus_thibetanus" 
  
  ## all galliformes  will go to node # 1 i.e. out of 22 mismatches 8 are adjusted
  # "Arborophila_mandellii"     "Arborophila_rufogularis"   "Arborophila_torqueola"     "Gallus_gallus"  "Lophura_leucomelanos" Polyplectron_bicalcaratum" "Tragopan_blythii" "Tragopan_temminckii"
  
  require(phangorn)
  #adding at node 2
  tree1 <- add.tips(schumm_tree_trunc1, mismatches1[2], c(1))
  tree1 <- add.tips(tree1, mismatches1[3], c(1))
  tree1 <- add.tips(tree1, mismatches1[4], c(1))
  tree1 <- add.tips(tree1, mismatches1[5], c(1))
  tree1 <- add.tips(tree1, mismatches1[7], c(1))
  tree1 <- add.tips(tree1, mismatches1[11], c(1))
  tree1 <- add.tips(tree1, mismatches1[14], c(1))
  tree1 <- add.tips(tree1, mismatches1[15], c(1))
  
  
  tiff("schumm2.tiff", width = 12, height = 24, unit = "cm", res = 300)
  plot(tree1, type = "phylogram", use.edge.length = TRUE, show.node.label = TRUE, cex = 0.2, no.margin = TRUE)
  jnk1 <- tree1$Nnode
  nodelabels(text = seq(1,jnk1), col = "blue", cex = 0.3, bg = "white")
  dev.off()
  
  
  # remaining
  #[1] "Accipiter_trivirgatus"     "Ictinaetus_malaiensis"     "Nisaetus_nipalensis"       "Pernis_ptilorhynchus"      "Phaenicophaeus_tristis"    
  # "Sibia_waldeni"             "Spilornis_cheela"           "Accipiter_nisus"           "Accipiter_virgatus"        "Buteo_refectus"        
  # "Lophotriorchis_kienerii"   "Mixornis_gularis"          "Pteruthius_aeralatus"     
  # [22] "Spinus_thibetanus" 
  
  ### 9 of these 16 will go to node # 24
  # ""Accipiter_trivirgatus"  Accipiter_virgatus Accipiter_nisus"  "Ictinaetus_malaiensis"     "Nisaetus_nipalensis"    "Spilornis_cheela" "Pernis_ptilorhynchus" "Spilornis_cheela" "Buteo_refectus" 
  # "Sibia_waldeni" "Spinus_thibetanus"
  
  tree1 <- add.tips(tree1, mismatches1[1], c(24))
  tree1 <- add.tips(tree1, mismatches1[6], c(24))
  tree1 <- add.tips(tree1, mismatches1[8], c(24))
  tree1 <- add.tips(tree1, mismatches1[9], c(24))
  tree1 <- add.tips(tree1, mismatches1[12], c(24))
  tree1 <- add.tips(tree1, mismatches1[16], c(24))
  tree1 <- add.tips(tree1, mismatches1[17], c(24))
  tree1 <- add.tips(tree1, mismatches1[18], c(24))
  tree1 <- add.tips(tree1, mismatches1[22], c(24))
  
  # remaining
  #  "Phaenicophaeus_tristis"  # node 44
  # "Lophotriorchis_kienerii"   # node 66
  # "Mixornis_gularis"  # node 29
  # Pteruthius_aeralatus"  = node 71
  
  
  tree1 <- add.tips(tree1, mismatches1[10], c(44))
  tree1 <- add.tips(tree1, mismatches1[19], c(66))
  tree1 <- add.tips(tree1, mismatches1[20], c(29))
  tree1 <- add.tips(tree1, mismatches1[21], c(71))
  
  annual_tree <- tree1
  return(annual_tree)
  
}