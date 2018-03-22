library(vegan)

load("Data/Matrix for each BCR/ATLANTIC_NORTHERN_FOREST_matrix.RData")

matrix <- matrix[,-(1:6)]
matrix <- ifelse(matrix > 0, 1, 0)

matrix_sm <- matrix[1:5000,]
matrix_sm <- matrix_sm[rowSums(matrix_sm) > 1, colSums(matrix_sm) > 1]


matrix_soren <- vegdist(matrix_sm, method = 'bray', binary = T)

matrix_soren2 <- designdist(matrix_sm, method = "(A+B-2*J)/(A+B)", terms = "binary") # order of magnitude quicker

matrix_kmeans <- kmeans(matrix_soren2, centers = 2)

