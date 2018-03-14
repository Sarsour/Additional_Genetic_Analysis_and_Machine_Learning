
data <- read.table( file = "pnas_expression.txt" , header = TRUE )
counts <- data[ , -c(1, ncol(data))]
rownames(counts) <- data[, 1]
colnames(counts) <- paste("Sample", seq(1:7), sep="")

print(dim(counts))

set.seed

group <- c(rep("Group1", 4) , rep("Group2", 3))
edgeRlist <- DGEList(counts, group = group)
#print(edgeRlist)

edgeRlist <- edgeRlist[rowSums(1000000*edgeRlist$counts/expandAsMatrix(edgeRlist$samples$lib.size, dim(edgeRlist)) > 1) >= 3, ]
edgeRlist <- calcNormFactors(edgeRlist)
edgeRlist <- estimateCommonDisp(edgeRlist)
print(edgeRlist)
min <- row.names(edgeRlist$counts)[which.min(edgeRlist$tagwise.dispersion)]
print(min)
max <- row.names(edgeRlist$counts)[which.max(edgeRlist$tagwise.dispersion)]
print(max)

edgeRlist <- estimateTagwiseDisp(edgeRlist)
edgeRresult <- exactTest(edgeRlist, dispersion="tagwise", pair = c( "Group1" , "Group2" ) )
output <- topTags( edgeRresult, n = 15600, sort.by = "PValue")

#print(output)
#print(output$table$PValue < 0.0001)
#print(output$table$PValue < 0.000000001)

