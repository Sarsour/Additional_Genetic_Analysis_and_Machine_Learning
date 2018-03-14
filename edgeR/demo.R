library(edgeR)

set.seed(0)
counts <- matrix(rnbinom(100000, size=1/0.2, mu=75), ncol=10)

rownames(counts) <- paste("Gene", seq(1:10000), sep="")
colnames(counts) <- paste("Sample", seq(1:10), sep="")
#print(head(counts))

group <- c(rep("Group1", 5) , rep("Group2", 5))
edgeRlist <- DGEList(counts, group = group)
#print(edgeRlist)

edgeRlist <- edgeRlist[rowSums(1000000*edgeRlist$counts/expandAsMatrix(edgeRlist$samples$lib.size, dim(edgeRlist)) > 1) >= 3, ]
edgeRlist <- calcNormFactors(edgeRlist)
edgeRlist <- estimateCommonDisp(edgeRlist)

edgeRlist <- estimateTagwiseDisp(edgeRlist)
edgeRresult <- exactTest(edgeRlist, dispersion="tagwise", pair = c( "Group1" , "Group2" ) )
output <- topTags( edgeRresult, n = 30, sort.by = "PValue")

print(edgeRlist)

print(output)


