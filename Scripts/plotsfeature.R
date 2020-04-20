#quality_bins_unfiltered <- data.frame(table(unlist(cut(freebayes_D2.unfiltered$V6,100))))
#ggplot(data=quality_bins_unfiltered,mapping = aes(quality_bins_unfiltered$Var1,quality_bins_unfiltered$Freq))+geom_bar(stat = 'identity')

#quality_bins_filtered <- data.frame(table(unlist(cut(freebayes_D2.filtered$V6,100))))
#ggplot(data=quality_bins_filtered,mapping = aes(quality_bins_filtered$Var1,quality_bins_filtered$Freq))+geom_bar(stat = 'identity')

#chrom_filtered_quality_bins_unfiltered <- data.frame(table(unlist(cut(chrom_filtered_freebayes_D2.unfiltered$V6,100))))
#ggplot(data=chrom_filtered_quality_bins_unfiltered,mapping = aes(chrom_filtered_quality_bins_unfiltered$Var1,chrom_filtered_quality_bins_unfiltered$Freq))+geom_bar(stat = 'identity')

library(VariantAnnotation)
Featureplots <- function(vcf.file){

vcf <- scanVcf(vcf.file)

png("Histogram for Quality.png")
hist(vcf$`*:*-*`$QUAL,100,col = 'blue1',plot = TRUE, main = 'Histrogram for Quality', xlab = 'Quality', border = 'red')
dev.off()
#ggsave(Histogram_for_Quality)

png("Histogram for Read Depth.png")
hist(vcf$`*:*-*`$INFO$DP,100,col = 'green',plot = TRUE, main = 'Histrogram for Read Depth', xlab = 'Read Depth', border = 'blue')
dev.off()

png("Histogram for Reference Allel Quality.png")
# R1eference Allel Quality
hist(vcf$`*:*-*`$INFO$QR,breaks=100,col = 'green',plot = TRUE, main = 'Histrogram for Reference Allel Quality', xlab = 'Read Depth', border = 'blue')
dev.off()



#hist(list(anzavcf$`*:*-*`$INFO$QR),breaks=100)
#GENO Type Quality
png("Histogram for Genotype Quality.png")
hist(rowSums(data.frame(vcf$`*:*-*`$GENO$GQ)),100,col = 'blue1',plot = TRUE, main = 'Histrogram for Genotype Quality', xlab = 'Quality', border = 'red')
dev.off()

# 
png("Histogram for Reference Allel Count.png")
hist(rowSums(data.frame(vcf$`*:*-*`$GENO$RO)),100,col = 'green',plot = TRUE, main = 'Histrogram for Reference Allel Count', xlab = 'Read Depth', border = 'blue')
dev.off()

png("Histogram for Quality for Reference Observations.png")
hist(rowSums(data.frame(vcf$`*:*-*`$GENO$QR)),100,col = 'green',plot = TRUE, main = 'Histrogram for Reference Observation Quality', xlab = 'Read Depth', border = 'blue')
dev.off()
}
#hist(rowSums(data.frame(anzavcf$`*:*-*`$GENO$DP)),100)

Featureplots(snakemake@input[[1]])



