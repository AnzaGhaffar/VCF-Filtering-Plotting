vcf_data_import <- function(){
  vcf <- readVcf("snps_all.vcf.gz")
  svcf <- scanVcf("snps_all.vcf.gz")
  final_data <- as.data.frame(geno(vcf)$GT)
  final_data$Seqnames <- as.data.frame(seqnames(vcf))$value
  final_data$Start <- as.data.frame(ranges(vcf))$start
  final_data$End <- as.data.frame(ranges(vcf))$end
  final_data$REF <- as.data.frame(ref(vcf))$x
  ALT <- as.character.Array(svcf$`*:*-*`$ALT)
  final_data$ALT <- as.data.frame(ALT)$ALT
  final_data$info <- rownames(final_data)
  rownames(final_data) <- NULL
  return(final_data)
  
  
  
}
