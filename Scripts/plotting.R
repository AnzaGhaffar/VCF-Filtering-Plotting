data_manipulator_1<-function(all_Sbicolor_filtered_chrome,chrome_length,ctrl_names)
  {

length_of_samples <- length(colnames(all_Sbicolor_filtered_chrome))
colnames_x <- all_Sbicolor_filtered_chrome[2,]
rownames(colnames_x) <- NULL
colnames_x[1,6:length_of_samples]<-all_Sbicolor_filtered_chrome[1,6:length_of_samples]
colnames(all_Sbicolor_filtered_chrome) <- unlist(colnames_x[1,])
all_Sbicolor_filtered_chrome <- all_Sbicolor_filtered_chrome[-c(1,2),]
rownames(all_Sbicolor_filtered_chrome) <- NULL
print(colnames_x)
colnames_x <- droplevels(colnames_x)
o=1
for (i in colnames_x[1,6:length_of_samples]){
  print(i)
  #tp=paste("chrome_type_",o)
  chrome_length[,paste("chrome_type_",o)] <- i
  o=o+1
  
}
print(chrome_length)
x <- colnames(chrome_length)
print(x[length(x)])
test <- paste(x[3])
print(x[3])
test_1 <- paste(x[length(x)])
print(test_1)
sbicolor_length<-gather(chrome_length,chrome_type,chrome_name,paste(x[3]):paste(x[length(x)]),factor_key = TRUE)
sbicolor_length$size <- as.integer(as.character(sbicolor_length$size))

all_colnames <- colnames(all_Sbicolor_filtered_chrome)
always_there <- c("CHROM","POS","REF","ALT","QUAL")
new_ctrl <- append(always_there,ctrl_names)
mutant_names <- setdiff(all_colnames,new_ctrl)  
print(mutant_names)
print(all_colnames)
df <- data.frame()
for (i in mutant_names){
  name_call <- paste(i,sep = "")
  print(name_call)
  #print(paste("free_bayes$", i,sep = ""))
  #print(gatk_testing[gatk_testing[,i]=="1/1",])
  df <-rbind(df,all_Sbicolor_filtered_chrome[all_Sbicolor_filtered_chrome[,i]=="1/1",])
}
df$POS <- as.integer(as.character(df$POS))
final_data <- df[!duplicated(df$POS),]
final_data <- final_data[order(final_data$POS),]
final_data <- final_data[order(final_data$CHROM),]

for (i in unique(sbicolor_length$chromosome))
{
  x<-sbicolor_length[sbicolor_length$chromosome==i,]
  p_data<- as.data.frame(final_data[final_data$CHROM==i,])
  print(colnames(x))
  y <- colnames(p_data)
  p_data <- droplevels(p_data)
  p_data1<- gather(p_data,chrome_type,GT,paste(y[6]):paste(y[length(y)]),factor_key=TRUE)
  print(colnames(p_data1))
  p_data1$POS <- as.integer(as.character(p_data1$POS))
  p_data1<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1" | p_data1$GT=="0/1" | p_data1$GT==".",]
  print(x)
  p_data1$chrome_type <- as.character(p_data1$chrome_type)
  file_name <- paste("Chromosome_",i,".png")
  #jpeg(file_name)
  p1 <- ggplot(data = x,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+ geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_type))+0.1,y=POS,xend=as.integer(factor(chrome_type))-0.1,yend=POS,color=factor(GT)))
  final_plot <- p1 +scale_color_manual(values = c("black","blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
  #dev.off()
  #ggsave(file_name)
  show(final_plot)

}
 }




# this function is if i want to plot all the chromosomes together then i use this one
data_manipulator_2<-function(all_Sbicolor_filtered_chrome,chrome_length)
{
  
  
  
  length_of_samples <- length(colnames(all_Sbicolor_filtered_chrome))
  colnames_x <- all_Sbicolor_filtered_chrome[2,]
  rownames(colnames_x) <- NULL
  colnames_x[1,6:length_of_samples]<-all_Sbicolor_filtered_chrome[1,6:length_of_samples]
  colnames(all_Sbicolor_filtered_chrome) <- unlist(colnames_x[1,])
  all_Sbicolor_filtered_chrome <- all_Sbicolor_filtered_chrome[-c(1,2),]
  rownames(all_Sbicolor_filtered_chrome) <- NULL
  print(colnames_x)
  colnames_x <- droplevels(colnames_x)
  o=1
  for (i in colnames_x[1,6:length_of_samples]){
    print(i)
    #tp=paste("chrome_type_",o)
    chrome_length[,paste("chrome_type_",o)] <- i
    o=o+1
    
  }
  print(colnames(all_Sbicolor_filtered_chrome))
  print(all_Sbicolor_filtered_chrome[1:3,])
  print(chrome_length)
  x <- colnames(chrome_length)
  print(x[length(x)])
  test <- paste(x[3])
  print(x[3])
  test_1 <- paste(x[length(x)])
  print(test_1)
  y <- colnames(all_Sbicolor_filtered_chrome)
  sbicolor_length<-gather(chrome_length,chrome_type,chrome_name,paste(x[3]):paste(x[length(x)]),factor_key = TRUE)
  p_data1<- gather(all_Sbicolor_filtered_chrome,chrome_type,GT,paste(y[6]):paste(y[length(y)]),factor_key=TRUE)
  print(colnames(p_data1))
  p_data1$POS <- as.integer(as.character(p_data1$POS))
  p_data1<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1",]
  p_data1$chrome_unique <- paste(p_data1$CHROM,"_",p_data1$chrome_type)
  sbicolor_length$chrome_unique <- paste(sbicolor_length$chromosome,"_",sbicolor_length$chrome_name)
  sbicolor_length$size <- as.integer(as.character(sbicolor_length$size))
  print(x)
  file_name <- paste("Chromosome_",i,".png")
    #jpeg(file_name)
  p1 <- ggplot(data = sbicolor_length,aes(x=chrome_unique,y=size))+geom_bar(stat = "identity",width = 0.5)+coord_flip()+ geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_unique))-0.1,y=POS,xend=as.integer(factor(chrome_unique))+0.1,yend=POS,color=factor(GT)))
  final_plot <- p1 +scale_color_manual(values = c("blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
    #dev.off()
  #ggsave(file_name)
  show(final_plot)
  
}
#name_file <- paste(""",all_filtered.vcf.gz,""")
xyz <- scanVcfHeader("all_filtered.vcf.gz")
chrome_length_1<- as.array(xyz@header$contig@rownames[1:11])
chrome_length_1<- as.data.frame(chrome_length_1)
colnames(chrome_length_1) <- c("chromosome")
chrome_length_1$size <- as.array(xyz@header$contig$length[1:11])



for (i in unique(sbicolor_length$chromosome))
  {
  x<-sbicolor_length[sbicolor_length$chromosome==i,]
  p_data<- as.data.frame(all_Sbicolor_filtered_chrome[all_Sbicolor_filtered_chrome$CHROM==i,])
  p_data1<- gather(p_data,chrome_type,GT,"con-2_S1":"R4_S25",factor_key=TRUE)
  p_data1$POS <- as.integer(as.character(p_data1$POS))
  p_data1<- p_data1[p_data1$allel=="0/0" | p_data1$allel=="0/1" | p_data1$allel=="1/1",]
  print(x)
  title_name <- paste("Chromosome_",i)
  p1 <- ggplot(data = x,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_type))+0.1,y=POS,xend=as.integer(factor(chrome_type))-0.1,yend=POS,color=factor(GT)))
  final_plot <- p1 +p2+scale_color_manual(values = c("blue","green","yellow"))
  show(final_plot)
  
  break
  }
o=1
for (i in xz){
  
  #tp=paste("chrome_type_",o)
  chrome_length[,paste("chrome_type_",o)] <- i
  o=o+1
  
}

for (i in unique(all_chrome_name$chromosome))
{
  x<-all_chrome_name[all_chrome_name$chromosome==i,]
  p<- as.data.frame(izza[izza$CHROM==i,])
  #print(typeof(p))
  #print(colnames(p))
  print(x)
  show(ggplot(data = x,aes(x=chrome_type,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+
         geom_segment(data = p,mapping = aes(x=as.integer(factor(chrome_type_1))+0.1,y=POS,xend=as.integer(factor(chrome_type_1))-0.1,yend=POS,color=factor(Allel))))
  break
}

length_of_samples <- length(colnames(GATK_Chr01))
colnames_x <- GATK_Chr01[2,]
rownames(colnames_x) <- NULL
colnames_x[1,10:length_of_samples]<-GATK_Chr01[1,10:length_of_samples]

opo <- ggplot(data = chrome_name[1,],aes(x=chromosome,y=size))+geom_bar(stat = "identity",width = 0.3,colour='white')+coord_flip()
opo+geom_segment(data = GATK_Chr01,mapping = aes(x=as.integer(factor(CHROM)),y=POS,xend=as.integer(factor(CHROM))+(0.15*(((as.integer(factor(QUAL)))-min(as.integer(factor(QUAL))))/(max(as.integer(factor(QUAL)))-min(as.integer(factor(QUAL)))))),yend=POS),size=0.3,color='lightblue',alpha=0.1)


opo+geom_segment(data = GATK_Chr01,mapping = aes(x=as.integer(factor(CHROM)),y=POS,xend=as.integer(factor(CHROM))+(0.15*(((as.integer(factor(MQ)))-min(as.integer(factor(MQ))))/(max(as.integer(factor(MQ)))-min(as.integer(factor(MQ)))))),yend=POS),size=0.3,color='blue',alpha=0.2)


                 (0.3*(((as.integer(factor(GATK_Chr01$DP)))-min(as.integer(factor(GATK_Chr01$DP))))/(max(as.integer(factor(GATK_Chr01$DP)))-min(as.integer(factor(GATK_Chr01$DP))))))-0.15
  first <- as.integer(factor(GATK_Chr01$DP))-min(as.integer(factor(GATK_Chr01$DP)))
  second <- 
GATK_Chr01$POS <- as.integer(as.character(GATK_Chr01$POS))

ggplot(data = GATK_Chr01,mapping=aes(x=0.1,y=POS,xend=as.integer(factor(GATK_Chr01$DP))-0.1,yend=GATK_Chr01$POS))+geom_segment(position = 'identity')+geom_bar(chrome_name,mapping=aes(x=chromosome,y=size),width = 0.3)+coord_flip()

ggplot(data=chrome_name,aes(x=0,xend=size,y=chromosome,yend=chromosome))+
  geom_segment(color=rgb(0,0,0),size=4)+
  geom_segment(data=GATK_Chr01,mapping=aes(x=as.integer(factor(DP))-0.15,xend=as.integer(factor(DP)),y=POS,yend=POS))

opo+geom_linerange(data=GATK_Chr01,mapping = aes(x=POS,ymax=0,ymin=as.integer(factor(DP))),size=0.5)







# hist plot

ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + 
  geom_bar(fill="bisque",color="white",alpha=0.7) + 
  stat_count(geom="text", aes(label=sprintf("%.4f",..count../length(group_tags))), vjust=-0.5) +
  labs(x='Quality') +
  theme_minimal() 
