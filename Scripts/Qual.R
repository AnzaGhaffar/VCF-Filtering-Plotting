all_plots<-function(all_Sbicolor_filtered_chrome,chrome_length,ctrl_names)
{
  
  
  
  length_of_samples <- length(colnames(all_Sbicolor_filtered_chrome))
  colnames_x <- all_Sbicolor_filtered_chrome[2,]
  rownames(colnames_x) <- NULL
  colnames_x[1,10:length_of_samples]<-all_Sbicolor_filtered_chrome[1,10:length_of_samples]
  colnames(all_Sbicolor_filtered_chrome) <- unlist(colnames_x[1,])
  all_Sbicolor_filtered_chrome <- all_Sbicolor_filtered_chrome[-c(1,2),]
  rownames(all_Sbicolor_filtered_chrome) <- NULL
  print(colnames_x)
  colnames_x <- droplevels(colnames_x)
  o=1
  for (i in colnames_x[1,5:length_of_samples]){
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
  
  df <- data.frame()
  for (i in mutant_names){
    name_call <- paste("gatk_",i,sep = "")
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
    p_data1<- gather(p_data,chrome_type,GT,paste(y[5]):paste(y[length(y)]),factor_key=TRUE)
    print(colnames(p_data1))
    p_data1$POS <- as.integer(as.character(p_data1$POS))
    p_datax<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1" | p_data1$GT=="0/1" | p_data1$GT=="./.",]
    p_data1$chrome_type <- as.character(p_data1$chrome_type)
    p_data3 <- p_datax[p_datax$chrome_type!="QUAL" | p_datax$chrome_type!="DP" | p_datax$chrome_type!="AC" | p_datax$chrome_type!="MQ",]
    p_data2 <- p_data1[p_data1$chrome_type=="QUAL" | p_data1$chrome_type=="DP" | p_data1$chrome_type=="AC" | p_data1$chrome_type=="MQ",]
    x$chrome_name <- as.character(x$chrome_name)
    
    x1<- x[x$chrome_name == "QUAL" | x$chrome_name == "DP" |x$chrome_name == "AC" | x$chrome_name == "MQ",]
    x2<- x[x$chrome_name != "QUAL" | x$chrome_name != "DP" | x$chrome_name != "AC" | x$chrome_name != "MQ",]
    print(x1)
    print(x2)
    file_name <- paste("Chromosome_",i,".png")
    #jpeg(file_name)
    #print(unique(p_data1$GT))
    p_data3 <- droplevels(p_data3)
    print(unique(p_data3$chrome_type))
    print(unique(p_data2$chrome_type))
    print(x$chrome_name)
    p1 <- ggplot(data = x2,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+ geom_segment(data = p_data3,mapping = aes(x=as.integer(factor(chrome_type))+0.1,y=POS,xend=as.integer(factor(chrome_type))-0.1,yend=POS,color=factor(GT)))
    
    final_plot <-  ggplot(data = x1,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.4)+coord_flip() +geom_point(data = p_data2,mapping = aes(x=as.integer(factor(GT)),y=POS),shape=1,color='blue',alpha=0.2)
    
    final_plot_1<- p1 +scale_color_manual(values = c("black","blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
    #ggsave(file_name)
    show(final_plot)
    show(final_plot_1)
    break()
  }
  
}

final_plot <-  ggplot(data = x1,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.4)+coord_flip() +geom_point(data = p_data2,mapping = aes(x=as.integer(factor(chrome_type))+(0.15*(((as.integer(factor(GT)))-min(as.integer(factor(GT))))/(max(as.integer(factor(GT)))-min(as.integer(factor(GT)))))),y=POS),shape=1,color='blue',alpha=0.2)
