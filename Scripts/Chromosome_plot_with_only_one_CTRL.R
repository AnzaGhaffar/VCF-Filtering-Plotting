# This fuction is to plot when there is only one control
Con_D21<-function(all_Sbicolor_filtered_chrome,chrome_length)
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
  
  
  all_Sbicolor_filtered_chrome$POS <- as.integer(as.character(all_Sbicolor_filtered_chrome$POS))
  
  #final_data <- all_Sbicolor_filtered_chrome[order(all_Sbicolor_filtered_chrome$POS),]
  #final_data <- final_data[order(final_data$CHROM),]
  
  for (i in unique(sbicolor_length$chromosome))
  {
    x<-sbicolor_length[sbicolor_length$chromosome==i,]
    p_data<- as.data.frame(all_Sbicolor_filtered_chrome[all_Sbicolor_filtered_chrome$CHROM==i,])
    print(colnames(x))
    y <- colnames(p_data)
    p_data1<- gather(p_data,chrome_type,GT,paste(y[6]):paste(y[length(y)]),factor_key=TRUE)
    print(colnames(p_data1))
    p_data1$POS <- as.integer(as.character(p_data1$POS))
    p_data1<- p_data1[p_data1$GT=="0/0" | p_data1$GT=="1/1" | p_data1$GT=="0/1" | p_data1$GT=="./.",]
    print(x)
    file_name <- paste("Chromosome_",i,".png",sep = "")
    #jpeg(file_name)
    p1 <- ggplot(data = x,aes(x=chrome_name,y=size))+geom_bar(stat = "identity",width = 0.3)+coord_flip()+ geom_segment(data = p_data1,mapping = aes(x=as.integer(factor(chrome_type))+0.1,y=POS,xend=as.integer(factor(chrome_type))-0.1,yend=POS,color=factor(GT)))
    final_plot <- p1 +scale_color_manual(values = c("black","blue","green","yellow"))+ggtitle(file_name)+labs(x="Samples",y="Size",colour="GT")
    #dev.off()
    ggsave(file_name)
    show(final_plot)
    
    
  }
}