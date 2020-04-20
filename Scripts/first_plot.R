data_manipulator<-function(plotting_191119){

colnames(plotting_191119)<-c("CHROM","POS","REF","ALT","QUAL","GK715_M4","Grinkan_CTRL")
plotting_191119<-plotting_191119[-c(1,2),]
rownames(plotting_191119)<- NULL
plotting_191119$REF <-as.character(plotting_191119$REF)
plotting_191119$ALT <-as.character(plotting_191119$ALT)
plotting_191119$POS <- as.integer(as.character(plotting_191119$POS))
plotting_191119$ALT_len<-nchar(plotting_191119$ALT)
plotting_191119$REF_len<-nchar(plotting_191119$REF)
plotting_191119$REF_len[plotting_191119$REF_len!=1]<-0
return(plotting_191119)
}
xyz<- ggplot(data=chrome_length,aes(x=chromosome,y=size))+geom_bar(stat = "identity",width = 0.5)+coord_flip()
xyz+geom_segment(data = complete_data_excluded,mapping = aes(x=as.integer(CHROM)+0.3,y=POS,xend=as.integer(CHROM)-0.3,yend=POS,color=factor(GK_REF)))


complete_data_excluded <- complete_data[which(complete_data$GK715_M4_1 <= 1 & complete_data$GK715_M4_2 <= 1 & complete_data$Grinkan_CTRL_1 <= 1 & complete_data$Grinkan_CTRL_2 <= 1),]



colnames(all_Sbicolor_filtered_chrome) <- c("CHROM","POS","REF","ALT","QUAL","con-2_S1","con-3_S2","con-4_S3","D1-1_S4","D1-2_S5","D1-4_S6","D2-1_S7","D2-3_S8","D2-4_S9","D3-1_S10","D3-3_S11","D3-4_S12","D4-1_S13","D4-3_S14","D4-4_S15","D5-1_S16","D5-3_S17","D5-4_S18","D6-2_S19","D6-3_S20","D6-4_S21","R1_S22","R2_S23","R3_S24","R4_S25")

data_manipulator1<-function(all_Sbicolor_filtered_chrome){
  
  colnames(all_Sbicolor_filtered_chrome) <- c("CHROM","POS","REF","ALT","QUAL","con-2_S1","con-3_S2","con-4_S3","D1-1_S4","D1-2_S5","D1-4_S6","D2-1_S7","D2-3_S8","D2-4_S9","D3-1_S10","D3-3_S11","D3-4_S12","D4-1_S13","D4-3_S14","D4-4_S15","D5-1_S16","D5-3_S17","D5-4_S18","D6-2_S19","D6-3_S20","D6-4_S21","R1_S22","R2_S23","R3_S24","R4_S25")
  
  
  
  
  
}

