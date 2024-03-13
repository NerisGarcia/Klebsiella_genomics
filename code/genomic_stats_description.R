

#! load datos total


ncontig_hist <- 
  datos_total %>%
  ggplot(aes(x=contigs))+
  geom_bar(fill="#084c61")+
  scale_fill_identity()+
  geom_vline(xintercept=20, 
             color = "darkred")+        
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA))  +
  xlab("Contigs")+
  ylab("Assemblies") 

#saveplot(filename = "CH3_ncontigshist", ncontig_hist, "t")