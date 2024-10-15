### cgMLST SL #####
colnames(ncbi.genomic.db)

ncbi.genomic.db %>% 
  count(cgMLST_Sublineage) %>% 
  arrange(desc(n))

selected_cgMLST_SL <- 
  ncbi.genomic.db %>%
  count(cgMLST_Sublineage) %>%
  slice_max(order_by = n,
            n = 10,
            with_ties = T) %>%
  arrange(desc(n)) %>%
  pull(cgMLST_Sublineage)



ncbi.genomic.db %>%
  group_by(cgMLST_Sublineage, ORIGIN) %>%
  ggplot() +
  aes(
    y =  ..count..,
    x = factor(ORIGIN, levels = rev(ORIGINORDER)),
    fill = forcats::fct_explicit_na(factor(cgMLST_Sublineage, levels = (selected_cgMLST_SL)), "Other")
  ) +
  geom_bar(width = 0.7) +
  scale_fill_manual(values = SLCOLORS) +
  labs(y = NULL, x = "Origin", fill = "cgMLST sL")+
  theme(legend.position = "bottom") 


ncbi.genomic.db %>%
  filter(Collection_year > 2005 & Collection_year < 2021) %>% 
  group_by(cgMLST_Sublineage, Collection_year) %>%
  dplyr::summarise(n = n()) %>%
  ggplot() +
  aes(
    y =  n,
    x = Collection_year,
    fill = forcats::fct_explicit_na(factor(cgMLST_Sublineage, levels = (selected_cgMLST_SL)), "Other")
  ) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = SLCOLORS) +
  labs(y = NULL, x = "Origin", fill = "cgMLST sL")+
  theme(legend.position = "bottom") 










