shinyPCA<-function(expressionMatrix,sampleData,numberOfPCs){
  expressionMatrix1 <- as_tibble(expressionMatrix,rownames = "Sample")
  sampleData <- as.tibble(sampleData)
  pca <- expressionMatrix1 %>% 
    nest() %>% 
    mutate(pca = map(data, ~ prcomp(.x %>% select(-Sample), 
                                    center = TRUE, scale = TRUE)),
           pca_aug = map2(pca, data, ~broom::augment(.x, data = .y)))
  
  var_exp <- pca %>%
    unnest(.,pca_aug) %>% 
    summarize_at(.vars = vars(contains("PC")), .funs = funs(var)) %>%
    gather(key = pc, value = variance) %>%
    mutate(var_exp = variance/sum(variance),cum_var_exp=cumsum(var_exp),pc = str_replace(pc, ".fitted", ""))
  
  importancePlot <- var_exp %>% 
    rename(
      Individual = var_exp,
      Cumulative = cum_var_exp
    ) %>%
    gather(key,value,Individual,Cumulative) %>% 
    ggplot(aes(x=pc,y=value,group=key,colour=key))+
    geom_point()+
    geom_line()+
    labs(y="Percent of Variance",x="",title="Variance Explained by each PC")+
    theme_bw()

  pcaRaw <- prcomp(t(expressionMatrix))
  
  pcaDF <- as_data_frame(pcaRaw$x,rownames = "Sample")
  
  pcaPlot<-GGally::ggpairs(data=pcaDF,columns=1:numberOfPCs+1,mapping = aes(colour=Sample),upper=NULL,legend = c(2,2))+
    theme_bw()+
    labs(title="Principal Components")
  
  returnList<-list(variancePlot=importancePlot, pcaGrid=pcaPlot)
  return(returnList)
}
