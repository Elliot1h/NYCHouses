library(tidyverse)
library(patchwork)

plot_missing <- function(data, percent) {
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  column_order <- colSums(is.na(data)) %>%
    sort(decreasing = TRUE)
  column_order <- colnames(data.frame(t(column_order)))
  
  p1_missing_patterns <- missing_patterns[, column_order] %>%
    rownames_to_column("id") %>% 
    mutate(id = fct_reorder(id, desc(as.numeric(as.character(id))))) %>% 
    gather(key, value, -id) %>% 
    mutate(missing = ifelse(value, "2", "1"))
  
  p1_missing_patterns <- p1_missing_patterns %>% 
    group_by(id) %>% 
    mutate(missing = replace(missing, sum(as.numeric(as.character(missing))) == ncol(data), "0")) %>% 
    ungroup()
  
  p1 <- ggplot(p1_missing_patterns, aes(x = factor(key, levels = column_order), y = id, fill = missing)) +
    geom_tile(color = "white") + 
    scale_fill_manual(values = c(alpha("darkgray", 0.9), alpha("grey", 0.7), alpha("mediumpurple", 0.7))) +
    xlab("variable") +
    ylab("missing pattern") +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45))
  p1 <- p1 + theme(legend.position = "none") + annotate("text", x = ncol(missing_patterns) / 2, y = nrow(missing_patterns) - mean(as.numeric(as.character(p1_missing_patterns$id[p1_missing_patterns$missing == "0"]))) + 1, label = "complete cases")
  
  if (percent != TRUE) {
    p2_missing_patterns <- missing_patterns %>%
      rownames_to_column("id") %>% 
      mutate(id = fct_reorder(id, desc(as.numeric(as.character(id))))) %>% 
      select("id", "count") %>% 
      mutate(missing = ifelse(id == mean(as.numeric(as.character(p1_missing_patterns$id[p1_missing_patterns$missing == "0"]))), "0", "1"))
    
    p2 <- ggplot(p2_missing_patterns, aes(x = id, y = count, fill = missing)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(alpha("cornflowerblue", 0.9), alpha("cornflowerblue", 0.6))) + 
      coord_flip() +
      xlab("") +
      ylab("row count") +
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
    p2 <- p2 + theme(legend.position = "none")
    
    p3_missing_patterns <- data.frame(count = colSums(is.na(data))) %>% rownames_to_column("variable")
    
    p3 <- ggplot(p3_missing_patterns, aes(x = factor(variable, levels = column_order), y = count)) +
      geom_bar(stat = "identity", fill="cornflowerblue", alpha = 0.6) +
      xlab("") +
      ylab("num rows missing") +
      ggtitle("Missing value patterns") +
      theme_bw() + 
      theme(axis.text.x=element_text(angle=45)) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  } else {
    p2_missing_patterns <- missing_patterns %>%
      rownames_to_column("id") %>% 
      mutate(id = fct_reorder(id, desc(as.numeric(as.character(id))))) %>% 
      mutate(percentage = count / sum(count) * 100) %>%
      select("id", "percentage") %>% 
      mutate(missing = ifelse(id == mean(as.numeric(as.character(p1_missing_patterns$id[p1_missing_patterns$missing == "0"]))), "0", "1"))
    
    p2 <- ggplot(p2_missing_patterns, aes(x = id, y = percentage, fill = missing)) +
      geom_bar(stat = "identity") +
      ylim(0, 100) +
      scale_fill_manual(values = c(alpha("cornflowerblue", 0.9), alpha("cornflowerblue", 0.6))) + 
      coord_flip() +
      xlab("") +
      ylab("% rows") +
      theme_bw() + 
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())
    p2 <- p2 + theme(legend.position = "none")
    
    p3_missing_patterns <- data.frame(count = colSums(is.na(data))) %>% rownames_to_column("variable") %>% mutate(percentage = count / nrow(data) * 100)
    
    p3 <- ggplot(p3_missing_patterns, aes(x = factor(variable, levels = column_order), y = percentage)) +
      geom_bar(stat = "identity", fill="cornflowerblue", alpha = 0.6) +
      ylim(0, 100) +
      xlab("") +
      ylab("% rows missing") +
      ggtitle("Missing value patterns") +
      theme_bw() + 
      theme(axis.text.x=element_text(angle=45)) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  }
  
  p <- p3 + plot_spacer() + p1 + p2 + 
    plot_layout(
      ncol = 2, 
      nrow = 2, 
      widths = c(4, 1),
      heights = c(1, 4)
    )
  p
}