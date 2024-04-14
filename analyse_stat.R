univarie <- function(data, var_cont, var_cat, stat) {
  data <- data
  var <- c(var_cont,var_cat)
  tbl <- data %>% tbl_summary(include = all_of(var) ,
                              statistic = list( 
                                all_continuous() ~ paste0(stat[1], ": {" ,stat[1],"} ", stat[2], ": {" ,stat[2],"}"),
                                
                                all_categorical() ~ "{p} %"),
                              sort = all_categorical() ~ "frequency"
  )
  
  
  
  for (i in  var_cont){
    print( ggplot(data) +
             aes(x = get(i)) +
             geom_density(adjust = .5) +
             scale_x_continuous(limits = c(min(data[[i]]), max(data[[i]])))+
             xlab(i) +
             ylab("Effectifs"))
    
    print(ggplot(data) +
            aes(x = get(i)) +
            geom_histogram(
              fill ="lightblue", 
              colour = "black", 
              binwidth = 0.5
            ) +
            xlab(i) +
            ylab("Effectifs"))
  }
  
  for (i in  var_cat){
    print(ggplot(data) +
            aes(x = forcats::fct_infreq(get(i)), 
                y = after_stat(prop), by = 1) +
            geom_bar(stat = "prop", 
                     fill = "#4477AA", colour = "black") +
            geom_text(
              aes(label = after_stat(prop) |> 
                    scales::percent(accuracy = .1)),
              stat = "prop",
              nudge_y = .02
            ) +
            theme_minimal() +
            theme(
              panel.grid = element_blank(),
              axis.text.y = element_blank()
            ) +
            xlab(NULL) + ylab(NULL) +
            ggtitle(i))
  }
  
  return(tbl)
}

bivarie <- function(data, var1, var2,type){
  ## type 0
  if(type == 0){
    # tableau 
    tbl <- data %>% 
      tbl_summary(
        include = all_of(var1),
        by = var2
      ) %>% 
      add_p()
    
    # graphique
    'print( ggplot(data %>% group_by(get(var2))) +
             aes(
               x = get(var1),
               y = ..prop..,
               fill = get(var2), 
               label = scales::percent(..prop.., accuracy = 1)
             ) +
             geom_bar(
               aes(y = ..prop..),
               stat = "prop", 
               position = position_dodge(.9)
             ) +
             geom_text(
               aes(y = ..prop.. - 0.01, label = scales::percent(..prop.., accuracy = 1)),
               stat = "prop", 
               position = position_dodge(.9),
               vjust = "top"
             ) +
             scale_y_continuous(labels = scales::percent) +
             
             theme_light() +
             xlab("") +
             ylab("") +
             labs(fill = "") +
             ggtitle(paste0(var1, " selon ", var2)) +
             theme(
               panel.grid = element_blank(),
               panel.border = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               legend.position = "top"
             ) +
             scale_fill_brewer())'
    
    data_0 <- data %>% mutate(v1 = get(var1), v2 = get(var2)) %>%
      group_by(v2, v1) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      group_by(v2) %>%
      mutate(prop = count / sum(count))
    print(dim(data_0))
    
    # Création du graphique
    print(ggplot(data_0) +
      aes(
        x = v1,
        y = prop,
        fill = v2, 
        label = scales::percent(prop, accuracy = 1)
      ) +
      geom_bar(
        aes(y = prop),
        stat = "identity", 
        position = position_dodge(.9)
      ) +
      geom_text(
        aes(y = prop - 0.01, label = scales::percent(prop, accuracy = 1)),
        position = position_dodge(.9),
        vjust = "top"
      ) +
      scale_y_continuous(labels = scales::percent) +
      theme_light() +
      xlab("") +
      ylab("") +
      labs(fill = "") +
      ggtitle(paste0(var1, "selon" ,var2)) +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top"
      ) +
      scale_fill_brewer())
    
    
    
  }
  
  ## type 1
  if(type == 1){
    ## courbe 
    print(ggplot(data) +
            aes(x = get(var1), y = get(var2)) +
            geom_smooth(method = "lm") +
            geom_point(colour = "blue", alpha = .25) +
            geom_rug() +
            theme_light())
    
    ## tableau de regression
    m <- lm(get(var1) ~ get(var2), data = data)
    #summary(m)
    tbl <- m |> 
      tbl_regression() |> 
      add_glance_source_note()
  }
  
  ## type 2
  if(type == 2){
    ## graphique
    print(ggplot(data) +
            aes(x = get(var1), y = get(var2)) +
            geom_boxplot(fill = "lightblue") +
            theme_light()+
            ggtitle(paste0(var1, " selon ", var2)))
    
    ## tableau
    tbl <- data |> 
      tbl_summary(
        include = all_of(var1),
        by = var2,
        statistic = all_continuous() ~ "{mean} ({sd})",
        digits = all_continuous() ~ c(1, 1)
      ) |> 
      add_overall(last = TRUE)
  }
  return(tbl)
}