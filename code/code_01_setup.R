library(tidyverse)
library(textreadr)
library(magrittr)
library(stm)
library(stmBrowser)
library(stmCorrViz)
library(stminsights)
library(LDAvis)
library(furrr)
library(viridis)


#### Functions Topic Model Top Words #####

safelog <- function(x, min=-1000) {
  out <- log(x)
  out[which(out< min)] <- min
  out
}

calcfrex.r = function (logbeta, w = 0.5, wordcounts = NULL, ranking = 0){
  excl <- t(t(logbeta) -  matrixStats::colLogSumExps(logbeta))
  if (!is.null(wordcounts)) {
    excl <- safelog(sapply(1:ncol(excl), function(x) js.estimate(exp(excl[,x]), wordcounts[x])))
  }
  freqscore <- apply(logbeta, 1, data.table::frank)/ncol(logbeta)
  exclscore <- apply(excl, 1, data.table::frank)/ncol(logbeta)
  frex <- 1/(w/freqscore + (1 - w)/exclscore)
  if(!ranking){apply(frex, 2, order, decreasing = TRUE)}else{frex}
}

calclift.r = function (logbeta, wordcounts, ranking = 0){
  emp.prob <- wordcounts / sum(wordcounts)
  lift <- exp(logbeta) / rep(emp.prob, each = nrow(logbeta))
  if(!ranking){apply(lift, 1, order, decreasing = TRUE)}else{t(lift)}
}

calcscore.r = function (logbeta, ranking = 0){
  ldascore <- exp(logbeta) * (logbeta - rep(colMeans(logbeta), 
                                            each = nrow(logbeta)))
  if(!ranking){apply(ldascore, 1, order, decreasing = TRUE)}else{t(ldascore)}
}

calcrelev.r = function (logbeta, theta, docs, wordcounts, lambda, ranking = 0){
  phi <- exp(logbeta)
  if (any(phi == 0)) {
    phi <- phi + .Machine$double.eps
    phi <- phi/rowSums(phi)
  }
  
  doc.length <- as.integer(unlist(lapply(docs, function(x) sum(x[2,]))))
  term.frequency  <- wordcounts
  topic.frequency <- colSums(theta * doc.length)
  
  term.topic.frequency <- phi * topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  
  lift <- phi/term.proportion
  
  relevance <- exp( lambda * log(phi) + (1 - lambda) * log(lift) )
  
  if(!ranking){apply(relevance, 2, order, decreasing = TRUE)}else{relevance}
}

top_words = function(model, docs){
  
  n       = 5
  w       <- 0.5
  lambda  <- 0.5
  
  logbeta   <- model$beta$logbeta
  theta     <- model$theta
  vocab     <- model$vocab
  K <- Ntop <- model$settings$dim$K
  
  logbeta <- logbeta[[1]]
  wordcounts <- model$settings$dim$wcounts$x
  
  prob.value   <- as_tibble(t(exp(logbeta)))                                                                       #Calculate Standard labels
  frex.value   <- as_tibble(try(calcfrex.r(logbeta, w, wordcounts, ranking = 1),silent=TRUE))                      #Calculate FREX Score
  lift.value   <- as_tibble(try(calclift.r(logbeta, wordcounts, ranking = 1), silent=TRUE))                        #Calculate Lift (Taddys thing this is beta_k,v divided by the empirical term probability)
  score.value  <- as_tibble(try(calcscore.r(logbeta, ranking = 1), silent=TRUE))                                   #Calculate score (Chang in LDA package etc.)
  relev.value  <- as_tibble(try(calcrelev.r(logbeta, theta, docs, wordcounts, lambda, ranking = 1), silent=TRUE))  #Calculate relevance (Sievert & Shirley, 2014 in LDAvis package etc.)
  
  prob.value  <- add_column(prob.value,  "prob",  .before = 1)
  frex.value  <- add_column(frex.value,  "frex",  .before = 1)
  lift.value  <- add_column(lift.value,  "lift",  .before = 1)
  score.value <- add_column(score.value, "score", .before = 1)
  relev.value <- add_column(relev.value, "relev", .before = 1)
  
  colnames(prob.value)  = c("method",paste0("Topic ", 1:K))
  colnames(frex.value)  = c("method",paste0("Topic ", 1:K))
  colnames(lift.value)  = c("method",paste0("Topic ", 1:K))
  colnames(score.value) = c("method",paste0("Topic ", 1:K))
  colnames(relev.value) = c("method",paste0("Topic ", 1:K))
  
  Panel.term <- enframe(vocab, name = NULL, value = "term")
  
  prob.labels  <- bind_cols(prob.value ,Panel.term)
  frex.labels  <- bind_cols(frex.value ,Panel.term)
  lift.labels  <- bind_cols(lift.value ,Panel.term)
  score.labels <- bind_cols(score.value,Panel.term)
  relev.labels <- bind_cols(relev.value,Panel.term)
  
  prob.labels  <- prob.labels  %>% gather("topic","value", paste0("Topic ", 1:K))
  frex.labels  <- frex.labels  %>% gather("topic","value", paste0("Topic ", 1:K)) 
  lift.labels  <- lift.labels  %>% gather("topic","value", paste0("Topic ", 1:K)) 
  score.labels <- score.labels %>% gather("topic","value", paste0("Topic ", 1:K)) 
  relev.labels <- relev.labels %>% gather("topic","value", paste0("Topic ", 1:K))  
  
  prob.labels  <- prob.labels  %>% group_by(method) %>% arrange(topic,desc(value))
  frex.labels  <- frex.labels  %>% group_by(method) %>% arrange(topic,desc(value))
  lift.labels  <- lift.labels  %>% group_by(method) %>% arrange(topic,desc(value)) 
  score.labels <- score.labels %>% group_by(method) %>% arrange(topic,desc(value))
  relev.labels <- relev.labels %>% group_by(method) %>% arrange(topic,desc(value)) 
  
  prob.labels  <- rownames_to_column(prob.labels )
  frex.labels  <- rownames_to_column(frex.labels )
  lift.labels  <- rownames_to_column(lift.labels )
  score.labels <- rownames_to_column(score.labels)
  relev.labels <- rownames_to_column(relev.labels)
  
  prob.labels$rowname  <- as.numeric(prob.labels$rowname )
  frex.labels$rowname  <- as.numeric(frex.labels$rowname )
  lift.labels$rowname  <- as.numeric(lift.labels$rowname )
  score.labels$rowname <- as.numeric(score.labels$rowname)
  relev.labels$rowname <- as.numeric(relev.labels$rowname)
  
  Panel.labels <- bind_rows(prob.labels,
                            frex.labels,
                            lift.labels,
                            score.labels,
                            relev.labels)
  
  Panel.labels %>%
    group_by(method, topic) %>%
    top_n(-5, rowname) %>%
    ungroup() -> Panel.labels
  
  Panel.labels$topic = as.factor(Panel.labels$topic)
  
  name  = paste0("Topic ", 1:Ntop)
  
  orden = 1:Ntop
  
  for (i in rev(orden)) {
    Panel.labels$topic = relevel(Panel.labels$topic, name[i])
  }
  
  #### 4.1.2.3      Visualización de las palabras m?s representativas                                ####
  
  topic.labels = paste0("Topic",1:Ntop)
  
  levels(Panel.labels$topic) <- topic.labels
  
  orden = rev(c(1:Ntop))
  
  for (i in orden) {
    Panel.labels$topic = relevel(Panel.labels$topic, topic.labels[i])
  }
  
  Panel.labels %>% filter(topic %in% topic.labels[1:10]) %>% 
    ggplot(aes(rowname, value, fill = topic)) +
    geom_col(alpha = 1, show.legend = FALSE, width = 0.8) +
    geom_text(aes(x = rowname, y = 0, label = term), 
              hjust = "left",
              vjust = "center",
              size  = 4,
              nudge_x = 0.0,
              colour = "black") +
    facet_grid(topic ~ method, 
               scales = "free", 
               shrink = TRUE) + #  labeller = labeller(topic = topic.labels))  #names(topic.labels) <- paste0("Topic ", 1:10)
    coord_flip() + 
    scale_fill_viridis(begin = 0.3, end = 0.5, discrete = TRUE) +
    scale_x_continuous(breaks = NULL, labels = NULL, trans = "reverse") +
    theme_bw() + 
    theme(axis.text.x =  element_text(color = "black",
                                      size  = 10,
                                      angle = 300,
                                      vjust = 0,
                                      hjust = 0,
                                      margin = margin(c(5,0,0,0), unit = "pt")),
          axis.text.y =  element_text(color = "black",
                                      size = 7),
          title        = element_text(color = "black",
                                      size = 18),
          plot.margin = margin(c(10,30,10,5), unit = "pt")) +
    labs(x = NULL, y = "valor",
         title = "Palabras más representativas por cada tópico") -> q1
  
  
  Panel.labels %>% filter(topic %in% topic.labels[11:20]) %>% 
    ggplot(aes(rowname, value, fill = topic)) +
    geom_col(alpha = 1, show.legend = FALSE, width = 0.8) +
    geom_text(aes(x = rowname, y = 0, label = term), 
              hjust = "left",
              vjust = "center",
              size  = 4,
              nudge_x = 0.0,
              colour = "black") +
    facet_grid(topic ~ method, 
               scales = "free", 
               shrink = TRUE) + #  labeller = labeller(topic = topic.labels))  #names(topic.labels) <- paste0("Topic ", 1:10)
    coord_flip() + 
    scale_fill_viridis(begin = 0.5, end = 0.7, discrete = TRUE) +
    scale_x_continuous(breaks = NULL, labels = NULL, trans = "reverse") +
    theme_bw() + 
    theme(axis.text.x =  element_text(color = "black",
                                      size  = 10,
                                      angle = 300,
                                      vjust = 0,
                                      hjust = 0,
                                      margin = margin(c(5,0,0,0), unit = "pt")),
          axis.text.y =  element_text(color = "black",
                                      size = 7),
          title        = element_text(color = "black",
                                      size = 18),
          plot.margin = margin(c(10,30,10,5), unit = "pt")) +
    labs(x = NULL, y = "valor",
         title = "Palabras más representativas por cada tópico") -> q2
  
  Panel.labels %>% filter(topic %in% topic.labels[21:30]) %>% 
    ggplot(aes(rowname, value, fill = topic)) +
    geom_col(alpha = 1, show.legend = FALSE, width = 0.8) +
    geom_text(aes(x = rowname, y = 0, label = term), 
              hjust = "left",
              vjust = "center",
              size  = 4,
              nudge_x = 0.0,
              colour = "black") +
    facet_grid(topic ~ method, 
               scales = "free", 
               shrink = TRUE) + #  labeller = labeller(topic = topic.labels))  #names(topic.labels) <- paste0("Topic ", 1:10)
    coord_flip() + 
    scale_fill_viridis(begin = 0.7,  end = 0.9, discrete = TRUE) +
    scale_x_continuous(breaks = NULL, labels = NULL, trans = "reverse") +
    theme_bw() + 
    theme(axis.text.x =  element_text(color = "black",
                                      size  = 10,
                                      angle = 300,
                                      vjust = 0,
                                      hjust = 0,
                                      margin = margin(c(5,0,0,0), unit = "pt")),
          axis.text.y =  element_text(color = "black",
                                      size = 7),
          title        = element_text(color = "black",
                                      size = 18),
          plot.margin = margin(c(10,30,10,5), unit = "pt")) +
    labs(x = NULL, y = "valor",
         title = "Palabras más representativas por cada tópico") -> q3
  
  return(list(q1,q2,q3))
}
