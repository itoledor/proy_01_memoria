# Setup -------------------------------------------------------------------

library(here)
source(here("code","code_01_setup.R"))
set.seed(1234)

# Read data ---------------------------------------------------------------

text_data  <- read_rds(here("data","text_data.rds"))

# Model -------------------------------------------------------------------

text_out <- textProcessor(text_data$text_raw, metadata = text_data, language = "es")
text_pre <- prepDocuments(text_out$documents, text_out$vocab, text_out$meta)

plan(multiprocess)

many_models <- tibble(K = c(10, 20, 25, 30, 35, 40, 50, 60))
many_models <- many_models %>% mutate(topic_model = future_map(K, ~stm(text_pre$documents, text_pre$vocab, K = .,verbose = FALSE)))
heldout     <- make.heldout(text_pre$documents, text_pre$vocab)

k_result <- many_models 
k_result <- k_result %>% mutate(exclusivity        = map(    topic_model, exclusivity))
k_result <- k_result %>% mutate(semantic_coherence = map(    topic_model, semanticCoherence, text_pre$documents))
k_result <- k_result %>% mutate(eval_heldout       = map(    topic_model, eval.heldout, heldout$missing))
k_result <- k_result %>% mutate(residual           = map(    topic_model, checkResiduals, text_pre$documents))
k_result <- k_result %>% mutate(bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)))
k_result <- k_result %>% mutate(lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)))
k_result <- k_result %>% mutate(lbound             = bound + lfact)
k_result <- k_result %>% mutate(iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

plot_k_diagnostics <- k_result
plot_k_diagnostics <- plot_k_diagnostics %>% mutate(`Lower bound`         = lbound)
plot_k_diagnostics <- plot_k_diagnostics %>% mutate( Residuals            = map_dbl(residual, "dispersion"))
plot_k_diagnostics <- plot_k_diagnostics %>% mutate(`Semantic coherence`  = map_dbl(semantic_coherence, mean))
plot_k_diagnostics <- plot_k_diagnostics %>% mutate(`Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout"))
plot_k_diagnostics <- plot_k_diagnostics %>% select(K, `Lower bound`,Residuals,`Semantic coherence`,`Held-out likelihood`)
plot_k_diagnostics <- plot_k_diagnostics %>% gather(Metric, Value, -K)
plot_k_diagnostics <- plot_k_diagnostics %>% ggplot(aes(K, Value, color = Metric))
plot_k_diagnostics <- plot_k_diagnostics %+% geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE)
plot_k_diagnostics <- plot_k_diagnostics %+% facet_wrap(~Metric, scales = "free_y")
plot_k_diagnostics <- plot_k_diagnostics %+% labs(x = "K (número de tópicos)")
plot_k_diagnostics <- plot_k_diagnostics %+% labs(y = NULL)
plot_k_diagnostics <- plot_k_diagnostics %+% labs(title = "Diagnóstico del modelo por número de tópicos")
plot_k_diagnostics <- plot_k_diagnostics %+% labs(subtitle = "Este diagnóstico indica que un buen número de tópicos estaría alrededor de 30")

k_exclusivity_semantic <-      k_result
k_exclusivity_semantic <-      k_exclusivity_semantic %>% select(K, exclusivity, semantic_coherence)
k_exclusivity_semantic <-      k_exclusivity_semantic %>% filter(K %in% c(10, 30, 50))
k_exclusivity_semantic <-      k_exclusivity_semantic %>% unnest(cols = c(exclusivity, semantic_coherence))
k_exclusivity_semantic <-      k_exclusivity_semantic %>% mutate(K = as.factor(K))

plot_k_exclusivity_semantic <-      k_exclusivity_semantic %>% ggplot(aes(semantic_coherence, exclusivity, color = K)) 
plot_k_exclusivity_semantic <- plot_k_exclusivity_semantic %+% geom_point(size = 2, alpha = 0.7) 
plot_k_exclusivity_semantic <- plot_k_exclusivity_semantic %+% labs(x = "Coherencia Semántica")
plot_k_exclusivity_semantic <- plot_k_exclusivity_semantic %+% labs(y = "Exclusividad")
plot_k_exclusivity_semantic <- plot_k_exclusivity_semantic %+% labs(title = "Comparación entre coherencia semántica y exclusividad")
plot_k_exclusivity_semantic <- plot_k_exclusivity_semantic %+% labs(subtitle = str_wrap("Lo modelos con menos tópicos tienen mayor coherencia pero menor exclusividad", 70))

# Write data --------------------------------------------------------------

ggsave(here("figs", "stm-diagnostico.png")   , plot_k_diagnostics           , width =  6.3, height =  6)
ggsave(here("figs", "stm-diagnostico_2.png") , plot_k_exclusivity_semantic  , width =  6, height =  6)

