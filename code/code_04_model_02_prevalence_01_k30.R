# Setup -------------------------------------------------------------------

library(here)
source(here("code","code_01_setup.R"))
library(stminsights)

# Read data ---------------------------------------------------------------

text_data  <- read_rds(here("data","text_data.rds"))

# Model -------------------------------------------------------------------

text_data               <- text_data %>% mutate(text_raw = text_raw %>% stringi::stri_trans_nfc())
text_out                <- textProcessor(text_data$text_raw, metadata = text_data, language = "es")
text_pre                <- prepDocuments(text_out$documents, text_out$vocab, text_out$meta)
text_stm_30_anyo        <- stm(text_pre$documents, text_pre$vocab, prevalence = ~anyo, data = text_pre$meta, K = 30, verbose = TRUE)
text_stm_30_anyo_effect <- estimateEffect(1:30 ~ anyo, text_stm_30_anyo, meta = text_pre$meta)

out <- list(documents = text_out$documents,
            vocab     = text_out$vocab,
            meta      = text_out$meta)

save.image(here("data", "memoria.RData"))



# STM insights ------------------------------------------------------------

load(here("data", "memoria.RData"))
stminsights::run_stminsights()


# STM top words -----------------------------------------------------------

text_stm_top <- top_words(text_stm_30, text_out$documents)
ggsave(here("figs", "top_words_1.png"), text_stm_top[[1]],  height = 24, width = 8)
ggsave(here("figs", "top_words_2.png"), text_stm_top[[2]],  height = 24, width = 8)
ggsave(here("figs", "top_words_3.png"), text_stm_top[[3]],  height = 24, width = 8)


# STM to LDAvis -----------------------------------------------------------

stm::toLDAvis(text_stm_30, text_out$documents)


# STM correlation visualization -------------------------------------------

text_stm_corr <- topicCorr(text_stm_30)
stmCorrViz(text_stm_30, file_out = "corrviz.html", text_data$text_raw)


# SRM Browser -------------------------------------------------------------

stmBrowser(text_stm_30, text_pre)
 

