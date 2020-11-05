# Setup -------------------------------------------------------------------

library(here)
source(here("code","code_01_setup.R"))
set.seed(1234)

# Read data ---------------------------------------------------------------

text_data  <- read_rds(here("data","text_data.rds"))

# Model -------------------------------------------------------------------

k <- 10
punctuation <- c("-","-","-","!","¡","¿","?","´")#,"'","'",""",""","«",".")

text_data               <- text_data %>% mutate(text_raw = text_raw %>% stringi::stri_trans_nfc())
text_out                <- textProcessor(text_data$text_raw, metadata = text_data, language = "es", custompunctuation = punctuation)
text_pre                <- prepDocuments(text_out$documents, text_out$vocab, text_out$meta)
text_stm_10_anyo        <- stm(text_pre$documents, text_pre$vocab, prevalence = ~anyo, data = text_pre$meta, K = 10, verbose = TRUE)
text_stm_10_anyo_effect <- estimateEffect(1:k ~ anyo, text_stm_10_anyo, meta = text_pre$meta)
text_stm_10_anyo_theta  <- text_pre$meta %>% as_tibble %>% bind_cols(text_stm_10_anyo$theta %>% set_colnames(str_c("topic_", 1:k)) %>% as_tibble)

# Transform ---------------------------------------------------------------

text_stm_10_anyo_ascii       <- text_stm_10_anyo
text_stm_10_anyo_ascii$vocab <- text_stm_10_anyo_ascii$vocab %>% str_replace("ñ", "ni")
text_stm_10_anyo_ascii$vocab <- text_stm_10_anyo_ascii$vocab %>% stringi::stri_trans_general("latin-ascii")

# Save data ---------------------------------------------------------------

text_pre                %>% write_rds(here("data", "text_pre.rds"))
text_stm_10_anyo        %>% write_rds(here("data", "text_stm_10_anyo.rds"))
text_stm_10_anyo_ascii  %>% write_rds(here("data", "text_stm_10_anyo_ascii.rds"))
text_stm_10_anyo_effect %>% write_rds(here("data", "text_stm_10_anyo_effect.rds"))
text_stm_10_anyo_theta  %>% write_rds(here("data", "text_stm_10_anyo_theta.rds"))


# STM correlation visualization -------------------------------------------

stmCorrViz_htmlwidget(stmJSON = stmJSON(text_stm_10_anyo_ascii, 
                                        documents_raw = text_out$meta$text_raw %>% stringi::stri_trans_general("latin-ascii"), 
                                        documents_matrix = text_out$documents,
                                        title = "Módelo de Tópicos Estructurales - Hacemos Memoria",
                                        clustering_threshold = 1.6,
                                        labels_number = 10,
                                        verbose = TRUE),
                      width = 1100,
                      height = 600)
stmBrowser_widget(text_stm_10_anyo_ascii, 
                  text_pre$meta, 
                  covariates = "anyo", 
                  text = "text_raw", 
                  id = "n", 
                  labeltype = "prob",
                  width = 1100,
                  height = 600)
