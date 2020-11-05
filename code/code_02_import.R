# Setup -------------------------------------------------------------------

library(here)
source(here("code","code_01_setup.R"))

# Read data ---------------------------------------------------------------

text_index <- read_csv2(here("raw", "indice.csv"))
text_raw   <- read_document(here("raw", "memoria.txt"), encoding = "UTF-8")
text_data  <- tibble(text_raw = text_raw)

# Tidy data ---------------------------------------------------------------

text_data <- text_data %>% filter(text_raw %in% as.character(2015:2019) %>% not)
text_data <- text_data %>% left_join(text_index, by = c("text_raw" = "titulo"))
text_data <- text_data %>% left_join(text_index, by = c("anyo", "n", "p"))
text_data <- text_data %>% fill(-text_raw, .direction = "down")
text_data <- text_data %>% filter(text_raw %>% equals(titulo) %>% not)
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("e ?n ?e ?r ?o",                "enero"       ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("f ?e ?b ?r ?e ?r ?o",          "febrero"     ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("m ?a ?r ?z ?o",                "marzo"       ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("a ?b ?r ?i ?l",                "abril"       ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("m ?a ?y ?o",                   "mayo"        ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("j ?u ?n ?i ?o",                "junio"       ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("j ?u ?l ?i ?o",                "julio"       ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("a ?g ?o ?s ?t ?o",             "agosto"      ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("s ?e ?p ?t ?i ?e ?m ?b ?r ?e", "septiembre"  ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("o ?c ?t ?u ?b ?re",            "octubre"     ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("n ?o ?v ?i ?e ?m ?b ?r ?e",    "noviembre"   ))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_replace("d ?i ?c ?i ?e ?m ?b ?r ?e",    "diciembre"   ))
text_data <- text_data %>% mutate(date = text_raw %>% str_extract("^ ?\\d ?\\d? +de +(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre) +de +\\d\\d\\d\\d\\.?$"))
text_data <- text_data %>% group_by(titulo)
text_data <- text_data %>% fill(date, .direction = "updown")
text_data <- text_data %>% anti_join(text_data %>% filter(text_raw %>% str_detect("^ ?\\d ?\\d? +de +(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre) +de +\\d\\d\\d\\d\\.?$")))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_c(collapse = " "))
text_data <- text_data %>% distinct()
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_remove("^Video ?"))
text_data <- text_data %>% mutate(text_raw = text_raw %>% str_remove("^Audio ?"))

text_data <- text_data %>% mutate(date = date %>% str_replace("enero"       , "january"   ))
text_data <- text_data %>% mutate(date = date %>% str_replace("febrero"     , "february"  ))
text_data <- text_data %>% mutate(date = date %>% str_replace("marzo"       , "march"     ))
text_data <- text_data %>% mutate(date = date %>% str_replace("abril"       , "april"     ))
text_data <- text_data %>% mutate(date = date %>% str_replace("mayo"        , "may"       ))
text_data <- text_data %>% mutate(date = date %>% str_replace("junio"       , "june"      ))
text_data <- text_data %>% mutate(date = date %>% str_replace("julio"       , "july"      ))
text_data <- text_data %>% mutate(date = date %>% str_replace("agosto"      , "august"    ))
text_data <- text_data %>% mutate(date = date %>% str_replace("septiembre"  , "september" ))
text_data <- text_data %>% mutate(date = date %>% str_replace("octubre"     , "october"   ))
text_data <- text_data %>% mutate(date = date %>% str_replace("noviembre"   , "november"  ))
text_data <- text_data %>% mutate(date = date %>% str_replace("diciembre"   , "december"  ))
text_data <- text_data %>% mutate(date = date %>% str_replace(" de "        , " "         ))
text_data <- text_data %>% mutate(date = date %>% str_replace(" "           , " "         ))
text_data <- text_data %>% mutate(date = date %>% str_remove("\\."))
text_data <- text_data %>% mutate(date = date %>% lubridate::parse_date_time("d! b! Y!"))
text_data <- text_data %>% mutate(date = date %>% lubridate::as_date())


# Write data --------------------------------------------------------------

text_data %>% write_rds(here("data","text_data.rds"))
text_data %>% write_excel_csv2(here("data","text_data.csv"))

