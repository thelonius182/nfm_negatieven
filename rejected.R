library(readr)
library(data.table)
library(dplyr)
tbl_rejected <- fread(file = "f:/documenten/cz_docs/Negatieven David/rejected.txt", 
                      sep = "¶",
                      stringsAsFactors = FALSE, 
                      encoding = "UTF-8")

tbl_rejected %<>% 
  mutate(negatiefnaam = sub("^(.*)(\\.tif$)", "\\1", negatiefnaam, perl=TRUE))

write_tsv(x = tbl_rejected_tif, 
          path = "f:/documenten/cz_docs/Negatieven David/rejected_issue.txt")

tbl_rejected_split <- tbl_rejected %>% 
  mutate(referentie = sub("^(EDJ-(Kb)?[0-9]{1,5}[a-z]?-h?[0-9]{1,3}) (.*)", "\\1", negatiefnaam, perl=TRUE),
         omschr = sub("^(EDJ-(Kb)?[0-9]{1,5}[a-z]?-h?[0-9]{1,3}) (.*)", "\\3", negatiefnaam, perl=TRUE)) %>% 
  select(-negatiefnaam) %>% 
  arrange(referentie)

tbl_rejected_issue <- tbl_rejected_split %>% 
  dplyr::filter(referentie == omschr) %>% 
  select(-omschr)

write_tsv(x = tbl_rejected_issue, 
          path = "f:/documenten/cz_docs/Negatieven David/rejected_issue.txt")

tbl_rejected_n <- tbl_rejected_split %>% 
  group_by(referentie) %>% 
  summarise(n1 = n()) 

tbl_rejected_keys_dubbel <- tbl_rejected_n %>% 
  dplyr::filter(n1 > 1)

tbl_rejected_dubbel <- tbl_rejected_split %>% 
  dplyr::filter(referentie %in% tbl_rejected_keys_dubbel$referentie)

write_tsv(x = tbl_rejected_dubbel, 
          path = "f:/documenten/cz_docs/Negatieven David/rejected_dubbel.txt")

tbl_rejected_keys_clean <- tbl_rejected_n %>% 
  dplyr::filter(n1 == 1)

tbl_rejected_clean <- tbl_rejected_split %>% 
  dplyr::filter(referentie %in% tbl_rejected_keys_clean$referentie) %>% 
  separate(col = referentie, sep = "-", into = c("ref_a", "ref_b", "ref_c"), remove = F) %>% 
  mutate(ref_b = sub("^(Kb)?([0-9]+)([^0-9])?", "\\1¶\\2¶\\3", ref_b, perl=TRUE)) %>% 
  separate(col = ref_b, sep = "¶", into = c("ref_b1", "ref_b2", "ref_b3")) %>% 
  mutate(ref_b1 = as.numeric(as.factor(ref_b1)),
         ref_b2 = str_pad(ref_b2, width = 5, pad = "0"),
         ref_b3 = as.numeric(as.factor(ref_b3)),
         ref_c = sub("^(h)?([0-9]+)", "\\1¶\\2", ref_c, perl=TRUE)) %>% 
  separate(col = ref_c, sep = "¶", into = c("ref_c1", "ref_c2")) %>% 
  mutate(ref_c1 = as.numeric(as.factor(ref_c1)),
         ref_c2 = str_pad(ref_c2, width = 3, pad = "0"),
         sort_ref = paste(ref_a, ref_b1, ref_b2, ref_b3, ref_c1, ref_c2, sep = "-")) %>% 
  select(-starts_with("ref_")) %>% 
  arrange((sort_ref))
  
write_tsv(x = tbl_rejected_clean, 
          path = "f:/documenten/cz_docs/Negatieven David/rejected_clean.txt")
