library(readr)
library(data.table)
library(dplyr)

tbl_selected <- fread(file = "f:/documenten/cz_docs/Negatieven David/selected.txt", 
                      sep = "¶",
                      stringsAsFactors = FALSE, 
                      encoding = "UTF-8")

tbl_selected %<>% 
  mutate(negatiefnaam = sub("^(.*)(\\.tif$)", "\\1", negatiefnaam, perl=TRUE))

tbl_selected_split <- tbl_selected %>% 
  mutate(referentie = sub("^(EDJ-(Kb)?[0-9]{1,5}[a-z]?-h?[0-9]{1,3}) (.*)", "\\1", negatiefnaam, perl=TRUE),
         omschr = sub("^(EDJ-(Kb)?[0-9]{1,5}[a-z]?-h?[0-9]{1,3}) (.*)", "\\3", negatiefnaam, perl=TRUE)) %>% 
  select(-negatiefnaam) %>% 
  arrange(referentie)

tbl_selected_issue <- tbl_selected_split %>% 
  dplyr::filter(referentie == omschr) %>% 
  select(-omschr)

write_tsv(x = tbl_selected_issue, 
          path = "f:/documenten/cz_docs/Negatieven David/selected_issue.txt")

tbl_selected_split %<>% 
  dplyr::filter(!referentie %in% tbl_selected_issue$referentie)

tbl_selected_n <- tbl_selected_split %>% 
  group_by(referentie) %>% 
  summarise(n1 = n()) 

tbl_selected_keys_dubbel <- tbl_selected_n %>% 
  dplyr::filter(n1 > 1)

tbl_selected_dubbel <- tbl_selected_split %>% 
  dplyr::filter(referentie %in% tbl_selected_keys_dubbel$referentie)

write_tsv(x = tbl_selected_dubbel, 
          path = "f:/documenten/cz_docs/Negatieven David/selected_dubbel.txt")

tbl_selected_keys_clean <- tbl_selected_n %>% 
  dplyr::filter(n1 == 1)


tbl_selected_clean <- tbl_selected_split %>% 
  dplyr::filter(referentie %in% tbl_selected_keys_clean$referentie) %>% 
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

write_tsv(x = tbl_selected_clean, 
          path = "f:/documenten/cz_docs/Negatieven David/selected_clean.txt")
