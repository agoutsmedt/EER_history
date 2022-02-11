
source(here::here("Script_paths_and_basic_objects_EER.R"))

require(arrow)

all_refs_ids <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_refs_ids.RDS"))
all_refs_noids <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_refs_noids.RDS"))
all_ref <- rbind(all_refs_ids,all_refs_noids)

# saveRDS(all_ref, here(data_path, "macro_AA","OST_generic_data", "all_ref.RDS"))

all_ref <- readRDS(here(data_path, "macro_AA","OST_generic_data", "all_ref.RDS"))

arrow::write_parquet(all_ref, here(data_path,"macro_AA","OST_generic_data","all_ref.parquet"), compression = "gzip")

arrow::write_parquet(all_refs_ids,(here(eer_data,"refs_parquet", compression = "gzip")))

in_value <-3319714

test <- read_parquet(here(data_path,"macro_AA","OST_generic_data","all_ref.parquet"), as_data_frame = FALSE)
refs <- test %>% filter(New_id2 %in% in_value) %>% collect()
