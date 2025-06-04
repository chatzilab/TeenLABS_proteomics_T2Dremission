library(limma)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(KEGGREST)
library(dplyr)

#############
# Gene list #
#############

kegg_pathway <- getKEGGPathwayNames("hsa")

gene_list <- getGeneKEGGLinks("hsa")
gene_list$Symbol <- mapIds(
  org.Hs.eg.db,
  gene_list$GeneID,
  column = "SYMBOL",
  keytype = "ENTREZID"
)

gene_list <- left_join(
  gene_list,
  kegg_pathway,
  by = "PathwayID"
)

gene_list$Description <- gsub(" - Homo sapiens \\(human)", "", gene_list$Description)

sum(duplicated(gene_list$GeneID)) #28,518

length(unique(gene_list$PathwayID)) #360

gene_list_compressed <- gene_list %>% 
  dplyr::select(-GeneID) %>% 
  dplyr::group_by(Symbol) %>% 
  reframe(
    PathwayID = paste(PathwayID, collapse = "; "),
    Description = paste(Description, collapse = "; ")
  )

sum(duplicated(gene_list_compressed$Symbol)) # No duplicates

saveRDS(
  gene_list_compressed,
  fs::path(
    dir_project,
    "0_Data",
    "assay_pathway_KEGG.rds"
  )
)
