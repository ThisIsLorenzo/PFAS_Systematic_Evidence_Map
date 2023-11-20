# A research synthesis of humans, animals, and environmental compartments exposed to PFAS: A systematic evidence map and bibliometric analysis
## DATA WAREHOUSING
This repository contains bibliographic files, data and analysis code for this Systematic Evidence Map. The different folders and their content are described below. Please feel free to contact Lorenzo Ricolfi (l.ricolfi@unsw.edu.au) if you need assistance navigating these documents.

## /data/
This folder contains the extracted data from the included studies (except for ```/pfas_info.csv```, which contains fixed data). Data are organised in different .csv files.
- ```/main.csv```: includes the extracted data for the systematic map.
- ```/amstar2.csv```: presents the questions and answers of the AMSTAR2 scheme for quality assessment applied to each included systematic review.
- ```/pfas_info.csv```: provides the fixed information on the PFAS types, including their abbreviations, CAS number and length of the carbon chain. It is not a data extraction table.
- ```/pfas_types.csv```: holds the list of all types of PFAS investigated in the included systematic reviews.
- ```/species.csv```: contains information on all species (other than Homo sapiens) investigated in the included systematic reviews.
- ```/scopus.bib```: bibliographic data downloaded from Scopus - bibliometric analysis (systematic-like reviews)
- ```/bib.csv```: data frame created from bibliographic data downloaded from Scopus (please see "Loading data" in /analysis_code.RMD)- bibliometric analysis (systematic-like reviews)
- ```/main_all_types_review```: all types of reviews file downloaded from Rayyan at the end of the screening process.
- ```/scopus_all_types.csv```: bibliographic data downloaded from Scopus - supplementary bibliometric analysis (all types of review)
- ```/bib_all_types.csv```: data frame created from bibliographic data downloaded from Scopus (please see "Loading data" in /analysis_code.RMD)- bibliometric analysis (all types of review)

## /R/
In this folder, we stored files providing deduplication code, analysis code, and Shiny app developmental code.
- ```/analysis_code.RMD```: Analysis code used for data processing and visualization.
### /deduplication_code/
Folder containing .RMD files on deduplication of documents exported from searched databases.
- ```/main_search_deduplication.RMD```: R Markdown for deduplication of bibliographic records
- ```/update_deduplication.RMD```: R Markdown for deduplication of bibliographic records (update search conducted in 2023)

### PFAS_SEM_Shiny_App
- ```/```:
- ```/```:

## /Figs/
Folder holding all figures generated by the analysis code (please see PFAS_Systematic_Evidence_Map/R/analysis_code.RMD).
- ```Fig.1.png```: please note this figure is not generated through R code
- ```Fig.2.png```: please note this figure is not generated through R code
- ```Fig.3.png```
- ```Fig.4.png```
- ```Fig.5.png```
- ```Fig.6.png```
- ```Fig.7.png```
- ```Fig.8.pdf```

### /Suppl_Figs/
Folder holding all supplementary figures generated by the analysis code (please see PFAS_Systematic_Evidence_Map/R/analysis_code.RMD).
- ```SFig.1.png```: please note this figure is not generated through R code
- ```SFig.2.png```
- ```SFig.3.png```
- ```SFig.4.png```
- ```SFig.5.png```
- ```SFig.6.png```
- ```SFig.7.png```
- ```SFig.8.png```
- ```SFig.9.png```
- ```SFig.10.png```
- ```SFig.11.png```
- ```SFig.12.png```
- ```SFig.13.png```
- ```SFig.14.png```
- ```SFig.15.png```
- ```SFig.16.png```
- ```SFig.17.png```
- ```SFig.18.png```
- ```SFig.19.png```
- ```SFig.20.pdf```
- ```SFig.21.pdf```
- ```SFig.22.pdf```
- ```SFig.23.png```
- ```SFig.24.png```
- ```SFig.25.png```
- ```SFig.26.pdf```
- ```SFig.27.pdf```
- ```SFig.28.pdf```

## /bibliographic_searches/ 
All the bibliographic files generated during literature searches and subsequently utilized for eligibility assessment are included in this folder. Files are organised in two folders: 'databases' and 'deduplication_process'.
### /databases/
In this folder, you will find bibliographic records generated through database searches. Please be aware that records obtained from Scopus or Web of Science had to be divided into multiple files due to the importing limitations imposed by the databases. Other databases are divided into multiple files (e.g., Prospero in ```update_search```) because they represent outputs of different search strings. Bibliographic records are organised in a different folder for each search (i.e., main, update, and supplementary searches).
#### /main_search/
- ```/scopus.csv```: bibliographic records from the Scopus database
- ```/wos_1.ciw```: bibliographic records from the Web of Science database
- ```/wos_2.ciw```: bibliographic records from the Web of Science database
- ```/pubmed.csv```: bibliographic records from the PubMed database
- ```/cochrne.csv```: bibliographic records from the Cochrane database
- ```/prospero.ris```: bibliographic records from the Prospero database
- ```/epistemonikos.ris```: bibliographic records from the Epistemonikos database
- ```/apo.ris```: bibliographic records from the Australian Policy Observatory database
#### /update_search/
This folder contains the bibliographic records retrieved during the update search conducted in February 2023.
- ```/scopus.csv```: bibliographic records from the Scopus database
- ```/wos.ris```: bibliographic records from the Web of Science database
- ```/pubmed.nbib```: bibliographic records from the PubMed database
- ```/cochrne.ris```: bibliographic records from the Cochrane database
- ```/prospero_1.ris```: bibliographic records from the Prospero database
- ```/prospero_2.ris```: bibliographic records from the Prospero database
- ```/epistemonikos_1.ris```: bibliographic records from the Epistemonikos database
- ```/epistemonikos_2.ris```: bibliographic records from the Epistemonikos database
- ```/epistemonikos_3.ris```: bibliographic records from the Epistemonikos database
- ```/biorxiv_1.ris```: bibliographic records from the Biorxiv database
- ```/bioxriv_2.ris```: bibliographic records from the Biorxiv database
- ```/base.ris```: bibliographic records from the BASE database
#### /supplementary_search/
This folder contains the bibliographic records retrieved during the supplementary search aiming to find all types of review (i.e., systematic and no systematic) on PFAS exposure
- ```/scopus_1.csv```
- ```/scopus_2.csv```

### /deduplication_process/
Contained within this folder are all bibliographic records both before and following the process of deduplication.
#### /deduplication_main_search/
- ```bibliographic records before deduplication.ris```: bibliographic records before deduplication
- ```articles.csv```: bibliographic records after deduplication
#### /deduplication_update_search/
- ```COMBINED_SRs_2022_update.csv```: bibliographic records before deduplication
- ```SRs_2022_update_deduplicated.csv```:bibliographic records after deduplication
