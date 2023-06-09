# A research synthesis of humans, animals, and environmental compartments exposed to PFAS: A systematic evidence map and bibliometric analysis
This repository contains bibliographic files, data and analysis code for this project. The different folders and their content is described below. Feel free to contact Lorenzo Ricolfi (l.ricolfi@unsw.edu.au) if you need assistance navigating these documents.

## data/
This folder contains the extracted data from the included studies (except for ```/pfas_info.csv```, which contains fixed data). Data are organised in different .csv files.
- ```/main.csv```: includes the extracted data for the systematic map.
- ```/amstar2.csv```: presents the questions and answers of the AMSTAR2 scheme for quality assessment applied to each included systematic review.
- ```/pfas_info.csv```: provides the fixed information on the PFAS types, including their abbreviations, CAS number and length of the carbon chain. It is not a data extraction table.
- ```/pfas_types.csv```: holds the list of all types of PFAS investigated in the included systematic reviews.
- ```/species.csv```: contains information on all species (other than Homo sapiens) investigated in the included systematic reviews.

## R/
In this folder, we stored files providing deduplication code and analysis code.
- ```/analysis_code.RMD```: Analysis code used for data processing and visualization.
### /deduplication_code/
Folder containing .RMD files on deduplication of documents exported from searched databases.
- ```/main_search_deduplication.RMD```: 
- ```/update_deduplication.RMD```:

## Figs/
Folder holding all figures generated by the analysis code.
- ```...```:
- ```...```:
- ```...```:
- ```...```:
- ```...```:
- ```...```:
- ```...```:

## bibliographic_searches/ 
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
- ```/scopus.csv```
- ```/wos.ris```
- ```/pubmed.nbib```
- ```/cochrne.ris```
- ```/prospero_1.ris```
- ```/prospero_2.ris```
- ```/epistemonikos_1.ris```
- ```/epistemonikos_2.ris```
- ```/epistemonikos_3.ris```
- ```/biorxiv_1.ris```
- ```/bioxriv_2.ris```
- ```/base.ris```
#### /supplementary_search/
- ```/scopus_1.csv```
- ```/scopus_2.csv```

### /deduplication_process/
Contained within this folder are all bibliographic records both before and following the process of deduplication.
#### /deduplication_main_search/
- ```...```:
#### /deduplication_update_search/
- ```...```:




