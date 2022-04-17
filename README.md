# Scientific Article Writing project with R markdown and Instruction page

This is solely made for My Graduate 'Spring2022,WFA 8993 Special Topic: R for Managing Wildlife and Fisheries Data' class for developing Reproducible Research Article using R markdown package.

Mississippi State University
Department of Wildlife, Fisheries and Aquaculture

# Instructions how to contribute 
## Download git software [https://git-scm.com/downloads] , and create account in www.github.com
## then configure your git software with your email and user name
## send your github user name email to hafezahmad100@gmail.com  
## I will add your name as contributor and then you will receive invitation for contribution.

# Instructions for R coding 

Three scripts
1. Data Preprocessing.R
This contains all preprocessing related code.

 a.data extraction, and formatting 
 
 b.Atmospheric correction 
 
 c. DN to Top of Reflectance,to land surface temperature
 
2. Data analysis Classification and Mapping.R
This contains all data analysis, land cover classification and mapping related code. 
3. R Reproducible Scientific Article.Rmd
This is the main R markdwon script which has all writing and formatting texts.

Project File (R Reproducible Scientific Article.Rproj)

## We can use citation add symbol 
[@Mukhopadhyay2018] This for single citation
[@Mukhopadhyay2018,@leidolf2002flora] Multiple citation 

I am adding all references in  the 'r-references.bib' file. You can also added.

## Initiate the upstream tracking of the project on the GitHub repo 

git remote add origin <https://github.com/hafez-ahmad/R-markdown-article-class.git>

## Pull all files from the GitHub repo (typically just readme, license, gitignore)

git pull origin master/ main

Remember 
## Pull means download 
## Push means upload
## Please comments for what your are doing and then push it

## Set up GitHub repo to track changes on local machine 

git push -u origin master