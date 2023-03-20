#'---
#'title: "Codebook | Presidents and Vice-Presidents of the Federal Courts of Germany (PVP-FCG)"
#'author: Seán Fobbe and Tilko Swalve
#'geometry: margin=3cm
#'papersize: a4
#'fontsize: 11pt
#'output:
#'  pdf_document:
#'    toc: true
#'    toc_depth: 3
#'    number_sections: true
#'    pandoc_args: --listings
#'    includes:
#'      in_header: General_Source_TEX_Preamble_EN.tex
#'      before_body: [PVP-FCG_Source_TEX_CodebookTitle.tex]
#'bibliography: packages.bib
#'nocite: '@*'
#' ---

#'\newpage

#+ echo = FALSE 
knitr::opts_chunk$set(dev = c("pdf", "png"),
                      fig.align = "center",
                      dpi = 300,
                      fig.path = paste0(getwd(),
                                        "/ANALYSIS/"),
                      echo = FALSE,
                      warning = FALSE,
                      message = FALSE)

 



############################
### Packages
############################

#+
library(knitr)
library(kableExtra)
library(magick)
library(parallel)
library(data.table)
library(ggplot2)


## Write Citations to Disk
knitr::write_bib(c(.packages()),
                 "packages.bib")





############################
### Parameters
############################

## Name of Data Set
datasetname <- "PVP-FCG"

## DOI of Data Set Concept
doi.concept <- "10.5281/zenodo.4568681"


## DOI of Data Set Version
doi.version <- "10.5281/zenodo.4568682"

## Date Stamp
datestamp <- Sys.Date()

## Output Directory (must end with a slash)

outputdir <- paste0(getwd(),
                    "/output/") 


dir.create(outputdir)



###############################
### Read Handcoded Source Files
###############################


## Read CSV
source.president <- fread("PVP-FCG_Source_Handcoded_Presidents.csv",
                          na.strings = "NA")

source.vpresident <- fread("PVP-FCG_Source_Handcoded_VicePresidents.csv",
                           na.strings = "NA")


## Format as IDate
date.cols <- c("term_begin_date",
               "term_end_date",
               "birth_date",
               "death_date")

source.president <- source.president[, c(date.cols) := lapply(.SD, as.IDate), .SDcols = date.cols]
source.vpresident <- source.vpresident[, c(date.cols) := lapply(.SD, as.IDate), .SDcols = date.cols]





###############################
### Define Function
###############################

# f.age: Calculate human age
# Calculates age from date of birth to target date. Vectorized.

# @param birthdate Date of birth
# @param target Target date

f.age <- function(birthdate, target){
    if (is.na(birthdate) | is.na(target)){
        return(NA)
    }else{
        age <- year(target) - year(birthdate) - 1
        if (month(target) > month(birthdate)){
            age <- age + 1
        }
        if (month(target) == month(birthdate) && mday(target) >= mday(birthdate)){
            age <- age + 1
        }
        return(age)
    }
}

f.age <- Vectorize(f.age)


# f.freqtable: Create frequency tables with summary row

# @param x A data.table
# @param varname The variable for which the frequency table is to be created.


f.freqtable <- function(x, varname){

    freqtable <- x[, .N, keyby=c(paste0(varname))]
    
    freqtable[, c("roundedpercent",
                  "cumulpercent") := {
                      exactpercent  <-  N/sum(N)*100
                      roundedpercent <- round(exactpercent, 2)
                      cumulpercent <- round(cumsum(exactpercent), 2)
                      list(roundedpercent,
                           cumulpercent)}]
    
    colsums <-  cbind("Total",
                      freqtable[, lapply(.SD, function(x){round(sum(x))}),
                                .SDcols = c("N",
                                            "roundedpercent")
                                ], round(max(freqtable$cumulpercent)))
    
    colnames(colsums)[c(1,4)] <- c(varname, "cumulpercent")
    freqtable <- rbind(freqtable, colsums)
    
    return(freqtable)
}




###############################
### Calculate Ages and Years
###############################

out.president <- source.president
out.vpresident <- source.vpresident


## Age: Death

out.president$death_age <- f.age(out.president$birth_date,
                                 out.president$death_date)

out.vpresident$death_age <- f.age(out.vpresident$birth_date,
                                  out.vpresident$death_date)

## Age: Begin of Term

out.president$term_begin_age <- f.age(out.president$birth_date,
                                      out.president$term_begin_date)

out.vpresident$term_begin_age <- f.age(out.vpresident$birth_date,
                                       out.vpresident$term_begin_date)

## Age: End of Term

out.president$term_end_age <- f.age(out.president$birth_date,
                                    out.president$term_end_date)

out.vpresident$term_end_age <- f.age(out.vpresident$birth_date,
                                     out.vpresident$term_end_date)




## Year: Begin of Term

out.president$term_begin_year <- year(out.president$term_begin_date)
out.vpresident$term_begin_year <- year(out.vpresident$term_begin_date)


## Year: End of Term

out.president$term_end_year <- year(out.president$term_end_date)
out.vpresident$term_end_year <- year(out.vpresident$term_end_date)


## Year: Birth

out.president$birth_year <- year(out.president$birth_date)
out.vpresident$birth_year <- year(out.vpresident$birth_date)

out.vpresident[name_last == "Hund" & name_first == "Michael"]$birth_year <- 1946
out.vpresident[name_last == "Christ" & name_first == "Josef"]$birth_year <- 1956
out.vpresident[name_last == "Korbmacher" & name_first == "Andreas"]$birth_year <- 1960


## Year: Death

out.president$death_year <- year(out.president$death_date)
out.vpresident$death_year <- year(out.vpresident$death_date)



###############################
### ADD DOIs and Version
###############################

out.president$doi_concept <- rep(doi.concept, out.president[,.N])
out.vpresident$doi_concept <- rep(doi.concept, out.vpresident[,.N])

out.president$doi_version <- rep(doi.version, out.president[,.N])
out.vpresident$doi_version <- rep(doi.version, out.vpresident[,.N])

out.president$version <- rep(datestamp, out.president[,.N])
out.vpresident$version <- rep(datestamp, out.vpresident[,.N])




###############################
### Write CSV to Disk
###############################



fwrite(out.president,
       paste(datasetname,
             datestamp,
             "GermanFederalCourts_Presidents.csv",
             sep = "_"),
       na = "NA")


fwrite(out.vpresident,
       paste(datasetname,
             datestamp,
             "GermanFederalCourts_VicePresidents.csv",
             sep = "_"),
       na = "NA")



############################
### Begin Text
############################


#'# Introduction

#' The Federal Courts are the highest courts of the Federal Republic of Germany. Each Federal Court is headed by a President and a Vice-President. The \textbf{\datatitle\ (\datashort)} data set is an attempt to exhaustively document the leadership of German apex courts to facilitate computational analysis of their composition, possible effects on the jurisprudence of their respective courts and their handling of internal administrative matters.
#'
#' The structure of the German court system is somewhat unusual in that there is not one primary federal supreme court, but many partially overlapping federal supreme court jurisdictions. At the apex stands the Federal Constitutional Court (\emph{Bundesverfassungsgericht}), which is not a supreme court in the traditional sense, but a dedicated constitutional court. While the Federal Constitutional Court often takes on the role of supreme court in fact, as a matter of law it exclusively reviews the interpretation and application of constitutional law by specialized courts (\emph{Fachgerichte}).
#'
#' Instead of a single Supreme Court there exist five different supreme court jurisdictions (civil/criminal law, administrative law, social law, labor law and tax law), each of which is assigned to a specific federal supreme court (Federal Court of Justice, Federal Administrative Court, Federal Social Court, Federal Labor Court and Federal Finance Court). A Joint Senate of the Highest Federal Courts (\emph{Gemeinsamer Senat der Obersten Gerichtshöfe des Bundes}) can take on the role of Federal Supreme Court on an ad-hoc basis, but rarely sits. It is composed of the leading judges of each of the federal supreme court jurisdictions.
#'
#' Not all Federal Courts are supreme courts in their respective jurisdictions. The Federal Patent Court (\emph{Bundespatentgericht}) and the Federal Courts of Military Discipline (\emph{Truppendienstgerichte}) are endowed with high-level jurisdiction over patent matters and military discipline, but their decisions may still be reviewed by the Federal Court of Justice and the Federal Administrative Court, respectively. The Federal Disciplinary Court (\emph{Bundesdisziplinargericht}) was abolished on 31 December 2003 and its jurisdiction transferred to the general administrative courts.
#'
#' 
#' The quantitative analysis of legal data in Germany is still in its infancy, a situation which is exacerbated by the lack of high-quality empirical data. Most advanced data sets are held in commercial databases and are generally unavailable to academic researchers, journalists and the wider public. With this data we hope to contribute to a more systematic and empirical view of the German court system. In a modern nation founded on the rule of law the activities of the judiciary must be public, transparent and defensible. In the 21st century this requires quantitative scientific review of decisions and actions. 
#'
#' Design, construction and compilation of this data set are based on the principles of general availability through freedom from copyright (public domain status), strict transparency and full scientific reproducibility. The FAIR Guiding Principles for Scientific Data Management and Stewardship (Findable, Accessible, Interoperable and Reusable) inspire both the construction and the manner of publication of this data set.\footnote{Wilkinson, M., Dumontier, M., Aalbersberg, I. et al. The FAIR Guiding Principles for Scientific Data Management and Stewardship. Sci Data 3, 160018 (2016). \url{https://doi.org/10.1038/sdata.2016.18}}





#+
#'# Data Set Construction

#+
#'## Description

#'The Federal Courts are the highest courts of the Federal Republic of Germany. Each Federal Court is headed by a President and a Vice-President. The \textbf{\datatitle\ (\datashort)} data set is an attempt to exhaustively document the leadership of German apex courts to facilitate computational analysis of their composition, possible effects on the jurisprudence of their respective courts and their handling of internal administrative matters.
#'
#' As of this version we have processed all Presidents and Vice-Presidents of the German Constitutional Court (\emph{Bundesverfassungsgericht}), German Federal Court of Justice (\emph{Bundesgerichtshof}) and German Federal Administrative Court  (\emph{Bundesverwaltungsgericht}). Data on further courts will be added in the foreseeable future.




#+
#'## Data Sources

#'\begin{centering}
#'\begin{longtable}{P{5cm}p{9cm}}

#'\toprule

#' Data Source & Link \\

#'\midrule

#' Primary Data Source & \url{https://www.wikidata.org/}\\
#' Source Code & \url{\softwareversionurldoi}\\

#'\bottomrule

#'\end{longtable}
#'\end{centering}


#' The data included in this data set was primarily sourced from Wikidata. The Source Code used to calculate additional variables and create this Codebook is published separately with a Digital Object Identifier (DOI) on Zenodo, the scientific repository of CERN. Where Wikidata data was unreliable or inconsistent we cross-referened the data with press releases of the courts, data held by the German national library and Wikipedia articles.



#+
#'## Limitations
#'Users should be aware of the following limitations
#' 
#'\begin{enumerate}
#'\item Public data may be unreliable. We have cross-checked data as far as is humanly possible, but there currently is no official court-authorized data set of the data we present here (\emph{publication bias}).
#'\item Much of the data is hand-coded and may therefore include manual coding errors (\emph{manual bias}).
#'\item Currently only three federal courts are included. We aim to expand the data set as soon as possible (\emph{work-in-progress bias}).
#'\end{enumerate}


#+
#'## Public Domain Status

#' Individual facts cannot be copyrighted under German copyright law. All individual contributions (e.g. hand-coding, cross-checking of data, structuring of variables etc.) and the full data set are released into the public domain by the authors under a \emph{CC0 1.0 Universal Public Domain License}.




#'\newpage
#+
#'# Data Set Structure

#+
#'## Presidents
str(out.president)

#'\newpage
#+
#'## Vice-Presidents
str(out.vpresident)



#+
#'\newpage



#+
#'# Variables


#'\begin{centering}
#'\begin{longtable}{p{3cm}p{3cm}p{8.5cm}}



#'\toprule

#'Variable & Type & Details\\

#'\midrule

#'\endhead

#' court & Alphabetic & The standard German abbreviation of the federal court's name. Abbreviations are explained in detail in Section \ref{courts}. Hand-coded.\\
#' name\_last & String & The last name of the President/Vice-President. If the position was vacant for at least one day this variable reads \enquote{VACANCY-X}, where X is the number of the vacancy. Hand-coded.\\
#' name\_first & String & The first name of the President/Vice-President. If the position was vacant for at least one day this variable reads \enquote{VACANCY-X}, where X is the number of the vacancy. Hand-coded.\\
#' sex & Alphabetic & The sex of the President/Vice-President. Based on human interpretation of the variable \enquote{name\_first}. Coded as \enquote{F} for females and \enquote{M} for males. As there is little public disagreement on this topic with regards to the persons in this data set, this variable should be accurate. Hand-coded.\\
#' term\_begin\_date & ISO Date & The date on which the President/Vice-President took office. The format is YYYY-MM-DD (ISO-8601). Hand-coded.\\ 
#' term\_end\_date & ISO Date & The date on which the President/Vice-President left office. The format is YYYY-MM-DD (ISO-8601). If the source gave the same date for incoming and outgoing President/Vice-President we always coded the end of term for the outgoing President/Vice-President as the day prior to the incoming President/Vice-President taking office. This is to ensure that there is no overlap, which might pose problems in machine-to-machine interaction. If still in office the value is \enquote{NA}. Hand-coded.\\ 
#' birth\_date & ISO Date & The date on which the President/Vice-President was born. The format is YYYY-MM-DD (ISO-8601). Coded as \enquote{NA} if unknown. Hand-coded.\\ 
#' death\_date & ISO Date & The date on which the President/Vice-President died. The format is YYYY-MM-DD (ISO-8601).  If still alive the value is \enquote{NA}. Hand-coded.\\ 
#' term\_begin\_year & Integer & The year in which the President/Vice-President took office. The format is YYYY (ISO-8601). Will often overlap with variable \enquote{term\_end\_year} for the outgoing President/Vice-President. Automatically calculated from variable \enquote{term\_begin\_date}.\\ 
#' term\_end\_year & Integer & The year in which the President/Vice-President left office. The format is YYYY (ISO-8601). Will often overlap with variable \enquote{term\_begin\_year} for the incoming President/Vice-President. Automatically calculated from variable \enquote{term\_end\_date}.\\ 
#' birth\_year & Integer & The year in which the President/Vice-President was born. The format is YYYY (ISO-8601). Automatically calculated from variable \enquote{birth\_date}.\\ 
#' death\_year & Integer & The year in which the President/Vice-President died. The format is YYYY (ISO-8601). Automatically calculated from variable \enquote{death\_date}.\\ 
#' term\_begin\_age & Integer & The age of the President/Vice-President on the date of taking office. Automatically calculated from variables \enquote{birth\_date} and \enquote{term\_begin\_date}.\\ 
#' term\_end\_age & Integer & The age of the President/Vice-President on the date of leaving office. Automatically calculated from variables \enquote{birth\_date} and \enquote{term\_end\_date}.\\ 
#' death\_age & Integer & The age of the President/Vice-President on the date of leaving office. Automatically calculated from variables \enquote{birth\_date} and \enquote{death\_date}.\\ 
#' wikidata & String & The QID used by Wikidata to uniquely identify persons. Hand-coded.\\
#' gnd & Integer & The ID of the Integrated Authority File (German: \emph{Gemeinsame Normdatei}), which is managed by the German National Library (\emph{Deutsche Nationalbibliothek}). Hand-coded.\\
#' comment & String & A comment on difficult coding decisions, if applicable. Otherwise \enquote{NA}.\\
#' version & ISO Date & (CSV only) The version of the data set, which is always the date it was created (ISO-8601).\\
#' doi\_concept & String & (CSV only) The Digital Object Identifier (DOI) for the \textbf{concept} of the data set. The DOI is a persistent identifier suitable for stable long-term citation. Resolving this DOI via www.doi.org allows researchers to always acquire the \textbf{latest version} of the data set. Principle F1 of the FAIR Data Principles (\enquote{data are assigned globally unique and persistent identifiers}) recommends the documentationm of each measurement with a persistent identifier. Even if the CSV data set is transmitted without the accompanying Codebook this allows researchers to establish provenance of the data.\\
#' doi\_version & String & (CSV only) The Digital Object Identifier (DOI) for the \textbf{specific version} of the data set. The DOI is a persistent identifier suitable for stable long-term citation. Resolving this DOI via www.doi.org allows researchers to always acquire this \textbf{specific version} of the data set. Principle F1 of the FAIR Data Principles (\enquote{data are assigned globally unique and persistent identifiers}) recommends the documentationm of each measurement with a persistent identifier. Even if the CSV data set is transmitted without the accompanying Codebook this allows researchers to establish provenance of the data.\\

#'\bottomrule


#'\end{longtable}
#'\end{centering}




#'\newpage

#+
#'# The Federal Courts of Germany

#'\label{courts}



#+
#'## Abbreviations and Full Names


#'\begin{centering}
#'\begin{longtable}{p{3cm}p{5cm}p{6cm}}


#'\toprule

#'Abbreviation & German Name & English Translation\\

#'\midrule

#'\endhead

#' BAG & Bundesarbeitsgericht & Federal Labor Court\\
#' BDiG & Bundesdisziplinargericht & Federal Disciplinary Court\\
#' BFH & Bundesfinanzhof & Federal Finance Court\\
#' BGH & Bundesgerichtshof & Federal Court of Justice\\
#' BSG & Bundessozialgericht & Federal Social Court\\
#' BPatG & Bundespatentgericht & Federal Patent Court\\
#' BVerfG & Bundesverfassungsgericht & Federal Constitutional Court\\
#' BVerwG & Bundesverwaltungsgericht & Federal Administrative Court\\
#' TDG-Nord & Truppendienstgericht Nord & Federal Court of Military Discipline North\\
#' TDG-Süd & Truppendienstgericht Süd & Federal Court of Military Discipline South\\


#'\bottomrule


#'\end{longtable}
#'\end{centering}



#+
#'## Notes

#'\begin{itemize}
#'\item The Federal Disciplinary Court (\emph{Bundesdisziplinargericht}) was abolished on 31 December 2003 and its jurisdiction transferred to the general administrative courts.
#'\item We code all courts with their standard German abbreviations to avoid confusion caused by English abbreviations, which are not in common usage.
#' \item The Federal Courts of Military Discpline only review administrative sanctions. Criminal proceedings against soldiers are conducted before the ordinary criminal courts.
#' \item There is no Federal Supreme Court of Germany. While the Constitutional Court often takes on this role in fact, as a matter of law it only reviews the interpretation and application of constitutional law. Instead there exists a Joint Senate of the Highest Federal Courts (Gemeinsamer Senat der Obersten Gerichtshöfe des Bundes), which can take on the role of Federal Supreme Court in an ad-hoc manner, but rarely sits.
#'\end{itemize}




#################################
### Names of Variables to Display
#################################


date.variables <- c("name_last",
                    "name_first",
                    "term_begin_date",
                    "term_end_date",
                    "birth_date",
                    "death_date")


date.names <- c("Last Name",
                "First Name",
                "Term Begin",
                "Term End",
                "Born",
                "Died")




#'\newpage
#'# Bundesgerichtshof (BGH)

#'## Presidents

kable(out.president[court == "BGH", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")


#'\newpage
#'## Vice-Presidents

kable(out.vpresident[court == "BGH", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")



#'\newpage
#'# Bundesverfassungsgericht (BVerfG)


#'## Presidents


kable(out.president[court == "BVerfG", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")


#'\newpage
#'## Vice-Presidents


kable(out.vpresident[court == "BVerfG", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")




#'\newpage
#'# Bundesverwaltungsgericht (BVerwG)

#'## Presidents

kable(out.president[court == "BVerwG", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")


#'\newpage
#'## Vice-Presidents


kable(out.vpresident[court == "BVerwG", ..date.variables],
      format = "latex",
      align = 'l',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = date.names) %>% kable_styling(latex_options = "repeat_header")












############################
### Remove Vacancies
############################

nv.president <- out.president[grep("VACANCY",
                                   out.president$name_last,
                                   invert = TRUE)]

nv.vpresident <- out.vpresident[grep("VACANCY",
                                     out.vpresident$name_last,
                                     invert = TRUE)]



###############################
### Calculate Frequency Tables
###############################


p.table.court <- f.freqtable(nv.president,
                           varname = "court")


p.table.sex <- f.freqtable(nv.president,
                         varname = "sex")

p.table.age.termbegin <- f.freqtable(nv.president,
                                   varname = "term_begin_age")


p.table.age.termend <- f.freqtable(nv.president,
                                   varname = "term_end_age")



vp.table.court <- f.freqtable(nv.vpresident,
                           varname = "court")


vp.table.sex <- f.freqtable(nv.vpresident,
                         varname = "sex")

vp.table.age.termbegin <- f.freqtable(nv.vpresident,
                                   varname = "term_begin_age")


vp.table.age.termend <- f.freqtable(nv.vpresident,
                                   varname = "term_end_age")



#+
#'\newpage
#+   
#'# Frequency Tables
#' 
#' \ra{1.3}


#'## By Court

#+
#'### Presidents

freqtable <- p.table.court[-.N]

#+ PVP-FCG_01_Barplot_Presidents_ByCourt, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = court,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_y_continuous(breaks = seq(0, 14, 2))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Presidents by Court"),
        caption = paste("DOI:",
                        doi.version),
        x = "Court",
        y = "Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )

#'\vspace{1cm}

kable(p.table.court,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Court",
                    "Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")



#'### Vice-Presidents

freqtable <- vp.table.court[-.N]


#+ PVP-FCG_01_Barplot_VicePresidents_ByCourt, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = court,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_y_continuous(breaks = seq(0, 16, 2))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Vice-Presidents by Court"),
        caption = paste("DOI:",
                        doi.version),
        x = "Court",
        y = "Vice-Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )



#'\vspace{1cm}

kable(vp.table.court,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Court",
                    "Vice-Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")




#'## By Sex

#+
#'### Presidents

freqtable <- p.table.sex[-.N]

#+ PVP-FCG_02_Barplot_Presidents_BySex, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = sex,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_y_continuous(breaks = seq(0, 30, 5))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Presidents by Sex"),
        caption = paste("DOI:",
                        doi.version),
        x = "Sex",
        y = "Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )


#'\vspace{1cm}

kable(p.table.sex,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Sex",
                    "Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")


#+
#'### Vice-Presidents

freqtable <- vp.table.sex[-.N]

#+ PVP-FCG_02_Barplot_VicePresidents_BySex, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = sex,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_y_continuous(breaks = seq(0, 40, 5))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Vice-Presidents by Sex"),
        caption = paste("DOI:",
                        doi.version),
        x = "Sex",
        y = "Vice-Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )


#'\vspace{1cm}

kable(vp.table.sex,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Sex",
                    "Vice-Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")



#'\newpage

#'## By Age at Begin of Term

#+
#'### Presidents

freqtable <- p.table.age.termbegin[-.N][,lapply(.SD, as.numeric)]

#+ PVP-FCG_03_Barplot_Presidents_ByAge_BeginOfTerm, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = term_begin_age,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Presidents by Age at Begin of Term"),
        caption = paste("DOI:",
                        doi.version),
        x = "Age at Begin of Term",
        y = "Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )


#'\vspace{0.3cm}

kable(p.table.age.termbegin,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Age (Term Begin)",
                    "Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")

#' \ra{1.2}
#'\newpage
#+
#'### Vice-Presidents

freqtable <- vp.table.age.termbegin[-.N][,lapply(.SD, as.numeric)]

#+ PVP-FCG_03_Barplot_VicePresidents_ByAge_BeginOfTerm, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = term_begin_age,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_x_continuous(breaks = seq(45, 70, 5))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Vice-Presidents by Age at Begin of Term"),
        caption = paste("DOI:",
                        doi.version),
        x = "Age at Begin of Term",
        y = "Vice-Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )




kable(vp.table.age.termbegin,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Age (Term Begin)",
                    "Vice-Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")






#' \ra{1.3}
#'\newpage
#'## By Age at End of Term

#+
#'### Presidents

freqtable <- p.table.age.termend[-.N][,lapply(.SD, as.numeric)]

#+ PVP-FCG_04_Barplot_Presidents_ByAge_EndOfTerm, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = term_end_age,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Presidents by Age at End of Term"),
        caption = paste("DOI:",
                        doi.version),
        x = "Age at End of Term",
        y = "Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )


#'\vspace{0.3cm}

kable(p.table.age.termend,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Age (Term End)",
                    "Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")



#'\newpage
#+
#'### Vice-Presidents

freqtable <- vp.table.age.termend[-.N][,lapply(.SD, as.numeric)]

#+ PVP-FCG_04_Barplot_Vice-Presidents_ByAge_EndOfTerm, fig.height = 5, fig.width = 8
ggplot(data = freqtable) +
    geom_bar(aes(x = term_end_age,
                 y = N),
             stat = "identity",
             fill = "#7e0731",
             color = "black",
             width = 0.5) +
    scale_y_continuous(breaks = seq(0, 10, 2))+
    theme_bw()+
    labs(
        title = paste(datasetname,
                      "| Version",
                      datestamp,
                      "| Vice-Presidents by Age at End of Term"),
        caption = paste("DOI:",
                        doi.version),
        x = "Age at End of Term",
        y = "Vice-Presidents"
    )+
    theme(
        text = element_text(size = 14),
        plot.title = element_text(size = 14,
                                  face = "bold"),
        legend.position = "none",
        plot.margin = margin(10, 20, 10, 10)
    )


#'\vspace{0.3cm}

kable(vp.table.age.termend,
      format = "latex",
      align = 'P{3cm}',
      booktabs = TRUE,
      longtable = TRUE,
      col.names = c("Age (Term End)",
                    "Vice-Presidents",
                    "% Total",
                    "% Cumulative")) %>% kable_styling(latex_options = "repeat_header")






#' \newpage

#+ results = "asis", echo = FALSE
cat(readLines("CHANGELOG.md"),
    sep = "\n")





###############################
### ZIP-Archive erstellen
###############################



## ZIP Analysis Data

zip(paste0(datasetname,
           "_",
           datestamp,
           "_EN_",
           basename(outputdir),
           ".zip"),
    basename(outputdir))



## ZIP Source Files

files.source <- c(list.files(pattern = "Source"),
                  "buttons")


files.source <- grep("spin",
                     files.source,
                     value = TRUE,
                     ignore.case = TRUE,
                     invert = TRUE)

zip(paste(datasetname,
           datestamp,
           "Source_Files.zip",
           sep = "_"),
    files.source)







#'\newpage
#+
#'# Strict Replication Parameters


sessionInfo()



#'\newpage
#+
#'# References
