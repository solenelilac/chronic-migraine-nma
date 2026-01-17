# Chronic Migraine Network Meta-Analysis

Frequentist network meta-analysis comparing galcanezumab with erenumab, eptinezumab, and onabotulinumtoxinA (Botox A) for reduction in monthly migraine days.

## Project structure
data/    Input dataset  
code/    R analysis script  
output/  Figures and tables  
report/  LaTeX report  

## Methods
A frequentist network meta-analysis was conducted using placebo-controlled trial data. Multi-arm trials were treated as independent treatment–placebo contrasts due to unavailable within-study correlation information. Contrasts without reported standard errors were excluded.

## Software
Analysis performed in R using the netmeta package.
