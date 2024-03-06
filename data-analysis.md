data analysis
================
Yiying Wu
2024-02-20

## R packages

``` r
# INSTALL PACKAGES
packages <- c("tidyverse", "knitr", "haven","gtsummary",
              "survey", "mice","jtools","surveyCV",
              "rpms","pROC")

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

# Load packages invisibly
invisible(lapply(packages, library, character.only = TRUE))

# # Remove variables associated with package installation
# rm(packages, installed_packages)
```

## data preparation

``` r
# input demo data
dat_demo<-read_xpt("./data/P_DEMO.XPT")%>%
  janitor::clean_names()

# input food insecurity data
dat_fsq<-read_xpt("./data/P_FSQ.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,fsdad)

# input bmi data
dat_bmi<-read_xpt("./data/P_BMX.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,bmxbmi)

#  chronic diseases
## input hbp data
dat_hbp<-read_xpt("./data/P_BPQ.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,bpq020)

## input diabetes data
dat_diabetes<-read_xpt("./data/P_DIQ.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,diq010)

## input ckd data
dat_ckd<-read_xpt("./data/P_KIQ_U.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,kiq022)

# input insurance data
dat_insr<-read_xpt("./data/P_HIQ.XPT")%>%
  janitor::clean_names()%>%
  select(seqn,hiq011)

# combine datasets
dat <- dat_demo %>% left_join(dat_fsq, by = "seqn")
dat <- dat %>% left_join(dat_bmi, by="seqn")
dat <- dat %>% left_join(dat_hbp, by="seqn")
dat <- dat %>% left_join(dat_diabetes, by="seqn")
dat <- dat %>% left_join(dat_ckd, by="seqn")
dat <- dat %>% left_join(dat_insr, by="seqn")
```

## data cleaning

**selecting variabes**

``` r
dat<-dat%>%
  select(
    # Survey variables 
    seqn, sdmvpsu,sdmvstra,wtmecprp,
    
    # outcome
    food_security=fsdad,
    
    # predictors
    gender=riagendr, age=ridageyr,
    race=ridreth3,country_of_birth=dmdborn4,bmi=bmxbmi,
    education=dmdeduc2,marital_status=dmdmartz, poverty=indfmpir,
    hbp=bpq020, diabetes=diq010,ckd=kiq022,insurance=hiq011)
```

**identify missing data**

``` r
dat <-dat %>% 
  mutate(
    # demo
    ## education
    education=ifelse(education %in% c(7,9),NA,education),
    ## marital_status
    marital_status=ifelse(marital_status %in% c(77,99),NA,marital_status),
    ## country_of_birth
    country_of_birth=ifelse(country_of_birth %in% c(77,99),NA,country_of_birth),
    
    # chronic disease
    ## hbp
    hbp=ifelse(hbp==9,NA,hbp),
    ## diabetes
    diabetes=ifelse(diabetes==9,NA,diabetes),
    ## ckd
    ckd=ifelse(ckd==9,NA,ckd),
    
    #insurance
    insurance=ifelse(insurance %in% c(7,9),NA,insurance)
  )
```

**recode categorical data**

``` r
dat <-dat %>% 
  mutate(
    # outcome variable
    ## adult food security, 1=food security, 0=food insecurity
    food_security =ifelse(food_security==1,1,ifelse(food_security %in% c(2,3,4),0,NA)),
    
    # Demographics variables
    ## gender
    female=ifelse(gender==2,1,ifelse(gender==1,0,NA)),
    ## race
    race=relevel(as.factor(race), ref ="1"),
    ## country_of_birth
    country_of_birth=ifelse(country_of_birth==2,1,ifelse(country_of_birth==1,0,NA)),
    ## education
    education=relevel(as.factor(education), ref ="1"),
    ## marital_status
    marital_status=relevel(as.factor(marital_status), ref ="1"),
    
    # chronic disease
    ## hbp
    hbp=ifelse(hbp==1,1,ifelse(hbp==2,0,NA)),
    ## diabetes
    diabetes=ifelse(diabetes==1,1,ifelse(diabetes %in% c(2,3),0,NA)),
    ## ckd
    ckd=ifelse(ckd==1,1,ifelse(ckd==2,0,NA)),
    
    #insurance
    insurance=ifelse(insurance==1,1,ifelse(insurance==2,0,NA))
  )
```

**combine categories**

``` r
dat<-dat%>%
  mutate(
    # Demographics variables
    ## race
    
  )
```

**summary table**

``` r
dat %>% 
  filter(age>=20)%>%
  select(everything()) %>% 
  tbl_summary() %>% 
  bold_labels()
```

<div id="wtemgljixh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wtemgljixh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#wtemgljixh thead, #wtemgljixh tbody, #wtemgljixh tfoot, #wtemgljixh tr, #wtemgljixh td, #wtemgljixh th {
  border-style: none;
}

#wtemgljixh p {
  margin: 0;
  padding: 0;
}

#wtemgljixh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wtemgljixh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wtemgljixh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wtemgljixh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wtemgljixh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wtemgljixh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wtemgljixh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wtemgljixh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wtemgljixh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wtemgljixh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wtemgljixh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wtemgljixh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wtemgljixh .gt_spanner_row {
  border-bottom-style: hidden;
}

#wtemgljixh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#wtemgljixh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wtemgljixh .gt_from_md > :first-child {
  margin-top: 0;
}

#wtemgljixh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wtemgljixh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wtemgljixh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#wtemgljixh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#wtemgljixh .gt_row_group_first td {
  border-top-width: 2px;
}

#wtemgljixh .gt_row_group_first th {
  border-top-width: 2px;
}

#wtemgljixh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wtemgljixh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wtemgljixh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wtemgljixh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wtemgljixh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wtemgljixh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wtemgljixh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#wtemgljixh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wtemgljixh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wtemgljixh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wtemgljixh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wtemgljixh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wtemgljixh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wtemgljixh .gt_left {
  text-align: left;
}

#wtemgljixh .gt_center {
  text-align: center;
}

#wtemgljixh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wtemgljixh .gt_font_normal {
  font-weight: normal;
}

#wtemgljixh .gt_font_bold {
  font-weight: bold;
}

#wtemgljixh .gt_font_italic {
  font-style: italic;
}

#wtemgljixh .gt_super {
  font-size: 65%;
}

#wtemgljixh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#wtemgljixh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wtemgljixh .gt_indent_1 {
  text-indent: 5px;
}

#wtemgljixh .gt_indent_2 {
  text-indent: 10px;
}

#wtemgljixh .gt_indent_3 {
  text-indent: 15px;
}

#wtemgljixh .gt_indent_4 {
  text-indent: 20px;
}

#wtemgljixh .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 9,232&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 9,232</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Respondent sequence number</td>
<td headers="stat_0" class="gt_row gt_center">117,022 (113,181, 120,945)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Masked variance pseudo-PSU</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">4,445 (48%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">4,571 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">216 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Masked variance pseudo-stratum</td>
<td headers="stat_0" class="gt_row gt_center">160 (154, 166)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Full sample MEC exam weight</td>
<td headers="stat_0" class="gt_row gt_center">15,584 (8,454, 30,029)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">food_security</td>
<td headers="stat_0" class="gt_row gt_center">5,380 (63%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">689</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Gender</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">4,479 (49%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">4,753 (51%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Age in years at screening</td>
<td headers="stat_0" class="gt_row gt_center">52 (36, 65)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">race</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">1,057 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">940 (10%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">3,217 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="stat_0" class="gt_row gt_center">2,459 (27%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    6</td>
<td headers="stat_0" class="gt_row gt_center">1,120 (12%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_0" class="gt_row gt_center">439 (4.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">country_of_birth</td>
<td headers="stat_0" class="gt_row gt_center">2,666 (29%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">5</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Body Mass Index (kg/m**2)</td>
<td headers="stat_0" class="gt_row gt_center">29 (25, 34)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">851</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">education</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">719 (7.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">1,041 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">2,225 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    4</td>
<td headers="stat_0" class="gt_row gt_center">2,975 (32%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    5</td>
<td headers="stat_0" class="gt_row gt_center">2,257 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">15</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">marital_status</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">5,279 (57%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">2,148 (23%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">1,795 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">10</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Ratio of family income to poverty</td>
<td headers="stat_0" class="gt_row gt_center">2.21 (1.19, 4.20)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">1,404</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">hbp</td>
<td headers="stat_0" class="gt_row gt_center">3,563 (39%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">12</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">diabetes</td>
<td headers="stat_0" class="gt_row gt_center">1,420 (15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">5</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">ckd</td>
<td headers="stat_0" class="gt_row gt_center">383 (4.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">15</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">insurance</td>
<td headers="stat_0" class="gt_row gt_center">7,743 (84%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">22</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">female</td>
<td headers="stat_0" class="gt_row gt_center">4,753 (51%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

**save the clean dataset**

``` r
# save
write.csv(dat, "./result_files/dat.csv")
```

## Define survey design

### Define survey design for overall dataset

``` r
svy <- svydesign(data=dat, id=~sdmvpsu, strata=~sdmvstra
                          , weights=~wtmecprp, nest=TRUE)
```

``` r
imputation.svy<-subset(svy,age >= 20)
```

## Doing Imputation and logistic regression at the Same Time

``` r
#create five imputations. The imputations object contains all five
#complete datasets.
imputations <- mice(dat, 5, printFlag=FALSE)
```

    ## Warning: Number of logged events: 1

``` r
lm_svy_mi <- function(formula, design, imputations) {
  
  #setting up null objects allows us to easily add results
  #later
  b <- se <- R2MF <- R2CU <-NULL
  aic <- NULL
  mean.aic <- NULL
  # AUC <- mean.AUC <-NULL
  
  #now loop through our imputations and run the model
  for(i in 1:imputations$m) {
    #grab the complete dataset
    imputation <- complete(imputations, i)
    
    #run the model
    model <- svyglm(formula, design=design,family=quasibinomial)
    #collect the results
    b <- cbind(b, coef(model))
    se <- cbind(se, summary(model)$coef[,2])
    
    summ_model <- summ(model)
      
    # # McFadden's R-squared
    R2MF <- attr(summ_model, which = "rsqmc")
    
    # # CRagg-Uhler R-squared
    R2CU <- attr(summ_model, which = "rsq")
    
    # AIC
    aic <- c(aic,(-2) * logLik(model) + 2 * length(coef(model)))
    
    # # Logistic Regression AUC calculation
    #predictions <- predict(model, type = "response")
    #ROC_res <- roc(imputation.svy$variables$bp_uncontrolled_140_90, predictions)  # Replace with dependent variable
    #AUC <- c(AUC, auc(ROC_res))
    
  }
  
  #now pool the results
  b.pool <- apply(b, 1, mean)
  between.var <- apply(b, 1, var)
  within.var <- apply(se^2, 1, mean)
  se.pool <- sqrt(within.var+between.var+between.var/imputations$m) 
  lower.ci <- b.pool - 1.96 * se.pool  # Lower bound of 95% CI
  upper.ci <- b.pool + 1.96 * se.pool  # Upper bound of 95% CI
  t.pool <- b.pool/se.pool 
  pvalue.pool <- (1-pnorm(abs(t.pool)))*2 
  coefficients <- data.frame(b.pool, se.pool, lower.ci, upper.ci, t.pool, pvalue.pool)
  
  #we can also grap n and p from the last model since 
  #they should be the same across all iterations
  n <- nobs(model)
  p <- length(model$coefficients)-1
  mean.R2MF <- mean(R2MF)
  mean.R2CU <- mean(R2CU)
  mean.aic <- mean(aic)
  #mean.AUC <- mean(AUC)
  
  #return everything in a list
  return(list(coef=coefficients,
              n=n,
              R2MF = mean.R2MF,
              R2CU = mean.R2CU,
              aic = mean.aic
              )
         )
}
```

``` r
#now lets try the model out
model1 <- lm_svy_mi(food_security~gender+age+
    race+country_of_birth+bmi+
    education+marital_status+ poverty+
    hbp+ diabetes+ckd+insurance, imputation.svy,imputations)
```

    ## Warning in logLik.svyglm(model): svyglm not fitted by maximum likelihood.

    ## Warning in logLik.svyglm(model): svyglm not fitted by maximum likelihood.

    ## Warning in logLik.svyglm(model): svyglm not fitted by maximum likelihood.

    ## Warning in logLik.svyglm(model): svyglm not fitted by maximum likelihood.

    ## Warning in logLik.svyglm(model): svyglm not fitted by maximum likelihood.

``` r
model1
```

    ## $coef
    ##                       b.pool     se.pool    lower.ci     upper.ci     t.pool
    ## (Intercept)      -2.44683032 0.327809674 -3.08933728 -1.804323364 -7.4641797
    ## gender           -0.11847949 0.057365863 -0.23091658 -0.006042396 -2.0653309
    ## age               0.02397088 0.003862951  0.01639950  0.031542265  6.2053278
    ## race2            -0.36459428 0.189796536 -0.73659549  0.007406934 -1.9209743
    ## race3             0.50840621 0.211229338  0.09439671  0.922415717  2.4068921
    ## race4             0.12202421 0.221890509 -0.31288118  0.556929610  0.5499298
    ## race6             0.54874999 0.226474209  0.10486054  0.992639441  2.4230132
    ## race7            -0.29116917 0.320057608 -0.91848209  0.336143737 -0.9097399
    ## country_of_birth  0.10935821 0.168199494 -0.22031279  0.439029222  0.6501697
    ## bmi              -0.01503282 0.005097588 -0.02502409 -0.005041545 -2.9490059
    ## education2        0.29561234 0.162971111 -0.02381103  0.615035719  1.8138941
    ## education3        0.52033381 0.170195970  0.18674971  0.853917909  3.0572628
    ## education4        0.75548888 0.135773391  0.48937303  1.021604722  5.5643368
    ## education5        1.31234315 0.155483172  1.00759614  1.617090171  8.4404192
    ## marital_status2  -0.33050611 0.151094158 -0.62665065 -0.034361555 -2.1874182
    ## marital_status3  -0.12591857 0.137878573 -0.39616057  0.144323432 -0.9132570
    ## poverty           0.70555271 0.053178644  0.60132257  0.809782856 13.2675951
    ## hbp              -0.15713599 0.112958021 -0.37853371  0.064261733 -1.3911008
    ## diabetes         -0.22798778 0.167756056 -0.55678965  0.100814091 -1.3590435
    ## ckd              -0.11927556 0.227365323 -0.56491160  0.326360470 -0.5245987
    ## insurance         0.21438932 0.144258477 -0.06835730  0.497135932  1.4861471
    ##                   pvalue.pool
    ## (Intercept)      8.371082e-14
    ## gender           3.889171e-02
    ## age              5.458303e-10
    ## race2            5.473494e-02
    ## race3            1.608892e-02
    ## race4            5.823675e-01
    ## race6            1.539237e-02
    ## race7            3.629597e-01
    ## country_of_birth 5.155826e-01
    ## bmi              3.187979e-03
    ## education2       6.969403e-02
    ## education3       2.233683e-03
    ## education4       2.631513e-08
    ## education5       0.000000e+00
    ## marital_status2  2.871201e-02
    ## marital_status3  3.611074e-01
    ## poverty          0.000000e+00
    ## hbp              1.641949e-01
    ## diabetes         1.741328e-01
    ## ckd              5.998622e-01
    ## insurance        1.372402e-01
    ## 
    ## $n
    ## [1] 7182
    ## 
    ## $R2MF
    ## [1] 0.2059237
    ## 
    ## $R2CU
    ## [1] 0.3103936
    ## 
    ## $aic
    ## [1] 6710.546

save the result

``` r
## create result dataframe
res_df<-function(model){
    coef.names = rownames(model$coef)
    OR = exp(model$coef$b.pool)
    ci.low = exp(model$coef$lower.ci)
    ci.up = exp(model$coef$upper.ci)
    res<-data_frame(coef.names,OR,ci.low,ci.up)
    return(res)
}
res_m1<-res_df(model1)
```

    ## Warning: `data_frame()` was deprecated in tibble 1.1.0.
    ## ℹ Please use `tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
# save res
write.csv(res_m1, "./result_files/res_m1.csv")
```

## for the machine learning model

Some algorithms can handle missing data internally. For instance,
tree-based methods like Random Forests can work with missing values.
Skip the imputation part and directly go to the modeling part.

The function folds.svy() generates design-based fold IDs for K-fold CV,
using any specified strata and clusters. Briefly: For a stratified
sample, each fold will contain data from each stratum. For a cluster
sample, a given cluster’s rows will all be assigned to the same fold.
(<https://onlinelibrary.wiley.com/doi/10.1002/sta4.454>)

plan to use the `surveyCV` package

<https://github.com/ColbyStatSvyRsch/surveyCV>

<https://stats.stackexchange.com/questions/238141/two-worlds-collide-using-ml-for-complex-survey-data>

<https://cran.r-project.org/web/packages/surveyCV/index.html>

``` r
# Generate fold IDs that account for clustering in the survey design
set.seed(100)
nfolds <- 3
dat_subset <- dat[which(dat$age >= 20), ]
dat_subset <- as.data.frame(dat_subset)

dat_subset<-na.omit(dat_subset) 
num_clusters <- length(unique(dat_subset$sdmvpsu))
nfolds <- min(nfolds, num_clusters)
dat_subset$.foldID <- folds.svy(dat_subset, nfolds = nfolds,  clusterID = "sdmvpsu")

# Use CV to tune the bin_size parameter of rpms_forest()
bin_sizes <- c(10, 20, 50, 100, 250, 500)

# model selection based on MSE
SSEs <- rep(0, length(bin_sizes))
for(ff in 1:nfolds) {
  train <- subset(dat_subset, .foldID != ff)
  test  <- subset(dat_subset, .foldID == ff)
  for(bb in 1:length(bin_sizes)) {
    rf <- rpms_forest(food_security~gender+age+
    race+country_of_birth+bmi+
    education+marital_status+ poverty+
    hbp+ diabetes+ckd+insurance, 
                      data = train,
                      weights = ~wtmecprp, strata = ~sdmvstra,
                      clusters = ~sdmvpsu,
                      bin_size = bin_sizes[bb], f_size = 50)
    yhat <- predict(rf, newdata = test)
    res2 <- (yhat - test$food_security)^2
    # Sum up weighted SSEs, not MSEs yet,
    # b/c cluster sizes may differ across folds and b/c of survey weights
    SSEs[bb] <- SSEs[bb] + sum(res2 * test$wtmecprp)
  }
}
# Divide entire weighted sum by the sum of weights
MSEs <- SSEs / sum(dat_subset$wtmecprp)
# Show results
cbind(bin_sizes, MSEs)
```

    ##      bin_sizes      MSEs
    ## [1,]        10 0.1413094
    ## [2,]        20 0.1401991
    ## [3,]        50 0.1392922
    ## [4,]       100 0.1389139
    ## [5,]       250 0.1405726
    ## [6,]       500 0.1435085

Use AUC instead of MSE:

``` r
AUCs <- rep(0, length(bin_sizes))
for(ff in 1:nfolds) {
  train <- subset(dat_subset, .foldID != ff)
  test  <- subset(dat_subset, .foldID == ff)
  for(bb in 1:length(bin_sizes)) {
    rf <- rpms_forest(food_security~gender+age+
    race+country_of_birth+bmi+
    education+marital_status+ poverty+
    hbp+ diabetes+ckd+insurance, 
                      data = train,
                      weights = ~wtmecprp, strata = ~sdmvstra,
                      clusters = ~sdmvpsu,
                      bin_size = bin_sizes[bb], f_size = 50)
    yhat <- predict(rf, newdata = test, type = "response") # Ensure predictions are on the scale of probabilities
    # Calculate AUC
    roc_obj <- roc(test$food_security, yhat, weights = test$wtmecprp, quiet = TRUE)
    AUCs[bb] <- AUCs[bb] + auc(roc_obj)
  }
}
# Average AUCs over folds
AUCs <- AUCs / nfolds
# Show results
cbind(bin_sizes, AUCs)
```

    ##      bin_sizes      AUCs
    ## [1,]        10 0.8311572
    ## [2,]        20 0.8401272
    ## [3,]        50 0.8392618
    ## [4,]       100 0.8399933
    ## [5,]       250 0.8391070
    ## [6,]       500 0.8357517

we can select $bin\_sizes=20$ and get the highest $AUC=0.8401272$

rpms package

<https://cran.r-project.org/web/packages/rpms/rpms.pdf>
