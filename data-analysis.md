data analysis
================
Yiying Wu
2024-02-20

## R packages

``` r
# INSTALL PACKAGES
packages <- c("tidyverse", "knitr", "haven","gtsummary",
              "survey", "mice")

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

**combine categories**

``` r
dat <-dat %>% 
  mutate(
    # outcome variable
    ## adult food security, 1=food security, 0=food insecurity
    food_security =ifelse(food_security==1,1,ifelse(food_security %in% c(2,3,4),0,NA)),
    
    # Demographics variables
    ## age
    
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

<div id="obnphxyxjz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#obnphxyxjz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#obnphxyxjz thead, #obnphxyxjz tbody, #obnphxyxjz tfoot, #obnphxyxjz tr, #obnphxyxjz td, #obnphxyxjz th {
  border-style: none;
}

#obnphxyxjz p {
  margin: 0;
  padding: 0;
}

#obnphxyxjz .gt_table {
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

#obnphxyxjz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#obnphxyxjz .gt_title {
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

#obnphxyxjz .gt_subtitle {
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

#obnphxyxjz .gt_heading {
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

#obnphxyxjz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#obnphxyxjz .gt_col_headings {
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

#obnphxyxjz .gt_col_heading {
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

#obnphxyxjz .gt_column_spanner_outer {
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

#obnphxyxjz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#obnphxyxjz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#obnphxyxjz .gt_column_spanner {
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

#obnphxyxjz .gt_spanner_row {
  border-bottom-style: hidden;
}

#obnphxyxjz .gt_group_heading {
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

#obnphxyxjz .gt_empty_group_heading {
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

#obnphxyxjz .gt_from_md > :first-child {
  margin-top: 0;
}

#obnphxyxjz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#obnphxyxjz .gt_row {
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

#obnphxyxjz .gt_stub {
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

#obnphxyxjz .gt_stub_row_group {
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

#obnphxyxjz .gt_row_group_first td {
  border-top-width: 2px;
}

#obnphxyxjz .gt_row_group_first th {
  border-top-width: 2px;
}

#obnphxyxjz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#obnphxyxjz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#obnphxyxjz .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#obnphxyxjz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#obnphxyxjz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#obnphxyxjz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#obnphxyxjz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#obnphxyxjz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#obnphxyxjz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#obnphxyxjz .gt_footnotes {
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

#obnphxyxjz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#obnphxyxjz .gt_sourcenotes {
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

#obnphxyxjz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#obnphxyxjz .gt_left {
  text-align: left;
}

#obnphxyxjz .gt_center {
  text-align: center;
}

#obnphxyxjz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#obnphxyxjz .gt_font_normal {
  font-weight: normal;
}

#obnphxyxjz .gt_font_bold {
  font-weight: bold;
}

#obnphxyxjz .gt_font_italic {
  font-style: italic;
}

#obnphxyxjz .gt_super {
  font-size: 65%;
}

#obnphxyxjz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#obnphxyxjz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#obnphxyxjz .gt_indent_1 {
  text-indent: 5px;
}

#obnphxyxjz .gt_indent_2 {
  text-indent: 10px;
}

#obnphxyxjz .gt_indent_3 {
  text-indent: 15px;
}

#obnphxyxjz .gt_indent_4 {
  text-indent: 20px;
}

#obnphxyxjz .gt_indent_5 {
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
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Race/Hispanic origin w/ NH Asian</td>
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
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Country of birth</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">6,561 (71%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">2,666 (29%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    77</td>
<td headers="stat_0" class="gt_row gt_center">4 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    99</td>
<td headers="stat_0" class="gt_row gt_center">1 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Body Mass Index (kg/m**2)</td>
<td headers="stat_0" class="gt_row gt_center">29 (25, 34)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">851</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Education level - Adults 20+</td>
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
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_0" class="gt_row gt_center">2 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    9</td>
<td headers="stat_0" class="gt_row gt_center">13 (0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Marital status</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">5,279 (57%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">2,148 (23%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">1,795 (19%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    77</td>
<td headers="stat_0" class="gt_row gt_center">8 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    99</td>
<td headers="stat_0" class="gt_row gt_center">2 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Ratio of family income to poverty</td>
<td headers="stat_0" class="gt_row gt_center">2.21 (1.19, 4.20)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">1,404</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Ever told you had high blood pressure</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">3,563 (39%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">5,657 (61%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    9</td>
<td headers="stat_0" class="gt_row gt_center">12 (0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Doctor told you have diabetes</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">1,420 (15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">7,545 (82%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    3</td>
<td headers="stat_0" class="gt_row gt_center">262 (2.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    9</td>
<td headers="stat_0" class="gt_row gt_center">5 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Ever told you had weak/failing kidneys?</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">383 (4.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">8,834 (96%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    9</td>
<td headers="stat_0" class="gt_row gt_center">15 (0.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left" style="font-weight: bold;">Covered by health insurance</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    1</td>
<td headers="stat_0" class="gt_row gt_center">7,743 (84%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2</td>
<td headers="stat_0" class="gt_row gt_center">1,467 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    7</td>
<td headers="stat_0" class="gt_row gt_center">7 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    9</td>
<td headers="stat_0" class="gt_row gt_center">15 (0.2%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

## Define survey design

### Define survey design for overall dataset

``` r
nhanes_all <- svydesign(data=dat, id=~sdmvpsu, strata=~sdmvstra
                          , weights=~wtmecprp, nest=TRUE)
```

``` r
nhanes<-subset(nhanes_all,age >= 20)
```
