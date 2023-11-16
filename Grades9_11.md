ST 558 Project 3 Education Level: Grades 9 through 11 (Some high school)
================
Heather Copley & Andy Johnson
2023-11-15

## Introduction

This report automates the construction of exploratory analyses and
prediction models using data from the 2015 [Behavioral Risk Factor
Surveillance System](https://www.cdc.gov/brfss/index.html) (BRFSS). The
BRFSS is a long-running telephone survey in the United States that
collects data on health-related issues, behaviors, and service
utilization. The primary outcome of interest in these analyses is if a
respondent reports being diabetic or prediabetic. Diabetes is an chronic
endocrine disorder in which the body is unable to properly regulate
blood glucose levels. [Approximately one in nine U.S. adults has
diabetes](https://diabetes.org/about-diabetes/statistics/about-diabetes),
and this prevalence has steadily increased over the last few decades.
[Prediabetes](https://www.mayoclinic.org/diseases-conditions/prediabetes/symptoms-causes/syc-20355278)
is a transitional state of chronically heightened blood glucose levels
that is just below the clinical threshold for diabetes. Early detection
and treatment of prediabetes can restore blood glucose to normal levels,
and the appropriate clinical management of diabetes can reduce long-term
negative health outcomes and healthcare costs due to the disease. For
these reasons, it is important to identify and understand the various
personal characteristics that are associated with having diabetes.

In this report, we use a subset of BRFSS data to explore a variety of
factors associated with having diabetes or prediabetes, and build
prediction models to estimate the probability of a survey respondent
having diabetes. This report is part of a larger series of reports, each
one focusing on a subset of BRFSS survey respondent defined by having a
common level of education. The analyses below include descriptive
statistical summaries, exploratory visualizations/graphs, and the
creation and evaluation of prediction models for diabetic status for
individuals with an education level of grades 9 through 11 (some high
school).

The variables used in these analyses include individually-reported
characteristics such as:

- a history of chronic comorbidities (hypertension,
  hypercholesterolemia, heart disease, depression),
- a history of acute health events (heart attacks, stroke),
- other relevant physical characteristics (BMI, age, sex),
- behavorial factors that influence health (smoking, diet, physical
  activity), and
- socioeconomic factors (income, health insurance coverage, education
  levels).

## Exploratory Data Analysis

The codebook for the variables included in this analysis can be found
here:

[Behavioral Risk Factor Surveillance System
Codebook](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf)

We used these definitions to properly format and describe the data.

``` r
#read in data 
#filter to parameter education
#and format factors
diabetes_data <- read_csv('diabetes_binary_health_indicators_BRFSS2015.csv') %>%
    filter(Education == params$education) %>%
    mutate_at(vars(Diabetes_binary,
                   HighBP,
                   HighChol,
                   CholCheck,
                   Smoker,
                   Stroke,
                   HeartDiseaseorAttack,
                   PhysActivity,
                   Fruits,
                   Veggies,
                   HvyAlcoholConsump, 
                   AnyHealthcare,
                   NoDocbcCost,
                   DiffWalk
                  ),
              ~factor(., 
                      levels = c(0,1), 
                      labels = c("no", "yes"))) %>%
    mutate(GenHlth = factor(GenHlth, 
                            levels = c(1:5), 
                            labels = c("excellent", "very good", "good", "fair", "poor")),
           Sex = factor(Sex, 
                        levels = c(0,1), 
                            labels = c("female", "male")),
           Age = factor(Age,
                        levels = c(1:13),
                        labels = c("18-24",
                                   "25-29",
                                   "30-34",
                                   "35-39",
                                   "40-44",
                                   "45-49",
                                   "50-54",
                                   "55-59",
                                   "60-64",
                                   "65-69",
                                   "70-74",
                                   "75-79",
                                   "80-99")),
           Education = factor(Education,
                        levels = c(1:6),
                        labels = c("Never attended school or only kindergarten",
                                    "Grades 1 through 8 (Elementary)",
                                    "Grades 9 through 11 (Some high school)",
                                    "Grade 12 or GED (High school graduate)",
                                    "College 1 year to 3 years (Some college or technical school)",
                                    "College 4 years or more (College graduate)")),
          Income = factor(Income,
                          levels = c(1:8), 
                            labels = c("Less than $10,000",
                                        "$10,000 to less than $15,000",
                                        "$15,000 to less than $20,000",
                                        "$20,000 to less than $25,000",
                                        "$25,000 to less than $35,000",
                                        "$35,000 to less than $50,000",
                                        "$50,000 to less than $75,000",
                                        "$75,000 or more"))
    )

#create a vector of column descriptions/labels
descriptions <- c("Diabetes or prediabetes?"
            ,"High Blood Pressure?"
            ,"High Cholesterol?"
            ,"Cholesterol check in the past 5 years?"
            ,"Body Mass Index"
            ,"Have you smoked at least 100 cigarettes in your entire life? [Note: 5 packs = 100 cigarettes]"
            ,"(Ever told) you had a stroke?"
            ,"Coronary heart disease (CHD) or myocardial infarction (MI)?"
            ,"Physical activity in past 30 days - not including job?"
            ,"Consume Fruit 1 or more times per day?"
            ,"Consume Vegetables 1 or more times per day?"
            ,"Heavy Alcohol Consumption (adult men >=14 drinks per week and adult women>=7 drinks per week)"
            ,"Have any kind of health care coverage, including health insurance, prepaid plans such as HMO, etc.?"
            ,"Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?" 
            ,"Would you say that in general your health is:" 
            ,"Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?"
            ,"Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?"
            ,"Do you have serious difficulty walking or climbing stairs?"
            ,"Sex"
            ,"Age Group"
            ,"What is the highest grade or year of school you completed?" 
            ,"Is your annual household income from all sources:") 


#add variable descriptions as labels
label(diabetes_data) <- descriptions

#subtitle <- diabetes_data$Education %>%
#    droplevels() %>%
#    unique() %>%
#    as.character() %>%
#    paste0('Education Level: ', .)
```

### Summary Statistics

Univariate summary statistics of each variable can be seen below:

``` r
#show univariate summary statistics
print(dfSummary(diabetes_data
                ,varnumbers = FALSE
                ,valid.col = FALSE
                ,graph.magnif = .8
                ),
      method = 'render',
      headings = FALSE,
      bootstrap.css = FALSE
      )
```

<div class="container st-container">
<table class="table table-striped table-bordered st-table st-table-striped st-table-bordered st-multiline ">
  <thead>
    <tr>
      <th align="center" class="st-protect-top-border"><strong>Variable</strong></th>
      <th align="center" class="st-protect-top-border"><strong>Label</strong></th>
      <th align="center" class="st-protect-top-border"><strong>Stats / Values</strong></th>
      <th align="center" class="st-protect-top-border"><strong>Freqs (% of Valid)</strong></th>
      <th align="center" class="st-protect-top-border"><strong>Graph</strong></th>
      <th align="center" class="st-protect-top-border"><strong>Missing</strong></th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td align="left">Diabetes_binary
[factor]</td>
      <td align="left">Diabetes or prediabetes?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7182</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">75.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">2296</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">24.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGcAAAAsBAMAAACK8LtlAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADpJREFUSMdjYBieQIkUoADVpGxMAhjVNKpp5GgiKz8JkgIEKNGE5gDiNKEGhNGoplFNo5ooyE/DDQAAwbfsdnyxE8oAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDCwnPihAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwwcFAHQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HighBP
[factor]</td>
      <td align="left">High Blood Pressure?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3975</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">41.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">5503</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">58.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFIAAAAsBAMAAAAEKvIZAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADpJREFUOMtjYBg6QAkXUBSEAahKZWMcYFTlqEqaqSQ+fQoSBiSrVCIMCPkIDoxGVY6qpLJK4tPnUAAAwpzSTu0/ARwAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDCwnPihAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwwcFAHQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HighChol
[factor]</td>
      <td align="left">High Cholesterol?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4741</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">50.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4737</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">50.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEkAAAAsBAMAAADbZIgGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADhJREFUOMtjYBi8QAkvUICqUjbGB0ZVjarCpYq49CWIFwiQpAq/jQLEuN5oVNWoKlyqiEtfgxEAAOvxyJoO9os0AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">CholCheck
[factor]</td>
      <td align="left">Cholesterol check in the past 5 years?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">344</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">3.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">9134</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">96.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAIAAAAAsBAMAAABfzq1tAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAD9JREFUSMdjYBgFIKCkpChIHoAaoGw8asCoAaMGDBMDKC4PyNRNRQOUyAbwQCQTGI0aMGrAqAGDzACKy4ORDgA4TglZOMmHrgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">BMI
[numeric]</td>
      <td align="left">Body Mass Index</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 29.6 (7.4)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">13 &le; 28 &le; 95</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 8 (0.2)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">71 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAIRJREFUWMPt2N0JwCAMRlFXsBs03aDZf7dakSLUn9C8FLwfvh5QCTEYwtqJT0Rkj+a8+KEKh8PhcHiLb+mJ8XBVhS/LU/GIg6d1wuFwOPzn3Njqe9y4ATjcxe+B1sMzqrjlC2bA1TBjwXt8evljPh1wp3w84BYuueRavPTNzjEq/iVh7Vz9Q5CrmTD2mAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Smoker
[factor]</td>
      <td align="left">Have you smoked at least 100 cigarettes
in your entire life? [Note: 5 packs =
100 cigarettes]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3582</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">37.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">5896</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">62.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFcAAAAsBAMAAADiAzldAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADxJREFUOMtjYBiaQAk7EEQBAlDFysbYgNGo4lHFg0UxSelZkBhAlmIlYoACXg+igVHFo4oHTjFJ6XmoAQCBrtlWqGU5sAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Stroke
[factor]</td>
      <td align="left">(Ever told) you had a stroke?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">8648</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">91.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">830</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">8.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHoAAAAsBAMAAABYoLE9AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAD1JREFUSMdjYBi5QIk8ANWtbEwOMBrVPap7VDdddFOWvwXJA1TRDXaHIrm6waE2qntU96juQaqbsvw9EgEAfNwCLV+Gj1kAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDCwnPihAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwwcFAHQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HeartDiseaseorAttack
[factor]</td>
      <td align="left">Coronary heart disease (CHD) or
myocardial infarction (MI)?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7860</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">82.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1618</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">17.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHAAAAAsBAMAAABPgiH0AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADxJREFUSMdjYBgZQIlkANWobEwiMBrVOKpxVCMBjWTnR0GSAaUaEU5QJE0jInBGNY5qHNVIJY1k58fhDgAgHvYq9lWBRAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">PhysActivity
[factor]</td>
      <td align="left">Physical activity in past 30 days - not
including job?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4119</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">43.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">5359</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">56.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFAAAAAsBAMAAAAA3yIkAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADpJREFUOMtjYBgaQAknUBSEAKhCZWNcYFThqEKqKSQ6PQoSBKQqVCIICHoGCoxGFY4qpFgh0elxsAMAS6fP6l2iB5cAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDCwnPihAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwwcFAHQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Fruits
[factor]</td>
      <td align="left">Consume Fruit 1 or more times per day?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4515</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">47.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4963</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">52.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEsAAAAsBAMAAADfkVg7AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADtJREFUOMtjYBjcQAkfEBQUgCpTNsYNjEaVjSojVhmR6U0QPyBRmRJ+oECEF4BgVNmoMuKUEZneBisAAA7Zyv49hUB7AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Veggies
[factor]</td>
      <td align="left">Consume Vegetables 1 or more times per
day?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3068</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">32.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">6410</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">67.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAF4AAAAsBAMAAAAeFhKXAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADtJREFUSMdjYBj6QAkTKApiA1D1ysYYYFT9qPohq57U9C9ILCBTvRKxALd/sQKjUfWj6oeAelLT/1AGADSV4KZutE3PAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HvyAlcoholConsump
[factor]</td>
      <td align="left">Heavy Alcohol Consumption (adult men
>=14 drinks per week and adult women>=7
drinks per week)</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">9108</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">96.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">370</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">3.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAIAAAAAsBAMAAABfzq1tAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAD9JREFUSMdjYBgFIKBENoAaoGxMJjAaNWDUgFEDBpkBFJcHgmQDahmgpKRImQHKxqMGjBowasAwMYDi8mCkAwAGGAlZfF7W5gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">AnyHealthcare
[factor]</td>
      <td align="left">Have any kind of health care coverage,
including health insurance, prepaid
plans such as HMO, etc.?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1134</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">12.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">8344</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">88.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHYAAAAsBAMAAABCnFGzAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAD9JREFUSMdjYBh5QAkCFAVJAVC9ysZgMKp3VO+o3gHSS0n+JUkPFfUqkQNQw4okYDSqd1TvqF6q6aUk/44kAACdMP1W+4LK6gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">NoDocbcCost
[factor]</td>
      <td align="left">Was there a time in the past 12 months
when you needed to see a doctor but
could not because of cost?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7935</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">83.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1543</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">16.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHEAAAAsBAMAAACgQErKAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAD5JREFUSMdjYBg5QIlkoADVqWxMKhjVOapzVCdpOsnPn4IkAwGKdSJcQapOeAgZjeoc1TmqkzY6yc+fIwEAAIdw+GpVmq2IAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">GenHlth
[factor]</td>
      <td align="left">Would you say that in general your
health is:</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. excellent</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. very good</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. good</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. fair</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. poor</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">710</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">7.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1573</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">16.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3233</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">34.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">2626</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">27.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1336</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">14.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAADYAAABmBAMAAABy5X8ZAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAGVJREFUSMftksEJACEMBG3BEk47MP33JqLcyRkWUQh57HznszAbgj2pEX8Ml0Wk0Ll2qF/U2HDp5VG3dOi8OtTv9BN7f5lYdn4UOnOHGpl8Qv3u2EjnzqF+159QXJ430Ll0qJ8lFfwjAFEnNU0aAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">MentHlth
[numeric]</td>
      <td align="left">Now thinking about your mental health,
which includes stress, depression, and
problems with emotions, for how many
days during the past 30 days was your
mental health not good?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 5.3 (9.8)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">0 &le; 0 &le; 30</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 5 (1.8)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">30 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAHhJREFUWMPt2MENgCAMhWFWkA3EDej+u0mTEpsAiYon+V9C0oNfQbg1hLWTSvbteYwfIgKHw+FwOBwOh8PhcDgc/iMedVwywQvJ8C+4PsQELyvb5Mt1GjbtcfsHLe0rV97iul3LYzORG/Ba2sErv+7WOnmeXiasnRMB2G5PobuvtgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">PhysHlth
[numeric]</td>
      <td align="left">Now thinking about your physical
health, which includes physical illness
and injury, for how many days during
the past 30 days was your physical
health not good?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 7.9 (11.3)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">0 &le; 0 &le; 30</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 15 (1.4)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">31 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAIFJREFUWMPt2MEJwCAMhWFXqBvUbqD771alkYZaEc3R/4GQy4eG4EGd2zsh5zzmI/xKKcHhcDgcDofD4XA4HN6PLy9PA88kwuHwVV4uoIHnFY184u/pj0sPqo9uSx3+6UNK35xrzMvOlTejGXNVKi4HGfBn51qGd7JeSsXDYtzeuQFIAXugJqWiSwAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">DiffWalk
[factor]</td>
      <td align="left">Do you have serious difficulty walking
or climbing stairs?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. no</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. yes</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">5969</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">63.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3509</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">37.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFgAAAAsBAMAAAATCGLQAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADlJREFUOMtjYBiaQIkoAFWsbEwEMBpVPKp4wBSTlJ4FiQLkKMZurSJ2xdg9OKp4VPGgUUxSeh5qAAB2otl6iyzM2gAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Sex
[factor]</td>
      <td align="left">Sex</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. female</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. male</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">5512</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">58.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3966</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">41.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFIAAAAsBAMAAAAEKvIZAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAADhJREFUOMtjYBg6QIkwgKpUNiYEjEZVjqqkskri06cgYUCySpx2KqKrxOmjUZWjKmmmkvj0ORQAAIUr0k4lZAV0AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Age
[factor]</td>
      <td align="left">Age Group</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 18-24</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 25-29</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. 30-34</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. 35-39</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. 40-44</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. 45-49</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">7. 50-54</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">8. 55-59</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">9. 60-64</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">10. 65-69</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">[ 3 others ]</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">193</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">2.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">192</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">2.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">373</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">3.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">418</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">4.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">516</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">5.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">619</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">6.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">975</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">10.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1087</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">11.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1034</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">10.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1022</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">10.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">3049</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">32.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAADMAAADWBAMAAACOBqcoAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAJlJREFUWMPt0sENgDAMA0BGoBtA2KDdfzceIGgb8KNAglTne49YsofB9mQO9Y07LSQnAqWE8DJxAD8k0wHoZ0fCRPIhUIpu8vEA5D5hJLkQKAVU2Uai13YmTCQXAqWAKttI9LMsYSJ5ECilrWVAIupbnrDKSDIhUMpHA5Ar2hJGkjmBUjiAHogD6JwcBpDfVCQsjmRCoBSrWwFZ+YWKv1GrTAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMLCc+KEAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMTZUMDI6NTc6MjUrMDA6MDDBwUAdAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Education
[factor]</td>
      <td align="left">What is the highest grade or year of
school you completed?</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. Never attended school or </td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. Grades 1 through 8 (Eleme</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. Grades 9 through 11 (Some</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. Grade 12 or GED (High sch</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. College 1 year to 3 years</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. College 4 years or more (</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">9478</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">100.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAIQAAAB4BAMAAADBOkf2AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAGVJREFUWMPt1LERgDAMBEG3QAmGDqD/3gggIMXSMNawV8AGH3xretaX8RAIBOIlERASiTm2QCAQPyECQiKxBrqJ7RhuRyAQhYiEv5jj+DoCgUB8RwQEx4dAIGoSAcHxIRCImoSuTh7rQjDQvEOrAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Income
[factor]</td>
      <td align="left">Is your annual household income from
all sources:</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. Less than $10,000</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. $10,000 to less than $15,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. $15,000 to less than $20,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. $20,000 to less than $25,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. $25,000 to less than $35,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. $35,000 to less than $50,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">7. $50,000 to less than $75,</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">8. $75,000 or more</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1536</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">16.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1465</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">15.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1709</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">18.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1453</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">15.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1268</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">13.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">921</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">9.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">590</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">6.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">536</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">5.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAACIAAACfBAMAAABuPRZaAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsQAjkZiTcWAAAAAGhJREFUSMdjYKAeUIIDQaiIsjEUGI2KYAsfQQQgQQRujCKGXSNQBDM0KAxVJSV0u4yNRpwIZmiMptXBl1YVBdHtGokimKFBSagii4DtMhqZIpihQX6ooooow2wagSKYoTEaqvQJZ2oAAJIl5UjM5hHXAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTE2VDAyOjU3OjI1KzAwOjAwsJz4oQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0xNlQwMjo1NzoyNSswMDowMMHBQB0AAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
  </tbody>
</table>
<p>Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> 1.0.1 (<a href='https://www.r-project.org/'>R</a> version 4.2.1)<br/>2023-11-15</p>
</div>

### Contingency Tables

#### High Blood Pressure

``` r
#create contingency table of High Blood Pressure and Diabetes
with(diabetes_data,
    ctable( x = HighBP, 
            y = Diabetes_binary,
            chisq = TRUE)) %>%
    print(method = 'render')
```

<div class="container st-container">
<h3>Cross-Tabulation, Row Proportions</h3>
<h4>HighBP * Diabetes_binary</h4>
<strong>Data Frame</strong>: diabetes_data
<br/>
<table class="table table-bordered st-table st-table-bordered st-cross-table ">
<thead>
<tr>
<th></th>
<th colspan="8" align="center" class="st-protect-top-border">Diabetes_binary</th>
<th colspan="4"></th>
</tr>
<tr>
<td align="center">
<strong>HighBP</strong>
</td>
<th colspan="4" align="center">no</th>
<th colspan="4" align="center">yes</th>
<th colspan="4" align="center">Total</th>
</tr>
</thead>
<tbody>
<tr>
<td>
<strong align="center">no</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">3528</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">88.8%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">447</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">11.2%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">3975</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">yes</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">3654</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">66.4%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">1849</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">33.6%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">5503</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">Total</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">7182</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">75.8%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">2296</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">24.2%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">9478</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="100"><em><strong>&nbsp;&#935;<sup>2</sup></strong> = 627.0852&nbsp;&nbsp;&nbsp;<strong>df</strong> = 1&nbsp;&nbsp;&nbsp;<strong>p</strong> = .0000</em><br/></td>
</tr>
</tfoot>
</table>
<p>Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> 1.0.1 (<a href='https://www.r-project.org/'>R</a> version 4.2.1)<br/>2023-11-15</p>
</div>

#### High Cholesterol

``` r
#create contingency table of High Cholesterol Pressure and Diabetes
with(diabetes_data,
    ctable( x = HighChol, 
            y = Diabetes_binary,
            chisq = TRUE)) %>%
    print(method = 'render')
```

<div class="container st-container">
<h3>Cross-Tabulation, Row Proportions</h3>
<h4>HighChol * Diabetes_binary</h4>
<strong>Data Frame</strong>: diabetes_data
<br/>
<table class="table table-bordered st-table st-table-bordered st-cross-table ">
<thead>
<tr>
<th></th>
<th colspan="8" align="center" class="st-protect-top-border">Diabetes_binary</th>
<th colspan="4"></th>
</tr>
<tr>
<td align="center">
<strong>HighChol</strong>
</td>
<th colspan="4" align="center">no</th>
<th colspan="4" align="center">yes</th>
<th colspan="4" align="center">Total</th>
</tr>
</thead>
<tbody>
<tr>
<td>
<strong align="center">no</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">4027</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">84.9%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">714</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">15.1%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">4741</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">yes</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">3155</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">66.6%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">1582</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">33.4%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">4737</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">Total</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">7182</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">75.8%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">2296</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">24.2%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">9478</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="100"><em><strong>&nbsp;&#935;<sup>2</sup></strong> = 433.0200&nbsp;&nbsp;&nbsp;<strong>df</strong> = 1&nbsp;&nbsp;&nbsp;<strong>p</strong> = .0000</em><br/></td>
</tr>
</tfoot>
</table>
<p>Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> 1.0.1 (<a href='https://www.r-project.org/'>R</a> version 4.2.1)<br/>2023-11-15</p>
</div>

#### Deferred care because of cost by Coverage Status

``` r
#create contingency table of Deferred care because of cost versus Coverage
with(diabetes_data,
    ctable( x = AnyHealthcare, 
            y = NoDocbcCost,
            chisq = TRUE)) %>%
    print(method = 'render')
```

<div class="container st-container">
<h3>Cross-Tabulation, Row Proportions</h3>
<h4>AnyHealthcare * NoDocbcCost</h4>
<strong>Data Frame</strong>: diabetes_data
<br/>
<table class="table table-bordered st-table st-table-bordered st-cross-table ">
<thead>
<tr>
<th></th>
<th colspan="8" align="center" class="st-protect-top-border">NoDocbcCost</th>
<th colspan="4"></th>
</tr>
<tr>
<td align="center">
<strong>AnyHealthcare</strong>
</td>
<th colspan="4" align="center">no</th>
<th colspan="4" align="center">yes</th>
<th colspan="4" align="center">Total</th>
</tr>
</thead>
<tbody>
<tr>
<td>
<strong align="center">no</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">654</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">57.7%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">480</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">42.3%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">1134</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">yes</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">7281</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">87.3%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">1063</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">12.7%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">8344</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
<tr>
<td>
<strong align="center">Total</strong>
</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">7935</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">83.7%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">1543</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">16.3%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
<td align="right" style="padding:0 0 0 15px;border-right:0;text-align:right">9478</td>
<td align="left" style="padding:0 1px 0 4px;border-left:0;border-right:0;text-align:left">(</td>
<td align="left" style="padding:0;border-left:0;border-right:0;text-align:right">100.0%</td>
<td align="left" style="padding:0 15px 0 1px;border-left:0;text-align:right">)</td>
</tr>
</tbody>
<tfoot>
<tr>
<td colspan="100"><em><strong>&nbsp;&#935;<sup>2</sup></strong> = 639.0887&nbsp;&nbsp;&nbsp;<strong>df</strong> = 1&nbsp;&nbsp;&nbsp;<strong>p</strong> = .0000</em><br/></td>
</tr>
</tfoot>
</table>
<p>Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> 1.0.1 (<a href='https://www.r-project.org/'>R</a> version 4.2.1)<br/>2023-11-15</p>
</div>

### Plots

#### BMI

``` r
#BMI boxplots

ggplot(data = diabetes_data, aes(y = BMI, fill = Diabetes_binary)) + 
    geom_boxplot()
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### Health Status (Past 30 Days)

``` r
#reformat the not healthy days variables into long 
health_days <- diabetes_data %>%
    select(Diabetes_binary, MentHlth, PhysHlth) %>%
    pivot_longer(cols = c(MentHlth, PhysHlth)) %>%
    mutate(name = if_else(name == 'MentHlth', 'Days of Not Good Mental Health (Past 30 days)', 'Days of Not Good Physical Health (Past 30 days)')) %>%
    mutate(Diabetes = if_else(Diabetes_binary == 'yes', 'Diabetes or Prediabetes', 'No Diabetes'))
    
#plot not good healthy days by diabetes
ggplot(data = health_days, aes(x = value, fill = Diabetes)) + 
    geom_histogram(bins =5, alpha = .5, position = "dodge", show.legend = FALSE) + 
    facet_wrap(.~name + Diabetes) 
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Income

``` r
ggplot(data = diabetes_data, aes(y = Income, fill = Diabetes_binary)) +
    geom_bar(position = 'dodge')
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### BMI by Vegetables in Diet, across Sex

``` r
ggplot(data = diabetes_data, aes(x=BMI, y=Veggies, fill=Sex )) + 
  geom_boxplot()
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

#### BMI by Smoking Status, across Sex

``` r
ggplot(data = diabetes_data, aes(x=BMI, y=Smoker, fill=Sex )) + 
  geom_boxplot()
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Modeling

### Data Partition

We split the data into training and testing sets with 70% in the
training set and 30% in the testing set.

``` r
#split data into 70/20 train/test
set.seed(42)
train_index <- createDataPartition(diabetes_data$Diabetes_binary, p = .7, list = FALSE) 

diabetes_train <- diabetes_data[train_index, ]
diabetes_test <- diabetes_data[-train_index, ]
```

### Data Preprocessing

We applied centering and scaling to our training and test data.

``` r
#set up center and scale from the training set
pre_process_values <- preProcess(diabetes_train, method = c("center", "scale"))
#apply center and scaling to both the training and test set
train_preprocessed <- predict(pre_process_values, diabetes_train)
test_preprocessed <- predict(pre_process_values, diabetes_test)
```

### Log Loss \[Heather\]

In binary classification problems log loss is used to measure
performance by comparing the predicted values to the actual values. Log
loss increases as the predicted probability diverges further away from
the actual. Log loss is a value between 0 and 1 with a perfect model
having a log loss of 0.

We can calculate log loss as follows:

$$log loss = \frac{1}{N}\sum[y_i*log(p_i)+(1-y_i)*log(1-p_i)]$$ Where:

- $N$ = the number of predictions
- $y_i$ = the actual outcome of instance $i$
- $p_i$ = the probability that the model predicts for instance $i$

Logloss has some advantages over other measures of performance. It works
well with imbalanced classes in the outcome variable where measures like
accuracy can be misleading.

We set our train control metric to logloss

``` r
train_control <- trainControl(method = 'cv',
                              number = 5,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss)
```

### Logistic Regression \[Heather\]

[Logistic
regression](https://simple.wikipedia.org/wiki/Logistic_regression) is a
generalized linear model appropriate to binary outcome data. The
predictor variables may be continuous or categorical. It models the
probability of success of the outcome class using the logistic function

$$P(success) = \frac{e^{\beta_0 + B_1x_1 + B_2x_2 +... + B_nx_n}}{1 + e^{\beta_0 + B_1x_1 + B_2x_2 +... + B_nx_n}}$$

In logistic regression the logit function links the probability to a
linear combination of the parameters:

$$logit(p) = log(\frac{p}{1-p})$$

It can be shown that:

$$log(\frac{p}{1-p}) = \beta_0 + \beta_1x_1 + \beta_2x_2 + ... + \beta_nx_n + \epsilon$$

Aside from the dependent variable being binary Logistic regression does
have a few other key assumptions:

- Independence of errors
- The predictor variables are linearly related to the log odds/logit
- Absence of Multicollinearity
- Absence of strongly influential outliers

For this project we fit three different logistic regression models

``` r
#note there are no tuning parameters available for this model
set.seed(42)
log_reg_1 <- train(Diabetes_binary ~ ., 
                   data = train_preprocessed, 
                   method = 'glm', family = 'binomial',
                   metric="logLoss",
                   trControl = train_control)

log_reg_2 <- train(Diabetes_binary ~ BMI + HighBP + HighChol + MentHlth,
                   data = train_preprocessed, 
                   method = 'glm', family = 'binomial',
                   metric="logLoss",
                   trControl = train_control)

log_reg_3 <- train(Diabetes_binary ~ HighBP + HeartDiseaseorAttack + AnyHealthcare,
                   data = train_preprocessed, 
                   method = 'glm', family = 'binomial',
                   metric="logLoss",
                   trControl = train_control)

#choose the best model: 
results <- tibble( 'model' = c('full model',  
                  'BMI + HighBP + HighChol + MentHlth', 
                  'HighBP + HeartDiseaseorAttack + AnyHealthcare' 
                  ),
        'Log Loss' = c(log_reg_1$results$logLoss,
                        log_reg_2$results$logLoss,
                        log_reg_3$results$logLoss
                       ))


knitr::kable(results, Caption = "Logistic Regression Model Log Loss")
```

| model                                         |  Log Loss |
|:----------------------------------------------|----------:|
| full model                                    | 0.4736374 |
| BMI + HighBP + HighChol + MentHlth            | 0.4939994 |
| HighBP + HeartDiseaseorAttack + AnyHealthcare | 0.5097199 |

We choose the model with the lowest log loss which is

### LASSO \[Andy\]

The [LASSO (Least Absolute Shrinkage and Selection
Operator)](https://en.wikipedia.org/wiki/Lasso_(statistics)) is a
regression modeling technique that adds a penalty term to the loss/cost
function. The penalty term (seen below) is the sum of the absolute
values of the estimated beta coefficients (also called the L1-norm or
“Manhattan” distance), multiplied by a hyperparameter $\lambda$. This
penalty term is added to the appropriate loss function, and the model
estimate procedure attempts to minimize the entire quantity.

$$lasso: log loss + penalty = \frac{1}{N}\sum[y_i*log(p_i)+(1-y_i)*log(1-p_i)] + \lambda\sum||\beta||$$

The first quantity (the original loss function) is minimized by choosing
values for $\beta$ that are not equal to zero (assuming there is any
“signal” at all present in the input variables). However, choosing
non-zero values for $\beta$ will then increase the value of the penalty
term. So in minimizing the entire penalized loss function, the two parts
are at odds with each other: decreasing one part necessarily increases
the other, and vice versa. The net effect is that the values of $\beta$
chosen in this procedure are shrunken (biased to be closer to zero)
versus those found in the usual un-penalized approach. The $\lambda$
hyperparameter adjusts the relative impact of the penalty on the overall
loss function. You can think of $\lambda$ as the “exchange rate that
governs the amount of non-zero coefficients you can buy”.

The end result is that you intentionally introduce bias into the
coefficients in order to reduce model variance. Models training using
penalization are simpler and less likely to be overfit. An interesting
side-effect of using the absolute value function in the penalty term is
that the model estimates for very small coefficients often get shrunk
directly to zero. In this manner, the LASSO performs variable selection.

In summary, there are two reasons why you would want to use the LASSO
over standard logistic regression:

1.  If you have many input variables (perhaps with some collinearity
    issues), and you want an automated variable selection routine that
    performs better than stepwise/forward/backward selection, and
2.  You want a final model that is not over-fit so it can predict well
    on data it hasn’t yet seen (and you prefer to stay within a linear
    model paradigm).

``` r
#fit lasso
set.seed(42)
lasso <- train(Diabetes_binary ~ ., 
               data = train_preprocessed, 
               method = 'glmnet', family = 'binomial',
               metric="logLoss",
               trControl = train_control,
               tuneGrid = expand.grid(alpha = 1, lambda = seq(0,1,0.005)))

#get the best tune
lasso$results %>%
    filter(logLoss == min(logLoss)) %>%
    knitr::kable(row.names = FALSE, caption = "LASSO Best Fit")
```

| alpha | lambda |   logLoss | logLossSD |
|------:|-------:|----------:|----------:|
|     1 |      0 | 0.4734542 | 0.0096779 |

LASSO Best Fit

``` r
# plot(lasso$finalModel, "lambda")
# abline(v = log(lasso$bestTune$lambda), lty = 2)
# text(x=log(lasso$bestTune$lambda), y=0, labels="Lambda for\nmin CV-LogLoss", cex=0.75, adj=0.55, srt=90)
```

### Classification Tree \[Andy\]

Classification tree models attempt to recursively partition the input
feature space into local areas that maximize the homogeneity of the
outcome instances contained within them. Or said more simply,
classification trees find combinations of input variable values that
tend to associate with one of the output variable’s classes. Predictions
made with classification tree models are very easy to express in simple
terms, as they are just intersections of logical conditions. For
example:

*if (class = senior) and (credit_hours \> 120) and
(parking_fines_outstanding = 0), then ready_to_graduate = yes.*

For this reason, classification trees are useful for prediction models
that must be directly inspected. They also innately handle interactions
between variables and non-linearities by simply having an interacting
effect appear in multiple “levels” of a branch of the tree.
Unfortunately, individual classification trees are considered to be weak
learners that are high-bias (typically underfitted). To remedy this, we
often create large ensembles of classification trees, each one “grown”
slightly different from each other through the use of resampling rows of
the training data (bagging) and limiting the selection set at each split
(random forests). Classification trees are also a poor choice if we
strongly believe the underlying mechanism that generated the data was a
linear combination of inputs (like a regression model).

``` r
set.seed(42)
# train a single classification tree
tree <- train(Diabetes_binary ~ ., 
                  data = train_preprocessed, 
                  method = "rpart",
                  metric="logLoss",
                  trControl = train_control,
                  tuneGrid = expand.grid(cp=seq(0,1,0.01)))

#get the best tune
tree$results %>%
    filter(logLoss == min(logLoss)) %>%
    knitr::kable(row.names = FALSE, caption = "Classification Tree Best Fit")
```

|   cp |   logLoss | logLossSD |
|-----:|----------:|----------:|
| 0.01 | 0.5031251 | 0.0073358 |

Classification Tree Best Fit

``` r
#if there is more than a root node in the final model then create a tree plot:
if (length(tree$finalModel$splits) > 0) {
fancyRpartPlot(tree$finalModel)

} 
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Random Forest \[Heather\]

Random forest is an ensemble method which uses many trees (in our case
classification trees) to predict the outcome. These trees are each fit
to a different bootstrap sample of the original training set. Each
bootstrap sample is sampled with replacement and has the same number of
observations as the training set. Each tree fit has a random selection
of the variables chosen.

``` r
set.seed(42)
random_forest <- train(Diabetes_binary ~ .,
            data = train_preprocessed,
            method = "ranger",
            num.trees = 100,
            importance = 'impurity', 
            trControl = train_control,
            metric="logLoss",
            tuneGrid = expand.grid(mtry = 1:7, min.node.size = 1, splitrule = 'gini')
            )


#get the best tune
random_forest$results %>%
    filter(logLoss == min(logLoss)) %>%
    knitr::kable(row.names = FALSE, caption = "Random Forest Best Fit")
```

| mtry | min.node.size | splitrule |   logLoss | logLossSD |
|-----:|--------------:|:----------|----------:|----------:|
|    4 |             1 | gini      | 0.4744094 | 0.0095702 |

Random Forest Best Fit

``` r
#get the top n variables by importance where n = the best tune (best # of predictors chosen)
variable_importance <- varImp(random_forest)$importance %>% 
    arrange(-Overall) %>% 
    head(20) %>%
    rownames_to_column('variable') %>%
    mutate(variable = fct_reorder(variable, Overall))

#create lollipop plot of the top n variables by variable importance
ggplot(variable_importance, aes(y = variable, x = Overall)) + 
    geom_point() + 
    geom_segment(aes(y=variable, yend=variable, x=0, xend=Overall)) +
    xlab('Overall Importance')
```

![](Grades9_11_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

In our model we tuned the variable mtry which is the number of predictor
variables to include in the models. The best tune turned out to be 4.
The top 10 variables by importance are shown above

### Ridge Regression \[Andy\]

Ridge regression can be thought of as a sibling to the LASSO: they vary
only by the function/norm used in the penalty function. As seen below,
the penalized loss function for ridge regression replaces the absolute
value in the penalty term with a squared exponent:

$$ridge: log loss + penalty = \frac{1}{N}\sum[y_i*log(p_i)+(1-y_i)*log(1-p_i)] + \lambda\sum\beta^2$$

Instead of summing the absolute values of the $\beta$ coefficients, we
sum their squares. This still enforces a penalty that grows along with
the coefficients, but it does not force smaller estimates to zero like
the LASSO does. Because of this, ridge regression does not perform
variable selection.

The reason you might want ridge regression over standard logistic
regression and the LASSO is that you have many input variabes (perhaps
with collinearity issues), and you want good out-of-sample predictive
performance but don’t want variable selection. In cases where you might
have real-but-small effects for some inputs, the LASSO would shrink them
to 0, while ridge would just shrink them smaller (but not necessarily to
0).

``` r
set.seed(42)
ridge <- train(Diabetes_binary ~ ., 
               data = train_preprocessed, 
               method = 'glmnet', family = 'binomial',
               metric="logLoss",
               trControl = train_control,
               tuneGrid = expand.grid(alpha = 0, lambda = seq(0,1,0.005)))

#get the best tune
ridge$results %>%
    filter(logLoss == min(logLoss)) %>%
    filter(lambda == ridge$bestTune$lambda) %>%
    knitr::kable(row.names = FALSE, caption = "Ridge Regression Best Fit")
```

| alpha | lambda |   logLoss | logLossSD |
|------:|-------:|----------:|----------:|
|     0 |   0.01 | 0.4730467 | 0.0083182 |

Ridge Regression Best Fit

### Elastic Net \[Heather\]

[Elastic net](https://en.wikipedia.org/wiki/Elastic_net_regularization)
combines the features of both Lasso and Ridge regression. It does so by
incorporating both of their penalty terms $L_1$ (Lasso) and $L_2$
(Ridge) into the loss function of the regression model. A weighted sum
of the two penalties is taken as the regularization term. The penalty
term is a linear combination of $L_1$ and $L_2$ controlled by a mixing
term $\alpha$ when $\alpha = 1$ the penalty is purely L_1 (Lasso) and
when $\alpha = 0$ the penalty is purely L_2 (Ridge).

Elastic net overcomes some of the limitations of Lasso:

- In the case of a large number of predictors with fewer observations
  (large p, small n) lasso will select n variables at most.
- In the case of highly corrolated variables Lasso tends to select one
  variable and ignore the others.

Elastic net overcomes these limitations by adding a quadratic to the
penalty $||\beta||^2$. When used alone this penalty is ridge regression.

The estimates of elastic net are modeled as:

$$\hat\beta = \underset{\beta}argmin(||y=X\beta||^2 +\lambda _{2}\|\beta \|^{2}+\lambda _{1}\|\beta \|_{1} )$$

``` r
lambda <- seq(0, 3, 0.1)
alpha <- seq(0, 1, 0.1)

grid <- expand.grid(alpha = alpha, lambda = lambda)


set.seed(42)
elastic_net <- train(Diabetes_binary ~ .,
                     data = train_preprocessed,
                     method = "glmnet",
                     metric="logLoss",
                     trControl = train_control,
                     tuneGrid = grid
)



#get the best tune
elastic_net$results %>%
    filter(logLoss == min(logLoss)) %>%
    knitr::kable(row.names = FALSE, caption = "Elastic Net Best Fit")
```

| alpha | lambda |   logLoss | logLossSD |
|------:|-------:|----------:|----------:|
|     0 |      0 | 0.4730467 | 0.0083182 |

Elastic Net Best Fit

## Final Model Selection

``` r
# stack model objects into a list
models <- list(logistic_regression_1 = log_reg_1,
               logistic_regression_2 = log_reg_2,
               logistic_regression_3 = log_reg_3,
               tree = tree,
               random_forest = random_forest,
               ridge = ridge,
               lasso = lasso,
               elastic_net = elastic_net)

# create function to extract logloss on test dataset
get_logloss_on_test <- function(model){
  test_pred <- predict(model, newdata = test_preprocessed, type="prob")
  test_set <- data.frame(obs = test_preprocessed$Diabetes_binary, 
                         yes = test_pred[,2],
                         no = test_pred[,1])
  ll <- mnLogLoss(test_set, lev = levels(test_set$obs))
  
  return( ll )
}

# use function to get test set accuracy numbers
perf <- sapply(models, get_logloss_on_test)
perf
```

    ## logistic_regression_1.logLoss logistic_regression_2.logLoss logistic_regression_3.logLoss                  tree.logLoss         random_forest.logLoss                 ridge.logLoss                 lasso.logLoss           elastic_net.logLoss 
    ##                     0.4687746                     0.4932483                     0.5191420                     0.5131128                     0.4766085                     0.4696453                     0.4692101                     0.4696453

``` r
#get the best performing model which minimizes logloss
top_model <- names(which.min(perf)) %>%
    str_replace_all('.logLoss|_', ' ') %>%
    str_squish %>%
    str_to_title
```

The model with the lowest log loss is the Logistic Regression 1 model
with a log loss of = 0.469.
