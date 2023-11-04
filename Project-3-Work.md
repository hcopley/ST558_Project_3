Untitled
================
Heather Copley & Andy Johnson
2023-11-04

## Introduction

## Exploratory Data Analysis

[Behavioral Risk Factor Surveillance System
Codebook](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.cdc.gov/brfss/annual_data/2015/pdf/codebook15_llcp.pdf)

``` r
diabetes_data <- read_csv('diabetes_binary_health_indicators_BRFSS2015.csv') %>%
    mutate_at(vars(-BMI, -MentHlth, -PhysHlth), as.factor) %>%
    filter(Education == params$education)

glossary <- list("Diabetes_binary" = "0 = no diabetes 1 = prediabetes 2 = diabetes"
            ,"HighBP" = "0 = no high BP 1 = high BP"
            ,"HighChol" = "0 = no high cholesterol 1 = high cholesterol"
            ,"CholCheck" = "0 = no cholesterol check in 5 years 1 = yes cholesterol check in 5 years"
            ,"BMI" = "Body Mass Index"
            ,"Smoker" = "Have you smoked at least 100 cigarettes in your entire life? [Note: 5 packs = 100 cigarettes] 0 = no 1 = yes"
            ,"Stroke" = "(Ever told) you had a stroke. 0 = no 1 = yes"
            ,"HeartDiseaseorAttack" = "coronary heart disease (CHD) or myocardial infarction (MI) 0 = no 1 = yes"
            ,"PhysActivity" = "physical activity in past 30 days - not including job 0 = no 1 = yes"
            ,"Fruits" = "Consume Fruit 1 or more times per day 0 = no 1 = yes"
            ,"Veggies" = "Consume Vegetables 1 or more times per day 0 = no 1 = yes"
            ,"HvyAlcoholConsump" = "(adult men >=14 drinks per week and adult women>=7 drinks per week) 0 = no 1 = yes"
            ,"AnyHealthcare" = "Have any kind of health care coverage, including health insurance, prepaid plans such as HMO, etc. 
            0 = no 1 = yes"
            ,"NoDocbcCost" = "Was there a time in the past 12 months when you needed to see a doctor but could not because of cost? 
            0 = no 1 = yes"
            ,"GenHlth" = "Would you say that in general your health is: 
                    1 = Excellent, 
                    2 = Very good, 
                    3 = Good, 
                    4 = Fair, 
                    5 = Poor, 
                    7 = Don't know/Not Sure, 
                    9 = Refused"
            ,"MentHlth" = "Now thinking about your mental health, which includes stress, depression, and problems with emotions, for how many days during the past 30 days was your mental health not good?"
            ,"PhysHlth" = "Now thinking about your physical health, which includes physical illness and injury, for how many days during the past 30 days was your physical health not good?"
            ,"DiffWalk" = "Do you have serious difficulty walking or climbing stairs? 0 = no 1 = yes"
            ,"Sex" = "0 = female 1 = male"
            ,"Age" = "1 = 18 <= AGE <= 24,
                    2 = 25 <= AGE <= 29, 
                    3 = 30 <= AGE <= 34,
                    4 = 35 <= AGE <= 39, 
                    5 = 40 <= AGE <= 44, 
                    6 = 45 <= AGE <= 49, 
                    7 = 50 <= AGE <= 54, 
                    8 = 55 <= AGE <= 59, 
                    9 = 60 <= AGE <= 64,
                    10 = 65 <= AGE <= 69,
                    11 = 70 <= AGE <= 74, 
                    12 = 75 <= AGE <= 79, 
                    13 = 80 <= AGE <= 99 "
            ,"Education" = "What is the highest grade or year of school you completed? 
                    1 = Never attended school or only kindergarten,
                    2 = Grades 1 through 8 (Elementary),
                    3 = Grades 9 through 11 (Some high school),
                    4 = Grade 12 or GED (High school graduate),
                    5 = College 1 year to 3 years (Some college or technical school),
                    6 = College 4 years or more (College graduate),
                    9 = Refused"
            ,"Income" = "1 = Less than $10,000,
                        2 = $10,000 to less than $15,000,
                        3 = $15,000 to less than $20,000,
                        4 = $20,000 to less than $25,000,
                        5 = $25,000 to less than $35,000,
                        6 = $35,000 to less than $50,000,
                        7 = $50,000 to less than $75,000,
                        8 = $75,000 or more") 
```

### Univariate Analysis

``` r
library(summarytools)

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
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">127</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">73.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">47</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">27.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGQAAAAsBAMAAABhxwBmAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0czSOiegAAADlJREFUSMdjYBheQIkEANWibEw0MBrVMqplWGkhI78IkgDI14JuuSJhLejeH9UyqmXEaCEjvwwXAACEPefSd7f2FQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0wNFQyMzoyOToyOCswMDowMNpHT1kAAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMDRUMjM6Mjk6MjgrMDA6MDCrGvflAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HighBP
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">86</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">49.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">88</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">50.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEkAAAAsBAMAAADbZIgGAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0czSOiegAAADhJREFUOMtjYBi8QAkvEICqUjbGA4xGVY2qwqWKuPQliBeQpgq/jQrEuN54VNWoKlyqiEtfgxEAAMJWyJro6/6JAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI4KzAwOjAw2kdPWQAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOCswMDowMKsa9+UAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HighChol
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">87</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">50.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">87</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">50.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAEgAAAAsBAMAAAA0puM4AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAC5JREFUOMtjYBicQAkvgCpSNsYDjEYVjSqCKyIqPQniBaQoGk2+o4ronnwHGwAAMAjFO7vLjXYAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">CholCheck
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">4.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">167</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">96.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAH8AAAAsBAMAAAC+iXp5AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAEFJREFUSMdjYBgFIKCkpCRIFhCAGqBsbDRqwKgBowYMDwMoLg/I001NA5TIBQrwQCQTjBowasCoAYPNAIrLg5EOAIJ+CTW+RczsAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">BMI
[numeric]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 29.8 (7.3)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">14 &le; 28 &le; 57</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 8 (0.2)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">34 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAIFJREFUWMPt2FEKgCAQRVG3YDvIdpD731uoQaZpOQRh3vch83NARHEYpcaOTjIZl1nfpcStCxwOh8PhcPj33Hc1RswXB9f+eTgGMQ8lHA6Hw//Na5/FA24re4C/wsOwpsCvBzknnpqY+7Vrnt3fNp69IQk3RyQ8KnceNbJy3h41djbBYsoTNDYTWQAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMHwwRO0AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDANbfxRAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Smoker
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">108</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">62.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">66</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">37.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFcAAAAsBAMAAADiAzldAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADpJREFUOMtjYBiaQIkYoABVrGxMBBhVPKp44BSTlJ4FiQEC5CjGYTF2xdg9aDSqeFTxYFFMUnoeagAAooDZVriyx3wAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Stroke
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">160</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">92.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">14</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">8.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHsAAAAsBAMAAAC3YtoDAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAD5JREFUSMdjYBjZQIk8oADVrmxMFhjVPqp9VPtAaKcwvwuSBwSoox3sFPK1g4LOaFT7qPZR7UNDO4X5faQCAAxnBG3YUYPIAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HeartDiseaseorAttack
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">145</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">83.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">29</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">16.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHAAAAAsBAMAAABPgiH0AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADxJREFUSMdjYBgZQIlkANWobEwiMBrVOKpxVCMBjWTnR0GSAaUaEU5QJE0jInBGNY5qHNVIJY1k58fhDgAgHvYq9lWBRAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMHwwRO0AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDANbfxRAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">PhysActivity
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">79</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">45.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">95</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">54.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAE4AAAAsBAMAAAA5uJN/AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADpJREFUOMtjYBj8QAk3UBQEAag6ZWOcYFTdqDqy1RGb/gQJARLVKREChP0BBkaj6kbVkaGO2PQ3mAEA6NPNhmD7B9AAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Fruits
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">64</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">36.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">110</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">63.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFgAAAAsBAMAAAATCGLQAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADtJREFUOMtjYBiaQAkrUBREBVDFysbYwKjiUcWDRjFJ6VmQKECOYiWiAF4PogGjUcWjigdMMUnpeagBAKC52Xqdu2uwAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Veggies
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">47</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">27.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">127</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">73.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGQAAAAsBAMAAABhxwBmAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADtJREFUSMdjYBheQAkNKAriBlAtysaoYFTLqJYRo4WM/CJIAiBfixIJAIf38QCjUS2jWoaVFjLyy3ABAH4h59IJtEqkAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">HvyAlcoholConsump
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">167</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">96.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">4.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAH8AAAAsBAMAAAC+iXp5AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAEBJREFUSMdjYBgFIKBELlCAGqBsTCYYNWDUgFEDBpsBFJcHguQCAWoZAHQMZQYoGxuNGjBqwKgBw8MAisuDkQ4AoAwJNRdFy/0AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">AnyHealthcare
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">37</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">21.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">137</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">78.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGsAAAAsBAMAAACQzFvrAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAD1JREFUSMdjYBjeQAkZCBIEAlBtysYIYDSqbVTbqDbitJGZ3wirpKY2JdKAApYgIQKMahvVNqqNgvw2XAEAkHrxPj5MYHYAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">NoDocbcCost
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">133</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">76.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">41</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">23.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAGgAAAAsBAMAAAB7++DoAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADhJREFUSMdjYBieQIkkANWkbEwCMBrVNKppxGgiKz8JkgQo0YRqvSJxmlADYlTTqKZRTZTkp+EGADag7JqlbFr4AAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">GenHlth
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 1</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 2</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. 3</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. 4</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. 5</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">9</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">5.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">26</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">14.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">49</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">28.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">56</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">32.2%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">34</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">19.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAADMAAABmBAMAAACUzLRdAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAGtJREFUSMdjYKAvUFJSUhREBQJQKWVj41GpwSaFJ74EMQERUkowoIghBXQGBIxKDR4pPPFFXgIgJm3AABYXQoHRqBSdpPBECu0TgJKSAg4XAsGo1EBL4YkvyhMArjIKnDpHpQaFFJ74ohcAAIP++CzH+2fsAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">MentHlth
[numeric]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 6.6 (10.8)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">0 &le; 0 &le; 30</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 10 (1.6)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">14 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAFhJREFUWMPt17sNACAIAFFWcARxA91/NxPjpzAWUJlwVxHDa+gUiZ3ukqnJS5tVOBwOh8PhcDgcDofD4Z/w89t38bMNh48HvTLxe4jM9w2zi6+hvbk6k9h1CRTAlbpHirAAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">PhysHlth
[numeric]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">Mean (sd) : 8.9 (12.4)</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">min &le; med &le; max:</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">0 &le; 0 &le; 30</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">IQR (CV) : 20 (1.4)</td></tr></table></td>
      <td align="left" style="vertical-align:middle">16 distinct values</td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAHwAAABaBAMAAACIxS+mAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAGJJREFUWMPt2LENwCAMAEGvkGwQs0G8/25pkCkQlkMTKf6vEOIElCBSO/WOV3XerHfD4XA4HA6Hw+Fw+F/5eDht8bEaDofDS/Mz+IbK8OA8AfdN55krwdcD+5b7LSaum0ntHjLszMFnHgaaAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">DiffWalk
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">114</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">65.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">60</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">34.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFsAAAAsBAMAAAD4P9nTAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADpJREFUSMdjYBjaQIk4oABVrmxMFBhVPqp8cConMb0LEgcEyFOO1WrcyrF51WhU+ajyoaKcxPQ+VAEAp+7eHpv1GdAAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Sex
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 0</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 1</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">102</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">58.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">72</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">41.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAFMAAAAsBAMAAADr6JknAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAADlJREFUOMtjYBhaQIkwUIAqVTYmCEaVjiqlrVIS0qsgYSBAulKc1mIqxeUto1Glo0rppZSE9DpUAABz/9SO4WMxEAAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMHwwRO0AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDANbfxRAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Age
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 1</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 2</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. 3</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. 4</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. 5</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. 6</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">7. 7</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">8. 8</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">9. 9</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">10. 10</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">[ 3 others ]</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">4</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">2.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">1</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">4.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">7</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">4.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">10</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">5.7%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">12</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">6.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">22</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">12.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">20</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">11.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">26</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">14.9%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">17</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">9.8%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">48</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">27.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAC4AAADWBAMAAABcVq1wAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAJlJREFUWMPt1MsNwCAMA1BWYATCBrD/br1Q8Sk+xbSq5BzfhciOCOH8mMV5muciZzrKOUaOJ/mnHklu66Hcd1WLnOgoZ/X4L3+hR9t5rstCcpejnJk9pv0+Vc5zlDOzx+lWhn3qeFtyl6OcyT2m1ds+cpKjnMk99hfmffpGcpejnLk9Pj3Pv4Pc6Shnbo/DrHfVpshdjnI+ORdoFWD+EkRaDgAAACV0RVh0ZGF0ZTpjcmVhdGUAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMHwwRO0AAAAldEVYdGRhdGU6bW9kaWZ5ADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDANbfxRAAAAAElFTkSuQmCC"></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Education
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 1</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 2</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. 3</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. 4</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. 5</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. 6</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">174</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">100.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">0</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">0.0%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAIQAAAB4BAMAAADBOkf2AAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAFxJREFUWMPt1DENgEAABMG3gATAAfj3RgEFNfchuWRWwJQ7ht5tQQ+xn587EAhEETHhF0vQPGJFIBCI/4hAMD4EAtFJBILxIRCITiIQjA+BQHQSgWB8CASik9DdBfXOQjDmnL8LAAAAJXRFWHRkYXRlOmNyZWF0ZQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwfDBE7QAAACV0RVh0ZGF0ZTptb2RpZnkAMjAyMy0xMS0wNFQyMzoyOToyOSswMDowMA1t/FEAAAAASUVORK5CYII="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
    <tr>
      <td align="left">Income
[factor]</td>
      <td align="left" style="padding:8;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">1. 1</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">2. 2</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">3. 3</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">4. 4</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">5. 5</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">6. 6</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">7. 7</td></tr><tr style="background-color:transparent"><td style="padding:0;margin:0;border:0" align="left">8. 8</td></tr></table></td>
      <td align="left" style="padding:0;vertical-align:middle"><table style="border-collapse:collapse;border:none;margin:0"><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">37</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">21.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">25</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">14.4%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">28</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">16.1%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">18</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">10.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">22</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">12.6%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">18</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">10.3%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">13</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">7.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr><tr style="background-color:transparent"><td style="padding:0 5px 0 7px;margin:0;border:0" align="right">13</td><td style="padding:0 2px 0 0;border:0;" align="left">(</td><td style="padding:0;border:0" align="right">7.5%</td><td style="padding:0 4px 0 2px;border:0" align="left">)</td></tr></table></td>
      <td align="left" style="vertical-align:middle;padding:0;background-color:transparent;"><img style="border:none;background-color:transparent;padding:0;max-width:max-content;" src="data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAACYAAACfBAMAAABn1rYgAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAAD1BMVEX////9/v2mpqbw8PD///+xh0SBAAAAAnRSTlMAAHaTzTgAAAABYktHRACIBR1IAAAAB3RJTUUH5wsEFx0duiSS7AAAAG1JREFUSMdjYKA+UEIGUDFlYwQwGhXDK4Yt/ASRAYlicLOQxGD2Go2KoYhhCyuqhL0Spr1gi0fF8IYV5WGviCIGsXdUDF0MW1hRI91j2gvJbqNi+MNqNN0P4XSPJqYMj/BRMUJhNRr2gys+qAkA0K76zqwABw0AAAAldEVYdGRhdGU6Y3JlYXRlADIwMjMtMTEtMDRUMjM6Mjk6MjkrMDA6MDB8METtAAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIzLTExLTA0VDIzOjI5OjI5KzAwOjAwDW38UQAAAABJRU5ErkJggg=="></td>
      <td align="center">0
(0.0%)</td>
    </tr>
  </tbody>
</table>
<p>Generated by <a href='https://github.com/dcomtois/summarytools'>summarytools</a> 1.0.1 (<a href='https://www.r-project.org/'>R</a> version 4.2.1)<br/>2023-11-04</p>
</div>

## 
