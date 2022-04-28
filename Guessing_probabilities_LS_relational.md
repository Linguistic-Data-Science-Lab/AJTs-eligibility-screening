---
title: "Guessing probabilities on relational accounts"
author: "Jutta Pieper"
date: "28.04.2022"
output: 
  html_document:
    keep_md: yes
bibliography: references/bibliography.bibtex
csl: references/apa.csl
link-citations: true
params:
  ls_pt: 5
  ls_neutral: 3
  no_events_max: 8
---




The aim of this report is to estimate the chances of guessing participants to pass control trials on relational accounts. Determining these probabilities  is analogous to estimating the FC guessing probabilities: We determine the proportion of outcomes out of the number of possible outcomes under which the decision criterion is fulfilled.  

To allow for comparisons of mean values,  we consider a pair of responses (i.e. to a single grammatical and to a single ungrammatical stimulus) as one event. Note that to allow for computing guessing probabilities  for the approach of @Zanuttini_et_al_2018, 
who reject participants who used the wrong side of scale (excluding the neutral point) at least once,
we determine if there are any slips simultaneously.[^1]

[^1]: They further do not tolerate participants with an average rating $> 2$ for ungrammatical and $< 4$ for grammatical control items.


```r
events = expand.grid(
    pairs = 1, 
    response_grammatical = c(1:params$ls_pt), # 5, 5-pt-LS 
    response_ungrammatical = c(1:params$ls_pt)
  ) %>%
  mutate(
      slips = (response_grammatical < params$ls_neutral | # 3 on a 5-pt-LS
               response_ungrammatical > params$ls_neutral), 
      count = 1  # later used for aggregating 
  )
```

On a 5-pt-\ac{LS}, there are 25 different outcomes, i.e. 25 different ways to respond to two trials (i.e. <1-1>, ..., <5-5>)
For each of these possible outcomes, we compute mean values for grammatical and ungrammatical trials, and the proportion of the optimal distance ($s$), 
i.e., 
\begin{align}
s = \frac{\text{mean}(grammatical) - \text{mean}(ungrammatical)}{\max(LS rating) - \min(LS rating)},
\label{eq:LS_accounts_OPT_DIST}
\end{align}
in order to  decide whether the distance between conditions is acceptable under **relational account**, i.e. $> 0$ and under the **extended relational account**, i.e. $\geq 0.5$. Additionally, we decide whether the distance between conditions is acceptable under the approach of @Zanuttini_et_al_2018.


```r
eval_first = events %>% 
  mutate(mean_grammatical = response_grammatical/pairs,
         mean_ungrammatical = response_ungrammatical/pairs,
         acceptance_rel = mean_grammatical > mean_ungrammatical,
         score = (mean_grammatical - mean_ungrammatical) / (params$ls_pt - 1),
         acceptance_rel_ext = score >= 0.5, 
         acc_zanuttini = mean_grammatical >= 4 & mean_ungrammatical <= 2 & slips==0 
  ) %>%   
  ## only for printing
  select(-count, -starts_with("response"))
```



```r
eval_first %>% slice_head(n=5) %>% kable()
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:left;"> slips </th>
   <th style="text-align:right;"> mean_grammatical </th>
   <th style="text-align:right;"> mean_ungrammatical </th>
   <th style="text-align:left;"> acceptance_rel </th>
   <th style="text-align:right;"> score </th>
   <th style="text-align:left;"> acceptance_rel_ext </th>
   <th style="text-align:left;"> acc_zanuttini </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 0.25 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 1.00 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> TRUE </td>
  </tr>
</tbody>
</table>



```r
eval_first %>% slice_tail(n=5) %>% kable()
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:left;"> slips </th>
   <th style="text-align:right;"> mean_grammatical </th>
   <th style="text-align:right;"> mean_ungrammatical </th>
   <th style="text-align:left;"> acceptance_rel </th>
   <th style="text-align:right;"> score </th>
   <th style="text-align:left;"> acceptance_rel_ext </th>
   <th style="text-align:left;"> acc_zanuttini </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> -1.00 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> -0.75 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> -0.50 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> -0.25 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
  </tr>
</tbody>
</table>
 
Under the assumption that each outcome is equally probable when responding randomly, we can estimate the probabilities to pass control trials by chance  by  determining the proportion of outcomes passing the decision criteria  under a certain amount of pairs.

The evaluation of the possible outcomes of the first pair looks like this:


```r
evaluate_outcomes = function(outcomes_table){
 outcomes_table %>% 
  ## evaluate individual outcomes
  mutate(mean_grammatical = response_grammatical/pairs,
         mean_ungrammatical = response_ungrammatical/pairs,
         acceptance_rel = mean_grammatical > mean_ungrammatical,
         score = (mean_grammatical - mean_ungrammatical) / (params$ls_pt - 1),
         acceptance_rel_ext = score >= 0.5, 
         acc_zanuttini = mean_grammatical >= 4 & mean_ungrammatical <= 2 & slips==0 
  ) %>% 
  ## select relevant columns, and transform from wide to long format
  select(pairs, starts_with("acc"), count) %>%
  gather(key = "criterion", value = "acceptance", -pairs, -count ) %>% 
  ## determining the proportion of outcomes passing the decision criteria 
  ## for different amount of pairs and different criteria
  group_by(pairs, criterion, acceptance) %>% 
  summarise(n = sum(count)) %>% 
  spread(key = acceptance, value = n) %>%
  rename(rejected = "FALSE", accepted = "TRUE") %>%
  mutate(props_acc = round(accepted/(accepted+rejected), digits = 4)) %>%
  mutate(outcomes = rejected+accepted) 
}
```



```r
eval_all = evaluate_outcomes(events)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:left;"> criterion </th>
   <th style="text-align:right;"> rejected </th>
   <th style="text-align:right;"> accepted </th>
   <th style="text-align:right;"> props_acc </th>
   <th style="text-align:right;"> outcomes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> acc_zanuttini </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> acceptance_rel </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> acceptance_rel_ext </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 25 </td>
  </tr>
</tbody>
</table>


For the evaluation of two events (e.g. four control trials, thereof  two grammatical and two ungrammatical), we need to consider every possible combination of events, resulting in an outcome (of two events). All combinations of these amount to 625 possible  outcomes for two pairs of trials    (e.g. <1-1, 1-1>, ..., <5-5, 5-5>). 


```r
outcomes_prev = events 
events = events %>% mutate(count = 0)
```

As the number of outcomes grows fast, we compute the sum of responses to grammatical and ungrammatical stimuli right away.


```r
generate_new_outcomes = function(previous_outcomes, events){
  outcomes_new = data.frame()
  for(r in c(1:nrow(previous_outcomes))){
    out = previous_outcomes[r,]
    for(e in c(1:nrow(events))){
      ## cell values are added up
      new_out = out + events[e,]
      outcomes_new = plyr::rbind.fill(outcomes_new, new_out)
    }
  }
  return(outcomes_new)
}
```

Generated outcomes for two pairs: 

```r
two_events = generate_new_outcomes(outcomes_prev, events) 
```

Before going on, we compress the table (using the `count` variable, giving the amount of outcomes with the same results, for aggregating), as there are outcomes with the same characterstics, i.e. sums of response values: 


```r
two_events %>% arrange(response_grammatical, response_ungrammatical) %>% head() %>% kable()
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:right;"> response_grammatical </th>
   <th style="text-align:right;"> response_ungrammatical </th>
   <th style="text-align:right;"> slips </th>
   <th style="text-align:right;"> count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

As we do not need to know the exact amount of slips, but only if any exist, we discard the exact amount to allow for stronger compression.


```r
summarize_outcomes = function(outcomes){
  outcomes %>%
        ## we are not interested in exact amounts of slips
        mutate(slips = ifelse(slips>0, TRUE,FALSE)) %>%
        dplyr::group_by(pairs, response_grammatical, response_ungrammatical, slips) %>%
        dplyr::summarise(count = sum(count)) %>%
        ungroup()
}
```


```r
two_events_comprised = summarize_outcomes(two_events)
head(two_events_comprised) %>% kable()
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:right;"> response_grammatical </th>
   <th style="text-align:right;"> response_ungrammatical </th>
   <th style="text-align:left;"> slips </th>
   <th style="text-align:right;"> count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
</tbody>
</table>

This reduced the number of rows from 625 to 97.
Note: this also reduces the amount of outcomes computed for three pairs of events from  
15,625  to  2,425. 

We repeat this produces until we reach the desired maximal amounts of pairs, whereby evaluating outcomes of each amount of pairs:


```r
# eval_all already contains evaluation for responses to a single pair
for(p in c(2:params$no_events_max)){
  outcomes_new = generate_new_outcomes(outcomes_prev, events) %>%
                  summarize_outcomes() 
  outcomes_new_eval = evaluate_outcomes(outcomes_new)
  eval_all = rbind(eval_all, outcomes_new_eval)
  outcomes_prev = outcomes_new
}
```

Giving us evaluations for the amount of pairs in the range 1 - 8



```{=html}
<div id="htmlwidget-d40e3c1e6448339144b8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d40e3c1e6448339144b8">{"x":{"filter":"none","vertical":false,"data":[[1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8],["acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext","acc_zanuttini","acceptance_rel","acceptance_rel_ext"],["21","15","19","589","355","555","15,336","8,688","14,923","388,125","214,395","379,075","9,744,016","5,311,285","9,628,604","243,951,400","131,875,900","242,295,003","6,101,851,525","3,279,160,690","6,075,833,510","152,573,191,069","81,619,689,707","152,186,472,515"],["4","10","6","36","270","70","289","6,937","702","2,500","176,230","11,550","21,609","4,454,340","137,021","189,225","112,264,725","1,845,622","1,664,100","2,824,354,935","27,682,115","14,699,556","70,968,200,918","401,418,110"],["0.16","0.4","0.24","0.0576","0.432","0.112","0.0185","0.444","0.0449","0.0064","0.4511","0.0296","0.0022","0.4561","0.014","8e-04","0.4598","0.0076","3e-04","0.4627","0.0045","1e-04","0.4651","0.0026"],["25","25","25","625","625","625","15,625","15,625","15,625","390,625","390,625","390,625","9,765,625","9,765,625","9,765,625","244,140,625","244,140,625","244,140,625","6,103,515,625","6,103,515,625","6,103,515,625","152,587,890,625","152,587,890,625","152,587,890,625"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>pairs<\/th>\n      <th>criterion<\/th>\n      <th>rejected<\/th>\n      <th>accepted<\/th>\n      <th>props_acc<\/th>\n      <th>outcomes<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"className":"dt-right","targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

Tidying up results in: 

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Table 5 in @Pieper_et_al_2022</caption>
 <thead>
<tr>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
<th style="empty-cells: hide;border-bottom:hidden;" colspan="1"></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">pure:</div></th>
<th style="border-bottom:hidden;padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">extended:</div></th>
</tr>
  <tr>
   <th style="text-align:right;"> pairs </th>
   <th style="text-align:right;"> outcomes </th>
   <th style="text-align:right;"> s &gt; 0 </th>
   <th style="text-align:right;"> s &gt; 0.5 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 0.400 </td>
   <td style="text-align:right;"> 0.240 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 625 </td>
   <td style="text-align:right;"> 0.432 </td>
   <td style="text-align:right;"> 0.112 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 15,625 </td>
   <td style="text-align:right;"> 0.444 </td>
   <td style="text-align:right;"> 0.045 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 390,625 </td>
   <td style="text-align:right;"> 0.451 </td>
   <td style="text-align:right;"> 0.030 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 9,765,625 </td>
   <td style="text-align:right;"> 0.456 </td>
   <td style="text-align:right;"> 0.014 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 244,140,625 </td>
   <td style="text-align:right;"> 0.460 </td>
   <td style="text-align:right;"> 0.008 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 6,103,515,625 </td>
   <td style="text-align:right;"> 0.463 </td>
   <td style="text-align:right;"> 0.004 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 152,587,890,625 </td>
   <td style="text-align:right;"> 0.465 </td>
   <td style="text-align:right;"> 0.003 </td>
  </tr>
</tbody>
</table>

# References

