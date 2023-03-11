---
title: "Model"
output_format: md_document
bibliography: "../../assets/references.bib"
---

We used multilevel ordinal regression to model the cumulative probability of a *No* response, a *Understands* response, or a *Understands and Says* response [@burkner2019ordinal] using the *logit* link function.. 


$$
\begin{aligned}
\textbf{Likelihood:} \\
y_{ij} &\sim \text{Cumulative}(p_{k})
\end{aligned}
$$


where:

- $y$ is an observed response ($y \in \{\text{No, Understands, Understands and Says}\}$)
- $i$ is the participant index
- $j$ is the translation equivalent (TE) index
- $p_{k}$ is a probability ($p \in (0, 1)$) that indicates the threshold $k$ ($k \in (1, 2)$)) between two response categories in the latent distribution
$p_{k}$ is then estimated using a logit regression model..


To test our hypotheses, we included several predictors in the regression model as fixed effects: the main effects of participant age ($\text{Age}$), word-form length ($\text{Length}$), word exposure index ($\text{Exposure}$), and of phonological similarity ($\text{Levenshtein}$), the two-way interactions $\text{Age} \times \text{Exposure}$, $\text{Age} \times \text{Levenshtein}$, and $\text{Exposure} \times text{Leveshtein}$, and the three-way interaction $\text{Age} \times \text{Exposure} \times \text{Leveshtein}$. We also included crossed random effects for participants and translation equivalents to account for the repeated measures in our dataset---each participant provided responses to multiple translation equivalents, and each translation equivalent was responded to by multiple participants [@gelman2020regression]. For both grouping variables, we included random intercepts, random slopes, and correlation parameters for all predictors were repeated measures were observed in our dataset [@barr2013random]. 

$$
\begin{aligned}
\textbf{Linear model:} \\
logit(p_{k}) = \text{ln} \frac{p_{k}}{1-p_{k}} &= (\beta_{0_{k}} + u_{0_{i_{k}}} + w_{0_{j_{k}}}) + \\
& (\beta_{1} + u_{1_{i}} + w_{1_{j}}) · \text{Age}_{i} + & \\
& (\beta_{2} + u_{2_{i}} + w_{2_{j}}) · \text{Length}_{ij} + & \\
& (\beta_{3} + u_{3_{i}} + w_{3_{j}}) · \text{Exposure}_{ij} + & \\
& (\beta_{4} + u_{4_{i}}) · \text{Levenshtein}_{ij} + & \\
& (\beta_{5} + u_{5_{i}} + w_{3_{j}}) · (\text{Age}_{i} \times \text{Exposure}_{ij}) + & \\
& (\beta_{6} + u_{6_{i}}) · (\text{Age}_{i} \times \text{Levenshtein}_{ij}) + & \\
& (\beta_{7} + u_{7_{i}}) · (\text{Exposure}_{ij} \times \text{Levenshtein}_{ij}) & \\
& (\beta_{8} + u_{8_{i}}) · (\text{Age}_{i} \times \text{Exposure}_{ij} \times \text{Levenshtein}_{ij}) & \\
\end{aligned}
$$

where:

- $i$ and $j$ index the participant and translation equivalent (TE)
- $\beta_{0_k}$ is the fixed coefficient of the regression model for the intercept of threshold $k$
- $u_{0_{i}}$ and $w_{0_{j}}$ are the by-participant and by-TE adjustments for $\beta_{0_{k}}$ (i.e., random intercepts), respectively
- $\beta_{1-8}$ are the fixed coefficients of the regression model for the predictors of interest
- $u_{1-8_{i}}$ and $w_{1-3_{j}}$ are the by-participant and by-TE adjustments for$\beta_{1-8}$ (i.e., random slopes), respectively

We used the Bayesian framework to estimate the parameters in our model. This involves using the Bayes theorem to compute a distribution (*posterior distribution*) that describes what values of each parameter in the model are more likely given the data (*likelihood*), and previous knowledge about such distribution (*prior distribution*) [@mcelreath2020statistical]. This posterior distribution not only informs about the most likely values of our regression coefficients of interest, but also about the uncertainty around such estimations. We used a weakly informative prior for our parameters, with the exception of the main effect of $\text{Age}$, for which we specified a strongly informative prior based on previous literature about how age affects the acquisition of words.

$$
\begin{aligned}
\\
\textbf{Prior:} \\
\beta_{0_{k}} &\sim \mathcal{N}(-0.25, 0.1) & [\mbox{Intercept/response category threshold}] \\
\beta_{1} &\sim \mathcal{N}(1, 0.1) & [\mbox{Age population-level coefficient}]\\
\beta_{2-8} &\sim \mathcal{N}(0, 1) & [\mbox{Rest of population-level coefficients}] \\
u_{0-8_{i}} &\sim \mathcal{N}(0, \sigma_{u_{0-8_{i}}}) & [\mbox{Participant-level coefficient variability}] \\
w_{0-3_{j}} &\sim \mathcal{N}(0, \sigma_{w_{0-3_{j}}}) & [\mbox{TE-level coefficient variability}] \\\\
&&\mbox{[Participant-level coefficient variability]} \\ \\
\Bigg(\begin{smallmatrix}
u_{k_{0}} \\ 
u_{1_{i}} \\ 
\vdots \\ 
u_{8_{i}} 
\end{smallmatrix}\Bigg) &\sim \mathcal{N} 
\Bigg(\Bigg(\begin{smallmatrix}0 \\
0 \\ 
\vdots \\
0\end{smallmatrix}\Bigg), \Sigma_{u}\Bigg) \\
\Sigma_{u} &= \Bigg(\begin{smallmatrix} \\
\rho_{u_{0}} & \rho_{u_{0}} \sigma_{u_{0_{k}}} \sigma_{u_{1}} & \dots & \rho_{u_{0}} \sigma_{u_{0}} \sigma_{w_{8}}\\ 
\rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{0}} & \rho_{u_{1}} & \dots & \rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{8}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{8} \sigma_{u_{8}} \sigma_{u_{0_{k}}} & \dots & \dots & \rho_{u_{8}} \end{smallmatrix}\Bigg) \\
\sigma_{u_{0-8}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{u} &\sim LKJcorr(2) \\
\\
&&\mbox{[TE-level coefficient variability]} \\ \\
\Bigg(\begin{smallmatrix}
w_{k_{0}}\\ 
w_{1_{j}} \\ 
\vdots \\ 
w_{3_{j}} 
\end{smallmatrix}\Bigg) &\sim \mathcal{N} \Bigg(\Bigg(\begin{smallmatrix}
0\\ 
0 \\ 
\vdots \\
0 
\end{smallmatrix}\Bigg), \Sigma_{w}\Bigg) \\
\Sigma_{w} &= \Bigg(\begin{smallmatrix} \\
\rho_{w_{0}} & \rho_{w_{0}} \sigma_{w_{0_{k}}} \sigma_{w_{1}} & \dots & \rho_{w_{0}} \sigma_{w_{0}} \sigma_{w_{3}}\\ 
\rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{0}} & \rho_{w_{1}} & \dots & \rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{3}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{3} \sigma_{w_{3}} \sigma_{w_{0_{k}}} & \dots & \dots & \rho_{w_{3}} \end{smallmatrix}\Bigg) \\
\sigma_{w_{0-3}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{w_{0-3}} &\sim LKJcorr(2)
\end{aligned}
$$

where:

- $\rho_{u_{0-8}}$ and $\rho_{w_{0-3}}$ indicate the correlation parameters between the by-participant and by-TE adjustments, respectively
- $\sigma_{u_{0-8}}^2$ and $\sigma_{w_{0-3}}^2$ indicate the variance of the by-participant and by-TE variance of the adjustments, respectively
- $\mathcal{N}$ indicates a normal distribution, $\mathcal{N}_{+}$ indicates a truncated normal distribution with only positive values, and $LKJcorr$ indicates a [LKJ correlation distribution](https://mc-stan.org/docs/2_22/functions-reference/lkj-correlation.html) [@lewandowski2009generating].