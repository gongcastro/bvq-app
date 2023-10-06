# Model

We used multilevel ordinal regression to model the cumulative
probability of a *No* response, a *Understands* response, or a
*Understands and Says* response (Bürkner and Vuorre 2019) using the
*logit* link function..

$$
\begin{aligned}
\textbf{Likelihood:} \\
y_{ij} &\sim \text{Cumulative}(p_{k})
\end{aligned}
$$

where:

- $y$ is an observed response
  ($y \in \{\text{No, Understands, Understands and Says}\}$)
- $i$ is the participant index
- $j$ is the translation equivalent (TE) index
- $p_{k}$ is a probability ($p \in (0, 1)$) that indicates the threshold
  $k$ ($k \in (1, 2)$)) between two response categories in the latent
  distribution

$p_{k}$ is then estimated using a logistic regression model in which we
added out predictors of interest: $\text{Age}$ (participants’ age in
months), $\text{Group}$ (monolinguals vs. bilinguals), and
$\text{Dominance}$ (L1 vs. L2), along with their tw- and three-way
interactions.

We also included crossed random effects for participants and translation
equivalents to account for the repeated measures in our dataset—each
participant provided responses to multiple translation equivalents, and
each translation equivalent was responded to by multiple participants
(Gelman, Hill, and Vehtari 2020). For both grouping variables, we
included random intercepts, random slopes, and correlation parameters
for all predictors were repeated measures were observed in our dataset
(Barr et al. 2013).

$$
\begin{aligned}
\textbf{Linear model:} \\
\text{Logit}(p_{k}) = \text{ln} \frac{p_{k}}{1-p_{k}} &= (\beta_{0_{k}} + u_{0_{i_{k}}} + w_{0_{j_{k}}}) + \\
& (\beta_{1} + u_{1_{i}} + w_{1_{j}}) · \text{Age}_{i} + & \\
& (\beta_{2} + w_{2_{j}}) · \text{Group}_{i} + & \\
& (\beta_{3} + u_{2_{i}} + w_{3_{j}}) · \text{Dominance}_{ij} + & \\
& (\beta_{5} + w_{4_{j}}) · (\text{Age}_{i} \times \text{Group}_{i}) + & \\
& (\beta_{6} + u_{3_{i}} + w_{5_{j}}) · (\text{Age}_{i} \times \text{Dominance}_{ij}) + & \\
& (\beta_{7} + w_{6_{j}}) · (\text{Group}_{i} \times \text{Dominance}_{ij}) & \\
& (\beta_{8} + w_{7_{j}}) · (\text{Age}_{i} \times \text{Group}_{i} \times \text{Dominance}_{ij}) & \\
\end{aligned}
$$

where:

- $i$ and $j$ index the participant and translation equivalent (TE)
- $\beta_{0_k}$ is the fixed coefficient of the regression model for the
  intercept of threshold $k$
- $u_{0_{i}}$ and $w_{0_{j}}$ are the by-participant and by-TE
  adjustments for $\beta_{0_{k}}$ (i.e., random intercepts),
  respectively
- $\beta_{1-8}$ are the fixed coefficients of the regression model for
  the predictors of interest
- $u_{1-3_{i}}$ and $w_{1-7_{j}}$ are the by-participant and by-TE
  adjustments for$\beta_{1-8}$ (i.e., random slopes), respectively

We used the Bayesian framework to estimate the parameters in our model.
This involves using the Bayes theorem to compute a distribution
(*posterior distribution*) that describes what values of each parameter
in the model are more likely given the data (*likelihood*), and previous
knowledge about such distribution (*prior distribution*) (McElreath
2020). This posterior distribution not only informs about the most
likely values of our regression coefficients of interest, but also about
the uncertainty around such estimations. We used a weakly informative
prior for our parameters, with the exception of the main effect of
$\text{Age}$, for which we specified a strongly informative prior based
on previous literature about how age affects the acquisition of words.

$$
\begin{aligned}
\\
\textbf{Prior:} \\
\beta_{0_{k}} &\sim \mathcal{N}(-0.25, 0.1) & [\mbox{Intercept/response category threshold}] \\
\beta_{1} &\sim \mathcal{N}(1, 0.1) & [\mbox{Age population-level coefficient}]\\
\beta_{2-8} &\sim \mathcal{N}(0, 1) & [\mbox{Rest of population-level coefficients}] \\
u_{0-3_{i}} &\sim \mathcal{N}(0, \sigma_{u_{0-3_{i}}}) & [\mbox{Participant-level coefficient variability}] \\
w_{0-7_{j}} &\sim \mathcal{N}(0, \sigma_{w_{0-7_{j}}}) & [\mbox{TE-level coefficient variability}] \\\\
&&\mbox{[Participant-level coefficient variability]} \\ \\
\Bigg(\begin{smallmatrix}
u_{k_{0}} \\ 
u_{1_{i}} \\ 
\vdots \\ 
u_{3_{i}} 
\end{smallmatrix}\Bigg) &\sim \mathcal{N} 
\Bigg(\Bigg(\begin{smallmatrix}0 \\
0 \\ 
\vdots \\
0\end{smallmatrix}\Bigg), \Sigma_{u}\Bigg) \\
\Sigma_{u} &= \Bigg(\begin{smallmatrix} \\
\rho_{u_{0}} & \rho_{u_{0}} \sigma_{u_{0_{k}}} \sigma_{u_{1}} & \dots & \rho_{u_{0}} \sigma_{u_{0}} \sigma_{w_{8}}\\ 
\rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{0}} & \rho_{u_{1}} & \dots & \rho_{u_{1}} \sigma_{u_{1}} \sigma_{u_{3}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{8} \sigma_{u_{3}} \sigma_{u_{0_{k}}} & \dots & \dots & \rho_{u_{3}} \end{smallmatrix}\Bigg) \\
\sigma_{u_{0-3}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{u} &\sim \text{LKJcorr(2)} \\
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
\rho_{w_{0}} & \rho_{w_{0}} \sigma_{w_{0_{k}}} \sigma_{w_{1}} & \dots & \rho_{w_{0}} \sigma_{w_{0}} \sigma_{w_{7}}\\ 
\rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{0}} & \rho_{w_{1}} & \dots & \rho_{w_{1}} \sigma_{w_{1}} \sigma_{w_{7}}\\ 
\vdots & \vdots & \vdots & \vdots \\
\rho_{7} \sigma_{w_{7}} \sigma_{w_{0_{k}}} & \dots & \dots & \rho_{w_{7}} \end{smallmatrix}\Bigg) \\
\sigma_{w_{0-7}} &\sim \mathcal{N_{+}}(1, 0.1) \\
\rho_{w_{0-7}} &\sim \text{LKJcorr(2)}
\end{aligned}
$$

where:

- $\rho_{u_{0-3}}$ and $\rho_{w_{0-7}}$ indicate the correlation
  parameters between the by-participant and by-TE adjustments,
  respectively
- $\sigma_{u_{0-3}}^2$ and $\sigma_{w_{0-7}}^2$ indicate the variance of
  the by-participant and by-TE variance of the adjustments, respectively
- $\mathcal{N}$ indicates a normal distribution, $\mathcal{N}_{+}$
  indicates a truncated normal distribution with only positive values,
  and $\text{LKJcorr}$ indicates a [LKJ correlation
  distribution](https://mc-stan.org/docs/2_22/functions-reference/lkj-correlation.html)
  (Lewandowski, Kurowicka, and Joe 2009).

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-barr2013random" class="csl-entry">

Barr, Dale J, Roger Levy, Christoph Scheepers, and Harry J Tily. 2013.
“Random Effects Structure for Confirmatory Hypothesis Testing: Keep It
Maximal.” *Journal of Memory and Language* 68 (3): 255–78.

</div>

<div id="ref-burkner2019ordinal" class="csl-entry">

Bürkner, Paul-Christian, and Matti Vuorre. 2019. “Ordinal Regression
Models in Psychology: A Tutorial.” *Advances in Methods and Practices in
Psychological Science* 2 (1): 77–101.

</div>

<div id="ref-gelman2020regression" class="csl-entry">

Gelman, Andrew, Jennifer Hill, and Aki Vehtari. 2020. *Regression and
Other Stories*. Cambridge University Press.

</div>

<div id="ref-lewandowski2009generating" class="csl-entry">

Lewandowski, Daniel, Dorota Kurowicka, and Harry Joe. 2009. “Generating
Random Correlation Matrices Based on Vines and Extended Onion Method.”
*Journal of Multivariate Analysis* 100 (9): 1989–2001.

</div>

<div id="ref-mcelreath2020statistical" class="csl-entry">

McElreath, Richard. 2020. *Statistical Rethinking: A Bayesian Course
with Examples in r and Stan*. Chapman; Hall/CRC.

</div>

</div>
