
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socmod

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/CSS4S/socmod/graph/badge.svg)](https://app.codecov.io/gh/CSS4S/socmod)

<!-- badges: end -->

`socmod` provides a framework and utilities for developing simulations
of social learning and social influence structured by social networks.
It is being developed to support the course *Computational Social
Science for Sustainability* at the Stanford Doerr School of
Sustainability. The course teaches theory and techniques for
understanding cognitive and social mechanisms influencing beliefs and
behaviors that can be combined in computational models to predict the
relative efficacy of different candidate interventions for
sustainability, e.g., to promote ecological protection, public health,
economic security and justice, climate action, to name just a few
[sustainable development goals](https://sdgs.un.org/goals).

`socmod` is flexible to encapsulate any social process that can be
modeled as follows: (1) individuals are initialized with some knowledge
and payoff from behaviors they do or or beliefs they hold; (2) they
exchange information socially over time through teaching, observation,
discourse, etc. This process is illustrated in this figure:

![](man/figures/general_model_box_diagram.png)

Currently the focus of socmod is developing models of informational and
behavioral interventions to promote sustainable behaviors that we call
*adaptations*, $A$. Those not yet doing $A$ are said to be doing a
legacy behavior, $L$. Each agent can be assigned or gain fitness that is
tracked over time, which can be linked to whether they do $A$ or $L$.

`socmod` provides tools for initializing simulated individuals (i.e.,
*agents*), their social networks, and their behaviors. These
capabilities are introduced in a simple example below.

### Installation

You can install the development version of socmod from
[GitHub](https://github.com/) with `devtools` or `pak`:

**devtools**

``` r
# Install this if you don't have devtools.
# install.packages("devtools")
# devtools::install_github("CSS4S/socmod")
```

**pak**

``` r
# Install this if you don't have pak.
# install.packages("pak")
# pak::pak("CSS4S/socmod")
```

## Simple example

To understand what socmod does to help organize and develop models of
social behavior, let’s consider a simple example with just four
individuals illustrated in the figure below. We will initialize our
agents with behaviors and payoffs as shown here, then explain how to use
different social learning strategies to simulate the diffusion of the
adaptation, $A$. $A$ yields a payoff of 2 while the legacy $L$ behavior
yields a payoff of 1. This matters only for the success-biased social
learning strategy, not for the frequency-biased strategy or unbiased
contagion learning.

<figure id="fig-example">
<img src="man/figures/small_example_network.png" data-fig-align="center"
width="300" alt="A simple network nieghborhood of individual/agent i" />
<figcaption aria-hidden="true">A simple network nieghborhood of
individual/agent <span
class="math inline"><em>i</em></span></figcaption>
</figure>

We can initialize these agents and their network as follows. In the next
code block we first initialize four agents named “i”, “n_i1”, “n_i2”,
and “n_i3” to match the illustration of focal/learner agent $i$ and its
three neighbors (each one is an *instance* of the `socmod::Agent`
class). Then we create the social network from the illustration using
`igraph`. Finally, we initialize our `socmod::AgentBasedModel`

``` r
library(socmod)
```

## More information and the philosophy of socmod

Different models of social behavior are specified by the details of how
many individuals are in a population, what behaviors or opinions they do
or have, what benefits they accrue(d) through their behaviors, how they
learn or influence one another, and any environmental or other relevant
factors. This framework seeks to encapsulate different approaches to
modeling diverse social behaviors, such as those thoroughly reviewed in
Paul Smaldino’s (2023) textbook [*Modeling Social
Behavior*](https://press.princeton.edu/books/paperback/9780691224145/modeling-social-behavior?srsltid=AfmBOop2zNSsOtNlOMs6uaLTlAQs8saVMC_I6y_OnyklIKz-GUnoNapR).

Technically, `socmod` uses object-oriented programming, provided by
[`R6`](https://r6.r-lib.org/), and functional-style agent and model
behavior specification inspired by
[Agents.jl](https://juliadynamics.github.io/Agents.jl/stable/), which I
myself have enjoyed using. But, I still had to do my plotting in R, and
more beginning students across disciplines will tend to know R than
Julia. R also seems to have a great community with the `r-lib` project
that seems to be bringing a continuity to scientific programming that I
have not seen in any other programming language.
