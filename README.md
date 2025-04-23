
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socmod

[![Codecov test
coverage](https://codecov.io/gh/CSS4S/socmod/graph/badge.svg)](https://app.codecov.io/gh/CSS4S/socmod)

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

## Quickstart examples

To understand what socmod does to help organize and develop models of
social behavior, let’s consider a simple example with just four
individuals illustrated in the figure below. We will initialize our
agents with behaviors and payoffs as shown here, then explain how to use
different social learning strategies to simulate the diffusion of the
adaptation, $A$. $A$ yields a payoff of 2 while the legacy $L$ behavior
yields a payoff of 1. This matters only for the success-biased social
learning strategy, not for the frequency-biased strategy or unbiased
contagion learning.

<figure>
<img src="man/figures/small_example_network.png" data-fig-align="center"
width="350" alt="A simple network nieghborhood of individual/agent i" />
<figcaption aria-hidden="true">A simple network nieghborhood of
individual/agent <span
class="math inline"><em>i</em></span></figcaption>
</figure>

We can initialize these agents and their network as follows. In the next
code block we first initialize four agents named “i”, “n_i1”, “n_i2”,
and “n_i3” to match the illustration of focal/learner agent $i$ and its
three neighbors (each one is an *instance* of the `socmod::Agent`
class). Then we create the social network from the illustration using
`igraph`. Finally, we initialize a new `socmod::AgentBasedModel` using
the helper function `make_example_abm`. In the next subsection we then
use this helper function to initialize new models that will be run with
frequency- and success-biased adaptive learning strategies, using
learning functions provided with `socmod`. After that we show how to
define non-adaptive contagion learning functions, adapted from
compartmental epidemiological modeling.

``` r
make_example_abm <- function(legacy_fitness = 1.0, adaptive_fitness = 2.0, ...) {
  # Example ABM builder with four agents as pictured above. Can pass arbitrary
  # named parameters in ... that will be passed to make_abm as additional
  # model parameters.
  
  agents <- c(
    Agent$new(1, name = "i", behavior = "Legacy", fitness = legacy_fitness), 
    Agent$new(2, name = "n_i1", behavior = "Legacy", fitness = legacy_fitness), 
    Agent$new(1, name = "n_i2", behavior = "Legacy", fitness = legacy_fitness), 
    Agent$new(1, name = "n_i3", behavior = "Adaptive", fitness = adaptive_fitness)
  )

  # Initialize network.
  socnet <- igraph::make_graph(~ 1-2:3:4, 3-4)

# Create the agent-based model and plot the model's network.
  return (make_abm(make_model_parameters(graph = socnet), agents = agents, ...))
}

abm <- make_example_abm()
# Visualize social network to ensure it was created properly.
ggnetplot(abm$graph) + 
      geom_edges(linewidth=0.1) + 
      geom_nodes(color = "#008566", size=4) + 
      # `name` in label = name was set automagically during ABM construction.
      geom_nodelabel_repel(aes(label = name), size = 2) + 
      theme_blank()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="50%" style="display: block; margin: auto;" />

### Model dynamics with different adaptive social learning strategies

Here we set up three different models with each of the three learning
strategies provided by socmod: success-biased, frequency-biased, and
contagion learning.

#### Success-biased learning

In success-biased learning, the probability a learner adopts a behavior
is proportional to the relative fitness of their neighbors. Note that in
the simplest model of success-biased transmission, if there is only one
agent doing $A$, then this agent always adopts $L$ on the first time
step. There is a “stubbornness” parameter that can be passed as a model
parameter that represents how frequently an individual ignores social
information and continues using their current behavior.

Success-biased learning is the default, so we do not have to specify it
explicitly.

``` r
abm <- make_example_abm()
trial <- run_trial(abm, stop = fixated)
print(tail(trial$get_observations(), n = 10))
#> # A tibble: 8 × 4
#>       t agent Behavior Fitness
#>   <dbl> <chr> <chr>      <dbl>
#> 1     0 i     Legacy         1
#> 2     0 n_i1  Legacy         1
#> 3     0 n_i2  Legacy         1
#> 4     0 n_i3  Adaptive       2
#> 5     1 i     Legacy         1
#> 6     1 n_i1  Legacy         1
#> 7     1 n_i2  Legacy         1
#> 8     1 n_i3  Legacy         1
plot_adoption(trial, tracked_behaviors = c("Legacy", "Adaptive"))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="50%" style="display: block; margin: auto;" />

#### Frequency-biased learning

In frequency-biased learning, the probability a learner adopts a
behavior is proportional to the number of network neighbors doing a
behavior. In the first time step, then, $n_{i2}$ has a 1/2 probability
of adopting $A$, and $i$ has a 1/3 probability of adopting $A$.
Frequency-biased learning is not the default, so we need to explicilty
specify its use in our model parameters by passing
`frequency_biased_learning_strategy` to the `make_model_parameters`
helper function:

``` r
abm <- make_example_abm()
trial <- run_trial(abm, stop = fixated)
print(tail(trial$get_observations(), n = 10))
#> # A tibble: 10 × 4
#>        t agent Behavior Fitness
#>    <dbl> <chr> <chr>      <dbl>
#>  1     4 n_i2  Legacy         1
#>  2     4 n_i3  Legacy         1
#>  3     5 i     Legacy         1
#>  4     5 n_i1  Adaptive       2
#>  5     5 n_i2  Legacy         1
#>  6     5 n_i3  Legacy         1
#>  7     6 i     Legacy         1
#>  8     6 n_i1  Legacy         1
#>  9     6 n_i2  Legacy         1
#> 10     6 n_i3  Legacy         1
plot_adoption(trial, tracked_behaviors = c("Legacy", "Adaptive"))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="50%" style="display: block; margin: auto;" />

### Networks

Socmod provides some helper functions to create and plot some important
network types, shown below.

#### Regular lattice

``` r
latnet <- socmod::make_regular_lattice(N = 10, k = 4)
ggnetplot(latnet, layout = \(g) 0.6*layout_in_circle(g)) + 
      geom_edges(linewidth=0.1, color="darkgray") + 
      geom_nodes(color = "#008566", size=1) + 
      theme_blank()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="50%" style="display: block; margin: auto;" />

#### Random networks

**Erdős–Rényi $G(N,M)$**

``` r
gnm_net <- G_NM(20, 30)
ggnetplot(gnm_net) + 
  geom_edges(linewidth=0.1, color= "darkgray") + 
  geom_nodes(color = "#008566", size=.75) + 
  theme_blank()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="50%" style="display: block; margin: auto;" />

**Small-world networks**

``` r
sw_net <- make_small_world(N = 10, k = 4, p=0.1)
print(sw_net)
#> IGRAPH 7805d83 U--- 10 20 -- 
#> + edges from 7805d83:
#>  [1] 1-- 2 1--10 3-- 5 1-- 9 2-- 3 2-- 4 2--10 3-- 4 3--10 4-- 5 6-- 9 5-- 6
#> [13] 5-- 7 6-- 7 6-- 8 5-- 8 7-- 9 8-- 9 8--10 2-- 9
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
