# `ramp.demog`

## Utilities for Human Demography and Orthogonal Stratification

The primary purpose of `ramp.demog` is to provide support for `ramp.xds` to build and solve models with human demography and stratification for epidemiological heterogeneity on age and other epidemiological traits. 

The utilities in this package set up and configure: 

+ `Hmatrix` a matrix, $\bf H,$  to compute all dynamical transitions among population strata, called by `dHdt` and for every state variable $X,$ it computes: 

$${\bf H} \cdot X$$

+ `Amatrix` a matrix, $\bf A,$ to compute dynamical transitions for tracking variables. For every tracking variable, $W,$ it computes 

$${\bf A} \cdot W$$


In this package, we have also developed a set of functions to check the accuracy of models that use age-stratification against a cohort model.


### Demography

Demographic functions supported include:

+ **Aging** - set up and solve models that stratify a population by age 

+ **Age specific mortality** - to develop models for age-specific mortality using data describing human populations by age 

+ **Births** - set up and solve models with a births; in age-structured models, into the class that includes newborns 

+ **Migration** - defined here to include a change of residence, including seasonal labor migration and some forms of nomadism 
    


### Orthogonal Stratification 

To support development of models for malaria policy, we have also developed a set of utilities to *stratify* a population along other traits.
To set up and solve models with dynamical strata, we have implemented a method we call *principled stratification* that defines a subset of all possible models. 
In a nutshell, we create one matrix for each process. For example:

1. A matrix describing residence and time spent; 
2. A matrix describing migration among patches; 
3. A matrix describing aging among age classes

In the set up, each stratum belongs to a residence and time spent matrix. In the next step, we develop a migration matrix, 
where every resident of a patch has the same probability of migrating to every other patch. 
Next, we split each stratum by age; every daughter stratum inherits the features of its parent stratum. 
The total number of strata is the product of the number of substrata for each trait. In this case, migration does not split
so we have the product of the number of strata along a time spent vector.

Setup thus outputs a generalized linear matrix `dHdt` describing demography and dynamic transitions among strata. 
We also have a matrix `dAdt` that describes aging (for tracking variables). 

We note that it is possible to formulate and run a model with any matrix, `Hmatrix`, so long as it has the right shape.  

For example, suppose we have a `residence` vector $1, 1, 2, 3$ on $3$ patches with a migration matrix $M$ and the residence matrix $J_r$, and we have two age classes, $1,2$ with an aging matrix $A$. 
There are a total of $8$ strata, but $M$ is a $3 \times 3$ matrix and $A$ is a $2 \times 2$ matrix. We formulate the membership matrix for aging, $J_a$ and full matrix by taking: 

$$ 
J_a^T \cdot A \cdot J_a +  J_r^T \cdot M \cdot J_ r
$$


At some point, `ramp.xds` made the decision to compute the derivative `dHdt` for human population density. In compartment models, a 
population is sub-divided into $N$ mutually exclusive and collectively exhaustive states (MECE), but since the state variables describe
densities and they sum up to $H,$ only $N-1$ of the state variables need to be computed. We also adopted the convention that the 
compartment that would be omitted is the state with newborns. 

