# Optimal Transport on Categorical Data for Counterfactuals using Compositional Data and Dirichlet Transport

<div align="center">
This repository contains the replication materials for the article titled 
<i>Optimal Transport on Categorical Data for Counterfactuals using Compositional 
Data and Dirichlet Transport</i>.

[![arXiv](https://img.shields.io/badge/arXiv-2408.03425-b31b1b.svg)](https://arxiv.org/abs/2501.15549)
</div>

📕 A companion html e-book that explains the codes and provides supplementary 
materials can be accessed at the following url: 
<https://fer-agathe.github.io/transport-simplex>.

The R scripts are also available: <https://fer-agathe.github.io/transport-simplex/replication-paper/scripts/>.

## Reference of the paper (preprint on arXiv)

```
@misc{machado2025optimaltransportcategoricaldata,
      title={Optimal Transport on Categorical Data for Counterfactuals using Compositional Data and Dirichlet Transport}, 
      author={Fernandes Machado, Agathe and Charpentier, Arthur and Gallic, Ewen},
      year={2025},
      eprint={2501.15549},
      archivePrefix={arXiv},
      primaryClass={cs.LG},
      url={https://arxiv.org/abs/2501.15549}, 
  }
```

## Objectives

We are interested in deriving counterfactuals for categorical data $\mathbf{x}$
with $d$ categories. In a first step, the categorical data is converted into 
compositional data. Then, we use optimal transport to build counterfactuals. 
We propose two methods:

1. The first method consists in using Gaussian optimal transport based on an 
alternative representation of the probability vector (in the Euclidean space 
$\mathbb{R}^{d-1}$).
2. The second method uses transport and matching directly within the simplex 
$\mathcal{S}_d$ using an appropriate cost function.

## Small Package

We defined some of the functions used in this ebook in a small R package, 
{transportsimplex}, which can be downloaded from the 
[github repository](https://github.com/fer-agathe/transport-simplex) associated 
with the paper.

To install the package:
```{r}
remotes::install_github(repo = "fer-agathe/transport-simplex")
```

Then, the package can be loaded as follows:
```{r}
library(transportsimplex)
```

The following small examples show how to use the package:

```{r}
# First three columns: probabilities of being of class A, B, or C.
# Last column: group (0 or 1)
data(toydataset)
X0 <- toydataset[toydataset$group == 0, c("A", "B", "C")]
X1 <- toydataset[toydataset$group == 1, c("A", "B", "C")]

# Method 1: Gaussian OT in the Euclidean Space
# --------------------------------------------
# Transport only, from group 0 to group 1, using centered log ratio transform:
transp <- transport_simplex(X0 = X0, X1 = X1, isomorphism = "clr")

# If we want to transport new points:
new_obs <- data.frame(A = c(.2, .1), B = c(.6, .5), C = c(.2, .4))
transport_simplex_new(transport = transp, newdata = new_obs)

# If we want to get interpolated values using McCann (1997) displacement
# interpolation: (here, with 31 intermediate points)
transp_with_interp <- transport_simplex(
  X0 = X0, X1 = X1, isomorphism = "clr", n_interp = 31
)
interpolated(transp_with_interp)[[1]] # first obs
interpolated(transp_with_interp)[[2]] # second obs

# And displacement interpolation for the new obs:
transp_new_obs_with_interp <- transport_simplex_new(
  transport = transp, newdata = new_obs, n_interp = 5
)
interpolated(transp_new_obs_with_interp)[[1]] # first new obs
interpolated(transp_new_obs_with_interp)[[1]] # second new obs

# Method 2: Optimal Transport within the simplex
# ----------------------------------------------
# Optimal Transport using Linear Programming:
mapping <- wasserstein_simplex(as.matrix(X0), as.matrix(X1))
# The counterfactuals of observations of group 0 in group 1
counterfactuals_0_1 <- counterfactual_w(mapping, X0, X1)
```

