A scraper for [Marsad](http://www.marsad.tn) data that builds a network out of constitutional amendment cosponsorships:

![](plots/constitution_network.jpg)

> Colors are arbitrary.

## DEMO

[![](demo.png)](http://briatte.org/marsad/)

> Click image to view.

## HOWTO

The main entry point is `make.r`, which will

1. collect MP and constitutional amendment data from Marsad.tn
2. build an undirected network from amendment cosponsorship
3. model the extent of political bloc homophily in the network

The `data.r`, `ergm.r` and `gexf.r` can be set to run on a subset of the complete data: adjust the `sample` to run on a specific segment of the Constitution, e.g. `"Préambule"` or `"ch1"`.

## SPECS

The model is parametered [as follows](https://github.com/briatte/marsad/blob/master/ergm.r#L4-L10):

```{S}
ergm(net ~ edges +
         gwdegree(decay = 1, fixed = TRUE) +
         nodefactor("bloc") +
         nodematch("bloc", diff = TRUE) + 
         nodefactor("sexe") +
         nodematch("sexe"),
       control = control.ergm(MCMLE.maxit = 100))
```

Differential homophily [estimates](http://cran.r-project.org/web/packages/ergm/) of political bloc cohesion, controlling for network size, bloc size and degree:

![](plots/ergm_homophilies.jpg)

## TODO

* fix birth year values
* add diagnostics to ERGM
* add [electoral law](http://www.marsad.tn/fr/loi_electorale/index) amendments
