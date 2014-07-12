A scraper for [Marsad](http://www.marsad.tn) data that builds a network out of constitutional amendment cosponsorships:

![](plots/constitution_network.jpg)

> Colors are arbitrary.

The code also estimates legislator ideal points from their voting records: [see below](#nominate).

## DEMO

[![](plots/demo.png)](http://briatte.org/marsad/)

> Click image to view.

## HOWTO

The main entry point is `make.r`, which will

1. collect MP and constitutional amendment data from Marsad.tn
2. build an undirected network from amendment cosponsorship
3. model the extent of political bloc homophily in the network
4. collect MP votes to estimate [alpha-NOMINATE](http://cran.r-project.org/web/packages/anominate/) and [OC](http://cran.r-project.org/web/packages/oc/) ideal points

> __Note:__ the `data.r`, `ergm.r` and `gexf.r` scripts can be set to run on a subset of the amendments data: adjust the `sample` to run on a specific segment of the Constitution (either `"Préambule"` or `"ch1"` to `"ch7"`). Each script defaults to `FALSE` to run on the complete network.

## SPECS

### ERGM

The network model is parametered [as follows](https://github.com/briatte/marsad/blob/master/code/ergm.r#L21-L27):

```{S}
ergm(net ~ edges +
         gwdegree(decay = 1, fixed = TRUE) +
         nodefactor("bloc") +
         nodematch("bloc", diff = TRUE) + 
         nodefactor("sexe") +
         nodematch("sexe"),
       control = control.ergm(MCMLE.maxit = 100))
```

### NOMINATE

The ideal points are estimated [as follows](https://github.com/briatte/marsad/blob/master/code/vote.r#L228-L229):

```{S}
anominate(RC, dims = 2, polarity = c(1, 1), nsamp = 1000, thin = 1,
          burnin = 500, random.starts = FALSE, verbose = TRUE)
```

In the two-dimensional model, all but 2 Nahdha members (_n_ = 87) score above 0 on the first dimension, and all but 12 non-Nahdha members (_n_ = 130) score below 0 on that same dimension:

![](plots/idealpoints.jpg)

> The range around the first dimension estimate is a 95% credible interval. The lighter dots mark the second dimension estimate.

Similarly, a two-dimensional [optimal classification][oc] assigns 83 out of 87 Nahdha members and 5 out of 130 non-Nahdha members to the same (top-right) quadrant:

![](plots/oc_2d.jpg)

[oc]: https://github.com/briatte/marsad/blob/master/code/vote.r#L201

## TODO

* [_] fix invalid birth year values
* [_] add diagnostics to ERGM
* [x] add [electoral law](http://www.marsad.tn/fr/loi_electorale/index) amendments[x]
* [x] add [votes](http://www.marsad.tn/fr/votes) and ideal points
