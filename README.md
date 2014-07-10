A scraper for [Marsad](http://www.marsad.tn) data that builds a network out of constitutional amendment cosponsorships:

![](plots/constitution_network.jpg)

> Colors are arbitrary. __See also: [interactive visualization](http://briatte.org/marsad/)__.

Differential homophily [estimates](http://cran.r-project.org/web/packages/ergm/) of political bloc cohesion, controlling for network size, bloc size and degree:

![](plots/ergm_homophilies.jpg)

> Parametered [as follows](https://github.com/briatte/marsad/blob/master/marsad.r#L255-L261):

```{S}
ergm(net ~ edges +
         gwdegree(decay = 1, fixed = TRUE) +
         nodefactor("bloc") +
         nodematch("bloc", diff = TRUE) + 
         nodefactor("sexe") +
         nodematch("sexe"),
       control = control.ergm(MCMLE.maxit = 100))
```

## HOWTO

The main entry point is `make.r`.

## TODO

* fix birth year variable
* add diagnostics to ERGM
