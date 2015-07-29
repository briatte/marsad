A scraper for [Marsad](http://www.marsad.tn) data that

* builds networks out of constitutional and electoral law amendments
* estimates legislator ideal points from their voting records

The repository was initially put together to build this [interactive visualization](http://f.briatte.org/parlviz/marsad/) of amendment cosponsorships:

[![](plots/demo.png)](http://f.briatte.org/parlviz/marsad/)

The current version produces almost exactly the same data, updated to the post-2014 election assembly.

## HOWTO

Replicate by running `make.r` in R.

## TODO

- [x] add [electoral law](http://www.marsad.tn/fr/loi_electorale/index) amendments
- [x] add [votes](http://www.marsad.tn/fr/votes) and ideal points
- [x] support post-2014 votes
- [x] optimize code
- [x] reorganise file structure

## THANKS

Thanks to [Jean-Baptiste Gallopin](http://yale.academia.edu/JeanBaptisteGallopin) and to the [Marsad](http://www.marsad.tn) team.
