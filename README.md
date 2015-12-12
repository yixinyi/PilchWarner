# INTRODUCTION #

This repository contains a folder called Packages, which contains Mathematica packages (.m files) and their correspondent notebooks (.nb files). These notebooks have a command that generates the packages, and they also contain examples and explanations. Let me introduce them briefly: <br />
* The geoMetric package contains common functions that can be used in any other context. <br />
* The PilchWarner package contains the Pilch-Warner geometry and its background fields, from the reference paper: <br />
http://arxiv.org/abs/hep-th/0306098 <br />
* matrixEDC.m is taken from: <br />
http://www.inp.demokritos.gr/~sbonano/EDC/ <br />
It's used only for exterior products and derivatives. <br />

The files in the main folder use the packages above. They are: <br />
* SupergravityPW.nb, that checks the supergravity equations for the Pilch Warner background. <br />
* SusyPW.nb, that computes the dilatino's and gravitino's variational equation for Pilch Warner geometry.
 


# HOW TO USE THE PACKAGES #

Please check either Supergravity.nb or SusyPW.nb, where I load the packages by adding its directory to $PATH and then, use Get (<<).




