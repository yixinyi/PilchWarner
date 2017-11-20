# INTRODUCTION #

This repository contains a folder called Packages, which contains Mathematica packages (.m files) and their correspondent notebooks (.nb files). These notebooks have a command that generates the packages, and they also contain examples and explanations. Let me introduce some of the packages: <br />
* **PilchWarner.m** contains the Pilch-Warner geometry and its background fields (**PilchWarnerFields.nb** provides a complete list), from the reference paper: <br />
http://arxiv.org/abs/hep-th/0306098 <br />
* **Dbrane.m** contains the kappa-symmetry projector. 
* **matrixEDC.m** is taken from <br />
http://www.inp.demokritos.gr/~sbonano/EDC/ <br />
Only it's wedge products and exterior derivatives are used. <br />


The files in the main folder use the packages above. They are: <br />
* **SupergravityPW.nb**, that checks the supergravity equations for the Pilch Warner background. <br />
* **SusyPW.nb**, that computes the dilatino's and gravitino's variational equation for Pilch Warner geometry.
* **D3branePW.nb**, which contains computations for the paper <br />
http://arxiv.org/pdf/1512.06420v1.pdf <br />



# HOW TO USE THE PACKAGES #

Load the packages by adding its directory to $PATH and then, use Get (<<).





