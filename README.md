INTRODUCTION

This repository contains a folder called Package, which contains Mathematica packages (.m files) and their correspondent notebooks (.nb files). These notebooks have a command that generates the packages, and they also contain examples and explanations. Let me introduce them briefly:
    - The geoMetric package contains common functions that can be used in any other context. 
    - The PilchWarner package contains the Pilch-Warner geometry and its background fields, from the reference paper:
      http://arxiv.org/abs/hep-th/0306098
    - matrixEDC.m is taken from:
      http://www.inp.demokritos.gr/~sbonano/EDC/
      It's used only for exterior products.

The files in the main folder uses the packages above. They are:
    - SupergravityPW.nb, that checks the supergravity equations for the Pilch Warner background.
    - SusyPW.nb, that computes the dilatino's and gravitino's variational equation for Pilch Warner geometry.
 


HOW TO USE THE PACKAGES

Please check either Supergravity.nb or SusyPW.nb, where I load the packages by adding it directory to $PATH and the use Get (<<).




