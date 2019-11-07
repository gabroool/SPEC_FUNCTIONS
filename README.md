This repository contains R code for computing quantum catches, chromatic and achromatic contrasts, using functions from the 'pavo' package for bee and bird vision. File examples are also included.

Five functions are provided:
<p>1: <b>getspec_full</b> (provided the name of the species/folder, corrects and averages all spectra combining <i>getspec()</i>, <i>procspec()</i> and <i>aggspec()</i> and outputting a mean curve); </p>

<p>2: <b>qcatch_cs()</b> (calculates quantum catches using color spaces for bee (<i>Apis</i>, <i>Bombus</i>) and bird vision (<i>Sephanoides</i>, avg.v, avg.uv) using <i>vismodel()</i>); </p>

<p>3: <b>qcatch_nl()</b> (calculates quantum catches using the noise-limited for bee (<i>Apis</i>, <i>Bombus</i>) and bird vision (<i>Sephanoides</i>, avg.v, avg.uv) using <i>vismodel()</i>); </p>

<p>4: <b>contrast_cs()</b> (computes chromatic and achromatic constrasts using color spaces for bee (<i>Apis</i>, <i>Bombus</i>) and bird vision (<i>Sephanoides</i>, avg.v, avg.uv) combining <i>vismodel()</i> and <i>colspace()</i>); </p>

<p>5: <b>contrast_nl()</b> (computes chromatic and achromatic constrasts using the noise-limited model for bee (<i>Apis</i>, <i>Bombus</i>) and bird vision (<i>Sephanoides</i>, avg.v, avg.uv) combining <i>vismodel()</i> and <i>coldist()</i>) </p>

