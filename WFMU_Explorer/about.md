---
title: "version: 0.22"
date: "August 21, 2017"
output: html_document
---
![](https://wfmu.org/wp-content/uploads/2016/03/BadgeCourage.png) 

## About WFMU Playlist Explorer


*Changes in 0.22: includes 2-DJ comparison.

*Changes in 0.21: includes DJ similarity.

*Changes in 0.20: includes DJ top plays.


Free form radio, WFMU.ORG, maintains a huge trove of past playlists from many DJ's radio shows.  More recently, web-only programming has been added to this.  This dataset offers lots of opportunities for analysis.  I scraped all the playlists I could from the web site and started asking questions.  The data set is here for your own explorations. It represents over a million plays spanning decades.

The scraping and data-cleaning process was the most time consuming part of the exercise. Playlist tables are not in a consistent format and my HTML skills are rudimentary.  I cleaned up the raw data to reduce errors and create consistency.  DJ's are inconsistent in how they enter artist names.  There are 12 different ways Andy Breckman can misspell "Bruce Springsteen"  I take a stab at fixing some of the glaring errors. I'm sure I missed many. Additionally, many artist names are variant due to collaborators with "featuring," "and the," "with," etc. in the name.  I condense the first two words of every artist name into a token and drop the rest. In a very few cases the air date is clearly wrong. I strip those shows out.

There is a final step which is really a judgement call.  Many DJs have signature songs to open and/or close their shows.  Including these skews the play count for songs.  I have chosen to strip those out, or try to.  Songs where one DJ accounts for just about all the plays are stripped out as well.  This is the ultimate data set I use here.

The end result is an reasonably accurate but incomplete record of all the playlists available at WFMU.ORG as of July 2017.  The code used for scraping,cleaning and analyzing is available at https://github.com/apsteinmetz/wfmu.

Thanks to station manager, Ken Freedman, for giving me permission to scrape the site.

WFMU is listener supported!  [PLEDGE HERE!](https://pledge.wfmu.org/donate?step=landing)

-- Art Steinmetz (apsteinmetz@yahoo.com)
