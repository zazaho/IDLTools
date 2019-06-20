Hello Reader and welcome to the wonderful world of machine readable
tables in IDL.

The following describes a suite of small routine that I wrote to
manipulate such tables inside IDL. For the more ignorant among us these
are simple ascii files that contain a header which describe the data.
The header if formatted as follows:

Title: Optical photometry of stars
================================================================================
Byte-by-byte Description of table                                               ***
--------------------------------------------------------------------------------***
   Bytes Format Units  Label   Explanations					***
--------------------------------------------------------------------------------***
   1-  9 I9     ---     ID      ID
  11- 18 F8.4   Deg     RA      Right ascension in degrees                      ***
  20- 27 F8.5   Deg     DEC     Declination in degrees                          *** 
  47- 54 F7.4   mag     Rmag    R magnitude                                     ***
--------------------------------------------------------------------------------***
102550984 100.0001  0.21852  17.0420  16.2890  15.8750  15.3740  14.5310  14.0060  14.2820   A7  V  0.60000000  0.41621990
102551001 100.0002  0.18248  17.2090  16.3000  15.9050  15.3550  14.4400  13.9570  14.0120   G2  V  0.30000000  0.08263000
102551021 100.0004 -0.25603  17.1670  16.1670  15.7240  15.1560  14.2500  13.7040  13.8360   K2  V  0.10000000  0.51176480

The lines marked with an *** are obligatory and you need some data
lines of course.

The data columns need to be aligned on their ascii column.

Once such a file is in place the following would allow you to use it:

;; read the file
observations = read_fmr('file.dat')

;; Visualise
pl_mr,observations,'RA','Dec',ps=1

;;Extract some column
rmags = col_mr(observations,'rmag')

;; select a subset of rows:
bright = select_mr(observations,constraint='Rmag lt 14')

;; Now for the fancy stuff
more_observations = read_fmr('file2.dat')

;; Join the two tables by matching columns
joined = match_mr(observations,more_observations,'id','srcid')

;; Match the two table by sky position
bandmerged = match_mr(observations,more_observations)

;; save a table in the same format:
write_fmr,bandmarged,'bmerged.dat'

;; The are several keyword and options. Use /help as a keyword to read
;; the full documentation

Greetings AND bon courage!!
