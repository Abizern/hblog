# Convenient script to build the executable.

ghc -O2 --make hblog.hs&&rm hblog.hi&&rm hblog.o&&strip hblog
