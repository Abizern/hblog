# Convenient script to build the executable.

ghc -O2 --make -dynamic hblog.hs&&rm hblog.hi&&rm hblog.o&&strip hblog
