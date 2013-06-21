# Copyright © 2013 Bart Massey
# [This program is licensed under the "MIT License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

chomp: chomp.hs
	ghc -Wall --make -o chomp chomp.hs
