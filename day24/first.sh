#!/bin/bash
ghc getInp.hs && ./getInp <inp.txt >first.hs && ghc first.hs && ./first
