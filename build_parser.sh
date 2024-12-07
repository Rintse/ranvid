#!/bin/env bash
set -e

cd src
rm -rf Syntax/Grammar
bnfc -p Syntax -d grammar.bnf
rm -r Syntax/Grammar/Test.hs
