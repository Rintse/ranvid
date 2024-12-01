#!/bin/env bash
set -e

cd src
rm -r Syntax/Grammar
bnfc -p Syntax -d grammar.bnf
rm -r Syntax/Grammar/Test.hs
