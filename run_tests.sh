#!/bin/bash

ocamlc -c polynomial.ml
ocamlc graphics.cma polynomial.cmo tests.ml -o tests
rm *.cm*
ocamlrun tests
