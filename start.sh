#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s pmpy \
	-s toolbar \
	-name pmpy
