#!/bin/bash
erl \
	-pa deps/*/ebin ebin \
	-s pmpy \
	-name pmpy
