#!/bin/sh

rebar3 as test do elvis, xref, dialyzer, ct
