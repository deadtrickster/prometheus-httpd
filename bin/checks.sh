#!/bin/sh

rebar3 do elvis, xref, dialyzer, eunit
