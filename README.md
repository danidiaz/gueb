What's this?
============

A command line tool that takes a shell command as parameter and allows users to
invoke it repeatedly from the web using REST calls.

Yeah, it does sound unsafe.

Development
===========

cabal exec -- ghcid --test=":run makeMain --port 8000 samples/plan.json"{
