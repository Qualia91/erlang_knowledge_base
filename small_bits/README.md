Stuff
-----

Compile and save compiled file (expanded macros etc)
```erlang
compile:file(M, ['P']).
```

Compiling and testing:
mkdir ebin
erlc -o ebin/ -DTEST=true -I include src/test_sup.erl 
erl -noshell -pa ebin -eval 'eunit:test("test_sup",[verbose])' -s init stop