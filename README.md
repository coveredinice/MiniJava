<h1>Interpreter for MiniJava</h1>
aka <i>I still live to tell the tale</i>.<br>

This is an implementation of an interpreter for MiniJava, done for the <i>Compilers</i> course at Ca' Foscari University of Venice,  master's degree in Computer science. It's written in Ocaml. I had a lot of fun.

<h2>Usage</h2>
On Unix systems, type `make` to compile the whole thing; type `make test` instead to interpret the two pieces of MiniJava code described below. 

<h2>Tests</h2>

Two pieces of MiniJava code are included to test the interpreter:

<ul>
  <li><b>test1.java</b> tests methods with or without parameters, objects, arrays, arrays of objects, arrays of arrays. It's a small beast, but a beast nonetheless.</li>
  <li><b>test2.java</b> tests arithmetic operations, booleans, conditionals. It's the easy-going brother of test1.
</ul>

The irony of extending class `Europe` with class `England` is not lost to me. 

<h2>What's next</h2>

A few things. Some comments are in Italian - just skip them until I translate them. The code could be restructured to be more compact and less redundant but I love it anyway.
