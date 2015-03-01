# anagrams

In this project we introduce the Clojure programming language by implementing
a simple algorithm to solve anagrams. We are going to be using JVM-based
Clojure, so before we start, make sure you have Java installed:

    java -version

Java downloads for most popular operating systems, and for Microsoft
Windows, are available [here](https://java.com/en/download/manual.jsp).

We are also going to use the [Leiningen](http://leiningen.org/) tool to
manage our dependencies and to run and compile our Clojure programs. You
will find installation instructions for Leiningen
[here](http://leiningen.org/#install). To check it's working, run

    lein version

Now you're ready to get started!

## Usage

The tutorial consists of a number of [Gorilla REPL](http://gorilla-repl.org/)
worksheets, found in the `worksheets` directory. We'll use Leiningen
to start the Gorilla REPL; make sure you're in the project directory, then
run:

    lein gorilla

After a few seconds, you should see a message: *Running at
http://127.0.0.1:48786/worksheet.html* (the port number could change
each time you run this command). Open the link in your favourite browser.

To load the first worksheet, type `alt-g alt-g` (or click on the menu
icon in the top-right of the browser window), scroll down to `Load a
worksheet`, and enter `worksheets/001-basics.clj`.

## License

Copyright © 2015 Ray Miller <ray@1729.org.uk>

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

The word list distributed in the resources is from the Debian package
[wbritish](https://packages.debian.org/sid/text/wbritish), from the
[SCOWL project](http://wordlist.aspell.net/).
