# Setting Up The Code

## Gitting started

First you will want to setup the code as you do with lab material:

  1. Go to [the project page][project] and fork the project.
  2. **Ensure that the `comp1100-tutors-2016` user has reporter access to your
     fork.**
  2. Next you need to download your fork of the code on your own (or lab) machine:
	 `git clone https://gitlab.cecs.anu.edu.au/<your uni id>/comp1100-2016-assignment1.git`
  3. Change into that directory `cd comp1100-2016-assignment-1`
  4. Add the assignment repository as a remote: 
    + `git remote add assignment https://gitlab.cecs.anu.edu.au/comp1100-2016/comp1100-2016-assignment1.git`
    + If your remote is incorrect you can run:
        - `git remote set-url assignment https://gitlab.cecs.anu.edu.au/comp1100-2016/comp1100-2016-assignment1.git`
  5. From now on you can work on your own code and update your own fork by running:
     `git pull origin master` or `git push origin master`. 
	  + To get updates from the main repository run `git pull assignment master`.
	  + git will open up an editor, nano is the default on the lab machines. 
      You just need to save the file to complete the merge. 
      (press and hold ctrl, then press o to save in nano)
	  + If you get, "Your local changes to the following files would be overwritten by merge". 
	    You can run `git stash` then `git pull assignment master` followed by `git stash pop`.
  6. Then share your forked repository with your tutor just like in
     [lab3](https://gitlab.cecs.anu.edu.au/comp1100-2016/comp1100-2016-lab03).

[project]: https://gitlab.cecs.anu.edu.au/comp1100-2016/comp1100-2016-assignment1

## On the lab machines: using a script

To compile your project you need to use:

```
./make_langtons_ant
```

In a similar fashion, every time you modify your code you will
want to run this same command to rebuild things.

## On your own computer: using Stack

A number of students are having trouble with getting Cabal to work on their machines. Stack
is a wrapper for cabal that fixes may of cabal's weak points.

Go here to [install](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md), then run:

```
stack setup
```

Stack may ask you some questions to guide it. Afterwards run:

```
stack build
```

Do note that once the installation is complete you will only need to
use the `stack build` command to recompile after you change the code.

## On your own computer: using Cabal

Cabal is a package manager included in Haskell distributions. Not only
does it manage extensions for the language you can also use it to
build projects.

If you are using your own computer you can therefore use the following
commands to setup a working environment and compile your code.

```
cabal update
cabal sandbox init
cabal install --dependencies-only
cabal build
```

A quick rundown of the commands:

  1. Update cabal to have access to the latest packages.
  2. Create a self-contained environment in the project's folder,
     called a *sandbox*.
  3. In this sandbox, install all the required dependencies to make
     the project run.
  4. Build the project and create the executable.

Do note that once the installation is complete you will only need to
use the `cabal build` command to recompile after you change the code.

# Running Langton's Ant

Once you have downloaded and installed the code you will want to run
it. To do so simply type:

```
./run_langtons_ant
```

If that works you should a small triangle in the middle of the screen
which is doing nothing. You can set a few options when you run the program
the first and most useful one is 
```
./run_langtons_ant -h
```
which will print out a help message which explains all the other options. 
Rather than re-iterating that, here are a few examples (note: most of 
these won't work until you finish the assignment, but `-h` will always
work)
```
./run_langtons_ant -m square -f 60 -s 10 -c 0
```
Run part 1 of the assignment, at 60 frames per second, doing 10 transitions
per frame with the colour of a cell in state 1 as 0 degrees (i.e red).
See [here](http://i.stack.imgur.com/FfDXb.png) for a picture of what is meant.
```
./run_langtons_ant -m hex -f 30 -s 10 -t R1R2NUR2R1L2 -c 240 --shift 17
```
Draw a nice blue spiral
```
./run_langtons_ant -m square -t LRRL --shift 60
```
Draw a growing square with a large difference between the colours
of cells

## Testing code in ghci

To test parts of your code or any part of the assignment you will use ghci.
Since this is a project with lots of files, calling ghci is done a little differently.

If you did not use cabal with a sandbox, then you can execute
`ghci src/Main.hs -isrc` to load the entire project.
Make sure you are in the comp1100-assignment-1 folder, not any of the sub folders.
Let's say you want to understand how `newSquareWorld` works and your code in
`LangtonsAnt.hs` does not compile, then you can execute
`ghci src/Datastructures/SquareWorld.hs -isrc` to simply load `SquareWorld.hs`
and its dependencies.

A quick breakdown of the command:

  1. `ghci`             — Compiles your code and you an interactive environment
  2. `src/Main.hs`      — The path to the file you want to load
  3. `-i`               — The option that tells ghci where to look for dependencies
  4. `src` (after -i)   — Where the dependencies are. Notice there is no space between the -i and the folder with the dependencies.

If you installed with cabal on your own machine then you will have to use
`cabal repl` instead of just calling ghci. Cabal knows where your dependencies are.

# Weird Bugs

## Missing packages errors

When compiling you may get errors about missing modules like this:

```Haskell
src/Main.hs:16:8:
    Could not find module ‘Graphics.Gloss.Interface.Pure.Simulate’
    Use -v to see a list of the files searched for.
```

Presumably, you installed with `cabal` on your own machine.
If the error came with running `./make_langtons_ant` then:

```
cabal install --dependencies-only
cabal build
```

Otherwise:

```
cabal install gloss
```

## On Mac using ghc-7.10.2 will you get this error:

```
<no location info>:
    <command line>: can't load .so/.DLL for: /Users/<YourUserName>/path/to/your/assignment/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/GLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh/libHSGLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh-ghc7.10.2.dylib (dlopen(/Users/<YourUserName>/path/to/your/assignment/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/GLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh/libHSGLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh-ghc7.10.2.dylib, 5): Symbol not found: _glutBitmap8By13
  Referenced from: /Users/<YourUserName>/path/to/your/assignment/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/GLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh/libHSGLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh-ghc7.10.2.dylib
  Expected in: flat namespace
 in /Users/<YourUserName>/path/to/your/assignment/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.2/GLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh/libHSGLUT-2.7.0.6-KJLiPQLGKik1ZXxNf3QrFh-ghc7.10.2.dylib)
```

It means you need to update to ghc-7.10.3. If you have brew then you can `brew unlink ghc && brew install ghc` to update to the newest version.
