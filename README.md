# hidd

This project aims to be a haskell isssue management system. It should
have the following features:

- List issues
- View issue details
- Create git branches off issues
- Comment on issues
- Close issues

It should also support gitlab and github for now.

It currently only lists issues from github in a scrollable window on the
TUI.

## Building and Usage
Clone the repo and run the following:

- stack install
- stack build
- stack exec -- hidd-exe

To use this globally, copy the compiled binary to a location in $PATH.

In my case its:

```
cp .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/hidd-exe
~/local/bin
```

## Basic Usage

- hidd-exe -url=gihubapiurl (e.g hidd-exe
  --url="https://api.github.com/repos/vmg/redcarpet/issues"(
- hidd-exe (if from a git repo, it will automatically fetch issues for
  the repo)
- hidd-exe -h (shows basic usage)

## Auth

To access private gitlab tokens, hidd uses [gitlab personal access
tokens](https://docs.gitlab.com/ee/api/README.html#personal-access-tokens).
The token is set in a config file with the path 
`~/.config/hidd/hiddrc`. An example config file is 

```
gitlab-private-key = "key"
```
