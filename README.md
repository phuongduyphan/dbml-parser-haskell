# dbml-parser
DBML command line

## Demo
Run the CLI from the dbml executable file
```
$ ./dbml -h

Usage: dbml COMMAND
  DBML cli tool

Available options:
  -h,--help                Show this help text

Available commands:
  parse                    parse DBML to AST
  normalize                convert DBML AST to Map structure
  export                   export DBML to other database format


```

## Installation
To build, you need: 
* [stack](https://docs.haskellstack.org/en/stable/README/)

Download dependencies:
```
$ stack setup
```

Build package:
```
$ stack build
```

Pull things up in ghci:
```
$ stack ghci
> :set -XOverloadedStrings
```
