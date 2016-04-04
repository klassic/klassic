# Grammar of Klassic in PEG

In this file, grammar of Klassic will be described in (pseudo) [Macro PEG](https://github.com/kmizu/macro_peg).

```
ClassDefinition <- "class" Identifier ParameterList ["<:" repsep(",", IDentifier) ] ("{"
                     MemberDefinition*
                   "}")
```