import "hint" HLint.HLint
import "hint" HLint.Dollar

ignore "Redundant do"  = Text.Kibr.Css
ignore "Use camelCase" =
    Test.Kibr.Css Test.Kibr.Http Test.Kibr.Irc Test.Kibr.Xml
