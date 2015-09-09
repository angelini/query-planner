# Query Syntax

```clojure
[[(load   :table.a) (load :table.b)]
 [(map    :f1)      (map  :f2)     ]
 [(filter :f3)      (none)         ]
 [(join   :other.c) (none)          (load :other.c)]
 [(none)            (sort 0)        (none)         ]
 [(select)          (empty)         (select)       ]]
```

## Nodes

`(load :<table>.<col>)`

Reads the column from the data storage layer
Valid with: load, empty, none, join

`(map :<fid>)`

Transforms values via the function at fid
Valid with: map, empty, none

`(filter :<fid>)`

Filters out rows based on the application of fid to the row's val
Valid with: filter, empty, none

`(join :<table>.<col>)`

Joins in a new table where the current val is equal to the joining val
Valid with: join, empty, none, load

`(select)`

Set every other column to empty
Valid with: select, empty

`(group)`

Group rows where the val of the current col is equal
Valid with: empty, none

`(sort :idx)`
Sort all rows by the cols in the order of idx
Valid with: sort, empty, none

`(none)`

No operation applied to the current col at the current stage
Valid with: none, empty, group, join, filter, map, load

`(empty)`

The current col can no longer be used or modified
Valid with: *

## Other Validations

1. All query columns must begin with `empty` or `load`. And if `empty`, it can only be `empty`
until a `load` happens.

2. All sort rows must have `idx`s incrementing from 0 to at most n, where n is the number of nodes
in the row.
