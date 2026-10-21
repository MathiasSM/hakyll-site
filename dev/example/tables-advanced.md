---
title: "Advanced tables"
description: "Grid, simple, and multiline tables"
path: "/example/tables-advanced"
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Besides pipe tables, pandoc supports three other table syntaxes that allow richer cell content.

## Grid tables

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. A grid table draws cell borders with `+` and `-`, so cells can hold several lines:

+-------------+--------------+
| Term        | Meaning      |
+=============+==============+
| Grid table  | Cells are    |
|             | drawn with + |
+-------------+--------------+

A grid table can also carry a footer, marked by an extra `=` separator:

+---------+-------+
| Item    | Price |
+=========+=======+
| Eggs    | 5     |
+---------+-------+
| Milk    | 3     |
+=========+=======+
| Total   | 8     |
+=========+=======+

## Simple tables

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. A simple table uses a single `-` row under the header:

Option    Description
--------- -----------
data      path to data files.
engine    template engine used.

## Multiline tables

Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. A multiline table needs a full separator line at the top:

---------- ----------
Left       Right
---------- ----------
alpha      beta
gamma      delta
---------- ----------
