# purescript-couchdb

Functions to log into Couchdb, manage users and databases.

## Dependency
This library uses Affjax. From the documentation of that library:

If you are intending to use the library in a Node.js setting rather than the browser, you will need an additional dependency from npm:

```
npm install xhr2
```
However, instead one should use a version of that library that handles cookies:

```
npm install xhr2-cookies
```
So make sure to add that dependency when you use purescript-couchdb in a node setting.
