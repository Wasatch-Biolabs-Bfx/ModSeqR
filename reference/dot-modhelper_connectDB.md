# Connect to a Database

This internal function establishes a connection to a DuckDB database. It
can handle both a character file name or an object of class \`mod_db\`
to open the database.

## Usage

``` r
.modhelper_connectDB(mod_db)
```

## Arguments

- mod_db:

  A character string representing the file path to the DuckDB database
  or an object of class \`mod_db\`.

## Value

A database connection object.

## Details

This function checks the class of \`mod_db\` and attempts to connect to
the database. If \`mod_db\` is a character string, it will create an
object of class \`mod_db\`. If \`mod_db\` is already of class
\`mod_db\`, it will directly establish a connection to the database.

## Note

This function is intended for internal use within the package.
