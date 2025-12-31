# Close Database Connection

This function updates the table list in the \`mod_db\` object and closes
the database connection.

## Usage

``` r
.modhelper_closeDB(mod_db)
```

## Arguments

- mod_db:

  An object containing information about the database, including the
  list of tables.

## Value

None. This function is called for its side effects (updating the object
and closing the connection).

## Details

The function updates the \`tables\` attribute of the \`mod_db\` object
with the current list of tables in the connected database before closing
the connection.
