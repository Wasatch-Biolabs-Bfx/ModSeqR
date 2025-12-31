# Export Tables from the mod Database

This function exports specified tables from the mod database to CSV
files. Can export one or multiple tables as a time. It checks whether
each table exists in the database before exporting, and provides
informative messages for any missing tables. The output CSV files are
saved at the specified path.

## Usage

``` r
export_mod_table(mod_db, table = "positions", out_path)
```

## Arguments

- mod_db:

  A string. The path to the database containing ch3 files from nanopore
  data.

- table:

  A character vector specifying the table to be exported from the
  database. Default is "positions".

- out_path:

  A string. The path to the directory where the CSV files will be saved.
  The file will automatically be named "table name.csv".

## Value

NULL. The function writes the specified tables to CSV files.

## Details

The function connects to the specified database and iterates through the
list of table names provided in the \`tables\` parameter. For each table
that exists in the database, it reads the table into R and writes it as
a CSV file to the location specified by \`out_path\`. If a table does
not exist in the database, a message is printed indicating this.

In case of any error during the execution, a custom error message is
displayed. The function ensures that the database connection is closed
safely using the \`finally\` block.

## Note

The function assumes that the tables specified in \`tables\` exist in
the database and can be accessed via the \`DBI\` package.
