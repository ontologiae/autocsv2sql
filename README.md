# autocsv2sql
CSV to SQL tool which automatically detect columns's type and generate SQL script with Create Table and insert Into

# Usage
Usage : 

```bash
autocsv2sql mycsvfile.csv > mysqlfile.sql
```


Other usage : 

```bash
autocsv2sql mycsvfile.csv | psql
```

# Build

```make build```


# Install

```sudo make install```


# Note 

Generate only three type of data : Numeric, text, int


# TODO

- Handle dates, timestamp
- Handle other database than PostgreSQL
