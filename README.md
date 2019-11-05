


# Getting Started

Install the following windows programs:
 - Stack: <https://docs.haskellstack.org/en/stable/README/>
 - Git: <https://git-scm.com/download/win>
 - Node: <https://nodejs.org/en/blog/release/v8.9.3/>
 - Yarn: <https://yarnpkg.com/lang/en/docs/install/#windows-stable>
 - PostgreSQL: <https://www.postgresql.org/download/windows/>


# Build for distribution
This will generate files and binaries in dist directory for distribution.

```bash
stack build --copy-bins --local-bin-path dist\
yarn run parcel build assets/elm/index.html -d dist/static
```

# Development

## Code formating
Use Ormolu: <https://github.com/tweag/ormolu>

## Building

```bash
stack build
```

## Run migrations
Create the database with the `migration` tool :  
  `./db/migration createdb db_name dba`
* Initialize the DB :  
  `./db/migration init "postgresql://db_user:password@db_server:db_port/db_name"`
* Run the migrations :  
  `./db/migration migrate "postgresql://db_user:password@db_server:db_port/db_name"`
* Use the `migration` tool for migrations :  
  `./db/migration --help` 

### Upload the test data:
  `ogr2ogr -f PostgreSQL "{POSTGRESQL CONNECTION STRING}'" data/test_data.csv -progress`
  




