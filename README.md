# EViE

Quickly build evaluation applications for strategic decisions.

EViE provides a concise API for accessing scenario and metric data stored in a PostgreSQL or SQLite database, and a set of templates for getting started with different use cases.

Download the latest releast from the releases page to get started.

# Getting Started

Download a pre-built package from the [releases page](https://github.com/state-machine-io/evie/releases). 

Unzip this and enter your PostgreSQL connection settings into the "config.json" file.

Run "evie.exe". This will open a browser window on the summary page.

# Contributing

Install the following windows programs:
 - Stack: <https://docs.haskellstack.org/en/stable/README/>
 - Git: <https://git-scm.com/download/win>
 - Node: <https://nodejs.org/en/blog/release/v8.9.3/>
 - Yarn: <https://yarnpkg.com/lang/en/docs/install/#windows-stable>
 - PostgreSQL: <https://www.postgresql.org/download/windows/>

## Code formating
Use Ormolu: <https://github.com/tweag/ormolu>

## Building

### Server
```bash
stack build
```

#### Common Problems:

Postgresql: If the build fails because of a missing libpq or similar, check that the postgresql bin directory has been added to your path. Eg: <https://github.com/lpsmith/postgresql-simple/pull/66>. Make sure you restart after changing the path!


### Elm

```bash
yarn
yarn run parcel watch assets/elm/index.html -d assets/static --public-url app
```

## Run migrations
Note: On windows, you will need to do this from inside a bash on windows session (enter 'bash' in powershell)

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
  

## Build for distribution
This will generate files and binaries in dist directory for distribution.

```bash
stack build --copy-bins --local-bin-path dist\
yarn run parcel build assets/elm/index.html -d dist/static
```
