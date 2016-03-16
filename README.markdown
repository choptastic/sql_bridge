# SQL_Bridge

An Erlang SQL Abstraction layer for interfacing with SQL databases.

### Supported Databases:

	* MySQL
	* PostgreSQL

#### Planned for support

	* Microsoft SQL Server
	* SQLite

## What it does

It provides a simple layer on top of existing SQL drivers, abstracting away the
pool management, and providing sensible default behaviors.  Further, it
provides a whole slew of methods for returning and updating data: lists,
tuples, proplists, dicts, existence checks, single fields returning and
setting, updating data from proplists, and more.

## Conventions

   * A process tracks which database it's connecting to, grabbing any of the
     available database pools. No need to identify a specific pool with
     requests.
   * Connections are made automatically when the first query is attempted
   * A process determines which database to connect to via the `sql_bridge`
     application variable `lookup`.
   * This is used commonly to simplify the process of running a single codebase
     which connects to different database for different users or for different
     host headers.

## Configuration

### `module_alias`

Probably the most unusual configuration to discuss is the `module_alias`.

This configuration option allows us to compile a module which exports all the
functions of `sql_bridge`, but allows us to use a different module name.

In my own apps, I tend to use a module called `db` which serves as the alias to
`sql_bridge`.  While it's easy enough to make your own module and do an
`import`, this configuration parameter allows us to skip that step.

This alias module is generated and loaded during the `sql_bridge:start()`
function.

### `lookup`

The most important configuration variable is the `lookup` variable. This tells
SQL_Bridge which database to use at any given time.

It can take two possible values:

   * An atom of the database name. For single-database apps, this is the simple
     solution: Whatever value you assign to `lookup` will be the database
     SQL_Bridge uses.

   * A {Module, Function} or {Module, Function, Args} tuple. This is for
     multi-database apps.  The return value of Module:Function() or
    `erlang:apply(Module, Function, Args)` will be ysed to determine which database
    to connect to. This value will then be cached within the process dictionary so
    that that (potentially expensive) function isn't repeatedly called within the
    same process.

### `adapter`

As the final important config variable, `adapter` determines which database driver to use.  Currently, SQL_Bridge ships with the following adapters (And the driver upon which it depends)

  + `sql_bridge_epgsql` - [epgsql](http://github.com/epgsql/epgsql) - The
    Erlang PostgreSQL driver (also uses poolboy).
  + `sql_bridge_mysql_otp` - [mysql-otp](http://github.com/mysql-otp/mysql-otp) -
     A New MySQL driver (also uses poolboy).
  + `sql_bridge_emysql` - [emysql](http://github.com/eonblast/emysql) - The
    (mostly abandoned) Emysql driver.

### Sample Config

There is a sample config file in
[sample.config](https://github.com/choptastic/sigma_sql/blob/master/sample.config),
but here are the configuration settings currently available:

```erlang
[
    {sql_bridge, [
        %% module_alias creates a module with the specified name, which can be
        %% used as an alias to the sigma_sql module.
        {module_alias, db},

        %% There are three adapters that can be used:
        %%  + sql_bridge_epgsql: PostgreSQL adapter using epgsql
        %%  + sql_bridge_mysql_otp: MySQL adapter using mysql-otp
        %%  + sql_bridge_emysql: MySQL adapter using the (mostly abandoned) Emysql driver.
        {adapter, sql_bridge_mysql_otp},

        %% connection parameters (self explanitory, I hope)
        {host, "127.0.0.1"},
        {port, 3306},
        {user, "user"},
        {pass, "userpass"},

        %% There are two different ways to determine database
        %%
        %% 1) All requests go to a single database, called 'database_name':
        {lookup, database_name}
        %%
		%% 2) Before a request is made, run the function
        %% `lookup_module:lookup_function()`, for which the return value will
        %% be the database name
        {lookup, {lookup_module, lookup_function}},

		%% Number of connections to establish per pool (which really means
		%% number of connections per database).
		{connections_per_pool, 10},

        %% If a connection pool is saturated, this allows additional "overflow"
        %% connections to be established up to the limit specified below.
        {overflow_connections_per_pool, 10},

        %% If you prefer your string types (varchars, texts, etc) to be returned as erlang lists rather than binaries, set the following to true:
        {stringify_binaries, false}
    ]}
].
```

The most complicated us the `lookup` application variable. Lookup can be one of
three different kinds of values:

  * Atom: That's the database every request will use.
  * {Module, Function}: Call Module:Function() to determine which database to
    connect to.

## API

### Conventions

#### Abbreviations

Due to my obsession with brevity, most all function calls have hyper-terse
versions which are acronyms of something.  Learning those conventions will save
you keystrokes, minimize the chance for typos, and shorten your code.  The
drawback is that it's not entirely obvious on a cursory glance what a function
returns (for example: `db:fffr` is not exactly obvious that it stands for
"(F)irst (F)ield of (F)irst (R)ecord").

But you'll learn common shortcuts:

  * `q` -> query
  * `t` -> tuple
  * `d` -> dict
  * `l` -> list
  * `pl` -> proplist
  * `ff` -> first field
  * `fr` -> first record
  * `i` -> insert
  * `u` -> update


**Conveniently, however,** There are also simpler, more semantic function
names, like `list`, `maps`, `proplist`, etc, which return exactly what the name
implies. All is documented below.

#### Prepared Statements?

SQL_Bridge currently does not offer prepared statements, but will do safe
variable replacement using a similar convention, either with MySQL's `?`
placeholder, or PostgreSQL's `$1, $2,...$X` placeholder.

##### Replacement Placeholders

Which placeholder is used can be modified by the configuration variable
`replacement_token_style`.  This value can be the atoms 'mysql' or 'postgres'
or it could also be the shortened version with the atom '?' or '$'
respectively.

Sample MySQL Placeholders:

```erlang
db:q("Select * from login where username=? or email=?", [Loginid, Email])
```

Sample PostgreSQL Placeholders:

```erlang
db:q("Select * from login where username=$1 or email=$2", [Loginid, Email])
```

#### Singular Table Names

I know it's common for database designers to name their tables with the plural
form of a noun to indicate that it's a collection of things (e.g. "logins"
instead of "login"), while still using the singular as the name of key fields
(so a table of logins would be called "logins", but the key would be
"loginid").

Well, despite english being my native language, I find it to be a terribly,
horribly inconsistent language, and refuse to try to make code figure out if
the plural of "child" is "children" or "childs". As such, SQL_Bridge makes a lot
of assumptions that your codebase will use a singular table name, and that the
key of that table is named `Tablename ++ "id"`.

Using the example above, my table of logins would be called "login" and the
primary key is "loginid".

#### Insert or Update Determination

There are some helper functions in SQL_Bridge that will attempt to determine if
we're updating or inserting a new one.  The basic rule is this: If the key
field specified has a value of `0` or `undefined`, it will be an insert,
assuming the database will do the auto increment for us. If it's anything else,
it's an update.

### Functions

#### One more convention before showing each function:

Almost all query functions in SQL_Bridge take one or two parameters.

  * 1 Argument: the query will be executed as-is. (e.g. `db:q("select * from
    whatever")`)
  * 2 Arguments: Argument two should be a list of arguments that correspond to
    and will replace question marks (`?`) within the query itself in order.
    (e.g. `db:q("select * from whatever where field1=? or field1=?", [SomeValue,
    SomeOtherValue])`)

#### Table Structure for our examples

For our example, we're going to have a table called `player`:

```
+----------+-----------------------------------+------+-----+---------+----------------+
| Field    | Type                              | Null | Key | Default | Extra          |
+----------+-----------------------------------+------+-----+---------+----------------+
| playerid | int(10) unsigned                  | NO   | PRI | NULL    | auto_increment |
| name     | varchar(40)                       | YES  |     | NULL    |                |
| race     | enum('dwarf','orc','elf')         | YES  |     | NULL    |                |
| class    | enum('wizard','archer','bruiser') | YES  |     | NULL    |                |
| level    | int(10) unsigned                  | YES  |     | 1       |                |
| alive    | tinyint(1)                        | NO   |     | 1       |                |
+----------+-----------------------------------+------+-----+---------+----------------+
```

#### Select Queries

##### Multi-record Queries

  * `db:lists` or `db:q`: The most basic query. Will return a list of rows formatted as
    simple lists.

    ```erlang
    > db:q("select playerid, name from player where race=?", ["elf"]).
    [[2,"Evan"],
     [3,"Marc"]]
    ```

  * `db:tuples` or `db:tq`: Like `db:q` except returns a list of rows formatted as tuples.
    ```erlang

    > db:tq("select playerid, name from player where race=?", ["elf"]).
    [{2,"Evan"},
     {3,"Marc"}]
    ```

  * `db:proplists` or `db:plq`: Like `db:q` except returns a list of proplists, with the keys of
    which are atomized versions of the database field names:

    ```erlang
    > db:plq("select name, race, level from player where alive=?",[false]).
    [[{name,"Rusty"},
      {race,"dwarf"},
      {level,35}],
     [{name,"Justin"},
      {race,"orc"},
      {level,15}]]
    ```

  * `db:dicts` or `db:dq`: Like `db:plq`, except returns a list of Erlang `dicts` with the
    keys again being atomized versions of the field names.

  * `db:maps` or `db:mq`: Like `db:plq`, except returns a list of Erlang `maps`, with keys
    atomized versions of field names.

##### Single-record Queries

Single-record queries correspond directly to their multi-record queries, except
they only return a single row. They all start with `fr` for "first record"

  * `db:list` or `db:fr`: Like `db:q` (Returns a list)
  * `db:tuple` or `db:tfr`: Like `db:tq` (Returns a tuple)
  * `db:proplist` or `db:plfr`: Like `db:plq` (Returns a proplist)
  * `db:dict` or `db:dfr`: Like `db:dq` (Returns a dict)
  * `db:map` or `db:mfr`: List `db:mq` (Returns a map)

##### Other convenience queries

  * `db:fffr`: (F)irst (F)ield of (F)irst (R)ecord. Returns the first field of
    the first record returned.

    ```erlang
    > db:fffr("select count(*) from player where class=?",[wizard]).
    2
    ```

  * `db:ffl`: (F)irst (F)ield (L)ist. Returns a list of the first field from
    each row.

    ```erlang
    > db:ffl("select playerid from player where alive=? or class=?",[true,wizard]).
    [1,2,3,5]
    ```

  * `db:qexists`: Returns `true` or `false` depending on whether or not the
    query returns any records.

    ```erlang
    > db:qexists("select playerid from player where playerid=?",[999]).
    false
    ```

  * `db:exists(Table, IDField, IDValue)`: Returns true or false depending on
    whether or not a record in `Table` exists where the specified `IDField` has
	the value `IDValue`.

  * `db:exists(Table, IDValue)`: Shortcut for `db:exists(Table, Table ++ "id",
    IDValue)`

  * `db:field(Table, Field, IDValue)`: Returns the value of the field `Field`
    from table `Table`, where the TableID value is `IDValue`.

    ```erlang
    > db:field(player, race, 1).
    "dwarf"
    ```
	The above is the equivilant to `db:fffr("select race from player where
	playerid=1")`

  * `db:field(Table, Field, IDField, IDValue)`

	Like `db:field/3`, except you get to specify which field you're querying
	for instead of assuming `Table ++ "id"` as the ID field.

  * `db:fields(Table)`: Returns a list of the names of the fields of the named
    `Table`

    ```erlang
    > db:fields(player).
    [playerid, name, race, class, level, alive]
    ```

#### Insert, Update, Delete Queries

### Insert

  * `db:qi` or `db:insert` Runs the specified query and returns the `insert_id`

### Update

  * `db:qu` or `db:update`: Run the specified query and returns the number of affected rows.

### Update or Delete from a Proplist, Map, or Record


  * `db:save(Table, Keyfield, Data)`: Run an update or insert query on the
    Table provided with the specified Data as the row data. `Data` can be
	either a proplist, a map, or a record (See *Workering with Records* below).
    If the value in `Data` associated with the Key `Keyfield` is a
    zero, or is undefined, then an insert query is performed. Otherwise, an update
    query is performed.  Regardless of insert or update method, the return value is
    the value of the `Keyfield` - if insert, then it returns the new `insert_id`,
	and if update, the value associated with the `Keyfield` from `Data`.

  * `db:save(Table, Data)`: Like `db:save(Table, Keyfield, Data)` except
    `Keyfield` is deduced with `list_to_atom(atom_to_list(Table) ++ "id")`

#### Working with Records

SQL_Bridge can work with records, however, since records are done at compile time, there are some additional steps that must be performed by you in order to accomplish this.  The simplest is to use the `save_record()` functions:

  * `db:save_record(Table, KeyField, Record, FieldList)`: In order to call this effectively, you must pass the return value of the built-in compile function `record_info(fields, RECORDNAME)` as the argument for `FieldList`. For example, if you have a record called `#foo` that is saved into the table `foo_tbl` you could save it like this:  `db:save_record(foo_tbl, fooid, FooRec, record_info(fields, foo))`

  * `db:save_record(Table, Record, FieldList`: Like `db:save/2`, this will automatically determine the KeyField as `list_to_atom(atom_to_list(Table) ++ "id")`.

#### Using a Record Handler

SQL_Bridge also has an option to intelligently convert records into a format SQL_Bridge can work with (namely, proplists and maps). You can do this by use a `record_handler` configuration option.  If the `record_handler` configuration option is specified in the sql_bridge.config file, it will call that specified function passing the record as an option.

To use this, the value of `record_handler` must be a 2-tuple of the format `{Module, Function}`, where `Module:Function` is a function of arity 1 and returns a proplist or map.

The simplest example would be to make a module in your app like:

```erlang
-module(my_record_handler).
-export([handle/1]).

handle(Foo = #foo{}) ->
	sql_bridge_utils:record_to_proplist(Foo, record_info(fields, foo));
handle(Bar = #bar{}) ->
	sql_bridge_utils:record_to_proplists(Bar, record_info(fields, bar)).
```

Then, in your config, set the `record_handler` value as follows:

```erlang
[{sql_bridge, [
	...
	{record_handler, {my_record_handler, handle}}
]}].
```

Once this is done, you can pass a record as the `Data` argument in `db:save/2-3`

### Delete

  * `db:delete(Table, ID)`: Delete records from a table.

  * `db:delete(Table, Field, ID)`: Delete records from a table.

## Transactions

SQL_Bridge supports transactions through two mechanisms:

  1. `db:start_trans()`, `db:commit()`, and `db:rollback()` - Manually initiate
	 a transaction. Note, if you run something like `db:q("BEGIN")`, SQL_Bridge
    is not intelligent enough to determine that you're in a transaction. Please use
    `db:start_trans()`.
  2. `db:trans(Fun)` - Mnesia-style transactions where the contents of the
	 function are run within a transaction.  Note that `Fun` is of arity 0
    (that is, no arguments). If the function completes successfully, the queries
    executed will be commited, and the return value of the `Fun()` will be the
    return value of `db:trans(Fun)`.  If `Fun()` crashes, the transaction will be
    automatically rolled back, and the return value will be `{error, Reason}`,
    where `Reason` is information about the crash (including a stacktrace).

## Misc Utilities

  * `db:encode(Term)`: Safely escapes a data for database interaction on in a
    SQL query, for the backend of your choice.
 
  * `db:encode_list(List)`: Takes a list of terms and safely encodes them for
    mysql interaction, separating them with commas. 

  * `db:encode64(Term)`

  * `db:decode64(Term)`: Encodes and decodes any Erlang term to base64.

  * `db:limit_clause(PerPage, Page)`: When you're doing a query that needs
    pagination, sometimes you just don't want to deal with figuring out the
    limit clause's offset and length.  In this case, you can build a simple limit
    clause for MySQL by passing this function which page you want to show (start at
    page 1), and how many items you per page you want to show. It will do the
    offset calculation for you and return a limit clause that can be inserted into
    the query.

## Known Issues

There are a number of inconsistencies between backends with regard to handling
date formats and decimal values.  Some backends use `{2016,4,3}` while others
will return "2016-04-03".  We need to ensure that the return values are
consistent between backends.
   
## TODO

### v0.2.0 
  * Maybe Experiment with [record-based querys](https://github.com/choptastic/sql_bridge/issues/1)

## About

Copyright (c) 2013-2016 [Jesse Gumm](http://sigma-star.com/page/jesse)
([@jessegumm](http://twitter.com/jessegumm))

[MIT License](https://github.com/choptastic/sql_bridge/blob/master/LICENSE)
