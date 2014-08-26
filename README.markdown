# SigmaSQL

A SQL helper library, currently only running on top of
[emysql](https://github.com/Eonblast/Emysql), but with the intention of
eventually running on top a PostgreSQL driver as well.

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
   * A process determines which database to connect to via the `sigma_sql`
     application variable `lookup`.
   * This is used commonly to simplify the process of running a single codebase
     which connects to different database for different users or for different
     host headers.

## Configuration

There is a sample config file in sample.config, but here are the configuration
settings currently available:

```erlang
[
    {sigma_sql, [
        {type, mysql},
        {host, "127.0.0.1"},
        {port, 3306},
        {user, "user"},
        {pass, "userpass"},
        %% There are three different ways to determine database
        %%
        %% 1) All requests go to a single database:
        {lookup, database_name}
        %%
        %% 2) Before a request is made, run a lookup function in another
        %%     module to determine which database:
        {lookup, {lookup_module, lookup_function}}
        %%
        %% 3) Before a request is made, run an anonymous lookup function:
        {lookup, fun() -> returns_an_atom_of_db_name() end}
    ]}
].
```

The most complicated us the `lookup` application variable. Lookup can be one of
three different kinds of values:

  * Atom: That's the database every request will use.
  * {Module, Function}: Call Module:Function() to determine which database to
    connect to.
  * Function (arity 1): Call Function() to determine which database to connect
    to.

## API

### Conventions

#### Abbreviations

Due to my obsession with brevity, almost all function calls are acronyms of
something.  Learning those conventions will save you keystrokes, minimize the
chance for typos, and shorten your code.  The drawback is that it's not
entirely obvious on a cursory glance what a function returns (for example:
`db:fffr` is not exactly obvious that it stands for "(F)irst (F)ield of (F)irst
(R)ecord").

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

#### Prepared Statements?

SigmaSQL currently does not offer prepared statements, but will do safe
variable replacement using a similar convention, replacing `?` in order.

For example:

```erlang
db:q("Select * from login where username=? or email=?", [Loginid, Email])`
```

#### Singular Table Names

I know it's common for database designers to name their tables with the plural
form of a noun to indicate that it's a collection of things (e.g. "logins"
instead of "login"), while still using the singular as the name of key fields
(so a table of logins would be called "logins", but the key would be
"loginid").

Well, despite english being my native language, I find it to be a terribly,
horribly inconsistent language, and refuse to try to make code figure out if
the plural of "child" is "children" or "childs". As such, SigmaSQL makes a lot
of assumptions that your codebase will use a singular table name, and that the
key of that table is named `Tablename ++ "id"`.

Using the example above, my table of logins would be called "login" and the
primary key is "loginid".

#### Insert or Update Determination

There are some helper functions in SigmaSQL that will attempt to determine if
we're updating or inserting a new one.  The basic rule is this: If the key
field specified has a value of `0` or `undefined`, it will be an insert,
assuming the database will do the auto increment for us. If it's anything else,
it's an update.

### Functions

#### One more convention before showing each function:

Almost all query functions in SigmaSQL take one or two parameters.

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

  * `db:q`: The most basic query. Will return a list of rows formatted as
    simple lists.

    ```erlang
    > db:q("select playerid, name from player where race=?", ["elf"]).
    [[2,"Evan"],
     [3,"Marc"]]
    ```

  * `db:tq`: Like `db:q` except returns a list of rows formatted as tuples.
    ```erlang

    > db:tq("select playerid, name from player where race=?", ["elf"]).
    [{2,"Evan"},
     {3,"Marc"}]
    ```

  * `db:plq`: Like `db:q` except returns a list of proplists, with the keys of
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

  * `db:dq`: Like `db:plq`, except returns a list of Erlang `dicts` with the
    keys again being atomized versions of the field names.

##### Single-record Queries

Single-record queries correspond directly to their multi-record queries, except
they only return a single row. They all start with `fr` for "first record"

  * `db:fr`: Like `db:q` (Returns a list)
  * `db:tfr`: Like `db:tq` (Returns a tuple)
  * `db:plfr`: Like `db:plq` (Returns a proplist)
  * `db:dfr`: Like `db:dq` (Returns a dict)

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

#### Insert, Update, Delete Queries

### Insert

  * `db:qi`: Runs the specified query and returns the `insert_id`

### Update

  * `db:qu`: Run the specified query and returns the number of affected rows.

### Update or Delete from a Proplist

  * `db:pl(Table, Keyfield, Proplist)`: Run an update or insert query on the
    Table provided with the specified Proplist as the row data. If the value in
    `Proplist` associated with the Key `Keyfield` is a zero, or is undefined, then
    an insert query is performed. Otherwise, an update query is performed.
    Regardless of insert or update method, the return value is the value of the
    `Keyfield` - if insert, then it returns the new `insert_id`, and if update, the
    value associated with the `Keyfield` from `Proplist`.

  * `db:pl(Table, Proplist)`: Like `db:pl(Table, Keyfield, Proplist)` except
    `Keyfield` is deduced with `list_to_atom(atom_to_list(Table) ++ "id")`

### Delete

  * `db:delete(Table, ID)`: Delete records from a table.

  * `db:delete(Table, Field, ID)`: Delete records from a table.

#### Misc Utilities

  * `db:encode(Term)`: Safely escapes a data for MySQL insertion. Will encode
    the atoms `true` and `false` as `1` and `0` respectively.
 
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

   
## TODO

By v0.1.0
  * Add PostgreSQL support
  * Add PostgreSQL-style variable replacement to be usable for either database.
    Example: `db:q("select * from mytab where a=$1 and b=$1", [123, "somestring"])`
  * Add proper [transaction support](https://github.com/choptastic/sigma_sql/issues/2)

By v0.2.0, maybe
  * Experiment with [record-based querys](https://github.com/choptastic/sigma_sql/issues/1)

## About

Copyright (c) 2013-2014 [Jesse Gumm](http://sigma-star.com/page/jesse)
([@jessegumm](http://twitter.com/jessegumm))

MIT License
