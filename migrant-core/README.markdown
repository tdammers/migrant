# Migrant

Opinionated SQL schema migration management

## Introduction

Migrant instruments SQL schema migrations in a semi-automated way. Writing the
actual up- and downgrade scripts is still a manual effort; but Migrant takes
care of tracking which scripts have been run already and which still need to be
run, runs them for you.

## The SQL Schema Migration Problem

Databases are notoriously difficult to version-control.

Versioning source code is a solved problem: we write code, put it in a
repository, and the source control software gives us a unique identifier for
that exact version. We can now deploy the code in whichever state we want, and
as long as we keep code and data separated, we can do this in a fairly
brute-force manner: we just delete the old code, copy the new code where it
needs to be, and restart what needs to be restarted. Easy. And because we can
deploy any version of the code we want on any host we want, we can test our
code on one machine (a test server), and then deploy it on another (a
production server), and be reasonably sure that if it works on the test
environment, it will also work in production. We can also, just as easily,
revert the code to an older version, if one of our changes turns out to have
introduced a fault.

But with SQL databases, this doesn't work. The schema and the data stored in
the database are intertwined; we want to manage the schema, but we want to do
it such that no data is lost. We cannot simply overwrite the schema: if we
delete a schema, we also delete all the data in it, because in an SQL database,
data cannot exist without the associated schema. Even small changes, such as
changing the type of a column, can be destructive, and so there is a real risk
of permanently losing data as a result of schema changes. And this means we
must be more surgical about our database mutations.

There are two fundamental approaches to this, which I call "snapshot-based" and
"delta-based".

The "snapshot-based" approach stores a snapshot of the schema at a given
version in the source control system; to migrate the database to that version,
it looks at the current schema, and infers the schema changes that are required
to get the schema into the desired state (or a compatible one). For example, if
there is a table `products` in the database that has three columns (`id`,
`name`, `price`), and the version-controlled schema description says it should
have columns `id`, `name`, `price`, `image`, then the migration code infers
that the `image` column must be added.

The "delta-based" approach, which is what Migrant uses, stores descriptions of
the steps required to arrive at the current schema. A migration run, then,
figures out which migration steps have already been executed, and which ones
are needed to get the schema where we want it, and executes the required steps.
In our example above, there may be two upgrade steps: `create-products-table`,
and `add-product-image`. The migration code detects that only the
`create-products-table` step has been run, and decides to run the
`add-product-image` step.

## How Migrant Works

Migrant migrations are implemented using three key parts:

- A `_migrations` table in the database. Migrant creates this table
  automatically; it is used to track which migrations have been run. Migrant
  will never inspect the schema itself or make any guesses: if the migrations
  table says a migration has been run, then Migrant trusts it.
- A list of migrations that represent the desired state of the database.
  Migrant takes this as a list of strings (`[MigrationName]`; `MigrationName`
  is a newtype over `Text`); you can hard-code that list (which means it will
  be committed to source control along with the rest of your code), you can
  load it dynamically at runtime (which means you can manage migrations
  independently from the application code), or you can use the `embed-file`
  package to compile a separate text file into your application code, and
  commit that text file along with the rest of your code. What matters is that
  the list is version controlled, and that is lists migrations in the order you
  want them to be run.
- A set of `up` and `down` migration actions. Migrant takes two functions that
  will be used to look up these scripts, both of type `MigrationName ->
  connection -> IO ()`, where the `connection -> IO ()` part is the function to
  be run for the "up" or "down" migration. Note that there is no way to signal
  a lookup failure other than throwing an exception: this is by design, and
  works fine, because the migration runner code will respond to a failed query
  exactly the same way as it responds to a missing migration script: it aborts,
  and rolls back the entire migration.

## Using Migrant

1. Add `migrant-core` to your project, and one of the backends
   (`migrant-sqlite-simple`, `migrant-postgresql-simple`, or `migrant-hdbc`).
2. Write some glue code to make your application call `migrate`
3. Write schema migrations as pairs of "up" and "down" scripts, and write a
   list that says in which order to run these scripts.
4. To deploy your migrations, compile the project, and make it run the
   `migrate` function. Migrant will now look for a `_migrations` table in the
   database, creating it if necessary, and run "up" and "down" scripts as
   needed to get the database into the state that your application expects.

Example:

```haskell
{-#LANGUAGE OverloadedStrings #-}

import qualified Database.HDBC as HDBC
import Database.Migrant
import Data.Text (Text)

runMigrations :: HDBC.ConnWrapper -> IO ()
runMigrations conn =
  migrate myMigrations migrateUp migrateDown conn

myMigrations :: [MigrationName]
myMigrations =
  [ "create-users-table"
  , "user-email"
  ]

migrateUp :: MigrationName -> HDBC.ConnWrapper -> IO ()
migrateUp name conn = case name of
  "create-users-table" ->
    HDBC.quickQuery
      conn
      "CREATE TABLE users (id INTEGER NOT NULL SERIAL, username TEXT NOT NULL, password BLOB NULL)"
      []
  "user-email" ->
    HDBC.quickQuery
      conn
      "ALTER TABLE users ADD COLUMN email TEXT NULL"
      []

migrateDown :: MigrationName -> HDBC.ConnWrapper -> IO ()
migrateDown name conn = case name of
  "create-users-table" ->
    HDBC.quickQuery
      conn
      "DROP TABLE users"
      []
  "user-email" ->
    HDBC.quickQuery
      conn
      "ALTER TABLE users DROP COLUMN email"
      []
  
```

## Suggested Practices

- Never manipulate the database schema manually. Migrant cannot know about
  manual changes to the database, so anything you do manually may violate the
  assumptions that the up and down scripts are based on. Any changes you make
  for development purposes should go in pairs of "up" and "down" scripts.
- Test your migrations. A good approach is to clone a production database into
  a test environment and run the updated migrations code against that, then
  check if everything works as intended.
- Also test your "down" scripts: after running the "up" scripts, revert the
  code to the previous version, and run another migration. It should undo the
  upgrade, and the database schema should be in the same (or compatible) state
  as it was before the upgrade.
- The migrations list should be considered "append-only". To undo changes that
  have already been deployed, add another script that undoes them, rather than
  deleting an entry from the migrations list.
- Likewise, do not change migration scripts once committed (unless they were
  committed to a branch that isn't integrated with upstream, such as a local
  WIP branch).
- Different branches can introduce conflicting changes. This is why it is
  important to have both the migration scripts and the migrations list in
  source control: conflicts will appear as merge conflicts, and you can
  manually resolve them the usual way, deciding on an appropriate ordering of
  the conflicting scripts.
- If at all possible, write your "up" and "down" scripts such that they are
  reversible and do not lead to data loss. Dropping tables or columns is a
  prime candidate for data loss, and usually not necessary, at least not
  immediately.
- Consider a staged upgrade strategy using three deployment cycles for each
  change:
    1. Upgrade the database to add the new feature
    2. Upgrade client code to use the new feature
    3. Once all clients are upgraded, drop support for the old feature from the
       database.
- To avoid naming conflicts between independent lines of development, it may be
  a good idea to tag migration names with a sufficiently entropic identifier,
  such as a randomized nonce or a fine-grained timestamp.

## Suggested Development Flow

1. In a development environment, write your intended database change, and an
   "undo", as Migrant migration pair, and reference it in your migrations list.
2. Run migrations on the dev environment.
3. Verify that the migration does what you want. If it does, proceed; if not,
   remove the reference from the list, run migrations (this will run the "undo"
   script), and try again. If your "up" script failed, there is no need to roll
   it back. If the "down" script doesn't work, reset your database by cloning a
   production database or starting with a blank database (this will recreate
   the entire thing from scratch, running all the migrations one by one).
4. Once you're happy with the migration script, commit it.
5. When merging, resolve conflicts in the migrations list and test the merged
   version against a development database. Changing the order of entries in the
   list and re-running migrations will roll back changes up to the last common
   situation, and then re-apply the migrations in the order you specified.
6. As a final test before deploying, clone a production database and run all
   migrations on it.
7. To deploy, first install the application code on the server, and then make
   it run migrations.
8. To undo a deployment, the procedure is exactly the same: install application
   code, then run migrations.
