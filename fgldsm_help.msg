.100

Database Schema Manager
-----------------------

This tool is a database schema editor.

The purpose of the database schema is to centralize database table definitions in a unique file (.dbs), and in a unique format, to generate database creation and update scripts for different sort of databases.

The tool can generate SQL scripts for:
  - Genero DB 3.x
  - Informix 9.x, 10.x, 11.x
  - Oracle 9i, 10g
  - DB2 8.x, 9.x
  - SQL Server 2000, 2005
  - MySQL 4.1, 5.x
  - PostgreSQL 8.x

If you have an existing database, the first step is to extract schema information from the database, by using the extraction function of this tool.

Only tables can be managed with this tool. Database objects such as schemas, privileges, synonyms, views, users, triggers and stored procedures are not supported, basically because each database vendor handles such objects in a different manner, which makes it non portable.

The top of the main form lets you define the table name and the PRIMARY KEY constraint name for the current table. 

.101

Column list
-----------

This panel allows you to edit table columns. You can specify the column name, data type, precision, scale, not null constraint and default value.

You can also define if a column is part of the primary key by selecting the checkbox in the PKey column.

You can specify one of more columns for the primary key. The columns of the primary key cannot be the same as for secondary keys.

To identify properly rows in a database, all tables should have a primary key, with a unique indentifier (integer or character string).

Usually only one column is use, because it simplifies the creation of foreign keys, when a reference of the table rows is used in another table.

.102

Unique constraints
------------------

This panel lets you create SECONDARY KEY constraints for the current table. 

You can specify one of more columns for a secondary key. The columns of the secondary key cannot be the same as for the primary key or for another secondary key.

To select columns, put the cursor in the field and click on the button with the pen image.

.103

Foreign key constraints
-----------------------

This panel allows you to define FOREIGN KEY constraints for the current table.

To create a foreign key, you must specify the columns of the main table, the reference table and the columns corresponding to the main columns.

The columns used in the reference table must be part of a primary key or a secondary key. 

.104

Check key constraints
---------------------

This panel lets you add CHECK constraints for the current table.

Check constraints are SQL boolean expressions that will be evaluated when a table row is inserted or updated. If the check expression is false, the database denies the insert or update.

Using check constraints enforces data integrity in the database.

.105

Table options
-------------

This panel allows you to add database vendor specific clauses to create tables. For example, with Oracle you can define in which tablespace the table has to be created, with Informix you can give the extend size...

When generating the SQL script, the SQL text will be added according to the target database selected in the generation dialog.

You may also add performance index creation here. The real common standard index syntax is very limitted, so it makes not much sens to provide a index editor:
Each database vendor defines is specific index extensions, and if you want to take benefit of those extensions, you better use this table option panel.

Note that all indexes attached to a given table will be removed when executing DROP TABLE, so you don't need to add DROP TABLE statements here, since DROP TABLE statements can be generated at the beginning of the SQL script.

.106

Schema extraction
-----------------

By using the extraction function, you can import an existing database schema from a database source.

Note however that you must consider the following cases:

- MySQL constraint names can be duplicated for the same table. This is not possible with databases like Informix, where constraint names must be unique for the whole database/schema. Therefore, when extracting a schema from MySQL, the tool will replace all duplicated constraint names by a unique name.

- When specifying the database owner in the extraction options, it will be used to extract the tables that belong to the specified schema/owner.
If no owner if specified, the tool will use the login name as filter.

The extraction log is written to a file named dbextract.log in current directory.

.107

Tool options
------------

This window allows you to modify database object creation options.

You can define the format for the default names of database objects.

In the format, use %1 to get the ordinal position of the object and %2 to use the table name.

You can also define the default data type and not null constraint.

