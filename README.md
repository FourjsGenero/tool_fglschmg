# Database schema maintenance tool

## Description

This tool is a database schema definition program (.dbs files)
You can import schemas from existing databases, manage tables, columns,
contraints, indexes, and produce SQL scripts for different type of
databases servers.

![Genero FGL Schema Editor (GDC)](https://github.com/FourjsGenero/tool_fglschmg/raw/master/docs/fglschmg-screen-001.png)

## Prerequisites

* Database server supported by Genero (tested with Informix IDS 12)
* Genero BDL 3.20+
* Genero Browser Client 1.00.52+
* Genero Desktop Client 3.20+
* Genero Studio 3.20+
* GNU Make

## Compilation from command line

1. make clean all

## Compilation in Genero Studio

1. Load the fglschmg.4pw project
2. Build the project

## Usage

1. Start the program
2. Import the schema an existing database
3. Save into dbname.dbs
4. Append/Delete/Modify database table definitions
5. Create SQL scripts

## Bug fixes:


