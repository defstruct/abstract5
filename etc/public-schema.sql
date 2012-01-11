SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET client_min_messages = warning;

CREATE PROCEDURAL LANGUAGE plpgsql;

CREATE FUNCTION new_schema_and_get_prev_schema(text) RETURNS text
    LANGUAGE plpgsql
    AS $_$
   DECLARE
      the_new_schema alias for $1;
      the_old_schema TEXT;
BEGIN
	select current_schema() into the_old_schema;
	execute	'set search_path to ' || the_new_schema;
        RETURN the_old_schema;
END;$_$;

CREATE SEQUENCE oid_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE admin (
    oid integer DEFAULT nextval('oid_seq'::regclass) NOT NULL PRIMARY KEY,
    name text NOT NULL,
    site_oid integer
);

CREATE TABLE site (
    oid integer DEFAULT nextval('oid_seq'::regclass) NOT NULL PRIMARY KEY,
    name text NOT NULL UNIQUE,
    description text,
    home_folder text,
    db_schema text NOT NULL
);

CREATE TABLE subdomain (
    oid integer DEFAULT nextval('oid_seq'::regclass) NOT NULL PRIMARY KEY,
    name text NOT NULL UNIQUE,
    site_oid integer NOT NULL REFERENCES site(oid)
);

CREATE TABLE repl_entry (
    status text NOT NULL,
    uri_path text NOT NULL,
    uri_filename text,
    reader text NOT NULL,
    evaluator text NOT NULL,
    printer text NOT NULL,
    pathname text,
    env text NOT NULL,
    UNIQUE(uri_path, uri_filename)
);