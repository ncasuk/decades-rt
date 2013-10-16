import psycopg2, psycopg2.extensions
from pydecades.configparser import DecadesConfigParser

def get_database():
    parser = DecadesConfigParser()
    conn = psycopg2.connect (host = parser.get('Database','host'),
                           user = parser.get('Database','user'),
                           password = parser.get('Database','password'),
                           database = parser.get('Database','database'))
    #turn off transactions so the incoming INSERTS do not interfere with each other
    conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT) 
    return conn
